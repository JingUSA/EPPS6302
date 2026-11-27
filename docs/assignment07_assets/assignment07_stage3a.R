# EPPS 6302 Assignment 7 — Stage 3A: local audit and blinded reading samples
# No API requests. All comment-level data remain under the ignored data/youtube directory.
setwd(path.expand('~/Downloads/EPPS6302'))
suppressPackageStartupMessages({library(tidyverse); library(quanteda); library(quanteda.textstats)})
root <- 'data/youtube'
stopifnot(system2('git',c('check-ignore','-q','data/youtube/probe.csv'))==0L)
clock <- readRDS(file.path(root,'collection_clock.rds'))
stopifnot(as.Date(clock$delete_by)>as.Date(Sys.time(),tz='America/Chicago'))
y <- readRDS(file.path(root,'comments.rds')) |> as_tibble()
log <- read_csv(file.path(root,'collection_log.csv'),show_col_types=FALSE)
stopifnot(!anyDuplicated(y$comment_id), !anyNA(y$period), all((y |> count(video_id))$n<=300L))
out <- file.path(root,'analysis'); dir.create(out,showWarnings=FALSE)
private <- file.path(root,'reading_private'); dir.create(private,showWarnings=FALSE)
save_table <- function(x,name) write_csv(x,file.path(out,paste0(name,'.csv')))
# Counts exclude missing likes from like-distribution denominators, and missing author IDs from authorship.
auth_col <- intersect(c('author_channel_id','author_id','author_channelId','authorChannelId','author_channel_url','author_channel'),names(y))
if(!length(auth_col)) stop('No author ID column found. Run names(y) and inspect locally; do not substitute display names.')
author <- y[[auth_col[1]]]
if(is.list(author)) author <- vapply(author,function(a) if(length(a)) as.character(a[[1]]) else NA_character_,character(1))
y$author_key_private <- as.character(author)
y$likes_numeric <- suppressWarnings(as.numeric(y$like_count))
concentration <- function(d){
  lk <- sort(d$likes_numeric[is.finite(d$likes_numeric)],decreasing=TRUE)
  a <- d |> filter(!is.na(author_key_private),nzchar(author_key_private)) |> count(author_key_private,sort=TRUE)
  k <- ceiling(length(lk)*.01); j <- ceiling(nrow(a)*.10)
  total_likes <- sum(lk); known_author_comments <- sum(a$n)
  tibble(comments=nrow(d),replies=sum(d$is_reply,na.rm=TRUE),reply_share=mean(d$is_reply,na.rm=TRUE),
         likes_observed=length(lk),likes_missing=nrow(d)-length(lk),total_likes=total_likes,
         likes_zero=sum(lk==0),likes_median=if(length(lk)) median(lk) else NA_real_,
         likes_p90=if(length(lk)) as.numeric(quantile(lk,.9)) else NA_real_,
         likes_p99=if(length(lk)) as.numeric(quantile(lk,.99)) else NA_real_,
         likes_max=if(length(lk)) max(lk) else NA_real_,top_1pct_n=k,
         top_1pct_like_share=if(total_likes>0) sum(head(lk,k))/total_likes else NA_real_,
         distinct_authors=nrow(a),known_author_comments=known_author_comments,
         missing_author_comments=nrow(d)-known_author_comments,top_10pct_authors_n=j,
         top_10pct_author_comment_share=if(known_author_comments>0) sum(head(a$n,j))/known_author_comments else NA_real_,
         repeated_text_rows=sum(duplicated(d$text_original[!is.na(d$text_original)])),
         missing_text=sum(is.na(d$text_original)),blank_text=sum(!is.na(d$text_original)&!nzchar(trimws(d$text_original))))
}
overall_audit <- concentration(y)
by_channel_period <- y |> group_by(channel_title,period) |> group_modify(~concentration(.x)) |> ungroup()
coverage_audit <- log |> select(any_of(c('video_id','channel_title','period','status','reported_comments','received','coverage','collected_at')))
status_counts <- log |> count(status,name='videos')
status_channels <- log |> count(channel_title,status,name='videos')
save_table(overall_audit,'overall_audit'); save_table(by_channel_period,'channel_period_audit')
save_table(coverage_audit,'coverage_audit'); save_table(status_counts,'status_counts'); save_table(status_channels,'status_channels')
# Text is retained exactly in the private sample; URL and @mention removal is only for token analysis.
# Emoji are retained in the original and KWIC context; removed from the modeling vocabulary in Stage 3B.
texts <- y |> filter(!is.na(text_original),nzchar(str_trim(text_original))) |> arrange(comment_id)
texts$text <- str_replace_all(texts$text_original,'https?://[^[:space:]]+|www\\.[^[:space:]]+',' ')
texts$text <- str_replace_all(texts$text,'(?<![[:alnum:]_])@[[:alnum:]_.-]+',' ')
texts$text[!nzchar(str_trim(texts$text))] <- ' '
corpus_yt <- corpus(texts |> select(comment_id,text,channel_title,video_id,is_reply,like_count,date,period,comment_period),text_field='text',docid_field='comment_id')
kw_tokens <- tokens(corpus_yt,remove_punct=TRUE,remove_url=TRUE) |> tokens_tolower()
patterns <- c('democrat*','republican*','trump','vote*','ballot*')
set.seed(6302)
kwic_sample <- map_dfr(patterns,function(p){
  all <- as.data.frame(kwic(kw_tokens,pattern=p,valuetype='glob',window=6)) |> as_tibble()
  n_matches <- nrow(all)
  if(n_matches==0L) return(tibble(pattern=character(),sample_id=character(),pre=character(),keyword=character(),post=character(),category=character()))
  all <- all[sample.int(n_matches,min(20L,n_matches)),,drop=FALSE]
  tibble(pattern=p,sample_id=paste0(p,'_',seq_len(nrow(all))),pre=all$pre,keyword=all$keyword,post=all$post,category=rep('',nrow(all)))
})
kwic_counts <- map_dfr(patterns,function(p){
 k <- kwic(kw_tokens,pattern=p,valuetype='glob',window=6)
 tibble(pattern=p,matches=nrow(k),documents=length(unique(k$docname)),sampled=min(20L,nrow(k)))
})
save_table(kwic_counts,'kwic_counts')
kwfile <- file.path(private,'kwic_review.csv')
if(!file.exists(kwfile)) write_csv(kwic_sample,kwfile)
# Independent simple random sample of 40 nonempty comments; no machine labels shown.
set.seed(630207)
stopifnot(nrow(texts)>=40L)
manual <- texts[sample.int(nrow(texts),40L),] |> transmute(sample_id=sprintf('S%02d',row_number()),comment_id,text=text_original,human_label='',reason='')
manual_file <- file.path(private,'sentiment_review.csv')
if(!file.exists(manual_file)) write_csv(manual,manual_file)
# Optional console review helper: records each decision immediately; resumes on rerun.
review_kwic <- function(){
  f <- read_csv(kwfile,show_col_types=FALSE,col_types=cols(.default=col_character()))
  for(i in which(is.na(f$category)|!nzchar(f$category))){
    cat('\n',i,'/',nrow(f),' [',f$pattern[i],'] ',f$pre[i],' >>> ',f$keyword[i],' <<< ',f$post[i],'\n',sep='')
    repeat { a <- tolower(trimws(readline('1 party | 2 candidate | 3 process | 4 grievance | 5 other/unclear | q quit: '))); if(a %in% c('1','2','3','4','5','q')) break }
    if(a=='q') break
    f$category[i] <- c('party','candidate','process','grievance','other')[as.integer(a)]
    write_csv(f,kwfile)
  }
  print(f |> count(pattern,category),n=Inf)
  invisible(f)
}
review_sentiment <- function(){
  f <- read_csv(manual_file,show_col_types=FALSE,col_types=cols(.default=col_character()))
  for(i in which(is.na(f$human_label)|!nzchar(f$human_label))){
    cat('\n',f$sample_id[i],'/40: ',f$text[i],'\n',sep='')
    repeat { a <- tolower(trimws(readline('1 positive | 2 negative | 3 neither/mixed/unclear | q quit: '))); if(a %in% c('1','2','3','q')) break }
    if(a=='q') break
    f$human_label[i] <- c('positive','negative','neither')[as.integer(a)]
    f$reason[i] <- readline('Short reason (optional; no names): ')
    write_csv(f,manual_file)
  }
  print(f |> count(human_label),n=Inf)
  invisible(f)
}
print(overall_audit,width=Inf); print(by_channel_period,n=Inf,width=Inf)
print(status_counts); print(kwic_counts,n=Inf,width=Inf)
cat('\nStage 3A complete. No API calls or keyness/sentiment model yet.\n')
cat('Read local KWIC sample via review_kwic(); code random comments via review_sentiment().\n')
cat('Private files:',kwfile,'and',manual_file,'\nDo not upload either file or raw text.\n')
