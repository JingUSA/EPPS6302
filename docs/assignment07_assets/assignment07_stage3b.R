# EPPS 6302 — Assignment 7, Stage 3B. Offline analysis only.
setwd(path.expand('~/Downloads/EPPS6302'))
suppressPackageStartupMessages({library(tidyverse); library(quanteda); library(quanteda.textstats)})
root <- 'data/youtube'; out <- file.path(root,'analysis'); priv <- file.path(root,'reading_private')
dir.create(out,showWarnings=FALSE,recursive=TRUE)
fig <- file.path(out,'figures'); dir.create(fig,showWarnings=FALSE)
stopifnot(system2('git',c('check-ignore','-q','data/youtube/probe.csv'))==0L)
clock <- readRDS(file.path(root,'collection_clock.rds'))
stopifnot(as.Date(clock$delete_by)>as.Date(Sys.time(),tz='America/Chicago'))
write <- function(x,n) readr::write_csv(x,file.path(out,paste0(n,'.csv')))
read_labels <- function(n) read_csv(file.path(priv,n),col_types=cols(.default=col_character()),show_col_types=FALSE)
kw <- read_labels('kwic_review.csv')
manual <- read_labels('sentiment_review.csv')
kw$category <- tolower(trimws(kw$category)); manual$human_label <- tolower(trimws(manual$human_label))
stopifnot(nrow(kw)==100L,!anyDuplicated(kw$sample_id),all(kw$category %in% c('party','candidate','process','grievance','other')),all(table(kw$pattern)==20L),nrow(manual)==40L,!anyDuplicated(manual$comment_id),!anyDuplicated(manual$sample_id),all(manual$human_label %in% c('positive','negative','neither')))
kwic_coding <- kw |> count(pattern,category,name='n') |> complete(pattern,category=c('party','candidate','process','grievance','other'),fill=list(n=0L))
write(kwic_coding,'kwic_coding')
# Restore originals from RDS: Excel may change text encoding or interpret strings as dates.
y <- readRDS(file.path(root,'comments.rds')) |> as_tibble()
stopifnot(!anyDuplicated(y$comment_id),!anyNA(y$period),all(y$period %in% c('before','after')))
texts <- y |> filter(!is.na(text_original),nzchar(str_trim(text_original))) |> arrange(comment_id)
stopifnot(all(manual$comment_id %in% texts$comment_id))
# Strip HTML line breaks/tags, URLs and mentions; preserve original text in local cache only.
texts$text <- str_replace_all(texts$text_original,'(?i)<br\\s*/?>|<[^>]+>',' ')
texts$text <- str_replace_all(texts$text,'https?://[^[:space:]]+|www\\.[^[:space:]]+',' ')
texts$text <- str_replace_all(texts$text,'(?<![[:alnum:]_])@[[:alnum:]_.-]+',' ')
corp <- corpus(texts |> select(comment_id,text,channel_title,video_id,is_reply,like_count,date,period,comment_period),text_field='text',docid_field='comment_id')
base_toks <- tokens(corp,remove_punct=TRUE,remove_url=TRUE,remove_symbols=TRUE) |> tokens_tolower()
# Explicitly remove emoji/pictographs if retained by tokenizer; do not strip letters with accents.
base_toks <- tokens_remove(base_toks,pattern='[\\p{So}\\x{1F1E6}-\\x{1FAFF}\\x{FE0F}\\x{200D}]',valuetype='regex')
# Two extra phrases seen in the KWIC review screenshot (verify their corpus frequencies below).
extra_phrases <- c('republican party','democratic senator')
phrases <- c('house majority','senate race','early voting','mail in','election fraud',extra_phrases)
# Normalize the hyphenated spelling of mail-in into the same compound.
pt <- tokens_replace(base_toks,pattern='mail-in',replacement='mail_in',valuetype='fixed')
pt <- tokens_compound(pt,pattern=phrase(phrases),concatenator='_')
phrase_dfm <- dfm(pt)
phrase_names <- gsub(' ','_',phrases,fixed=TRUE)
pd <- dfm_match(phrase_dfm,features=phrase_names)
phrase_counts <- tibble(phrase=phrases,frequency=as.numeric(colSums(pd)),documents=as.numeric(colSums(pd>0)))
write(phrase_counts,'phrase_counts')
# Keyness: unweighted counts of individual comments; trim only words occurring in fewer than 2 documents.
d <- dfm(tokens_remove(pt,stopwords('en'))) |> dfm_trim(min_docfreq=2)
keep <- ntoken(d)>0; d <- d[keep,]
meta <- as_tibble(docvars(d)); stopifnot(nrow(meta)==ndoc(d))
analysis_audit <- tibble(collected=nrow(y),nonblank=nrow(texts),keyness_documents=ndoc(d),keyness_features=nfeat(d),keyness_empty_excluded=sum(!keep),sentiment_sample=nrow(manual),kwic_sample=nrow(kw))
write(analysis_audit,'analysis_audit')
key <- function(mat,group,target,comparison){
  stopifnot(length(group)==ndoc(mat),!anyNA(group),length(unique(group))==2L,target %in% group)
  g <- dfm_group(mat,groups=group)
  k <- as_tibble(textstat_keyness(g,target=match(target,docnames(g)),measure='chi2'))
  k |> mutate(comparison=comparison,target=target,.before=1)
}
channels <- sort(unique(meta$channel_title)); stopifnot(length(channels)==3L)
channel_keyness <- map_dfr(channels,function(ch) key(d,ifelse(meta$channel_title==ch,ch,'Other two'),ch,paste(ch,'vs other two')))
period_keyness <- key(d,meta$period,'after','Video publication: after vs before')
ix <- meta$comment_period %in% c('before','after')
comment_date_keyness <- key(d[ix,],meta$comment_period[ix],'after','Comment publication: after vs before')
write(channel_keyness,'channel_keyness'); write(period_keyness,'period_keyness'); write(comment_date_keyness,'comment_date_keyness')
key_top <- function(k,n=15L) bind_rows(k |> filter(chi2>0) |> slice_max(chi2,n=n,with_ties=FALSE),k |> filter(chi2<0) |> slice_min(chi2,n=n,with_ties=FALSE))
keyness_top <- bind_rows(channel_keyness,period_keyness,comment_date_keyness) |> group_by(comparison) |> group_modify(~key_top(.x)) |> ungroup()
write(keyness_top,'keyness_top')
for(cmp in unique(keyness_top$comparison)){
 z <- keyness_top |> filter(comparison==cmp) |> mutate(feature=reorder(feature,chi2),direction=if_else(chi2>0,'Target','Reference'))
 p <- ggplot(z,aes(chi2,feature,fill=direction))+geom_col()+labs(x='Signed chi-square',y=NULL,title=cmp)+theme_minimal(base_size=12)+theme(legend.position='bottom')
 ggsave(file.path(fig,paste0('keyness_',gsub('[^a-z0-9]+','_',tolower(cmp)),'.png')),p,width=8,height=7,dpi=160)
}
# LSD2015 dictionary with negation compounds, following quanteda documentation.
# No stopword removal here, because it can delete negations.
data(data_dictionary_LSD2015,package='quanteda')
st <- tokens_compound(base_toks,data_dictionary_LSD2015)
sd <- dfm_lookup(dfm(st),dictionary=data_dictionary_LSD2015,valuetype='glob')
sd <- dfm_match(sd,features=c('negative','positive','neg_positive','neg_negative'))
sm <- as.matrix(sd)
scores <- tibble(comment_id=docnames(sd),positive=as.numeric(sm[,'positive']+sm[,'neg_negative']),negative=as.numeric(sm[,'negative']+sm[,'neg_positive']),tokens=as.numeric(ntoken(base_toks))) |> mutate(net=positive-negative,dict_label=case_when(net>0~'positive',net<0~'negative',TRUE~'neither'))
scores <- scores |> left_join(texts |> select(comment_id,channel_title,period,comment_period),by='comment_id',relationship='one-to-one')
stopifnot(nrow(scores)==nrow(texts),!anyNA(scores$period))
sent_summary <- function(s,period_col){
 s |> group_by(channel_title,period=.data[[period_col]]) |> summarise(comments=n(),positive_words=sum(positive),negative_words=sum(negative),tokens=sum(tokens),no_matches=sum(positive+negative==0),positive_comments=sum(dict_label=='positive'),negative_comments=sum(dict_label=='negative'),neither_comments=sum(dict_label=='neither'),net_per_100_words=if_else(tokens>0,100*(positive_words-negative_words)/tokens,NA_real_),.groups='drop')
}
sentiment_summary <- sent_summary(scores,'period')
sentiment_comment_date <- sent_summary(scores |> filter(comment_period %in% c('before','after')),'comment_period')
write(sentiment_summary,'sentiment_summary'); write(sentiment_comment_date,'sentiment_comment_date')
sentiment_plot <- sentiment_summary |> select(channel_title,period,comments,positive_comments,negative_comments,neither_comments) |> pivot_longer(ends_with('_comments'),names_to='label',values_to='n') |> mutate(share=n/comments,label=sub('_comments$','',label))
p <- ggplot(sentiment_plot,aes(period,share,fill=label))+geom_col()+facet_wrap(~channel_title)+scale_y_continuous(labels=scales::percent)+labs(x='Video publication period',y='Share of comments',fill='Dictionary label')+theme_minimal(base_size=12)
ggsave(file.path(fig,'sentiment_by_channel_period.png'),p,width=9,height=4.8,dpi=160)
# Validation is against AI-assisted labels reviewed by the student, not a second independent human coder.
v <- manual |> select(sample_id,comment_id,human_label) |> left_join(scores |> select(comment_id,dict_label,positive,negative),by='comment_id',relationship='one-to-one')
stopifnot(nrow(v)==40L,!anyNA(v$dict_label))
lev <- c('positive','negative','neither'); cm <- table(reference=factor(v$human_label,levels=lev),dictionary=factor(v$dict_label,levels=lev))
agreement <- sum(diag(cm))/sum(cm); expected <- sum(rowSums(cm)*colSums(cm))/sum(cm)^2
validation_summary <- tibble(n=nrow(v),matches=sum(diag(cm)),agreement=agreement,expected_agreement=expected,cohen_kappa=if(expected<1)(agreement-expected)/(1-expected) else NA_real_,reference='AI-assisted coding; student review required')
validation_confusion <- as_tibble(as.data.frame(cm)); write(validation_summary,'validation_summary'); write(validation_confusion,'validation_confusion')
# Local-only mismatches: inspect against original RDS, never export the text or comment IDs.
validation_private <- v |> filter(human_label!=dict_label) |> left_join(texts |> select(comment_id,text_original),by='comment_id',relationship='many-to-one')
write_csv(validation_private,file.path(priv,'validation_mismatches.csv'))
# Coverage and concentration plots use existing Stage 3A outputs.
cov <- read_csv(file.path(out,'coverage_audit.csv'),show_col_types=FALSE)
if(all(c('received','reported_comments','channel_title','period') %in% names(cov))){
 cp <- cov |> mutate(video_rank=row_number(),video=paste0('V',sprintf('%02d',video_rank))) |> select(video,channel_title,period,received,reported_comments) |> pivot_longer(c(received,reported_comments),names_to='series',values_to='n')
 p <- ggplot(cp,aes(video,n,fill=series))+geom_col(position='dodge')+facet_wrap(~channel_title,scales='free_x')+scale_y_continuous(trans='log1p')+labs(x='Video',y='Comments (log1p scale)',fill=NULL)+theme_minimal(base_size=11)
 ggsave(file.path(fig,'comment_coverage.png'),p,width=10,height=5,dpi=160)
}
print(analysis_audit,width=Inf); print(kwic_coding,n=Inf); print(phrase_counts,n=Inf); print(sentiment_summary,n=Inf,width=Inf); print(validation_summary,width=Inf); print(cm)
print(keyness_top |> group_by(comparison) |> slice_max(abs(chi2),n=8,with_ties=FALSE) |> ungroup(),n=Inf,width=Inf)
cat('\nStage 3B complete. No API requests. Only aggregate outputs under data/youtube/analysis; review mismatches locally.\n')
