# EPPS 6302 - Assignment 7, Stage 1: quota plan and four ranked searches
# Run from project root. All API data remain in gitignored data/youtube/.
setwd(path.expand('~/Downloads/EPPS6302'))
suppressPackageStartupMessages({library(tuber); library(tidyverse); library(jsonlite)})
stopifnot(packageVersion('tuber') >= '2.0.0.9000', packageVersion('httr2') >= '1.1.0')
if (!nzchar(Sys.getenv('YOUTUBE_KEY')) && file.exists(path.expand('~/.Renviron'))) readRenviron(path.expand('~/.Renviron'))
stopifnot(nzchar(Sys.getenv('YOUTUBE_KEY')))
for (p in c('data/youtube/probe.csv', '.Renviron')) {
  status <- system2('git', c('check-ignore','-q',p))
  if (status != 0L) stop('Not gitignored: ',p)
}
root <- 'data/youtube'; dir.create(root, recursive=TRUE, showWarnings=FALSE)
# Reserve budgets BEFORE any API call; estimates/allowances, not measured use.
quota_plan <- tibble(endpoint=c('search.list','channels.list','playlistItems.list','videos.list','commentThreads.list + comments.list'),
 purpose=c('Four ranked searches, two pages each','Three channel details, separately','Up to 300 upload pages per channel','Search results and title-filtered channel videos, batched','18 videos; top-level and reply pagination allowance'),
 planned_pages=c(8L,3L,900L,908L,6000L), bucket=c('search','data','data','data','data'),cost_per_page=1L) |> mutate(planned_cost=planned_pages*cost_per_page)
write_csv(quota_plan,file.path(root,'quota_plan.csv')); print(quota_plan,width=Inf)
print(quota_plan |> group_by(bucket) |> summarise(planned=sum(planned_cost),.groups='drop'))
# Freeze the time of this collection; reuse on restart without changing search windows.
clock_file <- file.path(root,'collection_clock.rds')
if(file.exists(clock_file)) clock <- readRDS(clock_file) else {
  now <- Sys.time(); clock <- list(cutoff=format(now,'%Y-%m-%dT%H:%M:%SZ',tz='UTC'),date=as.character(as.Date(now,tz='America/Chicago')),delete_by=as.character(as.Date(now,tz='America/Chicago')+30L)); saveRDS(clock,clock_file)
}
if (as.Date(clock$delete_by) <= as.Date(Sys.time(),tz='America/Chicago')) stop('Cache expired. Delete data/youtube before recollection.')
term <- 'US midterm election'
search_spec <- expand_grid(period=c('before','after'),order=c('relevance','viewCount')) |> mutate(published_after=if_else(period=='before','2026-09-01T00:00:00Z','2026-11-04T00:00:00Z'),published_before=if_else(period=='before','2026-10-28T00:00:00Z',clock$cutoff))
write_csv(search_spec |> mutate(term=term,max_results=100L),file.path(root,'search_spec.csv'))
quota_before <- yt_get_quota_usage(); print(quota_before)
capture.output(print(quota_before),file=file.path(root,'quota_before_stage1.txt'))
rows <- vector('list',nrow(search_spec)); audits <- rows
for(i in seq_len(nrow(search_spec))) {
 s <- search_spec[i,]; stem <- paste(s$period,s$order,sep='_'); path <- file.path(root,paste0('search_',stem,'.rds'))
 if(file.exists(path)) { x <- readRDS(path); message('Cached: ',stem) } else {
  message('Searching: ',stem)
  args <- list(term=term,published_after=s$published_after,published_before=s$published_before,order=s$order,max_results=100L)
  if('max_pages' %in% names(formals(yt_search))) args$max_pages <- 2L
  x <- do.call(yt_search,args); saveRDS(x,path); writeLines(format(Sys.time(),'%Y-%m-%dT%H:%M:%SZ',tz='UTC'),paste0(path,'.time.txt'))
 }
 stopifnot(all(c('video_id','channel_id','channel_title','title','published_at') %in% names(x)))
 total <- attr(x,'total_results'); if(length(total)==0L) total <- NA_real_
 u <- as_tibble(x) |> distinct(video_id,.keep_all=TRUE)
 audits[[i]] <- tibble(period=s$period,order=s$order,term=term,published_after=s$published_after,published_before=s$published_before,total_reported=as.numeric(total)[1],returned=nrow(x),duplicate_video_rows=sum(duplicated(x$video_id)),unique_videos=nrow(u))
 rows[[i]] <- u |> mutate(period=s$period,search_order=s$order)
}
search_audit <- bind_rows(audits); ranked_unique <- bind_rows(rows)
search_overlap <- map_dfr(c('before','after'),function(p){
 a <- ranked_unique |> filter(period==p,search_order=='relevance') |> pull(video_id)
 b <- ranked_unique |> filter(period==p,search_order=='viewCount') |> pull(video_id)
 tibble(period=p,relevance_unique=length(a),viewCount_unique=length(b),overlap=length(intersect(a,b)),union=length(union(a,b)))
})
channel_rankings <- ranked_unique |> count(period,search_order,channel_title,channel_id,sort=TRUE) |> group_by(period,search_order) |> arrange(desc(n),channel_title,.by_group=TRUE) |> mutate(rank=row_number()) |> ungroup()
write_csv(search_audit,file.path(root,'search_audit.csv')); write_csv(search_overlap,file.path(root,'search_overlap.csv')); write_csv(channel_rankings,file.path(root,'channel_rankings.csv'))
saveRDS(ranked_unique,file.path(root,'ranked_unique.rds'))
for(p in c('before','after')) {
 m <- list(source='YouTube Data API v3 via tuber',tuber=as.character(packageVersion('tuber')),credential='API key (not stored)',period=p,search_term=term,searches=search_audit |> filter(period==p),collection_date=clock$date,post_cutoff_utc=clock$cutoff,delete_by=clock$delete_by,status='Stage 1 searches only; channel/comment selection pending')
 write_json(m,file.path(root,paste0('manifest_',p,'.json')),auto_unbox=TRUE,pretty=TRUE,na='null')
}
quota_after <- yt_get_quota_usage(); capture.output(print(quota_after),file=file.path(root,'quota_after_stage1.txt'))
print(search_audit,width=Inf); print(search_overlap,width=Inf); print(channel_rankings |> filter(rank<=15) |> select(-channel_id),n=60,width=Inf); print(quota_after)
cat('\nStage 1 complete. Delete raw API cache by',clock$delete_by,'\n')
