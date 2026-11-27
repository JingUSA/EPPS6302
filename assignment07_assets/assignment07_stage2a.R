# EPPS 6302 Assignment 7 — Stage 2A: channel uploads and title audit
# Uses Stage 1 cache. No new search calls and no comment downloads.
setwd(path.expand('~/Downloads/EPPS6302'))
suppressPackageStartupMessages({library(tuber); library(tidyverse); library(jsonlite)})
readRenviron(path.expand('~/.Renviron'))
stopifnot(nzchar(Sys.getenv('YOUTUBE_KEY')))
stopifnot(system2('git',c('check-ignore','-q','data/youtube/probe.csv'))==0L)
root <- 'data/youtube'
clock <- readRDS(file.path(root,'collection_clock.rds'))
stopifnot(as.Date(clock$delete_by)>as.Date(Sys.time(),tz='America/Chicago'))
ranked <- readRDS(file.path(root,'ranked_unique.rds')) |> as_tibble()
channels <- tribble(~channel_title,~channel_id,~media_type,
 'NBC News','UCeY0bbntWzzVIaj2z3QigXg','National broadcast network',
 'CNN','UCupvZG-5ko_eiXAupbDfxWw','Cable news network',
 'BBC News','UC16niRr50-MSBwiO3YDb3RA','International broadcaster')
stopifnot(all(channels$channel_id %in% ranked$channel_id))
pattern <- '\\b(midterms?|elections?|senate|house\\s+(race|majority|control|of representatives)|ballots?|vot(e|es|ed|ing|ers?)|congress(ional)?)\\b'
# Dates match Stage 1, UTC; leave Oct 28–Nov 3 out.
start <- '2026-09-01T00:00:00Z'; pre_end <- '2026-10-28T00:00:00Z'; post_start <- '2026-11-04T00:00:00Z'
cutoff <- clock$cutoff
max_pages <- 300L
cache <- file.path(root,'channel_cache'); dir.create(cache,recursive=TRUE,showWarnings=FALSE)
write_csv(tibble(endpoint=c('channels.list','playlistItems.list','videos.list'),planned_pages=c(3L,900L,908L),cost_per_page=1L),file.path(root,'quota_plan_stage2a.csv'))
quota_before2a <- yt_get_quota_usage(); print(quota_before2a)
`%||%` <- function(x,y) if(is.null(x)) y else x
scalar <- function(x) if(length(x)==0L) NA_character_ else as.character(x[[1]])
get_time <- function(p) as.POSIXct(p,format='%Y-%m-%dT%H:%M:%OS',tz='UTC')
all_uploads <- list(); page_logs <- list(); ch_details <- list(); scan_logs <- list()
for(i in seq_len(nrow(channels))) {
 ch <- channels[i,]; id <- ch$channel_id; label <- ch$channel_title
 d <- file.path(cache,id); dir.create(d,recursive=TRUE,showWarnings=FALSE)
 details_path <- file.path(d,'details.rds')
 if(!file.exists(details_path)) saveRDS(get_channel_details(channel_ids=id),details_path)
 ch_details[[i]] <- as_tibble(readRDS(details_path))
 token <- NULL; pages <- list(); seen <- character(); reason <- 'page cap'; logs <- list()
 for(j in seq_len(max_pages)) {
  p <- file.path(d,sprintf('page_%04d.rds',j)); cached <- file.exists(p)
  if(cached) {
   obj <- readRDS(p)
   if(!identical(obj$input_token,token)) stop('Cached token mismatch: ',label,' page ',j)
  } else {
   raw <- list_channel_videos(channel_id=id,max_results=50L,page_token=token,simplify=FALSE,auth='key')
   obj <- list(input_token=token,response=raw,time=format(Sys.time(),'%Y-%m-%dT%H:%M:%SZ',tz='UTC'))
   saveRDS(obj,p)
  }
  raw <- obj$response
  x <- map_dfr(raw$items,function(a) tibble(video_id=scalar(a$contentDetails$videoId %||% a$snippet$resourceId$videoId),title=scalar(a$snippet$title),published_at=scalar(a$contentDetails$videoPublishedAt),playlist_added_at=scalar(a$snippet$publishedAt),privacy=scalar(a$status$privacyStatus)))
  if(nrow(x)==0L) x <- tibble(video_id=character(),title=character(),published_at=character(),playlist_added_at=character(),privacy=character())
  pages[[j]] <- x
  dates <- get_time(x$published_at); oldest <- if(any(!is.na(dates))) min(dates,na.rm=TRUE) else as.POSIXct(NA)
  logs[[j]] <- tibble(channel_title=label,page=j,rows=nrow(x),oldest_utc=format(oldest,'%Y-%m-%dT%H:%M:%SZ',tz='UTC'),collected_at=obj$time,cached=cached)
  message(label,' page ',j,': ',nrow(x),' videos; oldest ',logs[[j]]$oldest_utc)
  if(!is.na(oldest) && oldest < get_time(start)) {reason <- 'Reached pre-window boundary'; break}
  next_token <- raw$nextPageToken
  if(is.null(next_token)||!length(next_token)||!nzchar(next_token)) {reason <- 'No next-page token'; break}
  if(next_token %in% seen || identical(next_token,token)) stop('Repeated pagination token: ',label)
  seen <- c(seen,next_token); token <- next_token
 }
 a <- bind_rows(pages) |> mutate(channel_title=label,channel_id=id)
 all_uploads[[i]] <- a; page_logs[[i]] <- bind_rows(logs)
 scan_logs[[i]] <- tibble(channel_title=label,pages=length(pages),upload_rows=nrow(a),missing_publication_date=sum(is.na(a$published_at)),reached_sep1=any(get_time(a$published_at)<get_time(start),na.rm=TRUE),termination=reason)
}
upload_page_log <- bind_rows(page_logs); channel_scan <- bind_rows(scan_logs)
write_csv(upload_page_log,file.path(root,'upload_page_log.csv')); write_csv(channel_scan,file.path(root,'channel_scan.csv'))
write_csv(bind_rows(ch_details),file.path(root,'channel_details.csv'))
print(channel_scan,width=Inf)
if(any(!channel_scan$reached_sep1)) warning('One or more uploads lists did not reach September 1. Check scan log before collection.')
uploads <- bind_rows(all_uploads) |> filter(!is.na(video_id),nzchar(video_id)) |> distinct(video_id,.keep_all=TRUE) |> mutate(published_time=get_time(published_at),period=case_when(published_time>=get_time(start)&published_time<get_time(pre_end)~'before',published_time>=get_time(post_start)&published_time<=get_time(cutoff)~'after',TRUE~NA_character_)) |> filter(!is.na(period)) |> mutate(kept=str_detect(coalesce(title,''),regex(pattern,ignore_case=TRUE)),in_search=video_id %in% ranked$video_id)
writeLines(pattern,file.path(root,'title_filter.txt'))
write_csv(uploads,file.path(root,'channel_window_videos.csv'))
filter_audit <- uploads |> group_by(channel_title,period) |> summarise(all_uploads=n(),kept=sum(kept),dropped=sum(!kept),kept_missed_by_search=sum(kept & !in_search),.groups='drop')
write_csv(filter_audit,file.path(root,'title_filter_audit.csv')); print(filter_audit,n=Inf,width=Inf)
# Reproducible title review: 20 retained and 20 rejected per channel, if available.
set.seed(6302)
title_review <- uploads |> group_by(channel_title,kept) |> group_modify(~slice_sample(.x,n=min(20L,nrow(.x)))) |> ungroup() |> select(channel_title,period,kept,title,video_id) |> mutate(midterm_relevant=NA_character_)
write_csv(title_review,file.path(root,'title_review_private.csv'))
# Batch stats for all deduplicated search videos and title-filter candidates.
ids <- unique(c(ranked$video_id,uploads$video_id[uploads$kept])); batches <- split(ids,ceiling(seq_along(ids)/50L))
statlist <- vector('list',length(batches))
for(i in seq_along(batches)) {
 p <- file.path(cache,sprintf('stats_%04d.rds',i)); ids_i <- unname(batches[[i]])
 if(file.exists(p)) {obj<-readRDS(p);stopifnot(identical(obj$ids,ids_i))} else {obj<-list(ids=ids_i,stats=get_video_stats(ids_i,include_content_details=TRUE),time=format(Sys.time(),'%Y-%m-%dT%H:%M:%SZ',tz='UTC'));saveRDS(obj,p)}
 statlist[[i]]<-as_tibble(obj$stats)
}
video_stats <- bind_rows(statlist) |> distinct(video_id,.keep_all=TRUE)
write_csv(video_stats,file.path(root,'video_stats.csv'))
search_video_info <- ranked |> distinct(video_id,.keep_all=TRUE) |> select(video_id,channel_title,title,period,published_at) |> left_join(video_stats |> select(video_id,view_count,comment_count,any_of('like_count')),by='video_id',relationship='many-to-one')
write_csv(search_video_info,file.path(root,'search_video_info.csv'))
candidates <- uploads |> filter(kept) |> left_join(video_stats |> select(video_id,view_count,comment_count,any_of('like_count')),by='video_id',relationship='many-to-one') |> group_by(channel_title,period) |> arrange(desc(view_count),video_id,.by_group=TRUE) |> mutate(view_rank=row_number()) |> ungroup()
write_csv(candidates,file.path(root,'channel_candidates.csv'))
# Do not automatically treat broad title matches as genuine midterm coverage.
# Inspect these titles before selecting three per group, without using comment_count to screen.
candidate_preview <- candidates |> filter(view_rank<=15) |> select(channel_title,period,view_rank,video_id,title,view_count,comment_count,in_search)
write_csv(candidate_preview,file.path(root,'candidate_preview.csv'))
quota_after2a <- yt_get_quota_usage();print(quota_after2a)
capture.output(print(quota_before2a),file=file.path(root,'quota_before_stage2a.txt'));capture.output(print(quota_after2a),file=file.path(root,'quota_after_stage2a.txt'))
print(channel_scan,width=Inf); print(filter_audit,n=Inf,width=Inf);print(candidate_preview,n=Inf,width=Inf)
cat('\nStage 2A complete. Review title_review_private.csv and candidate_preview.csv before collecting comments. Delete raw API cache by ',clock$delete_by,'\n',sep='')
