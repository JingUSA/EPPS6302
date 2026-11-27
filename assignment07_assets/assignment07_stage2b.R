# EPPS 6302 — Assignment 7, Stage 2B: corrected audit and comment collection
setwd(path.expand("~/Downloads/EPPS6302"))
suppressPackageStartupMessages({library(tuber); library(tidyverse); library(jsonlite)})
readRenviron(path.expand("~/.Renviron"))
stopifnot(nzchar(Sys.getenv("YOUTUBE_KEY")),
          system2("git",c("check-ignore","-q","data/youtube/probe.csv"))==0L)
root <- "data/youtube"
clock <- readRDS(file.path(root,"collection_clock.rds"))
stopifnot(as.Date(clock$delete_by)>as.Date(Sys.time(),tz="America/Chicago"))
uploads <- read_csv(file.path(root,"channel_window_videos.csv"),show_col_types=FALSE)
candidates <- read_csv(file.path(root,"channel_candidates.csv"),show_col_types=FALSE)
# Do not overwrite `kept` until all logical-vector calculations are complete.
filter_audit_fixed <- uploads |> group_by(channel_title,period) |>
  summarise(all_uploads=n(),n_kept=sum(kept),n_dropped=sum(!kept),
            kept_missed_by_search=sum(kept & !in_search),
            kept_found_by_search=sum(kept & in_search),.groups="drop") |>
  rename(kept=n_kept,dropped=n_dropped)
stopifnot(all(filter_audit_fixed$all_uploads==filter_audit_fixed$kept+filter_audit_fixed$dropped),
          all(filter_audit_fixed$kept==filter_audit_fixed$kept_missed_by_search+filter_audit_fixed$kept_found_by_search))
write_csv(filter_audit_fixed,file.path(root,"title_filter_audit.csv"))
print(filter_audit_fixed,n=Inf,width=Inf)

# Three manually screened, high-view candidates per channel-period.
# Chosen from the printed candidate list for substantive US midterm relevance,
# NOT by comment availability. Skip unrelated foreign elections and other topics.
selection <- tribble(
 ~video_id,~expected_channel,~expected_period,
 "Pp5HAuHIUTs","BBC News","after",
 "rVYnYzeY-9A","BBC News","after",
 "xbgtOgYVGJM","BBC News","after",
 "ex2K7qcOiok","BBC News","before",
 "8G-NflYNCts","BBC News","before",
 "HGRNZwbzktQ","BBC News","before",
 "1TpMeZUrH1M","CNN","after",
 "0cPRdwHWf64","CNN","after",
 "8ndxlUvsQ8U","CNN","after",
 "yZIIkGsOcLw","CNN","before",
 "LpnB3mhjVkw","CNN","before",
 "7sRTDY_zbNY","CNN","before",
 "b043z24ErQk","NBC News","after",
 "NOZL0wdrJJI","NBC News","after",
 "hY2pr51lw4M","NBC News","after",
 "hGlJi746ll0","NBC News","before",
 "d4OouWc_oV8","NBC News","before",
 "aSG0tp_iuqU","NBC News","before"
)
selected_videos <- selection |> left_join(candidates,by="video_id",relationship="one-to-one")
stopifnot(nrow(selected_videos)==18L,all(!is.na(selected_videos$channel_title)),
          all(selected_videos$expected_channel==selected_videos$channel_title),
          all(selected_videos$expected_period==selected_videos$period),
          all((selected_videos |> count(channel_title,period))$n==3L))
selected_videos <- selected_videos |> select(-expected_channel,-expected_period)
write_csv(selected_videos,file.path(root,"selected_videos_private.csv"))
print(selected_videos |> select(channel_title,period,view_rank,video_id,title,comment_count),n=Inf,width=Inf)
# Local title review: the supplied console truncates some titles. Keep the
# 120-row original review sheet for reading and final verification on the Mac.
review <- read_csv(file.path(root,"title_review_private.csv"),show_col_types=FALSE)
stopifnot(nrow(review)==120L)
cat("\nFull titles for the retained/rejected audit are in title_review_private.csv.\n")
# Quota plan, written before calls. Upper allowance from Stage 1, not actual.
write_csv(tibble(endpoint="commentThreads.list + comments.list",videos=18L,
                 max_comments_per_video=300L,planned_units_allowance=6000L,
                 cost_per_page=1L),file.path(root,"quota_plan_stage2b.csv"))
quota_before2b <- yt_get_quota_usage()
print(quota_before2b)
capture.output(print(quota_before2b),file=file.path(root,"quota_before_stage2b.txt"))
cache <- file.path(root,"comment_cache"); dir.create(cache,recursive=TRUE,showWarnings=FALSE)
utc_now <- function() format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ",tz="UTC")
# Instructor's Part 5c wrapper, adapted to 3.0.0 and local caching.
# get_all_comments includes replies; max_results caps total rows.
collect_comments <- function(video_id,n_max=300L) {
 status <- "ok"
 out <- tryCatch(withCallingHandlers(
   get_all_comments(video_id,max_results=n_max,auth="key"),
   tuber_comments_disabled=function(w){status <<- "disabled"; invokeRestart("muffleWarning")},
   tuber_no_comments=function(w){if(status=="ok") status <<- "none"; invokeRestart("muffleWarning")}
 ),error=function(e)e)
 if(inherits(out,"error")){
   msg <- conditionMessage(out)
   if(grepl("commentsDisabled|comments are disabled|comments disabled",msg,ignore.case=TRUE)) status <- "disabled" else status <- "error"
   # Avoid persisting raw error strings, which may include credentialed URLs.
   return(list(data=tibble(),status=status,returned=0L,duplicates=0L,
               collected_at=utc_now(),note="Request failed or comments disabled; inspect status"))
 }
 x <- as_tibble(out)
 returned <- nrow(x)
 if(returned==0L && status=="ok") status <- "none"
 if(returned>0L){
   stopifnot(all(c("comment_id","text_original","parent_id","published_at","like_count") %in% names(x)))
   stopifnot(all(!is.na(x$comment_id)),all(nzchar(x$comment_id)))
   if("video_id" %in% names(x)) stopifnot(all(is.na(x$video_id)|x$video_id==video_id))
   dup <- sum(duplicated(x$comment_id))
   x <- x |> distinct(comment_id,.keep_all=TRUE) |> slice_head(n=n_max) |> mutate(video_id=video_id)
 } else dup <- 0L
 list(data=x,status=status,returned=returned,duplicates=dup,
      collected_at=utc_now(),note=NA_character_)
}
runs <- vector("list",nrow(selected_videos))
for(i in seq_len(nrow(selected_videos))){
 id <- selected_videos$video_id[i]
 f <- file.path(cache,paste0(id,".rds"))
 if(file.exists(f)) {
   obj <- readRDS(f)
 } else {
   obj <- collect_comments(id)
   saveRDS(obj,f)
   Sys.sleep(3)
 }
 runs[[i]] <- obj
 message(i," / ",nrow(selected_videos)," | ",selected_videos$channel_title[i]," | ",
         selected_videos$period[i]," | ",obj$status," | ",nrow(obj$data)," comments")
}
yt_log <- map_dfr(seq_along(runs),function(i){
 z <- runs[[i]]
 tibble(video_id=selected_videos$video_id[i],status=z$status,
        api_returned=z$returned,duplicate_rows=z$duplicates,
        n=nrow(z$data),collected_at=z$collected_at,note=z$note)
}) |> left_join(selected_videos |> select(video_id,channel_title,period,
        view_rank,reported_comments=comment_count),by="video_id",relationship="many-to-one")
raw <- map_dfr(runs,"data")
if(nrow(raw)==0L) {
 write_csv(yt_log,file.path(root,"collection_log.csv"))
 stop("No comments retrieved. Review collection_log.csv before retrying.")
}
global_duplicates <- sum(duplicated(raw$comment_id))
# Remove any API-supplied channel/title fields to make the join unambiguous.
yt_comments <- raw |> distinct(comment_id,.keep_all=TRUE) |>
 select(-any_of(c("channel_title","video_title","period","title"))) |>
 left_join(selected_videos |> select(video_id,channel_title,video_title=title,period),
           by="video_id",relationship="many-to-one") |>
 mutate(is_reply=!is.na(parent_id)&nzchar(parent_id),
        date=as.Date(substr(published_at,1,10)),
        comment_period=case_when(date<as.Date("2026-11-03")~"before",
          date>as.Date("2026-11-03")~"after",date==as.Date("2026-11-03")~"election_day",
          TRUE~"unknown"))
stopifnot(!anyDuplicated(yt_comments$comment_id),all(!is.na(yt_comments$period)),
          all((yt_comments |> count(video_id))$n<=300L))
saveRDS(yt_comments,file.path(root,"comments.rds"))
received <- yt_comments |> count(video_id,name="received")
yt_log <- yt_log |> left_join(received,by="video_id",relationship="one-to-one") |>
 mutate(received=coalesce(received,0L),coverage=if_else(!is.na(reported_comments)&reported_comments>0,received/reported_comments,NA_real_))
write_csv(yt_log,file.path(root,"collection_log.csv"))
status_audit <- yt_log |> count(channel_title,period,status,name="videos")
collection_summary <- yt_comments |> group_by(channel_title,period) |>
 summarise(comments=n(),replies=sum(is_reply),reply_share=mean(is_reply),
           likes=sum(as.numeric(like_count),na.rm=TRUE),
           repeated_text_rows=sum(duplicated(text_original)),.groups="drop")
comment_timing <- yt_comments |> count(period,comment_period,name="comments")
write_csv(status_audit,file.path(root,"status_audit.csv"))
write_csv(collection_summary,file.path(root,"collection_summary.csv"))
write_csv(comment_timing,file.path(root,"comment_timing.csv"))
# Save separate manifests; use the earliest cache expiration for the entire folder.
for(p in c("before","after")){
 mfile <- file.path(root,paste0("manifest_",p,".json"))
 m <- fromJSON(mfile,simplifyVector=FALSE)
 lp <- yt_log |> filter(period==p)
 m$selection <- "Three manually screened high-view US midterm candidates per channel and video-publication period; no screening on comment availability."
 m$channels <- c("NBC News","CNN","BBC News")
 m$title_filter <- readLines(file.path(root,"title_filter.txt"),warn=FALSE)[1]
 m$max_comments_per_video <- 300L
 m$comment_order <- "tuber get_all_comments default; not a random sample"
 m$comment_collected_at <- unique(lp$collected_at)
 m$delete_by <- clock$delete_by
 m$n_videos <- nrow(lp); m$n_comments <- sum(lp$received)
 m$video_ids <- lp$video_id
 m$status <- "Stage 2B comment collection; text analysis pending"
 write_json(m,mfile,auto_unbox=TRUE,pretty=TRUE,na="null")
}
quota_after2b <- yt_get_quota_usage()
capture.output(print(quota_after2b),file=file.path(root,"quota_after_stage2b.txt"))
print(yt_log,n=Inf,width=Inf)
print(status_audit,n=Inf,width=Inf)
print(collection_summary,n=Inf,width=Inf)
print(comment_timing,n=Inf,width=Inf)
cat("Global duplicate comment IDs removed:",global_duplicates,"\n")
print(quota_after2b)
cat("\nStage 2B complete. Raw comments stay local. Delete data/youtube by ",clock$delete_by,"\n",sep="")
