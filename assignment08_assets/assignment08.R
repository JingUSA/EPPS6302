# EPPS 6302 Assignment 8 -- extension to Karl Ho's workshop.
# AI-assisted code; review each function against R documentation before submission.
# Run from the EPPS6302 project or its data/assignment08 directory.
suppressPackageStartupMessages({library(tidyverse); library(stringdist); library(logger)})
root <- path.expand('~/Downloads/EPPS6302')
setwd(root)
stopifnot(file.exists('.gitignore'))
ignore <- trimws(readLines('.gitignore', warn=FALSE))
stopifnot(any(ignore %in% c('data/', '/data/')))
work <- file.path('data','assignment08')
out <- file.path(work,'results')
dir.create(out, recursive=TRUE, showWarnings=FALSE)
original <- file.path(work,'record_linkage_workshop.R')
stopifnot(file.exists(original))
# Load only the original function definitions, not the example side effects.
needed <- c('create_trs_data','create_tea_data','create_validation_set',
            'standardize_names','standardize_date','deterministic_linkage')
ws <- new.env(parent=globalenv())
for (e in parse(original)) {
  if (is.call(e) && length(e)==3L && identical(e[[1]],as.name('<-')) &&
      is.symbol(e[[2]]) && as.character(e[[2]]) %in% needed) eval(e,ws)
}
stopifnot(all(vapply(needed,exists,logical(1),envir=ws,inherits=FALSE)))
ids <- c('record_id_a','record_id_b')
fields <- c('last_name_sim','first_name_sim','dob_sim')
w0 <- setNames(c(.40,.30,.30),fields)
clean <- function(d) d %>% mutate(first_name_std=ws$standardize_names(first_name),
  last_name_std=ws$standardize_names(last_name), first_initial=substr(first_name_std,1,1),
  dob_std=ws$standardize_date(dob))
pair_key <- function(x) paste(x$record_id_a,x$record_id_b,sep='|')
empty_pairs <- function() tibble(record_id_a=character(),record_id_b=character())
# All pairwise similarities preserve the workshop's stringdist defaults (p=0).
score_pairs <- function(a,b,block=FALSE) {
  aa <- a %>% transmute(record_id_a=record_id, last_a=last_name_std,
                        first_a=first_name_std, dob_a=dob_std,
                        initial=substr(last_name_std,1,1))
  bb <- b %>% transmute(record_id_b=record_id, last_b=last_name_std,
                        first_b=first_name_std, dob_b=dob_std,
                        initial=substr(last_name_std,1,1))
  if (block) {
    z <- inner_join(aa,bb,by='initial',relationship='many-to-many')
  } else {
    z <- cross_join(select(aa,-initial),select(bb,-initial))
  }
  z %>% transmute(record_id_a,record_id_b,
    last_name_sim=1-stringdist(last_a,last_b,method='jw'),
    first_name_sim=1-stringdist(first_a,first_b,method='jw'),
    dob_sim=case_when(abs(as.numeric(dob_a-dob_b))==0~1,
                      abs(as.numeric(dob_a-dob_b))==1~.95,TRUE~0),
    block_ok=substr(last_a,1,1)==substr(last_b,1,1))
}
# Explicit matrix multiplication (also works for an empty candidate table).
add_score <- function(p,w) {p$match_score <- as.numeric(as.matrix(p[,fields]) %*% unname(w[fields])); p}
# Greedy one-to-one decision, stable ID tie break. Not a global optimal assignment.
unique_links <- function(p, score_col='match_score') {
  if (!nrow(p)) return(p)
  p <- p[order(-p[[score_col]],p$record_id_a,p$record_id_b),,drop=FALSE]
  used_a <- new.env(hash=TRUE,parent=emptyenv()); used_b <- new.env(hash=TRUE,parent=emptyenv())
  keep <- logical(nrow(p))
  for(i in seq_len(nrow(p))) {
    a <- p$record_id_a[i]; b <- p$record_id_b[i]
    if (!exists(a,used_a,inherits=FALSE) && !exists(b,used_b,inherits=FALSE)) {
      keep[i] <- TRUE; assign(a,TRUE,used_a); assign(b,TRUE,used_b)
    }
  }
  p[keep,,drop=FALSE]
}
metric <- function(pred,truth,universe=250000L) {
  p <- unique(pair_key(pred)); t <- unique(pair_key(truth))
  tp <- sum(p %in% t); fp <- length(p)-tp; fn <- length(t)-tp
  precision <- if(length(p)) tp/length(p) else NA_real_
  recall <- if(length(t)) tp/length(t) else NA_real_
  tibble(links=length(p),tp=tp,fp=fp,fn=fn,tn=universe-tp-fp-fn,
         precision=precision,recall=recall,
         f1=if(2*tp+fp+fn>0) 2*tp/(2*tp+fp+fn) else NA_real_)
}
combine <- function(d,p) bind_rows(select(d,all_of(ids)),select(p,all_of(ids)))
# Original experiments: exact original deterministic function and strict > .95.
run_scenario <- function(rate) {
  set.seed(42)
  a <- clean(ws$create_trs_data(500))
  b <- clean(ws$create_tea_data(a,500,100,rate))
  truth <- ws$create_validation_set(a,b,100) %>%
    transmute(record_id_a=trs_record_id,record_id_b=tea_record_id)
  stopifnot(nrow(truth)==100L,anyDuplicated(pair_key(truth))==0L)
  det <- ws$deterministic_linkage(a,b)
  p <- add_score(score_pairs(det$remaining_a,det$remaining_b),w0)
  auto <- filter(p,match_score>.95)
  all <- combine(det$matches,auto)
  actual <- sum(a$first_name_std[1:100]!=b$first_name_std[1:100] |
                a$last_name_std[1:100]!=b$last_name_std[1:100] |
                a$dob_std[1:100]!=b$dob_std[1:100])
  summary <- bind_cols(tibble(corruption_rate=rate,actual_corrupted=actual,
    rule1=det$summary$Matches[1],rule2=det$summary$Matches[2],rule3=det$summary$Matches[3],
    probabilistic=nrow(auto),candidate_pairs=nrow(p),
    duplicated_trs=sum(duplicated(all$record_id_a)),duplicated_tea=sum(duplicated(all$record_id_b))),
    metric(all,truth))
  list(a=a,b=b,truth=truth,det=det,p=p,auto=auto,summary=summary)
}
s <- lapply(c(.10,.20,.40),run_scenario)
corruption_results <- bind_rows(lapply(s,`[[`,'summary'))
base <- s[[2]]; a <- base$a; b <- base$b; truth <- base$truth
D <- base$det$matches; P <- base$p
# Reproduction guard based on the observed original baseline.
stopifnot(nrow(P)==165649L,nrow(D)==93L,nrow(base$auto)==4L,
          base$summary$tp==97L,base$summary$fp==0L)
stopifnot(anyDuplicated(D$record_id_a)==0L,anyDuplicated(D$record_id_b)==0L)
# 2b: isolate effect of key truncation versus adding the deterministic decisions.
grid <- seq(70,99)/100
curve_for <- function(p,d,truth,mode,one_to_one=FALSE) {
  map_dfr(grid,function(t) {
    links <- filter(p,match_score>=t)
    if(one_to_one) links <- unique_links(links)
    bind_cols(tibble(threshold=t,curve=mode),metric(combine(d,links),truth))
  })
}
threshold_comparison <- bind_rows(
  curve_for(P,empty_pairs(),slice_head(truth,n=100),'Original truncated key'),
  curve_for(P,empty_pairs(),truth,'Probabilistic only, full key'),
  curve_for(P,D,truth,'Combined, full key (original pair decisions)'))
key_change <- threshold_comparison %>% select(threshold,curve,precision,recall,f1) %>%
  pivot_wider(names_from=curve,values_from=c(precision,recall,f1))
# 2c: actual blocking BEFORE scoring, compare with original strict threshold.
B <- add_score(score_pairs(base$det$remaining_a,base$det$remaining_b,block=TRUE),w0)
stopifnot(setequal(pair_key(B),pair_key(filter(P,block_ok))))
true_eligible <- sum(pair_key(P) %in% pair_key(truth))
true_removed <- sum(!P$block_ok & pair_key(P) %in% pair_key(truth))
blocking_results <- bind_rows(
  bind_cols(tibble(method='Unblocked',candidate_pairs=nrow(P),true_eligible=true_eligible,true_removed=0L),
            metric(combine(D,filter(P,match_score>.95)),truth)),
  bind_cols(tibble(method='Last-name initial',candidate_pairs=nrow(B),true_eligible=true_eligible-true_removed,true_removed=true_removed),
            metric(combine(D,filter(B,match_score>.95)),truth))) %>%
  mutate(pair_reduction=1-candidate_pairs/nrow(P))
# 2d: estimate exact-agreement m and u on the full known pair universe.
full_pairs <- score_pairs(a,b)
full_pairs$is_true <- pair_key(full_pairs) %in% pair_key(truth)
stopifnot(nrow(full_pairs)==250000L,sum(full_pairs$is_true)==100L)
weights_table <- map_dfr(fields,function(f) {
  agree <- full_pairs[[f]]==1
  m_n <- sum(agree & full_pairs$is_true); u_n <- sum(agree & !full_pairs$is_true)
  m <- (m_n+.5)/(sum(full_pairs$is_true)+1)
  u <- (u_n+.5)/(sum(!full_pairs$is_true)+1)
  tibble(field=f,match_agree=m_n,nonmatch_agree=u_n,m=m,u=u,log_m_u=log(m/u))
}) %>% mutate(weight=log_m_u/sum(log_m_u),original_weight=unname(w0[field]))
stopifnot(all(weights_table$weight>=0),abs(sum(weights_table$weight)-1)<1e-10)
w <- setNames(weights_table$weight,weights_table$field)
# If blocking excludes even one true residual pair, favor recall and keep unblocked.
use_block <- true_removed==0
candidate <- if(use_block) B else P
W <- add_score(candidate,w)
weight_comparison <- bind_rows(
  bind_cols(tibble(weight_scheme='Original',threshold=.95),metric(combine(D,unique_links(filter(candidate,match_score>.95))),truth)),
  bind_cols(tibble(weight_scheme='Normalized log(m/u)',threshold=.95),metric(combine(D,unique_links(filter(W,match_score>.95))),truth)))
# 3: Choose a threshold using explicit loss: missed person costs 2, false link 1.
# In-sample calibration only; the known answer key is used both to estimate and evaluate.
threshold_curve <- curve_for(W,D,truth,'Final combined',one_to_one=TRUE) %>%
  mutate(loss=2*fn+fp)
chosen <- threshold_curve %>% arrange(loss,desc(precision),desc(threshold)) %>% slice(1)
t <- chosen$threshold[[1]]
auto <- unique_links(filter(W,match_score>=t))
final <- combine(D,auto)
stopifnot(anyDuplicated(final$record_id_a)==0L,anyDuplicated(final$record_id_b)==0L)
# REVIEW: among remaining records only, not candidates conflicting with accepted links.
remaining <- W %>% filter(!record_id_a %in% final$record_id_a,!record_id_b %in% final$record_id_b)
review_grid <- map_dfr(grid[grid<t],function(lo) {
  r <- filter(remaining,match_score>=lo,match_score<t)
  ntrue <- sum(pair_key(r) %in% pair_key(truth))
  tibble(lower=lo,upper=t,pairs=nrow(r),true_pairs=ntrue,
         true_share=if(nrow(r)) ntrue/nrow(r) else NA_real_)
})
if(!nrow(review_grid)) review_grid <- tibble(lower=numeric(),upper=numeric(),pairs=integer(),true_pairs=integer(),true_share=numeric())
feasible <- filter(review_grid,true_pairs>0,true_share>=.5) %>% arrange(desc(true_pairs),desc(true_share),desc(lower))
if(nrow(feasible)) {
  review_band <- slice_head(feasible,n=1) %>% mutate(rule='Maximize true candidates with at least 50% correct')
} else {
  lo <- max(.70,t-.05); r <- filter(remaining,match_score>=lo,match_score<t)
  nr <- sum(pair_key(r) %in% pair_key(truth))
  review_band <- tibble(lower=lo,upper=t,pairs=nrow(r),true_pairs=nr,
    true_share=if(nrow(r)) nr/nrow(r) else NA_real_,rule='Fallback five-point band; no qualifying positive-yield band')
}
# 4: metrics on a common universe, plus conditional incremental recall.
step_metrics <- bind_rows(
  bind_cols(tibble(step='Deterministic'),metric(D,truth)),
  bind_cols(tibble(step='Probabilistic only (full key)'),metric(auto,truth)),
  bind_cols(tibble(step='Combined'),metric(final,truth)))
residual_truth <- truth[!pair_key(truth)%in%pair_key(D),,drop=FALSE]
incremental <- metric(auto,residual_truth) %>% select(links,tp,fp,fn,precision,recall,f1) %>%
  mutate(remaining_true=nrow(residual_truth))
final_metric <- metric(final,truth)
downstream_bias <- final_metric %>% transmute(true_overlap=nrow(truth),estimated_overlap=links,
  upward_error=fp,downward_error=fn,net_bias=fp-fn,relative_bias=(fp-fn)/nrow(truth))
# Error diagnosis: score all actual mistakes, even when a true pair was blocked.
# Publish only synthetic IDs and numerical similarities, never names or birth dates.
all_scored <- add_score(full_pairs,w)
fn <- truth[!pair_key(truth)%in%pair_key(final),,drop=FALSE]
fp <- final[!pair_key(final)%in%pair_key(truth),,drop=FALSE]
err <- bind_rows(mutate(fn,error='False negative'),mutate(fp,error='False positive')) %>%
  left_join(select(all_scored,all_of(ids),all_of(fields),match_score,block_ok),by=ids)
if(nrow(err)) {
  deficits <- sweep(1-as.matrix(err[,fields]),2,w[fields],`*`)
  evidence <- sweep(as.matrix(err[,fields]),2,w[fields],`*`)
  err$responsible_field <- ifelse(err$error=='False negative',fields[max.col(deficits,ties.method='first')],
                                 fields[max.col(evidence,ties.method='first')])
  err <- err %>% mutate(cause=case_when(error=='False positive'~'Unrelated people with similar fields',
    last_name_sim<1~'Last-name typo',first_name_sim<1~'First-name abbreviation',dob_sim<1~'Date corruption',TRUE~'No field disagreement'),
    decision_reason=case_when(error=='False positive'~'Accepted wrong pair',
    !paste(record_id_a,record_id_b,sep="|")%in%pair_key(candidate) & !block_ok & use_block~'Blocked or previously linked endpoint',
    !paste(record_id_a,record_id_b,sep="|")%in%pair_key(candidate)~'Previously linked endpoint',match_score<t~'Below threshold',TRUE~'One-to-one conflict'))
} else {
  err$responsible_field <- character(); err$cause <- character(); err$decision_reason <- character()
}
error_counts <- err %>% count(error,cause,name='n')
error_examples <- err %>% group_by(error) %>% arrange(desc(match_score),record_id_a,.by_group=TRUE) %>% slice_head(n=10) %>% ungroup()
error_availability <- tibble(error=c('False negative','False positive'),total=c(nrow(fn),nrow(fp)),
                             examples_available=pmin(10L,total))
# Additional raw-workshop error counts at .95, useful if the chosen model is nearly perfect.
original_quality <- metric(combine(D,base$auto),truth)
# Save only aggregate results, error IDs and scores, no raw administrative records.
outputs <- list(corruption_results=corruption_results,threshold_comparison=threshold_comparison,
  key_change=key_change,blocking_results=blocking_results,weights_table=weights_table,
  weight_comparison=weight_comparison,threshold_curve=threshold_curve,chosen_threshold=chosen,
  review_grid=review_grid,review_band=review_band,step_metrics=step_metrics,
  incremental=incremental,downstream_bias=downstream_bias,error_counts=error_counts,
  error_examples=error_examples,error_availability=error_availability,original_quality=original_quality,
  design=tibble(seed=42,blocking_used=use_block,fn_cost=2,fp_cost=1,
    threshold_rule='>= for calibration; > .95 for workshop comparisons',
    weight_estimation='Full known key; Jeffreys smoothing; normalized log(m/u); in-sample'))
for(nm in names(outputs)) write_csv(outputs[[nm]],file.path(out,paste0(nm,'.csv')))
plot <- threshold_curve %>% select(threshold,precision,recall,f1) %>%
  pivot_longer(-threshold,names_to='metric',values_to='value') %>%
  ggplot(aes(threshold,value,color=metric,linetype=metric)) + geom_line(linewidth=.9) +
  geom_point(size=1.4) + geom_vline(xintercept=t,linetype='dotted') +
  labs(title='Combined linkage quality by threshold',subtitle=paste('Chosen threshold:',t),
       x='Match score threshold',y='Metric',color=NULL,linetype=NULL) +
  scale_y_continuous(limits=c(0,1)) + theme_minimal(base_size=12) + theme(legend.position='bottom')
ggsave(file.path(out,'threshold_curve.png'),plot,width=8,height=4.7,dpi=200)
writeLines(c(capture.output(sessionInfo()),paste('Workshop MD5:',tools::md5sum(original))),file.path(out,'session_info.txt'))
for(nm in c('corruption_results','blocking_results','weights_table','weight_comparison','chosen_threshold','review_band','step_metrics','incremental','downstream_bias','error_availability','error_counts')) {
  cat('\n',nm,'\n'); print(outputs[[nm]],n=Inf,width=Inf)
}
cat('\nAssignment 8 analysis complete. Results:',normalizePath(out),'\n')
