# Originated from the 8-model-C.R script to look at distribution of outcome among clients with disabilities
rm(list = ls(all.names = TRUE)) # Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.
cat("\014") # Clear the console
# verify root location
cat("Working directory: ", getwd()) # Must be set to Project Directory
# Project Directory should be the root by default unless overwritten

# ---- load-packages -----------------------------------------------------------
# Choose to be greedy: load only what's needed
# Three ways, from least (1) to most(3) greedy:
# -- 1.Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(ggplot2)   # graphs
library(forcats)   # factors
library(stringr)   # strings
library(lubridate) # dates
library(labelled)  # labels
library(dplyr   )# Avoid attaching dplyr, b/c its function names conflict with a lot of packages (esp base, stats, and plyr).
library(scales)
library(tidyr)
library(broom)
library(emmeans)
library(ggpubr)
library(twang)
# -- 2.Import only certain functions of a package into the search path.
library(magrittr)
# -- 3. Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("readr"    )# data import/export
requireNamespace("readxl"   )# data import/export
requireNamespace("tidyr"    )# tidy data
requireNamespace("janitor"  )# tidy data
requireNamespace("testit"   )# For asserting conditions meet expected patterns.

# ---- load-sources ------------------------------------------------------------
base::source("./scripts/common-functions.R") # project-level
base::source("./scripts/graphing/graph-presets.R") # project-level
base::source("./scripts/operational-functions.R") # project-level
base::source("./analysis/4-link-rdb-cra/binary-categorical-functions.R")
base::source("./analysis/4-link-rdb-cra/trajectory-change-functions.R")


base::source("./analysis/8-model-C/group-balancing-functions.R")
base::source("./analysis/8-model-C/nia-variables.R")
# ---- declare-globals ---------------------------------------------------------
# printed figures will go here:
# printed figures will go here:
prints_folder <- paste0("./analysis/8-model-C/prints/disability")
if (!fs::dir_exists(prints_folder)) {fs::dir_create(prints_folder)}

# to be used to print equations of the line in trajectory graphs
line_equation <- 
  ggpmisc::stat_poly_eq(formula = y ~ + x 
                        # ,aes(label = paste0(c(after_stat(rr.label),after_stat(eq.label)))) # can't get the same behavior
                        ,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"))
                        ,parse = TRUE
                        ,label.x = 0.1
                        ,label.y = 1.5,color = "blue",vjust=1.2) 
line_equation_only <- 
  ggpmisc::stat_poly_eq(formula = y ~ + x 
                        # ,aes(label = paste0(c(after_stat(rr.label),after_stat(eq.label)))) # can't get the same behavior
                        ,aes(label = paste(..eq.label.., sep = "~~~"))
                        ,parse = TRUE
                        ,label.x = 0.1
                        ,label.y = 1.5,color = "blue",vjust=1.2)  

# ---- intervention-matrix -----------------------------------------------------
intervention_matrix <- tibble::tribble(
  ~program_class1     , ~intervention                    ,~fold1             , ~fold2,
  "Assessment"        , "Employability"                 ,"EA"               ,"EA",
  "Assessment"        , "Needs Identification"          ,"EA"               ,"EA",
  "Assessment"        , "Service Needs Determination"   ,"EA"               ,"EA",
  "Career Information", "Exposure Course"               ,"Exposure Course"  ,"Exposure Course",
  "Career Information", "Job Placement"                 ,"Job Placement"    ,"Job Placement",
  "Career Information", "Career Planning"               ,"Career Planning"  ,"Workshop",
  "Career Information", "Job Search"                    ,"Workshop"         ,"Workshop",
  "Career Information", "Resume Writing"                ,"Workshop"         ,"Workshop",
  "Career Information", "Cover Letters"                 ,"Workshop"         ,"Workshop",
  "Career Information", "Interview Skills"              ,"Workshop"         ,"Workshop",
  "Career Information", "Labour Market Information"     ,"Workshop"         ,"Workshop",
  "Career Information", "Self Assessment"               ,"Workshop"         ,"Workshop",
  "Career Information", "Workshop(Other)"               ,"Workshop"         ,"Workshop",
  "Training for Work" , "Training for Work"             ,"Training for Work","Training for Work",
  "Work Foundations"  , "Work Foundations"              ,"Work Foundations" ,"Work Foundations",
)
intervention_matrix %>% neat()

# ---- declare-functions -------------------------------------------------------
`%not in%` <- Negate(`%in%`)

# ---- declare-globals-hidden --------------------------------------------------
sample_of_interest1 <- 1017460 
sample_of_interest1 <- 4147359


# ---- load-data ---------------------------------------------------------------
config      <- config::get()
ds4_tax_year0 <- readr::read_rds("./data-private/derived/eda-tax_year/ds4.rds")
# ds_sample_track <- readr::read_rds("./data-private/derived/model-A/ds_sample_track.rds")
# intermediate form from 2-scribe, useful for this analysis (its starting point)

# Need this ONLY IF want to quantify the intervention beyond 0/1
path_interval_intervention <- "./data-private/derived/2-scribe-interval-intervention.rds"
ds_ii <- readr::read_rds(path_interval_intervention)


path_save_d6a <- "./data-private/derived/2-scribe-interval-ds6a.rds"
ds_interval_long <- readr::read_rds(path_save_d6a) # one with summarized interventions
# source("./analysis/8-model-B/get-ea.R")
ds_asmt <- readr::read_rds("./data-private/derived/8-model-B-ea_asmt_ds5Bonly.rds") 
# hack, added tags from assessments


ds0 <- readr::read_rds("./analysis/8-model-C/local_data/model-C-ds5C.rds")




# ---- tweak-data-0 ------------------------------------------------------------
ds4_tax_year <- 
  ds4_tax_year0 %>% 
  left_join(ds_asmt) %>% 
  mutate(
    across( # to standardized the use of NAs, now means the absence of tag
      .cols = c("criminal_record","addictions","mental_health","physical_health")
      ,.fns = ~ case_when(is.na(.) ~ FALSE,TRUE ~ .)
    )
  ) %>% 
  arrange(person_oid)

ds4_tax_year %>% glimpse()
ds4_tax_year %>% summarize(oid_count = n_distinct(person_oid)) # 74,907
# data as we used it for model C.
ds0 %>% summarize(oid_count = n_distinct(person_oid)) # 43,067
# see 8-model-C.R for definitions
ds0 %>% keep_random_id() %>% glimpse()

ds0 %>% group_by(waveF) %>% count()

ds0 %>% 
  filter(waveF == "Before") %>% 
  group_by(
    tfw, wf, ec, jp, wk, wk_combo, disability2
  ) %>% 
  summarize(
    person_count = n_distinct(person_oid)
  ) %>% 
  ungroup() %>% 
  arrange(disability2,tfw,wf) %>% print_all()

# ---- tweak-data-1 ----------------------

ds1 <- 
  ds0 %>% 
  mutate(wtr = ifelse(tfw|wf,TRUE,FALSE)) %>%  # was client in Work Training program (TFW, WF)
  filter(!is.na(earnings_total)) %>% 
  mutate(
    earnings_cat = case_when(
      earnings_total <=100 ~ "0"
      ,earnings_total > 100000 ~ "100+"
      ,TRUE ~ (floor(earnings_total/5000)*5000 + 5000) %>% scales::comma(scale=.001)
    ) %>% factor(levels = c("0", seq(5,100,5),"100+"))
  ) %>% 
  mutate(
    tx_cp = if_else(cp,"With Career Planning","No Career Planning")
    ,tx_ec = if_else(ec,"With Exposure Course","No Exposure Course")
    ,tx_jp = if_else(jp,"With Job Placement"  ,"No Job Placement")
    ,tx_wk = if_else(wk,"With Workshop"       ,"No Workshop")
  ) %>% 
  group_by(person_oid) %>% 
  arrange(waveL) %>% 
  mutate(
    delta_from_prev = earnings_total - lag(earnings_total)
  ) %>% 
  ungroup() %>% 
  mutate(
    delta_cat = case_when(
      delta_from_prev <=100 ~ "0"
      ,delta_from_prev > 100000 ~ "100+"
      ,TRUE ~ (floor(delta_from_prev/5000)*5000 + 5000) %>% scales::comma(scale=.001)
    ) %>% factor(levels = c("0", seq(5,100,5),"100+"))
  )  
  
# ds1 %>% count(delta_cat) %>% print_all()

# ----- tweak-data-2 ------------------------

# ---- distribution-single-case ----------
# assemble data set
# demonstration for a single case
# d <-
#   ds1 %>%
#   group_by_at(.vars = c("waveF","earnings_cat","disability2","tx_cp")) %>%
#   summarize(
#     client_count = n_distinct(person_oid)
#     ,outcome_count = sum(!is.na(earnings_total))
#     ,p05 = quantile(earnings_total, 0.05, na.rm = T)
#     ,q1  = quantile(earnings_total, 0.25, na.rm = T)
#     ,q2  = quantile(earnings_total, 0.5, na.rm = T)
#     ,q3  = quantile(earnings_total, 0.75, na.rm = T)
#     ,p95 = quantile(earnings_total, 0.95, na.rm = T)
#     ,mean = mean(earnings_total,na.rm = T)
#     ,min = min(earnings_total, na.rm=T)
#     ,max = max(earnings_total, na.rm=T)
#     ,skewness = e1071::skewness(earnings_total, na.rm =T)
#     ,kurtosis = e1071::kurtosis(earnings_total, na.rm =T)
#     ,.groups = "drop"
#   )
# d

intervention_levels <- c(
  "cp" = "Career Planning"
  ,"ec"  = "Exposure Course"
  ,"jp" = "Job Placement"
  ,"wk" = "Workshop"
)

# ---- distribution-outcome ----------
# Frequency distributions using fixed bins of outcomes (5K)

ls_temp <- list()
for(i in c("cp","ec","jp","wk")){
  ls_temp[[i]] <- 
    ds1 %>% 
    filter(!wtr) %>% # peoplein WTR programs are very different
    rename(
      tx = paste0("tx_",i)
    ) %>% 
    mutate(
      tx_name = intervention_levels[i]
    ) %>% 
    group_by_at(.vars = c("waveF","earnings_cat","disability2","tx_name","tx")) %>% 
    summarize(
      client_count = n_distinct(person_oid)
      ,outcome_count = sum(!is.na(earnings_total))
      ,p05 = quantile(earnings_total, 0.05, na.rm = T)
      ,q1  = quantile(earnings_total, 0.25, na.rm = T)
      ,q2  = quantile(earnings_total, 0.5, na.rm = T)
      ,q3  = quantile(earnings_total, 0.75, na.rm = T)
      ,p95 = quantile(earnings_total, 0.95, na.rm = T)
      ,mean = mean(earnings_total,na.rm = T)
      ,min = min(earnings_total, na.rm=T)
      ,max = max(earnings_total, na.rm=T)
      ,skewness = e1071::skewness(earnings_total, na.rm =T)
      ,kurtosis = e1071::kurtosis(earnings_total, na.rm =T)
      ,.groups = "drop"
    ) 
}

ls_temp
ds_outcome_cat <- bind_rows(ls_temp)

# g1 <- 
#   d %>% 
#   ggplot(
#     aes(
#       x=earnings_cat
#       ,y = client_count
#       ,fill = waveF
#     )
#   )+
#   geom_col(alpha = .3, position = "identity")+
#   facet_wrap(c("disability2","tx_cp"), scales = "free_y")
# 
# g1
# g1 %>% quick_save("test",w=12,h=8) 

# ---- distribution-of-delta ----------------------


ls_temp <- list()
for(i in c("cp","ec","jp","wk")){
  ls_temp[[i]] <- 
    ds1 %>% 
    filter(!wtr) %>% # peoplein WTR programs are very different
    rename(
      tx = paste0("tx_",i)
    ) %>% 
    mutate(
      tx_name = intervention_levels[i]
    ) %>% 
    group_by_at(.vars = c("waveF","earnings_cat","disability2","tx_name","tx")) %>% 
    summarize(
      client_count = n_distinct(person_oid)
      ,outcome_count = sum(!is.na(delta_from_prev))
      ,p05 = quantile(delta_from_prev, 0.05, na.rm = T)
      ,q1  = quantile(delta_from_prev, 0.25, na.rm = T)
      ,q2  = quantile(delta_from_prev, 0.5, na.rm = T)
      ,q3  = quantile(delta_from_prev, 0.75, na.rm = T)
      ,p95 = quantile(delta_from_prev, 0.95, na.rm = T)
      ,mean = mean(delta_from_prev,na.rm = T)
      ,min = min(delta_from_prev, na.rm=T)
      ,max = max(delta_from_prev, na.rm=T)
      ,skewness = e1071::skewness(delta_from_prev, na.rm =T)
      ,kurtosis = e1071::kurtosis(delta_from_prev, na.rm =T)
      ,.groups = "drop"
    ) 
}

ls_temp
ds_delta <- bind_rows(ls_temp)


# ---- summary-table-A --------------



# summary for a given intervention

# remove extreme values
ids_with_100kplus <- 
  ds0 %>% 
  filter(earnings_total> 100000) %>% 
  pull(person_oid) %>% 
  unique()

ls_temp <- list()

for(i in c("cp","ec","jp","wk")){
  ls_temp[[i]] <- 
    ds1 %>% # because keeping waveF 
    filter(!wtr) %>% 
    filter(!(person_oid %in% ids_with_100kplus)) %>% 
    rename(
      tx = paste0("tx_",i) # for looping
    ) %>% 
    mutate(
      tx_name = intervention_levels[i]
    ) %>% 
    group_by_at(.vars = c("waveF","disability2","tx_name","tx")) %>% 
    summarize(
      client_count = n_distinct(person_oid)
      ,outcome_count = sum(!is.na(earnings_total))
      ,p05 = quantile(earnings_total, 0.05, na.rm = T)
      ,q1  = quantile(earnings_total, 0.25, na.rm = T)
      ,q2  = quantile(earnings_total, 0.5, na.rm = T)
      ,q3  = quantile(earnings_total, 0.75, na.rm = T)
      ,p95 = quantile(earnings_total, 0.95, na.rm = T)
      ,mean = mean(earnings_total,na.rm = T)
      ,min = min(earnings_total, na.rm=T)
      ,max = max(earnings_total, na.rm=T)
      ,skewness = e1071::skewness(earnings_total, na.rm =T)
      ,kurtosis = e1071::kurtosis(earnings_total, na.rm =T)
      ,.groups = "drop"
    ) 
}

ds_wave <- ls_temp %>% bind_rows()





ds_outcome_cat %>% readr::write_rds("./analysis/8-model-C/local_data/disability/by-outcome-category.rds")
ds_delta %>% readr::write_rds("./analysis/8-model-C/local_data/disability/by-outcome-delta.rds")
ds_wave %>% readr::write_rds("./analysis/8-model-C/local_data/disability/by-wave.rds")






































# ----- input-twang-solution -----------------------
weights_folder <- "./analysis/8-model-C/twang/w"
path_weight <- list.files(weights_folder,full.names = T)
intervention_w <- basename(path_weight) %>% str_remove("^w-") %>% str_remove(".rds$")
ls_temp <- list()
for(i in seq_along(path_weight)){
  # i <- 1
  intervention_name <- intervention_w[i]
  ls_temp[[intervention_name]] <- path_weight[i] %>% readr::read_rds() 
  
}
ls_temp

ds_weight <- bind_rows(ls_temp) %>% arrange(person_oid, tx_name, tx)
ds_weight
ds_weight %>% keep_random_id()

ls_temp1<-ls_temp
for(i in seq_along(ls_temp)){
  # i <- 1
  ls_temp1[[i]] <- ls_temp1[[i]] %>% select(1:3)
  adj_names <- names(ls_temp1[[i]]) %>% setdiff("person_oid") %>% paste0("_",intervention_w[i])
  names(ls_temp1[[i]]) <- c("person_oid",adj_names)
}

ds_weight_wide <- 
  purrr::reduce(
    ls_temp1, dplyr::full_join
  ) %>% 
  select(person_oid, starts_with("w_"), starts_with("tx_"))
arrange(person_oid)


ds_weight_wide %>% group_by(tx_career_planning) %>% summarize(mean = mean(w_career_planning))

ds_weight %>% keep_random_id(seed=42)
ds_weight_wide %>% keep_random_id(seed=42)

# ---- tweak-data-twang -------------------------------------------
# right now it looks like a wrong way to go ( to add weights)
ds1 <- 
  ds0 %>%  # input as ds5C
  inner_join(
    ds_weight_wide 
    , by = "person_oid"
  )
ds1 %>% glimpse()

ds1 %>% keep_random_id() %>% select(person_oid, tax_year,earnings_total, career_planning, w_career_planning)



# ---- inspect-data ------------------------------------------------------------
ds1 %>% filter(person_oid %in% sample_of_interest1) %>% mask_ids() %>%  glimpse()
# ---- inspect-design ------------------------------------------------------------

ds1 %>% filter(person_oid %in% sample_of_interest1) %>% mask_ids() %>%  select(all_of(c(design,outcomes)))
# Design variables
ds1 %>%  select(all_of(design)) %>% summary()
# ---- inspect-outcomes ------------------------------------------------------------
# Outcomes
tableone::CreateTableOne(
  data  = ds1 
  ,vars = c(outcomes)
  ,strata = "waveF"
) %>% 
  print()

# What is the overall trend of the outcome over time since IS?
ds1 %>% 
  group_by(waveF, disability2) %>% 
  summarize(
    earnings_total_mean = mean(earnings_total,na.rm = T)
    ,earnings_total_median = median(earnings_total, na.rm = T)
    ,sample_size = n()
    ,.groups = "drop"
  ) %>% 
  print() %>% 
  tidyr::pivot_longer(
    cols = c("earnings_total_mean", "earnings_total_median","sample_size")
    ,names_to = "metric",names_prefix =  "earnings_total_"
  ) %>% 
  mutate(
    statistic = case_when(metric == "sample_size" ~ "count",
                          TRUE ~ "typical value")
  ) %>% 
  ggplot(aes(x=waveF, y = value, color=metric, shape=metric, group=metric))+
  geom_line()+
  # geom_point(size=3)+
  geom_label(aes(label=scales::comma(value,accuracy=1)))+
  scale_y_continuous(labels = scales::comma_format())+
  # geom_boxplot()+
  facet_wrap(facets = "statistic", ncol=1, scales = "free_y")+
  labs(
    y = "Total earnings (annual)"
    ,color = "Statistic", shape = "Statistic"
    ,x = "Time point relevant to the start of the Income Support spell"
  )

# ---- inspect-covariates ------------------------------------------------------------
# Covariates
# only those case were selected for estimating the impact of services
# which had the outcome measured at both waves (before/after IS)
# We examine the effect of this censorship on marginal distribution of  covariates
tableone::CreateTableOne(
  data = ds4_tax_year %>% filter(timeline_is == -1L) %>% mutate(across(where(is.factor),fct_drop))
  ,vars = covariates
  ,strata = "has_before_and_after"
) %>%
  print(formatOptions = list(big.mark = ","))


# ---- inspect-intervention ------------------------------------------------------------
# Outcomes
options(max.print = 2000)
# getOption("max.print")
tableone::CreateTableOne(
  data = ds5C %>% filter(waveL==1L) # no matter which wave, person-level
  ,vars = intervention
  
  ,factorVars = intervention
) %>% summary()
# student_finance_issues has no positives, drop


# Notes:
# 1. Less than 1% of our sample had Needs Identification assessment, which has
# extremely limited window of application
# We recommend it's being dropped from the list of "intervention"
# intervention_used <- setdiff(intervention,c("assessment_ni","ab_job_corps"))
# intervention_used <- setdiff(intervention,c("assessment_ni"))


# ---- empirical-reference-group -----------------------------------------------
# let us determine what combination of predictors used in the model is the most
# frequently occurring one, to serve as an "empirical reference group"

ds5C %>% 
  # filter(used_in_nia) %>% 
  filter(timeline_is == -1L) %>%
  # group_by_at( c( covariates )) %>%
  group_by_at( c( setdiff(covariates,c("outcome_before")) )) %>%
  # group_by_at( c( setdiff(covariates,c("age_in_years","outcome_before")),"age_category5" )) %>%
  # group_by_at( c( 
  #   str_replace_all(covariates,"spell_duration","spell_duration_f")
  #   ,"year_before")) %>%
  # group_by_at( setdiff(covariates,c("year_before")) ) %>% 
  summarize(
    person_count = n()
    ,.groups = "drop"
  ) %>% 
  ungroup() %>% 
  arrange(desc(person_count)) %>% 
  slice(1) %>% 
  t() 
# because of high  number of dimensions, the specific point of intersection maybe
# poorly populated. study the most common deviations from this point


# ---- group-balancing ---------------------------------------------------------
# script
# source("./analysis/8-model-C/group-balancing.R")
# implemented group balancing with twawng, using timeline_is == -1 as a time slice
# now the derived weights will be applied in estimation of effect

# ---- effect-presentation -----------------------
