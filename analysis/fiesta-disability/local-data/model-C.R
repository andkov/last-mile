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
prints_folder <- paste0("./analysis/8-model-C/prints")
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
# data as we used it for model B. 
ds5B <-
  ds4_tax_year %>% 
  filter(has_before_and_after) %>% # cases with non-missing outcome BEFORE and AFTER, but may be missing at other time points
  # now keep only two time points
  filter(
    timeline_is == -1L | timeline_is > 0L # time points the year before or since exit == remove years on IS
    # notice that we exclude time points of Income Support (timeline_is == 0L)
  ) %>%
  mutate(
    across(
      .cols = where(is.factor)
      ,.fns = ~forcats::fct_drop(.) # to remove empty levels
    )
  ) %>% 
  mutate(
    waveL = case_when(timeline_is==-1L~0L,timeline_is >0 ~ timeline_is) # for easy specification in models
    ,waveF = waveF %>% relevel(ref="Before")
  )

ds5B %>% summarize(oid_count = n_distinct(person_oid)) # 43,067
ds5B %>% glimpse()
ds_interval_long %>% summarize(oid_count = n_distinct(person_oid)) # 73,034
# ds_ii %>% summarize(oid_count = n_distinct(person_oid)) # 88,602

d <- 
  ds5B %>% 
  select(person_oid, assessment_snd:cover_letters) %>%
  distinct() %>% 
  group_by(person_oid) %>% 
  mutate(
    any_intevervention = sum(
              
       assessment_snd    
      ,assessment_ea     
      ,career_planning   
      ,assessment_ni     
      ,interview_skills  
      ,job_search        
      ,resume_writing    
      ,training_for_work 
      ,work_foundations  
      ,job_placement     
      ,exposure_course   
      ,workshop_other    
      ,labour_market_info
      ,self_assessment   
      ,cover_letters
      ,na.rm = T
    ) > 0L
  ) %>% 
  ungroup()

d %>% filter(any_intevervention == 0L)


oids_interval <- ds_interval_long %>% pull(person_oid) %>% unique()
oids_tax_year <- ds4_tax_year %>% pull(person_oid) %>% unique()
oids_ds5B <-ds5B %>% pull(person_oid) %>% unique()

oids_interval %>% length() # 73034
oids_tax_year %>% length() # 74907
oids_ds5B %>% length() # 43067

setdiff(oids_tax_year, oids_ds5B)
setdiff( oids_ds5B, oids_tax_year)# all in former are in latter
setdiff( oids_ds5B, oids_interval)# some in former and not in the latter
target <- setdiff( oids_ds5B, oids_interval)# some in former and not in the latter
# all these in `target` have NONE intervention
target %>% length()



ds5B %>% 
  filter(person_oid %in% target) %>% 
  keep_random_id() %>% 
  select(1:2, earnings_total, assessment_snd:cover_letters) %>% 
  glimpse()

ds5B %>% 
  filter(person_oid == 4016217) %>% 
  select(1:9) 
ds_interval_long %>% 
  filter(person_oid == 4016217) %>% 
  select(1:9) # because had NONE intervention

ds_ii %>% 
  filter(person_oid == 4016217) 
# ---- inspect-data-0 --------------
# sample_of_interest1 <- 1678491
# the most granular data form contains individual events of interventions (ii)
# each interval occupies multiple lines, as many rows as there are distinct 
# intervention events  associated with it, which we define here as occurring 
# either 12 months prior to the start of the interval or during it. 
ds_ii %>% filter(person_oid %in% sample_of_interest1)

# the next form summarizes distinct types of intervention 
# (granularity of intervention defined during the summarizing) - we'll coarsen it as needed
ds_interval_long %>% filter(person_oid %in% sample_of_interest1)

# after linking to CRA data, we have 9 rows for each person, one for each year
# we focus on a single IS stint (the first one)
ds4_tax_year %>% 
  filter(person_oid == sample_of_interest1) %>% 
  select(1:2, earnings_total, timeline_is, waveF, outcome_before) %>% 
  mask_ids()

# finally, we remove the rows that are irrelevant for the modeling of this outcome
ds5B %>% 
  filter(person_oid == sample_of_interest1) %>% 
  select(1:2, earnings_total, timeline_is, waveF, waveL, outcome_before, career_planning) %>% 
  mask_ids()
# this is the data form used for model B
ds5B %>% glimpse() # used for model B, long, row =  year

ds4_tax_year %>% summarize(n_distinct(person_oid))
ds5B %>% summarize(n_distinct(person_oid))

# ---- tweak-data-5C ------------------
# on 2024-02-12 you stopped here
# use ds5C as the starting point. that's the sample size we want/need
ds5C <- 
  ds5B %>%
  # filter(year_before_is) %>% 
  relocate(assessment_ni,.before = "assessment_ea") %>% # better looks
  group_by(person_oid) %>% 
  # for computing workshop combos, not for predicting
  mutate(
    ea   = sum(assessment_snd+assessment_ea+assessment_ni, na.rm = T)>0L
    ,tfw = training_for_work > 0L
    ,wf  = work_foundations > 0L
    ,cp  = career_planning > 0L
    ,ec  = exposure_course > 0L
    ,jp  = job_placement > 0L
    ,wk  = sum(
      interview_skills
      , job_search
      , resume_writing
      , workshop_other
      , labour_market_info
      , self_assessment
      , cover_letters
      , na.rm =T) > 0L
  ) %>%
  mutate(
    wk_combo = case_when(
      wk & cp & ec & jp     ~ "wk + cp + ec + jp",
      wk & cp & ec & !jp    ~ "wk + cp + ec + __",
      wk & cp & !ec & jp    ~ "wk + cp + __ + jp",
      wk & cp & !ec & !jp   ~ "wk + cp + __ + __",
      wk & !cp & ec & jp    ~ "wk + __ + ec + jp",
      wk & !cp & ec & !jp   ~ "wk + __ + ec + __",
      wk & !cp & !ec & jp   ~ "wk + __ + __ + jp",
      wk & !cp & !ec & !jp  ~ "wk + __ + __ + __",
      
      !wk & cp & ec & jp    ~ "__ + cp + ec + jp",
      !wk & cp & ec & !jp   ~ "__ + cp + ec + __",
      !wk & cp & !ec & jp   ~ "__ + cp + __ + jp",
      !wk & cp & !ec & !jp  ~ "__ + cp + __ + __",
      !wk & !cp & ec & jp   ~ "__ + __ + ec + jp",
      !wk & !cp & ec & !jp  ~ "__ + __ + ec + __",
      !wk & !cp & !ec & jp  ~ "__ + __ + __ + jp",
      !wk & !cp & !ec & !jp ~ "__ + __ + __ + __" # should be empty bc all clients here must have at least one wk
    )
  ) %>% 
  # compute again, to reflect a new folding system
  mutate(
    assessment_snd = sum(assessment_snd, na.rm =T)/n()
    ,assessment_ea = sum(assessment_ea, na.rm =T)/n()
    ,assessment_ni = sum(assessment_ni, na.rm =T)/n()
    ,training_for_work  = sum(training_for_work, na.rm =T)/n() 
    ,work_foundations   = sum(work_foundations, na.rm =T)/n() 
    ,career_planning    = sum(career_planning, na.rm =T)/n()           
    ,exposure_course    = sum(exposure_course, na.rm =T)/n() 
    ,job_placement      = sum(job_placement, na.rm =T)/n() 
    ,workshop_other     = sum(
      interview_skills  
      ,job_search        
      ,resume_writing    
      ,labour_market_info
      ,self_assessment   
      ,cover_letters    
      ,workshop_other
      , na.rm =T)/n() 
  ) %>% 
  ungroup() %>% 
  select(-c(
    "interview_skills"  
    ,"job_search"        
    ,"resume_writing"    
    ,"labour_market_info"
    ,"self_assessment"   
    ,"cover_letters"    
  )) # to avoid confusion with the older

# ----- save-to-disk-1 -------------------------------------------------------
# ds5C is the data that enters the balancing procedure
ds5C %>% readr::write_rds("./analysis/8-model-C/local_data/model-C-ds5C.rds")

# ------ inspect-collapse --------------------------
ds5C %>% keep_random_id() %>% glimpse()
  
ds5C %>% 
  filter(person_oid %in% ds3$person_oid) %>% 
  # filter(timeline_is == -1L) %>% 
  select(person_oid, assessment_snd:cover_letters,ea:wk_combo) %>% 
  keep_random_id() %>% 
  glimpse()

ds5C1 <- 
  ds5C %>% 
  # keep_random_id(n=100) %>% 
  group_by(person_oid) %>% 
  # for using as new predictors
  mutate(
     assessment_snd = sum(assessment_snd, na.rm =T)/n()
    ,assessment_ea = sum(assessment_ea, na.rm =T)/n()
    ,assessment_ni = sum(assessment_ni, na.rm =T)/n()
    ,training_for_work  = sum(training_for_work, na.rm =T)/n() 
    ,work_foundations   = sum(work_foundations, na.rm =T)/n() 
    ,career_planning    = sum(career_planning, na.rm =T)/n()           
    ,exposure_course    = sum(exposure_course, na.rm =T)/n() 
    ,job_placement      = sum(job_placement, na.rm =T)/n() 
    ,workshop_other     = sum(
      interview_skills  
      +job_search        
      +resume_writing    
      +labour_market_info
      +self_assessment   
      +cover_letters    
      +workshop_other
      , na.rm =T)/n() 
  ) %>% 
  ungroup


cat('\014')
random_id <- ds3$person_oid %>% unique() %>% sample(1)
ds5C %>% 
  filter(person_oid %in% random_id) %>% 
  # filter(timeline_is == -1L) %>%
  select(person_oid 
         ,assessment_snd
         ,assessment_ea
         ,assessment_ni
         ,training_for_work
         ,work_foundations
         ,career_planning
         ,exposure_course
         ,job_placement
         ,interview_skills
         ,job_search
         ,resume_writing
         ,labour_market_info
         ,self_assessment
         ,cover_letters
         ,workshop_other
         ) %>% 
  # keep_random_id(seed = random_seed) %>% 
  glimpse()

ds5C1 %>% 
  filter(person_oid %in% random_id) %>% 
  # filter(timeline_is == -1L) %>%
  select(person_oid
         ,assessment_snd
         ,assessment_ea
         ,assessment_ni
         ,training_for_work
         ,work_foundations
         ,career_planning
         ,exposure_course
         ,job_placement
         ,workshop_other    
         ) %>% 
  glimpse()

ds5C %>% glimpse() # with added binary flags

ds5C %>% 
  # filter(person_oid == sample_of_interest1) %>% 
  keep_random_id(n = 1) %>% 
  select(1:2, earnings_total, timeline_is, waveF, waveL, outcome_before, career_planning, cp) %>% 
  mask_ids()


ds5C %>% 
  filter(assessment_ea > 7)
# Combinations of interventions
dt <- 
  ds5C %>% 
  filter(year_before_is) %>%
  select(person_oid, ea:wk, assessment_snd:cover_letters) %>% 
  distinct() %>% 
  group_by(ea, tfw, wf, cp, ec, jp, wk) %>% 
  summarize(
    person_count = n_distinct(person_oid)
  ) %>% 
  ungroup() %>% 
  mutate(
    total = sum(person_count)
  )
  
dt %>% print_all()
dt %>% arrange(desc(person_count))

dt %>% readr::write_csv("fiesta-phase3-sample-size-considerations.csv")

# Combinations of Addictions and Health flags
dt <- 
  ds5C %>% 
  filter(year_before_is) %>%
  mutate(
    assessment_ea = assessment_ea>0L
  ) %>% 
  group_by(assessment_ea, criminal_record, addictions, mental_health, physical_health) %>% 
  summarize(
    person_count = n_distinct(person_oid)
  ) %>% 
  ungroup() %>% 
  mutate(
    total_count = sum(person_count)
    ,total_cr = sum(person_count[which(criminal_record==TRUE)])
    ,total_ad = sum(person_count[which(addictions==TRUE)])
  ) 

dt  %>% print_all()
dt %>% arrange(desc(person_count)) %>%  print_all()


ds5C %>% 
  filter(person_oid == 1678491) %>% glimpse()




# ---- tweak-data-wk-combo-1 ------------------------------------------------------------
ds1 <- 
  ds_interval_long %>% # one with summarized interventions
  filter(person_oid %in% (ds5C %>% pull(person_oid) %>% unique()))
ds1 %>% summarize(oid_count = n_distinct(person_oid)) # 38,236 - MUST HAVE AT LEAST ONE INTERVENTION
ds5C %>% summarize(oid_count = n_distinct(person_oid)) # 43,0667
# Verify:
# a <- setdiff( ds5B$person_oid,ds1$person_oid)
# ds5B %>% 
#   filter(person_oid %in% a) %>% 
#   keep_random_id() %>% glimpse()

# ---- tweak-data-wk-combo-2 ------------------------------------------------------------
# data form [ii] in which unique intervention (in a person) are summarized
ds2 <- 
  ds1 %>% 
  # filter(person_oid %in% sample_of_interest1) %>% 
  filter(interval_num==1, interval_type == "stint") %>% 
  left_join(intervention_matrix) %>% 
  filter(program_class1 %not in% c("Assessment","Training for Work","Work Foundations") )

ds2 %>% filter(person_oid %in% 111687  )
ds2 %>% summarize(oid_count = n_distinct(person_oid)) # 6,672
ds2 %>% keep_random_id()
# but remember that:
ds_ii %>% summarize(oid_count = n_distinct(person_oid)) # 88,602
ds_ii %>% filter(person_oid %in% sample_of_interest1)


# ---- tweak-data-wk-combo-3 ------------------------------------------------------------


ds3 <-
  ds2 %>% 
  group_by(person_oid) %>% 
  mutate(
    cp = case_when(any(intervention=="Career Planning") ~ TRUE,TRUE~FALSE)
    ,ec = case_when(any(intervention=="Exposure Course") ~ TRUE,TRUE~FALSE)
    ,jp = case_when(any(intervention=="Job Placement") ~ TRUE,TRUE~FALSE)
    ,wk = case_when(any(!intervention  %in% c("Career Planning","Exposure Course","Job Placement"))~TRUE, TRUE~FALSE)
  ) %>% 
  mutate(
    wk_combo = case_when(
      wk & cp & ec & jp     ~ "wk + cp + ec + jp",
      wk & cp & ec & !jp    ~ "wk + cp + ec + __",
      wk & cp & !ec & jp    ~ "wk + cp + __ + jp",
      wk & cp & !ec & !jp   ~ "wk + cp + __ + __",
      wk & !cp & ec & jp    ~ "wk + __ + ec + jp",
      wk & !cp & ec & !jp   ~ "wk + __ + ec + __",
      wk & !cp & !ec & jp   ~ "wk + __ + __ + jp",
      wk & !cp & !ec & !jp  ~ "wk + __ + __ + __",
      
      !wk & cp & ec & jp    ~ "__ + cp + ec + jp",
      !wk & cp & ec & !jp   ~ "__ + cp + ec + __",
      !wk & cp & !ec & jp   ~ "__ + cp + __ + jp",
      !wk & cp & !ec & !jp  ~ "__ + cp + __ + __",
      !wk & !cp & ec & jp   ~ "__ + __ + ec + jp",
      !wk & !cp & ec & !jp  ~ "__ + __ + ec + __",
      !wk & !cp & !ec & jp  ~ "__ + __ + __ + jp",
      !wk & !cp & !ec & !jp ~ "__ + __ + __ + __" # should be empty bc all clients here must have at least one wk
    )
  ) %>% 
  ungroup() 

ds3 %>% group_by(wk, cp,ec,jp, wk_combo) %>% summarize(person_count = n_distinct(person_oid))  

# ds3 %>% filter(person_oid %in% sample_of_interest1)
# ds3 %>% keep_random_id()


# ----- inspect-combos --------------

ds5C %>% 
  filter(year_before_is) %>%
  # filter() %>% 
  group_by( wk, cp, ec, jp, wk_combo) %>% 
  summarize(
    person_count = n_distinct(person_oid)
  ) %>% 
  ungroup() %>% 
  mutate(
    total = sum(person_count)
  ) %>% 
  arrange(desc(person_count))


ds3 %>% 
  group_by(wk, cp,ec,jp, wk_combo) %>% 
  summarize(person_count = n_distinct(person_oid)) %>% 
  ungroup() %>% 
  arrange(desc(person_count))



# ---- inspect-data ------------------------------------------------------------
ds5C %>% filter(person_oid %in% sample_of_interest1) %>% mask_ids() %>%  glimpse()
# ---- inspect-design ------------------------------------------------------------

ds5C %>% filter(person_oid %in% sample_of_interest1) %>% mask_ids() %>%  select(all_of(c(design,outcomes)))
# Design variables
ds5C %>%  select(all_of(design)) %>% summary()
# ---- inspect-outcomes ------------------------------------------------------------
# Outcomes
tableone::CreateTableOne(
  data  = ds5C 
  ,vars = c(outcomes)
  ,strata = "waveF"
) %>% 
  print()

# What is the overall trend of the outcome over time since IS?
ds5C %>% 
  group_by(waveF) %>% 
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
