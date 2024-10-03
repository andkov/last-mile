rm(list = ls(all.names = TRUE))
cat("\014") # Clear the console
# ---- load-packages -----------------------------------------------------------
library(tidyverse)
# ---- load-data -------------------------------------------------------------

path_file_outcome <- "./analysis/fiesta-disability/data-local/by-outcome-category.rds"
path_file_delta   <- "./analysis/fiesta-disability/data-local/by-outcome-delta.rds"
path_file_wave    <- "./analysis/fiesta-disability/data-local/by-wave.rds"

ds_outcome <- readr::read_rds(path_file_outcome) # earnings binned by 5K 
ds_delta <- readr::read_rds(path_file_delta) # delta, binned by 5K
ds_wave <- readr::read_rds(path_file_wave) # summary by waves in Income Support timeline

ds_outcome %>% 
  filter(waveF %in% c("Before","After")) %>% 
  select(tx_name,waveF,disability2, tx,earnings_cat, everything()) %>% 
  arrange(tx_name, waveF, disability2, tx) %>% 
  readr::write_csv("./analysis/fiesta-disability/data-local/by-outcome-category.csv")

ds_delta %>% 
  filter(waveF %in% c("After")) %>% 
  select(tx_name,waveF,disability2, tx,earnings_cat, everything()) %>% 
  arrange(tx_name, waveF, disability2, tx) %>% 
  readr::write_csv("./analysis/fiesta-disability/data-local/by-outcome-delta.csv")

ds_wave %>% 
  select(tx_name,waveF,disability2, tx, everything()) %>% 
  arrange(tx_name, waveF, disability2, tx) %>% 
  readr::write_csv("./analysis/fiesta-disability/data-local/by-wave.csv")



