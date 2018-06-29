#run all
setwd("/home/b/zbaran/Documents/Genf/Malawi-SNF/pcalg_DHS/DAGS_DHS/R/")
set.seed(1)
rm(list=ls())
source("./run_DAG_female.R")
source("./run_DAG.R")
source("./odds_ratio_female.R")
source("./odds_ratio_male.R")
