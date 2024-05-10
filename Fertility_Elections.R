cat("\014")
rm(list=ls())
setwd("/Users/hectorbahamonde/research/Fertility_Elections")

# Pacman
if (!require("pacman")) install.packages("pacman"); library(pacman) 

# import inequality data
p_load("foreign")
inequality.d <- read.dta("/Users/hectorbahamonde/research/Fertility_Elections/ess_data/ess_large.dta")
