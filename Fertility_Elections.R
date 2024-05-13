cat("\014")
rm(list=ls())
setwd("/Users/hectorbahamonde/research/Fertility_Elections")

# Pacman
if (!require("pacman")) install.packages("pacman"); library(pacman) 

# import fertility.large.d data
p_load("read.table")
fertility.d <- read.csv("/Users/hectorbahamonde/research/Fertility_Elections/ess_data/ess.csv")

# gen ID variable
#fertility.large.d$id = as.numeric(rownames(fertility.large.d))
#p_load(tidyverse)
#fertility.large.d <- fertility.large.d %>% select(id, everything()) 

# convert to NA
fertility.d$ycldbyr[fertility.d$ycldbyr==6666] <- NA # Not applicable* *) Missing Value
fertility.d$ycldbyr[fertility.d$ycldbyr==7777] <- NA # Refusal* *) Missing Value
fertility.d$ycldbyr[fertility.d$ycldbyr==8888] <- NA # Don't know* *) Missing Value
fertility.d$ycldbyr[fertility.d$ycldbyr==9999] <- NA # No answer* *) Missing Value

# recode country
p_load(dplyr)
fertility.d = fertility.d %>% mutate(Country = 
                         recode(cntry, 
                                'AL' = 'Albania',
                                'AT' = 'Austria',
                                'BE' = 'Belgium',
                                'BG' = 'Bulgaria',
                                'CH' = 'Switzerland',
                                'CY' = 'Cyprus',
                                'CZ' = 'Czechia',
                                'DE' = 'Germany',
                                'DK' = 'Denmark',
                                'EE' = 'Estonia',
                                'ES' = 'Spain',
                                'FI' = 'Finland',
                                'FR' = 'France',
                                'GB' = 'United Kingdom',
                                'GE' = 'Georgia',
                                'GR' = 'Greece',
                                'HR' = 'Croatia',
                                'HU' = 'Hungary',
                                'IE' = 'Ireland',
                                'IS' = 'Iceland',
                                'IL' = 'Israel',
                                'IT' = 'Italy',
                                'LT' = 'Lithuania',
                                'LU' = 'Luxembourg',
                                'LV' = 'Latvia',
                                'ME' = 'Montenegro',
                                'MK' = 'North Macedonia',
                                'NL' = 'Netherlands',
                                'NO' = 'Norway',
                                'PL' = 'Poland',
                                'PT' = 'Portugal',
                                'RO' = 'Romania',
                                'RS' = 'Serbia',
                                'RU' = 'Russian Federation',
                                'SE' = 'Sweden',
                                'SI' = 'Slovenia',
                                'SK' = 'Slovakia',
                                'TR' = 'Turkey',
                                'UA' = 'Ukraine',
                                'XK' = 'Kosovo'))


# Recode pregancy year: 1 year before year birth
fertility.d$Pregnancy = fertility.d$ycldbyr-1

#
fertility.d <- fertility.d %>% select(Country, Pregnancy, ycldbyr, everything()) 


# Election Data
## https://politicaldatayearbook.com

# HERE below

austria.d = data.frame(
  Country = c("Austria"),
  Election = c("29-Sep-1999",
               "1-Oct-2006",
               "28-Sep-2008",
               "29-Sep-2013",
               "15-Oct-2017",
               "9-Oct-1994",
               "3-Oct-1999",
               "24-Nov-2002",
               "17-Dec-1995"))




# controls
## polintr How interested in politics
## psppsgva Political system allows people to have a say in what government does
## trstprl Trust in country's parliament
## trstplt Trust in politicians
## trstprt Trust in political parties
## vote Voted last national election
## lrscale Placement on left right scale





fertility.d = fertility.large.d %>%  select(
  id, 
  essround,
  cntry,
  dweight,
  psweight,
  pweight,
  anweight,
  )

fertility.d$id = nrow(fertility.d)