cat("\014")
rm(list=ls())
setwd("/Users/hectorbahamonde/research/Fertility_Elections")

# Pacman
if (!require("pacman")) install.packages("pacman"); library(pacman) 

# import fertility.large.d data
# p_load(read.table)
fertility.d <- read.csv("/Users/hectorbahamonde/research/Fertility_Elections/ess_data/ess.csv")

# gen ID variable
#fertility.large.d$id = as.numeric(rownames(fertility.large.d))
#p_load(tidyverse)
#fertility.large.d <- fertility.large.d %>% dplyr::select(id, everything()) 

# convert to NA or 0, depending on the case
# fertility.d$ycldbyr[fertility.d$ycldbyr==6666] <- 0 # Not applicable* *) Missing Value ACTIVATE THIS ONE IF WE WANNA STUDY CASES WITHOUT CHILDREN
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
                                'CH' = 'Switzerland',#
                                'CY' = 'Cyprus',
                                'CZ' = 'Czechia',
                                'DE' = 'Germany',
                                'DK' = 'Denmark',#
                                'EE' = 'Estonia',
                                'ES' = 'Spain',
                                'FI' = 'Finland',
                                'FR' = 'France',#
                                'GB' = 'United Kingdom',
                                'GE' = 'Georgia',
                                'GR' = 'Greece',
                                'HR' = 'Croatia',#
                                'HU' = 'Hungary',
                                'IE' = 'Ireland',
                                'IS' = 'Iceland',
                                'IL' = 'Israel',
                                'IT' = 'Italy',#
                                'LT' = 'Lithuania',
                                'LU' = 'Luxembourg',
                                'LV' = 'Latvia',
                                'ME' = 'Montenegro',
                                'MK' = 'North Macedonia',#
                                'NL' = 'Netherlands',
                                'NO' = 'Norway',
                                'PL' = 'Poland',
                                'PT' = 'Portugal',
                                'RO' = 'Romania',
                                'RS' = 'Serbia',#
                                'RU' = 'Russian Federation',
                                'SE' = 'Sweden',
                                'SI' = 'Slovenia',
                                'SK' = 'Slovakia',
                                'TR' = 'Turkey',
                                'UA' = 'Ukraine',
                                'XK' = 'Kosovo'))


# Recode pregancy year: 1 year before year birth
fertility.d$Pregnancy = ifelse(fertility.d$ycldbyr != 0, fertility.d$ycldbyr-1, 0)

#
fertility.d <- fertility.d %>% dplyr::select(Country, Pregnancy, ycldbyr, everything()) 

# drop if older than 1998 (election data is from 1998 on wards)
fertility.d = subset(fertility.d, Pregnancy >=1998 | Pregnancy == 0)

# Election Data
## https://www.electionguide.org/election-search/

# import data
p_load(readxl)
electoral.d = read_excel("/Users/hectorbahamonde/research/Fertility_Elections/electoral_calendar_data/electoral_calendar_data.xlsx")

# exclude Senate elections
p_load(dplyr,stringr)
electoral.d = electoral.d %>% 
  filter(!str_detect(Election, 'Senate'))

# select usable columns
electoral.d = electoral.d %>%  dplyr::select(
  Country, 
  Date)

# format Date variable
electoral.d$Date = as.Date(electoral.d$Date, format = "%Y-%m-%d")

# create merged df
## Extract the year from the election date
p_load(lubridate)
electoral.d <- electoral.d %>%
  mutate(Year = year(Date))

## Function to compute the minimum distance to the closest next election year
compute_min_distance <- function(preg_year, country, elections) {
  next_elections <- elections %>%
    filter(Country == country, Year >= preg_year)
  if (nrow(next_elections) > 0) {
    min(next_elections$Year) - preg_year
  } else {
    NA  # In case there is no next election after the pregnancy year
  }
}

## Function to compute the minimum distance to the closest next pre-election year
compute_min_distance_pre_election <- function(preg_year, country, elections) {
  prev_elections <- elections %>%
    filter(Country == country, Year <= preg_year)
  if (nrow(prev_elections) > 0) {
    preg_year - max(prev_elections$Year)
  } else {
    NA  # In case there is no previous election before the pregnancy year
  }
}

## Apply the function to each row of pregnancy data
fertility.d <- fertility.d %>% rowwise() %>% mutate(Next.Election.Years = compute_min_distance(Pregnancy, Country, electoral.d))

## Apply the function to each row of pregnancy data
fertility.d <- fertility.d %>% rowwise() %>% mutate(Prior.Election.Years = compute_min_distance_pre_election(Pregnancy, Country, electoral.d))
fertility.d$Prior.Election.Years = -1*(fertility.d$Prior.Election.Years)


# order columns
p_load(tidyverse)
fertility.d <- fertility.d %>% dplyr::select(Country, Pregnancy, ycldbyr, Prior.Election.Years, Next.Election.Years, everything()) 

# cut is the last electoral year. If the last election was in 2018, and Pregancy is 2019 == NA
# Drop NA
p_load(tidyverse)
fertility.d = fertility.d %>% drop_na(Next.Election.Years)

# export electoral.d to stata
p_load(foreign)
write.dta(electoral.d, "/Users/hectorbahamonde/research/Fertility_Elections/electoral_calendar.dta")

# age variable
## yrbrn = born year
## inwyys = start of the interview
fertility.d$Age = fertility.d$inwyys - fertility.d$yrbrn

# age at first pregnancy
fertility.d$Age.at.Pregn = fertility.d$Pregnancy - fertility.d$yrbrn

# drop weird ages
fertility.d = subset(fertility.d, yrbrn < 2024)

# keep folks in reproductive age 
fertility.d = fertility.d %>% filter(Age.at.Pregn <= 45 & Age.at.Pregn >= 18)

# recode no answer and don't knows
p_load(naniar)
fertility.d = fertility.d %>% replace_with_na(replace = list(trstprl = c(77, 88, 99)))
fertility.d = fertility.d %>% replace_with_na(replace = list(lrscale = c(77, 88, 99)))
fertility.d = fertility.d %>% replace_with_na(replace = list(trstplt = c(77, 88, 99)))
fertility.d = fertility.d %>% replace_with_na(replace = list(trstprt = c(77, 88, 99)))
fertility.d = fertility.d %>% replace_with_na(replace = list(vote = c(3, 7, 8, 9)))

# recode binary variable: pregnancy on election year
fertility.d$Preg.Elect.Year = ifelse(fertility.d$Prior.Election.Years == 0 | fertility.d$Prior.Election.Years == -1 , 1, 0)


fertility.d = fertility.d %>% dplyr::select(
  Country,
  yrbrn,
  inwyys,
  Age,
  Pregnancy,
  ycldbyr,  
  Age.at.Pregn,
  Prior.Election.Years,
  Preg.Elect.Year,
  Next.Election.Years,
  everything()) 

##################
# Ideology Dataset
##################

# HERE
## https://www.chesdata.eu/ches-europe
## I am computing the ideology of the parliament (or the party that won most votes).

p_load(foreign)
data <- read.dta("/Users/hectorbahamonde/research/Fertility_Elections/ches_data/ches.dta")

# recode country names
p_load(dplyr)
data$Country = recode(data$country, 
                       be = "Belgium", 
                       dk = "Denmark",
                       ge = "Germany",
                       gr = "Greece",
                       esp = "Spain",
                       fr = "France",
                       irl = "Ireland",
                       it = "Italy",
                       nl = "Netherlands",
                       uk = "United Kingdom",
                       por = "Portugal",
                       aus = "Austria",
                       fin = "Finland",
                       sv = "Sweden",
                       bul = "Bulgaria",
                       cz = "Czech Republic",
                       est = "Estonia",
                       hun = "Hungary",
                       lat = "Latvia",
                       lith = "Lithuania",
                       pol = "Poland",
                       rom = "Romania",
                       slo = "Slovakia",
                       sle = "Slovenia",
                       cro = "Croatia",
                       mal = "Malta",
                       lux = "Luxembourg",
                       cyp = "Cyprus"
                       )

# recode country names
data$Gov = data$govt
data$Gov = ifelse(data$Gov == 0.5, 1, data$Gov)

# change name of year columns
data <- rename(data,c('Year2'='electionyear'))
data <- rename(data,c('Party'='party'))
data <- rename(data,c('Vote'='vote'))
data <- rename(data,c('Seat'='seat'))
data <- rename(data,c('Party.Ideology'='family'))

ideology.d = data %>%  dplyr::select(
  Country,
  Year2,
  Vote,
  Party,
  Seat,
  Party.Ideology,
  Gov,
  govt)
  

########################
# Plots
########################

# Net Count of Pregnancies (by country) post-election
lattice::histogram(~ Next.Election.Years | Country, data = fertility.d, 
                   type = c("count"), 
                   nint = 20,
                   xlab = "Next Election (in years)",
                   ylab = "Count of Pregnancies",
                   #scales = list(x = list(at = 0:max(fertility.d$Next.Election.Years, na.rm=T))),
                   aspect = 1)

# Net Count of Pregnancies (by country) pre-election
lattice::histogram(~ Prior.Election.Years | Country, data = fertility.d, 
                   type = c("count"), 
                   nint = 20,
                   xlab = "Pre Election (in years)",
                   ylab = "Count of Pregnancies",
                   #scales = list(x = list(at = 0:max(fertility.d$Next.Election.Years, na.rm=T))),
                   aspect = 1)

p_load(gridExtra) 

# Net Count of Pregnancies (next elections)
p1 = lattice::histogram(
  fertility.d$Next.Election.Years, 
  type = c("count"), # "percent", "count", "density"
  nint=20,#,
  xlab = "Next Election (in years)",
  ylab = "Count of Pregnancies",
  #xlim= c(min(fertility.d$Next.Election.Years), max(fertility.d$Next.Election.Years)),
  #breaks = 0:max(fertility.d$Next.Election.Years),
  scales = list(x = list(at = 0:max(fertility.d$Next.Election.Years))),
  aspect = 1
)

# Net Count of Pregnancies (pre elections)
p2= lattice::histogram(
  fertility.d$Prior.Election.Years, 
  type = c("count"), # "percent", "count", "density"
  nint=10,#,
  xlab = "Pre Election (in years)",
  ylab = "Count of Pregnancies",
  #xlim= c(min(fertility.d$Next.Election.Years), max(fertility.d$Next.Election.Years)),
  #breaks = 0:max(fertility.d$Next.Election.Years),
  #scales = list(x = list(at = 0:max(abs(fertility.d$Prior.Election.Years), na.rm = TRUE))),
  aspect = 1
)

grid.arrange(p2, p1, ncol = 2) 

# controls
## polintr How interested in politics
## psppsgva Political system allows people to have a say in what government does
## trstprl Trust in country's parliament
## trstplt Trust in politicians
## trstprt Trust in political parties
## vote Voted last national election
## lrscale Placement on left right scale


fertility.d = fertility.large.d %>%  dplyr::select(
  id, 
  essround,
  cntry,
  dweight,
  psweight,
  pweight,
  anweight,
  )

fertility.d$id = nrow(fertility.d)

##################
# Models
##################

# Poisson for count DV
p_load(MASS)

# Take the absolute value of fertility.d$Prior.Election.Years,
fertility.d$abs.Prior.Election.Years = abs(fertility.d$Prior.Election.Years)

# Check distribution
lattice::histogram(
  fertility.d$abs.Prior.Election.Years, 
  type = c("count"), # "percent", "count", "density"
  nint=10,#,
  xlab = "Pre Election (in years)",
  ylab = "Count of Pregnancies",
  #xlim= c(min(fertility.d$Next.Election.Years), max(fertility.d$Next.Election.Years)),
  #breaks = 0:max(fertility.d$Next.Election.Years),
  #scales = list(x = list(at = 0:max(abs(fertility.d$Prior.Election.Years), na.rm = TRUE))),
  aspect = 1
)


# FE specification: transform Country and Year vectors to factor variable
fertility.d$Country = as.factor(fertility.d$Country)
fertility.d$Pregnancy = as.factor(fertility.d$Pregnancy)


m1 <- glm(Preg.Elect.Year ~ 
            # lrscale + 
            trstprt * lrscale +
            # trstprl +
            # Pregnancy + # year FE
            Country, # country FE 
          family= binomial(link = "logit"), 
          data=fertility.d)

summary(m1)


p_load(sjPlot,sjmisc,ggplot2)

# plot_model(m1, type = "pred", terms = "trstprl")
 plot_model(m1, type = "int")






## polintr How interested in politics
## psppsgva Political system allows people to have a say in what government does
## trstprl Trust in country's parliament
## trstplt Trust in politicians
## trstprt Trust in political parties
## vote Voted last national election
## lrscale Placement on left right scale


