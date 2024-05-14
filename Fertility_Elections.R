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
#fertility.large.d <- fertility.large.d %>% select(id, everything()) 

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
fertility.d <- fertility.d %>% select(Country, Pregnancy, ycldbyr, everything()) 

# drop if older than 1998 (election data is from 1998 onwards)
fertility.d = subset(fertility.d, Pregnancy >=1998 | Pregnancy == 0)

# Election Data
## https://www.electionguide.org/election-search/

# import data
p_load(readxl)
electoral.d = read_excel("/Users/hectorbahamonde/research/Fertility_Elections/electoral_calendar_data/electoral_calendar_data.xlsx")

# exclude Senate elections
p_load(dplyr)
electoral.d = electoral.d %>% 
  filter(!str_detect(Election, 'Senate'))

# select usable columns
electoral.d = electoral.d %>%  select(
  Country, 
  Date)

# format Date variable
electoral.d$Date = as.Date(electoral.d$Date, format = "%Y-%m-%d")

# create merged df
## Extract the year from the election date
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

## Apply the function to each row of pregnancy data
fertility.d <- fertility.d %>% rowwise() %>% mutate(Next.Election.Years = compute_min_distance(Pregnancy, Country, electoral.d))

# order columns
p_load(tidyverse)
fertility.d <- fertility.d %>% select(Country, Pregnancy, ycldbyr, Next.Election.Years, everything()) 

# cut is the last electoral year. If the last election was in 2018, and Pregancy is 2019 == NA
# Drop NA
p_load(tidyverse)
fertility.d = fertility.d %>% drop_na(Next.Election.Years)


# HERE below
lattice::histogram(
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