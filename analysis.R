# Workbook 6: analyze NHANES data

# Set up
library(foreign)
library(survey)
library(Hmisc)
library(tidyverse)
library(dplyr)

# Load data
alc <- sasxport.get("ALQ_I.XPT")
demo <- sasxport.get("DEMO_I.XPT")

# Join
dat <- left_join(alc, demo, by = "seqn")

# Weight sum
ws <- sum(dat$wtint2yr)
# This sum represents an estimate for the total population of interest in the study
# sum ~ 231 million

# Data formatting
dat <- dat %>%
  filter(!is.na(alq151)) %>%
  filter(alq151 <= 2)

dat <- dat  %>%
  mutate(alq151 = replace(alq151, alq151 == 2, 0))

# Survey design
survey <- svydesign(id = ~seqn, strata = ~riagendr, weights = ~wtint2yr, data = dat)

# Svymean
percent <- svymean(x = ~alq151, design = survey, data = dat)[1] * 100
percent
# 17% of respondents have ever had at least 4/5 drinks every day

# Svyby
gendered <- svyby(~alq151, ~riagendr, survey, svymean, data = dat)[,2] * 100
# 23.7% of male and 10.1% of female respondents have ever had at least 4/5 drinks
# every day



