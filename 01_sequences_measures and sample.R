# Set-up

## Working directory
setwd("C:/Users/Joanna/Dropbox/Repositories/ATUS_Sequences")

## Create a data extract using ATUS-X
# Samples:          2003-2017
# Variables:
# "RECTYPE"       "YEAR"          "CASEID"        "PERNUM"        "LINENO         "LINENO_CPS8"   "PRESENCE"      "DAY"
# "WT06"          "AGE"           "SEX"           "RACE"          "HISPAN"        "MARST"         "RELATE"        "EDUC"
# "EDUCYRS"       "EMPSTAT"       "CLWKR"         "FULLPART"      "UHRSWORKT"     "SPOUSEPRES"    "HH_SIZE"       
# "ACTIVITY"      "DURATION"      "ACTLINE"

## Set up instructions for importing the data 
# https://cran.r-project.org/web/packages/ipumsr/vignettes/ipums.html
# Updated ATUS Data

## Load libraries
library(ipumsr)
library(tidyverse, warn.conflicts = FALSE)

## Load ATUS Data into R
ddi <- read_ipums_ddi("atus_00031.xml")
data <- read_ipums_micro(ddi)

## Make the variable names lowercase
data <- data %>% rename_all(tolower)

### Check-out the variable names
names(data)

# Clean the data

## Change class from labelled
data <- data %>%
  mutate( rectype    = as.integer(lbl_clean(rectype)),
          year       = as.integer(lbl_clean(year)),
          caseid     = as.character(lbl_clean(caseid)),
          hh_size    = as.integer(lbl_clean(hh_size)),
          pernum     = as.integer(lbl_clean(pernum)),
          lineno     = as.integer(lbl_clean(lineno)),
          lineno_cps8 = as.integer(lbl_clean(lineno_cps8)),
          presence   = as.integer(lbl_clean(presence)),
          day        = as_factor(lbl_clean(day)),
          wt06       = as.integer(lbl_clean(wt06)),
          age        = as.integer(lbl_clean(age)),
          sex        = as_factor(lbl_clean(sex)),
          race       = as_factor(lbl_clean(race)),
          hispan     = as_factor(lbl_clean(hispan)),
          marst      = as_factor(lbl_clean(marst)),
          relate     = as_factor(lbl_clean(relate)),
          educ       = as.integer(lbl_clean(educ)),
          educyrs    = as.integer(lbl_clean(educyrs)),
          empstat    = as.character(lbl_clean(empstat)),
          clwkr      = as.character(lbl_clean(clwkr)),
          fullpart   = as.character(lbl_clean(fullpart)),
          uhrsworkt  = as.integer(lbl_clean(uhrsworkt)),
          spousepres = as_factor(lbl_clean(spousepres)),
          actline    = as.integer(lbl_clean(actline)),
          activity   = as.character(lbl_clean(activity)))

## Change NA to 0 for duration minutes
data[["duration"]][is.na(data[["duration"]])] <- 0
summary(data$duration)

data[["activity"]][is.na(data[["activity"]])] <- "0"
summary(data$activity)


## Check that duration = 1440
data %>%
  group_by(caseid) %>%
  summarise(total= sum(duration))