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

# Create activity dataset
actdata <- data %>%
  filter(rectype == 3) %>%
  select(caseid, actline, activity, duration)

## Change NA to 0 for duration minutes
actdata[["duration"]][is.na(actdata[["duration"]])] <- 0
summary(actdata$duration)

actdata[["activity"]][is.na(actdata[["activity"]])] <- "0"
summary(actdata$activity)


## Check that duration = 1440
actdata %>%
  group_by(caseid) %>%
  summarise(total= sum(duration))

## /We're going to group activities together in local macros that we can use later

### Groups are:
  # Sleep/Grooming 
  # Eating
  # Work and education
  # Care work 
  # Housework 
  # Passive leisure 
  # All other activities

## Sleep/Grooming
selfcare <- actdata$activity %in% 
  c(010100:020000, 080500:080600)

## Eating
eating  <- actdata$activity %in% 
  c(110000:120000, 181101, 181199)
  
## Paid Work & Education -- and related travel
workedu <- actdata$activity %in% 
  c(050100:060000, 060100:070000, 160103, 180500:180600, 180600:180700)

## Care work -- childcare & eldercare
allcare <- actdata$activity %in% 
  c(030100:040000, 080100:080200, 180300:180400)

## Household Work (Housework, Shopping/Services)
hwork <- actdata$activity %in% 
  c(020100:030000, 080200:080300, 080700:080800, 090100:100000, 070101, 180701, 180904, 180807, 180903, 080699, 160106)
  
## Passive leisure
passleis <- actdata$activity %in% 
  c(120300:120308, 120503, 120399)

actdata$actcat                        <- NA
actdata$actcat[selfcare]              <- "Sleep & Self-care"
actdata$actcat[eating]                <- "Eating"
actdata$actcat[workedu]               <- "Work & Edu"
actdata$actcat[allcare]               <- "Carework"
actdata$actcat[hwork]                 <- "Housework"
actdata$actcat[passleis]              <- "Passive Leisure"
actdata$actcat[is.na(actdata$actcat)] <- "Other"

actdata$actcat <- as.character(actdata$actcat)

  