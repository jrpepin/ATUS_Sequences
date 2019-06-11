#####################################################################################
# Set-up the environment

## Set-up the Directories
repoDir <- "C:/Users/Joanna/Dropbox/Repositories/ATUS_Sequences" # This should be your master project folder (Project GitRepository)
subDir1 <- "data" # This will be the name of the folder where data output goes
subDir2 <- "figures" # This will be the name of the folder where figures are saved
dataDir <- file.path(repoDir, subDir1)
figDir  <- file.path(repoDir, subDir2)

## This will create sub-directory data folder in the master project directory if doesn't exist
if (!dir.exists(dataDir)){
  dir.create(dataDir)
} else {
  print("data directory already exists!")
}

## This will create sub-directory figures folder in the master project directory if doesn't exist
if (!dir.exists(figDir)){
  dir.create(figDir)
} else {
  print("figure directory already exists!")
}

setwd(file.path(repoDir)) # Set the working-directory to the master project folder

## Create a data extract using ATUS-X
# Samples:          2003-2017 - Respondents and Household Members
# Variables:
# "RECTYPE"       "YEAR"          "CASEID"        "PERNUM"        "LINENO         "LINENO_CPS8"   "PRESENCE"      "DAY"
# "WT06"          "AGE"           "SEX"           "RACE"          "HISPAN"        "MARST"         "RELATE"        "EDUC"
# "EDUCYRS"       "EMPSTAT"       "CLWKR"         "FULLPART"      "UHRSWORKT"     "SPOUSEPRES"    "HH_SIZE"       
# "ACTIVITY"      "DURATION"      "ACTLINE"       "START"         "STOP"


## Set up instructions for importing the data 
# https://cran.r-project.org/web/packages/ipumsr/vignettes/ipums.html
# Updated ATUS Data

## Load libraries
library(ipumsr)
library(tidyverse, warn.conflicts = FALSE)

## Load ATUS Data into R
ddi <- read_ipums_ddi("atus_00038.xml") # This assumes your data extract was saved in the repoDir folder.
data <- read_ipums_micro(ddi)

## Make the variable names lowercase
data <- data %>% rename_all(tolower)

### Check-out the variable names
names(data)

#####################################################################################
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
          wt06       = as.numeric(lbl_clean(wt06)), # I changed this from integer. Change back if creates errors.
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

#####################################################################################
# Create activity dataset
actdata <- data %>%
  filter(rectype == 3) %>%
  select(caseid, year, actline, activity, duration, start, stop)

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
  c(020100:030000, 080200:080300, 080700:080800, 090100:100000, 070101, 
    180701, 180904, 180807, 180903, 080699, 160106)
  
## Passive leisure
passleis <- actdata$activity %in% 
  c(120300:120308, 120503, 120399)

actdata$actcat                        <- NA
actdata$actcat[selfcare]              <- "Sleep & Selfcare"
actdata$actcat[eating]                <- "Eating"
actdata$actcat[workedu]               <- "Work & Edu"
actdata$actcat[allcare]               <- "Carework"
actdata$actcat[hwork]                 <- "Housework"
actdata$actcat[passleis]              <- "Passive Leisure"
actdata$actcat[is.na(actdata$actcat)] <- "Other"

actdata$actcat <- as.character(actdata$actcat)

## Duration summary activty variables  -- person level
actsum <- actdata %>%
  group_by(caseid) %>%
  summarise (selfcare    = sum(duration[actcat ==  "Sleep & Selfcare"],       na.rm=TRUE),
             eating      = sum(duration[actcat ==  "Eating"],                 na.rm=TRUE),
             workedu     = sum(duration[actcat ==  "Work & Edu"],             na.rm=TRUE),
             allcare     = sum(duration[actcat ==  "Carework"],               na.rm=TRUE),
             hwork       = sum(duration[actcat ==  "Housework"],              na.rm=TRUE),
             passleis    = sum(duration[actcat ==  "Passive Leisure"],        na.rm=TRUE),
             otheract    = sum(duration[actcat ==  "Other"],                  na.rm=TRUE))

#####################################################################################
# Create person level demographic data

## Household variable
rec1 <- data %>% 
  filter(rectype == 1) %>%
  select(caseid,  hh_size)

colnames(rec1)[colnames(rec1)=="hh_size"] <- "numterrp"

## Household member demographics
rec2 <- data %>% 
  filter(rectype == 2) %>%
  select(caseid, year, pernum, lineno, lineno_cps8, presence, day, wt06, 
         age, sex, race, hispan, marst, relate, educ, educyrs, empstat, clwkr, fullpart, uhrsworkt, spousepres)

### who lives in household
rec2 <- rec2 %>%
  mutate(
    spouse        = ifelse(relate == "Spouse",                          1, 0),
    umpartner     = ifelse(relate == "Unmarried Partner",               1, 0),
    hhchild       = ifelse(relate == "Own household child",             1, 0),
    hhchildu18    = ifelse(relate == "Own household child" & age <  18, 1, 0),
    hhchildo18    = ifelse(relate == "Own household child" & age >= 18, 1, 0),
    hhchildu13    = ifelse(relate == "Own household child" & age <  13, 1, 0),
    grandchild    = ifelse(relate == "Grandchild",                      1, 0),
    grandchildu18 = ifelse(relate == "Grandchild"          & age <  18, 1, 0),
    grandchildo18 = ifelse(relate == "Grandchild"          & age >= 18, 1, 0),
    parent        = ifelse(relate == "Parent",                          1, 0),
    sibling       = ifelse(relate == "Brother/Sister",                  1, 0),
    siblingo18    = ifelse(relate == "Brother/Sister"      & age >= 18, 1, 0),
    otherrelative = ifelse(relate == "Other relative",                  1, 0),
    foster        = ifelse(relate == "Foster child",                    1, 0),
    nonrelative   = ifelse(relate == "Other nonrelative",               1, 0),
    notinhh       = ifelse(relate == "Own non-household child lt 18",   1, 0),
    kidu2dum      = ifelse((relate == "Own household child" | relate == "Foster child") &  age <  2,            1, 0),
    kid2to5       = ifelse((relate == "Own household child" | relate == "Foster child") & (age >= 2 & age<=5),  1, 0))

rec2 <- rec2 %>%
  mutate(
    exfamdum     = case_when(
      (hhchildo18 == 1 |  parent == 1 |   siblingo18 == 1 | otherrelative == 1)  ~ 1))
rec2$exfamdum[is.na(rec2$exfamdum)] <- 0

max <- rec2 %>% 
  group_by(caseid) %>% 
  summarise_at(vars(spouse, umpartner, hhchild, hhchildu18, hhchildo18, hhchildu13, grandchild, grandchildu18, grandchildo18, 
                    parent, sibling, siblingo18, otherrelative, foster, nonrelative, notinhh, kidu2dum, kid2to5, exfamdum), ~max(., na.rm = TRUE))

sum <- rec2 %>%
  select(caseid, kidu2dum, hhchild, kid2to5) %>%
  group_by(caseid) %>% 
  summarise(kidu2num   = sum(!is.na(kidu2dum)),
            numhhchild = sum(!is.na(hhchild)),
            kid2to5num = sum(!is.na(kid2to5)))

demo <- rec2 %>%
  filter(relate == "Self") %>%
  select(caseid, year, wt06, day, age, sex, race, hispan, marst, educ, educyrs, empstat, 
                    clwkr, fullpart, uhrsworkt, spousepres)

#####################################################################################
# Combine datasets
atus <- reduce(list(actsum, rec1, max, sum, demo), 
               left_join, by = "caseid")

head(atus, n = 5)
# remove unnecessary databases
remove(actsum)
remove(rec1)
remove(rec2)
remove(max)
remove(sum)
remove(demo)

#####################################################################################
# Respondent demographics

## Gender
atus$sex <- droplevels(atus)$sex         # Drop "NIU (Not in universe)" level
levels(atus$sex) <- c('Man', 'Woman')

## Marital status
atus <- atus %>%
  mutate(
    mar = case_when(
      spousepres == "No spouse or unmarried partner present" & marst == "Never married"                      ~ "Never married",
      spousepres == "No spouse or unmarried partner present" & 
      (marst     == "Married - spouse present" | marst == "Married - spouse absent" | marst == "Separated")  ~ "Separated",
      spousepres == "No spouse or unmarried partner present" & marst == "Divorced"                           ~ "Divorced",
      spousepres == "No spouse or unmarried partner present" & marst == "Widowed"                            ~ "Widowed",
      spousepres == "Spouse present"                                                                         ~ "Married",
      umpartner  == 1                                                                                        ~ "Cohabiting",
      TRUE                                                                                                   ~  NA_character_ 
    ))

atus$nevmar    <- as.numeric(atus$mar == "Never married")
atus$separated <- as.numeric(atus$mar == "Separated")
atus$divorced  <- as.numeric(atus$mar == "Divorced")
atus$widowed   <- as.numeric(atus$mar == "Widowed")
atus$married   <- as.numeric(atus$mar == "Married")
# umpartner=1 is cohabiting
atus$divsep    <-as.numeric(atus$mar == "Divorced" | atus$mar == "Separated")

atus <- atus %>%
  mutate(
    marstat = case_when(
      married    == 1                                 ~ "Married",
      umpartner  == 1                                 ~ "Cohabiting",
      nevmar     == 1                                 ~ "Never married",
      divorced   == 1 | separated == 1 | widowed == 1 ~ "Divorced/Separated/Widowed",
      TRUE                                            ~  NA_character_ 
    ))

atus$marstat <- factor(atus$marstat, ordered = TRUE, 
                       levels = c("Married", "Cohabiting", "Never married", "Divorced/Separated/Widowed"))

## Race/Ethnicity 
atus <- atus %>%
  mutate(
    raceethnicity = case_when(
      race   == "White only" & hispan == "Not Hispanic" ~ "White",
      race   == "Black only" & hispan == "Not Hispanic" ~ "Black",
      race   == "Asian only" & hispan == "Not Hispanic" ~ "Asian",
      hispan != "Not Hispanic"                          ~ "Hispanic",
      race   != "White only" & race   != "Black only"   &
      race   != "Asian only" & hispan == "Not Hispanic" ~ "Other race",
      TRUE                                              ~  NA_character_ 
    ))

atus$raceethnicity <- factor(atus$raceethnicity, ordered = TRUE, 
                       levels = c("White", "Black", "Asian", "Hispanic", "Other race"))

atus$white       <- as.numeric(atus$raceethnicity == "White")
atus$black       <- as.numeric(atus$raceethnicity == "Black")
atus$asian       <- as.numeric(atus$raceethnicity == "Asian")
atus$hispanic    <- as.numeric(atus$raceethnicity == "Hispanic")
atus$otherrace   <- as.numeric(atus$raceethnicity == "Other race")

## Weekend
atus$weekend     <- as.numeric(atus$day == "Sunday" | atus$day == "Saturday")

## Education
atus <- atus %>%
  mutate(
    edcat = case_when(
      educ >= 10 & educ <= 17 ~ "Less than high school",
      educ >= 20 & educ <= 21 ~ "High school",
      educ >= 30 & educ <= 32 ~ "Some college",
      educ >= 40 & educ <= 43 ~ "College",
      TRUE                    ~ NA_character_ 
    ))

atus$lths       <- as.numeric(atus$edcat == "Less than high school")
atus$highschool <- as.numeric(atus$edcat == "High school")
atus$somecol    <- as.numeric(atus$edcat == "Some college")
atus$baormore   <- as.numeric(atus$edcat == "College")

## Age
atus <- atus %>%
  mutate(
    agecat = case_when(
      age >= 15 & age <= 17 ~ "15-17",
      age >= 18 & age <= 24 ~ "18-24",
      age >= 25 & age <= 34 ~ "25-34",
      age >= 35 & age <= 44 ~ "35-44",
      age >= 45 & age <= 54 ~ "45-54",
      age >= 55             ~ "55+",
      TRUE                  ~ NA_character_ 
    ))

atus$under18       <- as.numeric(atus$agecat == "15-17")
atus$a18to24       <- as.numeric(atus$agecat == "18-24")
atus$a25to34       <- as.numeric(atus$agecat == "25-34")
atus$a35to44       <- as.numeric(atus$agecat == "35-44")
atus$a45to54       <- as.numeric(atus$agecat == "45-54")
atus$a55plus       <- as.numeric(atus$agecat == "55+")

## Employment
atus <- atus %>%
  mutate(
    employ = case_when(
      fullpart == 1  ~ "Full time",
      fullpart == 2  ~ "Part time",
      fullpart == 99 ~ "Not in labor force",
      TRUE           ~ NA_character_ 
    ))
      
atus$fulltime       <- as.numeric(atus$employ == "Full time")
atus$parttime       <- as.numeric(atus$employ == "Part time")
atus$unemployed     <- as.numeric(atus$employ == "Not in labor force")