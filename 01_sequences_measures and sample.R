# Set-up

## Working directory
setwd("C:/Users/Joanna/Dropbox/Repositories/ATUS_Sequences")

## Create a data extract using ATUS-X
# Samples:          2003-2017 - Respondents and Household Members
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
ddi <- read_ipums_ddi("atus_00034.xml")
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

# Duration summary activty variables  -- person level
actdata <- actdata %>%
  group_by(caseid) %>%
  summarise (selfcare    = sum(duration[actcat ==  "Sleep & Self-care"],      na.rm=TRUE),
             eating      = sum(duration[actcat ==  "Eating"],                 na.rm=TRUE),
             workedu     = sum(duration[actcat ==  "Work & Edu"],             na.rm=TRUE),
             allcare     = sum(duration[actcat ==  "Carework"],               na.rm=TRUE),
             hwork       = sum(duration[actcat ==  "Housework"],              na.rm=TRUE),
             passleis    = sum(duration[actcat ==  "Passive Leisure"],        na.rm=TRUE),
             otheract    = sum(duration[actcat ==  "Other"],                  na.rm=TRUE))

# Create person level demographic data

## Household variable
rec1 <- data %>% 
  group_by(caseid) %>%
  filter(rectype == 1) %>%
  select(caseid, hh_size)

colnames(rec1)[colnames(rec1)=="hh_size"] <- "numterrp"

## Household member demographics
rec2 <- data %>% 
  group_by(caseid) %>% 
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
    kidu2dum      = ifelse((relate == "Own household child" | relate == "Foster child") &  age<2,            1, 0),
    kid2to5       = ifelse((relate == "Own household child" | relate == "Foster child") & (age>=2 | age<=5), 1, 0))

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
            humhhchild = sum(!is.na(hhchild)),
            kid2to5num = sum(!is.na(kid2to5)))

demo <- rec2 %>%
  group_by(caseid) %>% 
  filter(relate == "Self") %>%
  summarise_at(vars(year, age, sex, race, hispan, marst, educ, educyrs, empstat, 
                    clwkr, fullpart, uhrsworkt, spousepres), funs(first))

# Combine datasets
atus <- reduce(list(actdata, rec1, max, sum, demo), 
               left_join, by = "caseid")

head(atus, n = 5)
# remove unnecessary databases
remove(actdata)
remove(rec1)
remove(rec2)
remove(max)
remove(sum)
remove(demo)

# Respondent demographics

## Gender
atus$sex <- droplevels(atus)$sex
levels(atus$sex) <- c('Men', 'Women')

## Age
summary(atus$age)

######## START HERE

### Marital status
atus <- atus %>%
  mutate(
    mar = case_when(
      spousepres == "Spouse present"                                                         ~ "Married",
      spousepres == "Unmarried partner present"                                              ~ "Cohabiting",
      marst      == "Never married" & spousepres == "No spouse or unmarried partner present" ~ "Never married",
      marst      != "Widowed" & marst != "Never married" & 
        spousepres == "No spouse or unmarried partner present"                                 ~ "Divorced/Separated", 
      TRUE                                                                                   ~  NA_character_ 
    ))
atus$mar <- as_factor(atus$mar, levels = c("Married", "Cohabiting", "Single", "Divorced/Separated", ordered = TRUE))


gen nevmar=(spousepres==3 & marst==6);
gen separated=(spousepres==3 & (marst==1 | marst==2 | marst==5));
gen divorced=(spousepres==3 & marst==4);
gen widowed=(spousepres==3 & marst==3);
gen married=(spousepres==1);
/*umpartner=1 is cohabiting*/