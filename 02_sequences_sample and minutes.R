# Create 2017 activity data
min2017 <- actdata %>%
  filter(year == 2017)

## Create minute record
min2017 <- min2017[order(min2017$caseid, min2017$actline),]

min2017  <- min2017  %>% 
  group_by(caseid)   %>%
  uncount(duration)

min2017 <- min2017 %>% 
  group_by(caseid) %>%
  mutate(minute = row_number())

summary(min2017$minute) #Max should be 1440

## Create wide minute file
min2017 <- min2017 %>%
  select(caseid, minute, actcat) %>%
  spread(minute, actcat)

# Create 2017 demographic data
atus2017 <- atus %>%
  filter(year == 2017 &
           employ == "Full time" &
           hhchildu18 == 1) %>%
  select(caseid, year, wt06, sex, married, nevmar, umpartner, divsep, 
         exfamdum, numhhchild, kidu2dum, kid2to5, 
         lths, highschool, somecol, baormore, 
         white, black, asian, hispanic, otherrace, age, weekend)

## Missing data  
colSums(is.na(atus2017))

atus2017  <- left_join(atus2017, min2017) #Merge minute data only for cases in sample

