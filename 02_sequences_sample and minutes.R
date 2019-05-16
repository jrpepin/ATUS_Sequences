# Create 2017 activity data
act2017 <- actdata %>%
  filter(year == 2017)

## Create minute record
act2017  <- act2017  %>% 
  group_by(caseid)   %>%
  uncount(duration)

act2017 <- act2017[order(act2017$caseid, act2017$actline),]

act2017 <- act2017 %>% 
  group_by(caseid) %>%
  mutate(minute = row_number())

summary(act2017$minute) #Max should be 1440

## Create wide minute file
act2017 <- act2017 %>%
  select(caseid, minute, actcat) %>%
  spread(minute, actcat)

# Create 2017 demographic data
atus2017 <- atus %>%
  filter(year == 2017 &
           employ == "Full time" &
           hhchildu18 == 1) %>%
  select(caseid, year, sex, married, nevmar, umpartner, divsep, 
         exfamdum, numhhchild, kidu2dum, kid2to5, 
         lths, highschool, somecol, baormore, 
         white, black, asian, hispanic, otherrace, age, weekend)

## Missing data  
colSums(is.na(atus2017))

atus2017  <- left_join(atus2017, act2017) #Merge minute data only for cases in sample