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

# Create 2017 demographic data (person level)
atus2017p <- atus %>%
  filter(year == 2017 &
           employ == "Full time" &
           hhchildu18 == 1) %>%
  select(caseid, year, wt06, selfcare, eating, workedu, allcare, hwork, passleis, otheract, 
         sex, marstat, raceethnicity, edcat, 
         exfamdum, numhhchild, kidu2dum, kid2to5, age, weekend,
         married, nevmar, umpartner, divsep,
         lths, highschool, somecol, baormore, 
         white, black, asian, hispanic, otherrace)

## Missing data  
colSums(is.na(atus2017p))

atus2017  <- left_join(atus2017p, min2017) #Merge minute data only for cases in sample

##################################
# Sample characteristics
library("survey")
library("tableone")
## Create Table 1
# Create a variable list which we want in Table 1

## Set as survey data
dataSvy <- svydesign(ids = ~1, weights = ~ wt06, data = atus2017p)
summary(dataSvy)

## Number of observations (unweighted)
table(atus2017p$sex)

### Create Table 1 variable list
catVars <- c("married", "umpartner", "nevmar", "divsep", 
             "white", "black", "asian", "hispanic", "otherrace", 
             "lths", "highschool", "somecol", "baormore",              
             "exfamdum", "numhhchild", "kidu2dum", "kid2to5", 
             "age", "weekend")

### Total Population (Weighted)
table1_svy <- svyCreateTableOne(vars = catVars, data = dataSvy)
table1_svy

### By Gender
table1_svy <- svyCreateTableOne(vars = catVars, data = dataSvy, strata = c("sex"), test = F)
table1_svy

####################
## Mean minutes of actitives by gender
library(ggeffects)

#Selfcare
lm_selfcare <- lm(selfcare  ~ sex + marstat + raceethnicity + edcat + exfamdum + numhhchild + kidu2dum + kid2to5 + age + weekend,
                  data = atus2017p, weight=wt06)
pselfcare   <- ggeffect(lm_selfcare, terms = "sex")

#Eating
lm_eating <- lm(eating  ~ sex + marstat + raceethnicity + edcat + exfamdum + numhhchild + kidu2dum + kid2to5 + age + weekend,
                  data = atus2017p, weight=wt06)
peating   <- ggeffect(lm_eating, terms = "sex")

#Work & Education
lm_workedu <- lm(workedu  ~ sex + marstat + raceethnicity + edcat + exfamdum + numhhchild + kidu2dum + kid2to5 + age + weekend,
                data = atus2017p, weight=wt06)
pworkedu   <- ggeffect(lm_workedu, terms = "sex")

#Carework
lm_allcare <- lm(allcare  ~ sex + marstat + raceethnicity + edcat + exfamdum + numhhchild + kidu2dum + kid2to5 + age + weekend,
                 data = atus2017p, weight=wt06)
pallcare   <- ggeffect(lm_allcare, terms = "sex")

#Housework
lm_hwork <- lm(hwork  ~ sex + marstat + raceethnicity + edcat + exfamdum + numhhchild + kidu2dum + kid2to5 + age + weekend,
                 data = atus2017p, weight=wt06)
phwork   <- ggeffect(lm_hwork, terms = "sex")

#Passive Leisure
lm_passleis <- lm(passleis  ~ sex + marstat + raceethnicity + edcat + exfamdum + numhhchild + kidu2dum + kid2to5 + age + weekend,
               data = atus2017p, weight=wt06)
ppassleis   <- ggeffect(lm_passleis, terms = "sex")

#Other Activities
lm_otheract <- lm(otheract  ~ sex + marstat + raceethnicity + edcat + exfamdum + numhhchild + kidu2dum + kid2to5 + age + weekend,
                  data = atus2017p, weight=wt06)
potheract   <- ggeffect(lm_otheract, terms = "sex")

## Label the group values as the activity
levels(pallcare$group)[levels(pallcare$group)=="1"]   <- "Carework"
levels(peating$group)[levels(peating$group)=="1"]     <- "Eating"
levels(phwork$group)[levels(phwork$group)=="1"]       <- "Housework"
levels(potheract$group)[levels(potheract$group)=="1"] <- "Other"
levels(ppassleis$group)[levels(ppassleis$group)=="1"] <- "Passive Leisure"
levels(pselfcare$group)[levels(pselfcare$group)=="1"] <- "Self-care"
levels(pworkedu$group)[levels(pworkedu$group)=="1"]   <- "Work & Education"


# Change the variable to be a factor variable from numeric
pallcare$x <- as.factor(pallcare$x)
peating$x <- as.factor(peating$x)
phwork$x <- as.factor(phwork$x)
potheract$x <- as.factor(potheract$x)
ppassleis$x <- as.factor(ppassleis$x)
pselfcare$x <- as.factor(pselfcare$x)
pworkedu$x <- as.factor(pworkedu$x)

# Combine the datatables
pred <- rbind(pallcare, peating, phwork, potheract, ppassleis, pselfcare, pworkedu)

# Revalue the gender factors to be readable
levels(pred$x)[levels(pred$x)=="1"] <- "Fathers"
levels(pred$x)[levels(pred$x)=="2"] <- "Mothers"

# Order the activity factors
pred$group <- ordered(pred$group, levels = c("Self-care", "Work & Education", "Housework", "Carework", "Passive Leisure", "Eating", "Other"))

## Graph it

fig1 <- pred %>%
  ggplot(aes(x, predicted, fill = x, label = round(predicted, 0))) +
  geom_col() +
  facet_grid(~group) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.2,
                position=position_dodge(.9), color="grey") +
  theme_minimal() +
  ggtitle("Figure 1. Average Time Employed Parents Spend Per Day in Each Activity") +
  labs(x = NULL, y = NULL, subtitle = "Predicted minutes per day with model controls",
       caption = "Source: American Time Use Surveys (2017) \n Models control for education, race-ethnicity, marital status, extra adults,
       number of household kids, kids under 2, age, weekend diary day") +
  theme(plot.subtitle = element_text(size = 11, vjust = 1),
        plot.caption  = element_text(vjust = 1, size =8, colour = "grey"), 
        legend.position="none",
        strip.text.x  = element_text(size = 8, face = "bold"),
        axis.title    = element_text(size = 9), 
        axis.text     = element_text(size = 8), 
        plot.title    = element_text(size = 12, face = "bold"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank())

fig1

ggsave("figures/sequences_fig1.png", fig1, height = 6, width = 8, dpi = 300)