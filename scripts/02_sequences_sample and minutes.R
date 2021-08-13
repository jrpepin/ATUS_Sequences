#------------------------------------------------------------------------------------
# ATUS SEQUENCE ANALYSIS
# 02_sample and minutes.R
# Joanna R. Pepin & Sarah Flood
#------------------------------------------------------------------------------------

# Create minute record --------------------------------------------------------------
mindata <- actdata # create a new dataset

mindata <- mindata[order(mindata$caseid, mindata$actline),]

mindata  <- mindata  %>% 
  group_by(caseid)   %>%
  uncount(duration)  %>%
  mutate(minute = row_number())

summary(mindata$minute) #Max should be 1440

## Create wide minute file
mindata <- mindata %>%
  select(caseid, minute, actcat) %>%
  spread(minute, actcat)

# Create analytic sample (person level)

atussample <- atus %>%
  filter(hhchildu18 == 1 &  ## parents
         month      >= 5) %>% ## diaries from May to Dec.
  select(caseid, year, month, wt20, 
         selfcare, eating, workedu, allcare, hwork, passleis, otheract, 
         sex, marstat, raceethnicity, edcat, 
         exfamdum, numhhchild, kidu2dum, kid2to5, age, weekend,
         married, nevmar, umpartner, divsep,
         lths, highschool, somecol, baormore, 
         white, black, asian, hispanic, otherrace)

## Missing data  
colSums(is.na(atussample))

seqdata  <- left_join(atussample, mindata) #Merge minute data only for cases in sample

##################################
# Sample characteristics

## Create Table 1
# Create a variable list which we want in Table 1

## Set as survey data
dataSvy <- svydesign(ids = ~1, weights = ~ wt20, data = atussample)
summary(dataSvy)

## Number of observations (unweighted)
table(atussample$sex)

### Create Table 1 variable list
catVars <- c("year", "married", "umpartner", "nevmar", "divsep", 
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
## Mean minutes of activities by gender


#Selfcare
lm_selfcare <- lm(selfcare  ~ year + sex + marstat + raceethnicity + edcat + exfamdum + numhhchild + kidu2dum + kid2to5 + age + weekend,
                  data = atussample, weight=wt20)
pselfcare   <- ggeffect(lm_selfcare, terms = c("year", "sex"))

#Eating
lm_eating <- lm(eating  ~ year + sex + marstat + raceethnicity + edcat + exfamdum + numhhchild + kidu2dum + kid2to5 + age + weekend,
                  data = atussample, weight=wt20)
peating   <- ggeffect(lm_eating, terms = c("year", "sex"))

#Work & Education
lm_workedu <- lm(workedu  ~ year + sex + marstat + raceethnicity + edcat + exfamdum + numhhchild + kidu2dum + kid2to5 + age + weekend,
                data = atussample, weight=wt20)
pworkedu   <- ggeffect(lm_workedu, terms = c("year", "sex"))

#Carework
lm_allcare <- lm(allcare  ~ year + sex + marstat + raceethnicity + edcat + exfamdum + numhhchild + kidu2dum + kid2to5 + age + weekend,
                 data = atussample, weight=wt20)
pallcare   <- ggeffect(lm_allcare, terms = c("year", "sex"))

#Housework
lm_hwork <- lm(hwork  ~ year + sex + marstat + raceethnicity + edcat + exfamdum + numhhchild + kidu2dum + kid2to5 + age + weekend,
                 data = atussample, weight=wt20)
phwork   <- ggeffect(lm_hwork, terms = c("year", "sex"))

#Passive Leisure
lm_passleis <- lm(passleis  ~ year + sex + marstat + raceethnicity + edcat + exfamdum + numhhchild + kidu2dum + kid2to5 + age + weekend,
               data = atussample, weight=wt20)
ppassleis   <- ggeffect(lm_passleis, terms = c("year", "sex"))

#Other Activities
lm_otheract <- lm(otheract  ~ year + sex + marstat + raceethnicity + edcat + exfamdum + numhhchild + kidu2dum + kid2to5 + age + weekend,
                  data = atussample, weight=wt20)
potheract   <- ggeffect(lm_otheract, terms = c("year", "sex"))

## Create an activity identity variable
pallcare$act  <- "Carework"
peating$act   <- "Eating"
phwork$act    <- "Housework"
potheract$act <- "Other"
ppassleis$act <- "Passive Leisure"
pselfcare$act <- "Self-care"
pworkedu$act  <- "Work & Education"

# Combine the datatables
pred <- rbind(pallcare, peating, phwork, potheract, ppassleis, pselfcare, pworkedu)

# Rename gender variables as parents
levels(pred$group)[levels(pred$group)=="Man"] <- "Fathers"
levels(pred$group)[levels(pred$group)=="Woman"] <- "Mothers"

# Order the activity factors
pred$act <- ordered(pred$act, levels = c("Self-care", "Work & Education", "Housework", "Carework", "Passive Leisure", "Eating", "Other"))

## Graph it

fig1 <- pred %>%
  ggplot(aes(group, predicted, fill = x, label = round(predicted, 0))) +
  geom_col(width = 0.7, position   =  position_dodge(.8)) +
  facet_grid(~act) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.2,
                position=position_dodge(.9), color="grey") +
  geom_text(. %>% filter(act  == "Work & Education" & group == "Fathers"), 
            mapping  = aes(label   =  x, 
                           color    = x,
                           y        = 370),
            position = position_dodge(1.5),
            size     = 3,
            fontface = "bold") +
  theme_minimal() +
  theme(plot.subtitle = element_text(size = 11, vjust = 1),
        plot.caption  = element_text(vjust = 1, size =8, colour = "grey"), 
        legend.position="none",
        strip.text.x  = element_text(size = 8, face = "bold"),
        axis.title    = element_text(size = 9), 
        axis.text     = element_text(size = 8), 
        plot.title    = element_text(size = 12, face = "bold"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()) +
  ggtitle("Figure 1. Average Time Parents Spend Per Day in Each Activity") +
  labs(x = NULL, y = NULL, subtitle = "Predicted minutes per day",
       caption = "Source: American Time Use Surveys (2019/2020) \n Models control for education, race-ethnicity, marital status, extra adults,
       number of household kids, kids under 2, age, weekend diary day\nDue to COVID-19 pandemic, data range: May through December in 2019 vs. 2020")

fig1

ggsave(file.path(outDir, "sequences_fig1.png"), fig1, height = 6, width = 8, dpi = 300)