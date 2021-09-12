#------------------------------------------------------------------------------------
# ATUS SEQUENCE ANALYSIS
# 02_sequences_sample and minutes.R
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
mindata_w <- mindata %>%
  select(caseid, minute, actcat) %>%
  spread(minute, actcat)

# Create analytic sample (person level) --------------------------------------------

atussample <- atus %>%
  filter(month      >= 5) %>% ## diaries from May to Dec.
  select(caseid, year, month, wt20, 
         selfcare, workedu, allcare, hwork, passleis, socleis, travel, otheract, 
         sex, marstat, raceethnicity, edcat, employ, 
         exfamdum, numhhchild, kidu2dum, kid2to5, age, weekend,
         married, nevmar, umpartner, divsep,
         lths, highschool, somecol, baormore, 
         fulltime, parttime, unemployed,
         white, black, asian, hispanic, otherrace,
         # Newly added variables (process first)
         hh_ec,
         ownrent,
         region,
         diffany,
         ageychild, 
         hhchildu18,
         hhchildu13)

## Missing data  
colSums(is.na(atussample))

seqdata  <- left_join(atussample, mindata_w) # Merge minute data only for cases in sample

mindata$year <- as.factor(mindata$year)
tempdata <- left_join(atussample, mindata)   # Merge minute data only for cases in sample for tempograms (long)

# Table 01: Sample characteristics -----------------------------------------------------

## Number of observations (unweighted)
table(atussample$sex)

## Create survey data
tab1data <- atussample %>%
  select("wt20", "year", "weekend", 
         "sex",  "marstat", "raceethnicity", "age",
         "edcat", "employ", "ownrent", 
         "hhchildu18", "numhhchild", 
         "kidu2dum", "kid2to5", "hhchildu13", 
         "exfamdum", "hh_ec", "diffany",
         "region")

tab1Svy <- svydesign(ids = ~1, weights = ~ wt20, data = tab1data)

tab1 <- tab1Svy %>%
  tbl_svysummary(by = "sex",
                 statistic = all_categorical() ~ "{n_unweighted} ({p}%)",
    label = list(marstat    ~ "Relationship Status",
                 raceethnicity ~ "Race/ethnicity",
                 edcat      ~ "Educational attainment",
                 employ     ~ "Employment status",
                 exfamdum   ~ "Extended HH Family Member",
                 hh_ec      ~ "HH Member Eldercare Recipient",
                 diffany    ~ "Physical or Cognitive difficulty",
                 hhchildu18 ~ "Parent",
                 kidu2dum   ~ "Presence of Kid under 2",
                 kid2to5    ~ "Presence of Kid 2 to 5",
                 hhchildu13 ~ "Presence of Kid under 13",
                 age        ~ "Age",
                 numhhchild ~ "Number of Kids in Household",
                 ownrent    ~ "Home Ownership",
                 year       ~ "Year",
                 weekend    ~ "Weekend diary day"),
    value = list(kidu2dum   = "1",
                 kid2to5    = "1"),
    type = list(numhhchild ~ "continuous"))  %>%
  add_overall() %>%
  modify_header(
    update = list(
      label ~ "**Variable**",
      stat_0 ~ "**Overall** N = {N_unweighted}",
      stat_1 ~ "**{level}** N = {n_unweighted}",
      stat_2 ~ "**{level}** N = {n_unweighted}")) %>%
  as_flex_table() 

## https://mran.microsoft.com/snapshot/2017-12-11/web/packages/officer/vignettes/word.html
read_docx() %>% 
  body_add_par("Table 01. Sample Characteristics") %>% 
  body_add_flextable(value = tab1) %>% 
  print(target = file.path(outDir, "sequences_table01.docx"))


# gtsummary::as_gt() %>%
# gt::tab_source_note(gt::md("*Source: American Time Use Survey (2019 & 2020)*
  #                           *Notes: Sample proportions are weighted.*
   #                          *List-wise deletion used to address missing.*"))

# Predicted minutes of activities by gender * year with controls -----------------

models  <- list() # create a list to store the linear models
dvnames <- list("selfcare", "workedu", "allcare", "hwork", 
                "passleis", "socleis", "travel", "otheract")
ivnames <- c("year", "sex",  "marstat", "raceethnicity", 
             "age", "edcat", "employ", "ownrent", 
             "hhchildu18", "numhhchild", 
             "kidu2dum", "kid2to5", "hhchildu13", 
             "exfamdum", "hh_ec", "diffany", "weekend", 
             "region")

## create a function to loop over dv and ivs
for (y in dvnames){
  form <- as.formula(paste(y, 
                           paste(ivnames, collapse = " + "), 
                           sep = " ~ "))
  models[[y]] <- lm(form, data = atussample, weight=wt20) 
}

## create a list of the linear models
lapply(models, summary) 


## Create a list of Predicted minutes
means <- ggeffect(models, terms = c("year", "sex")) 
meansDF <- bind_rows(means, .id = "column_label") # turn the list into a df

## Rename gender variables as parents
levels(meansDF$group)[levels(meansDF$group)=="Man"]   <- "Men"
levels(meansDF$group)[levels(meansDF$group)=="Woman"] <- "Women"

## Order and label activity factors
meansDF$act <- as.factor(meansDF$column_label)

levels(meansDF$act)[levels(meansDF$act)=="allcare"]  <- "Carework"
levels(meansDF$act)[levels(meansDF$act)=="hwork"]    <- "Housework"
levels(meansDF$act)[levels(meansDF$act)=="otheract"] <- "Other"
levels(meansDF$act)[levels(meansDF$act)=="passleis"] <- "Passive Leisure"
levels(meansDF$act)[levels(meansDF$act)=="socleis"]  <- "Social Leisure"
levels(meansDF$act)[levels(meansDF$act)=="travel"]   <- "Travel"
levels(meansDF$act)[levels(meansDF$act)=="selfcare"] <- "Self-care"
levels(meansDF$act)[levels(meansDF$act)=="workedu"]  <- "Work & Educ."

meansDF$act <- ordered(meansDF$act, levels = c("Self-care", "Work & Educ.", 
                                               "Housework", "Carework", 
                                               "Passive Leisure", "Social Leisure", 
                                               "Travel", "Other"))

# Graph it ------------------------------------------------------------------

fig1 <- meansDF %>%
  ggplot(aes(group, predicted, fill = x, label = round(predicted, 0))) +
  geom_col(width = 0.7, position   =  position_dodge(.8)) +
  facet_grid(~act) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.2,
                position=position_dodge(.9), color="grey") +
  geom_text(. %>% filter(act  == "Work & Educ." & group == "Men"), 
            mapping  = aes(label   =  x, 
                           color    = x,
                           y        = 190),
            position = position_dodge(1.5),
            size     = 3,
            fontface = "bold") +
  theme_minimal() +
  scale_fill_manual(values  =c("#ec7014", "#7570b3")) +
  scale_colour_manual(values=c("#ec7014", "#7570b3")) +
  theme(plot.subtitle = element_text(size = 11, vjust = 1),
        plot.caption  = element_text(vjust = 1, size =8, colour = "grey"), 
        legend.position="none",
        strip.text.x  = element_text(size = 8, face = "bold"),
        axis.title    = element_text(size = 9), 
        axis.text     = element_text(size = 8), 
        plot.title    = element_text(size = 12, face = "bold"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = NULL, y = NULL, 
       subtitle = "Predicted minutes per day",
       caption = "Source: American Time Use Surveys \n Models control for education, employment, race-ethnicity, marital status, extra adults,
       number of household kids, kids under 2, age, weekend diary day\nDue to COVID-19 pandemic, data range: May through December in 2019 and 2020") +
  ggtitle("Figure 1. Average Time Spent Per Day in _________") 

fig1

ggsave(file.path(outDir, "sequences_fig1.png"), fig1, height = 6, width = 8, dpi = 300)


# FIGURE 2 --------------------------------------------------------------------------

tempdata <- tempdata %>% 
  group_by(year, sex, minute) %>% 
  count(actcat , wt = wt20) %>% 
  mutate(per = n/sum(n))

# Renaming factor levels
levels(tempdata$sex) <- c("Men", "Women")

tempdata$actcat <- as.factor(tempdata$actcat)
tempdata$actcat <- ordered(tempdata$actcat, 
                           levels = c( "Other", "Travel",
                                       "Social Leisure", "Sedentary Leisure",  
                                       "Carework", "Housework", 
                                        "Personal Care", "Work & Educ."))

fig2 <- ggplot(tempdata, aes(x=minute, y=per, fill=actcat)) + 
  geom_area(alpha    = 0.6, 
            size     = 1, 
            colour   = "black", 
            position = position_fill(reverse = TRUE)) +
  facet_grid(sex ~ year, switch = "y") +
  theme_minimal() +
  theme(legend.position     = "bottom",
        legend.title        = element_blank(),
        plot.subtitle       = element_text(size = 11, vjust = 1),
        plot.caption        = element_text(vjust = 1, size =8, colour = "grey"), 
        strip.text.y.left   = element_text(angle = 0, face = "bold"),
        strip.text.x        = element_text(face = "bold"),
        strip.placement     = "outside") +
  scale_fill_manual(values  = c("#7570b3", "#ec7014", 
                                "#1b9e77", "#e6ab02", 
                                "#e7298a", "#1f78b4",
                                "#e5d8bd", "#e32636")) +
  scale_x_continuous(limits = c(0, 1440),
                     breaks = c(0, 500, 1000, 1440)) +
  labs(x        = NULL, 
       y        = NULL,
       subtitle = "for each minute on time diary day",
       caption  = "Source: American Time Use Surveys \nDue to COVID-19 pandemic, data range: May through December in 2019 and 2020") +
  ggtitle("Figure 2. Tempograms: Proportion of Men' and Women' in Each Activity") 
  

fig2

ggsave(file.path(outDir, "sequences_fig2.png"), fig2, height = 6, width = 8, dpi = 300)


setOutputLevel(Info)
report(Info, "End of 02_sequences_sample and minutes")     # Marks end of R Script
