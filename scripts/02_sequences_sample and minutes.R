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
tempo_data   <- left_join(atussample, mindata)   # Merge minute data only for cases in sample for tempograms (long)

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

tab1

## https://mran.microsoft.com/snapshot/2017-12-11/web/packages/officer/vignettes/word.html
read_docx() %>% 
  body_add_par("Table 01. Sample Characteristics") %>% 
  body_add_flextable(value = tab1) %>% 
  print(target = file.path(outDir, "sequences_table01.docx"))

save_as_html(tab1, path = file.path(outDir, "sequences_table01.html")) # save a shared copy

# gtsummary::as_gt() %>%
# gt::tab_source_note(gt::md("*Source: American Time Use Survey (2019 & 2020)*
  #                           *Notes: Sample proportions are weighted.*
   #                          *List-wise deletion used to address missing.*"))

# Predicted minutes of activities by gender * year with controls -----------------

modelsW  <- list() # create a list to store the linear models for Women
modelsM  <- list() # create a list to store the linear models for Men
dvnames <- list("selfcare", "workedu", "allcare", "hwork", 
                "passleis", "socleis", "travel", "otheract")
ivnames <- c("year", "marstat", "raceethnicity", 
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
  modelsW[[y]] <- lm(form, data = atussample %>% filter(sex == "Woman"), weight=wt20) 
}

for (y in dvnames){
  form <- as.formula(paste(y, 
                           paste(ivnames, collapse = " + "), 
                           sep = " ~ "))
  modelsM[[y]] <- lm(form, data = atussample %>% filter(sex == "Man"), weight=wt20) 
}

## create a list of the linear models
lapply(modelsW, summary) 
lapply(modelsM, summary) 


## Create a list of Predicted minutes
meansW <- ggeffect(modelsW, terms = c("year")) 
femDF <- bind_rows(meansW, .id = "column_label") # turn the list into a df

meansM <- ggeffect(modelsM, terms = c("year")) 
menDF <- bind_rows(meansM, .id = "column_label") # turn the list into a df

femDF$group <- "Women"
menDF$group <- "Men"

means <- merge(femDF, menDF, all = TRUE)

meansDF <- means %>%
  select(column_label, x, predicted, group) %>%
  pivot_wider(names_from = x, values_from = predicted)

meansDF <- meansDF %>% 
  mutate(value = get("2020") - get("2019")) 

## Rename gender variables
levels(meansDF$group)[levels(meansDF$group)=="Man"]   <- "Men"
levels(meansDF$group)[levels(meansDF$group)=="Woman"] <- "Women"

meansDF$group <- ordered(meansDF$group, 
                         levels = c("Men", "Women"))

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
                                               "Travel", "Other", ordered = FALSE))

diverge_hcl(2, palette = "Cyan-Magenta") #get color codes

# Graph it ------------------------------------------------------------------

fig1 <- meansDF %>%
  ggplot(aes(x = group, y = value, fill = group, label = round(value, 2))) +
  geom_bar(stat = "identity") + 
  facet_grid(cols = vars(act)) +
  geom_text(. %>% filter(act  == "Self-care"), 
            mapping  = aes(label   = group,
                           y       = -3),
            size     = 5,
            colour  = c("#0FCFC0", "#F79CD4"),
            fontface = "bold") +
  geom_text(position=position_dodge(0.5), vjust=-0.25) + 
  scale_fill_discrete_diverging(palette = "Cyan-Magenta") +
  theme_minimal() +
  theme(legend.position="none",
        text = element_text(size = 19),
        strip.text.x  = element_text(face = "bold"),
        axis.text.x=element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = NULL, y = NULL)
 #       subtitle = "Difference between 2020 and 2019",
 #      caption = "Source: American Time Use Surveys \n Models control for education, employment, race-ethnicity, marital status, extra adults,
 #     number of household kids, kids under 2, age, weekend diary day\nDue to COVID-19 pandemic, data range: May through December in 2019 and 2020") +
 # ggtitle("Figure 1. Change in Time Spent Per Day in _________") 


fig1

ggsave(file.path(outDir, "sequences_fig1.png"), fig1, width = 14, height = 5, units = "in", dpi = 300)


# FIGURE 2 --------------------------------------------------------------------------

tempdata <- tempo_data %>% 
  group_by(year, sex, minute) %>% 
  count(actcat , wt = wt20) %>% 
  mutate(per = n/sum(n))

# Renaming factor levels
levels(tempdata$sex) <- c("Men", "Women")

tempdata$actcat <- as.factor(tempdata$actcat)
tempdata$actcat <- ordered(tempdata$actcat, 
                           levels = c( "Carework", "Housework", "Other", 
                                       "Personal Care", "Sedentary Leisure", "Social Leisure",
                                       "Travel", "Work & Educ."))
#E16A86 pink
#C7821C orange
#909800 yellow-green
#e5d8bd tan
#00A846 green
#00A2D3 blue
#9183E6 purple
#D766C9 pink-purple


fig2 <- ggplot(tempdata, aes(x=minute, y=per, fill=actcat)) + 
  geom_area(alpha    = 0.8, 
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
  scale_fill_manual(values  = c("#E16A86", "#C7821C", 
                                "#909800", "#e5d8bd", 
                                "#00A846", "#00A2D3",
                                "#9183E6", "#D766C9")) +
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
