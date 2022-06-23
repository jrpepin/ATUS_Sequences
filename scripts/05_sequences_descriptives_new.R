#CLUSTER SOLUTION 6	OMS	PAM	Ward -----------------------------------------------
  # https://rpubs.com/Kolpashnikova/sequenceAnalysis

clust6 <- cutree(ward.oms, k = 6)

## label the clusters
clust6 <- factor(clust6, labels = c("Errands", "Houseworkers", "Paid Workers",  "Work & CareTakers",  "Players", "Caregivers"))

## contains a list of clustering solution with associated statistics
# keep <- as.clustrange(ward.oms, 
#                       diss = dist.oms,
#                       weights = seqdata$wt20, 
#                       ncluster = 6)
#   
# saveRDS(keep, file = file.path(outDir, "keep.RDS")) # save this version

keep <- readRDS(file.path(outDir, "keep.RDS")) # load saved version

# Assign respondents to a cluster ----------------------------------------------
  # seqdata$hcm <- factor(keep$clustering$cluster6, 
  #                       levels = c(1, 2, 3, 4, 5, 6), 
  #                       labels = c("Errands", "Houseworkers", "Paid Workers",  "Work & CareTakers",  "Players", "Caregivers"))
  # 
  # ## Save the data as a csv file.
  # write.csv(seqdata, file.path(outDir, "seqdata.csv"))

## Start from saved output file. 
seqdata <-  read.csv(file.path(outDir, "seqdata.csv"), header = TRUE)

# DESCRIPTIVES -----------------------------------------------------------------
seqdata$hcm = fct_infreq(seqdata$hcm) # put clusters in order of frequency
table(seqdata$hcm)

seqdata.seq %>%
  ggseqdplot(group = seqdata$hcm)

## subset data by cluster

### cl1
cl1<-(seqdata.seq[seqdata$hcm ==  "Paid Workers",])

cl1_gg <- cl1 %>%
  ggseqdplot() +
  theme_minimal() +
  theme(legend.position="none",
        text = element_text(size = 19),
        strip.text.x  = element_text(face = "bold"),
        axis.text.x=element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(title = "Cluster 1: Paidworkers",
       subtitle = paste("N = ", nrow(cl1)),
       x = NULL, 
       y = NULL) 

cl1_gg

ggsave(file.path(outDir, "sequences_cl1_gg.png"), cl1_gg, width = 5, height = 5, units = "in", dpi = 300)


### cl2
cl2<-(seqdata.seq[seqdata$hcm ==  "Houseworkers",])

cl2_gg <- cl2 %>%
  ggseqdplot() +
  theme_minimal() +
  theme(legend.position="none",
        text = element_text(size = 19),
        strip.text.x  = element_text(face = "bold"),
        axis.text.x=element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(title = "Cluster 2: Houseworkers",
       subtitle = paste("N = ", nrow(cl2)),
       x = NULL, 
       y = NULL) 

cl2_gg

ggsave(file.path(outDir, "sequences_cl2_gg.png"), cl2_gg, width = 5, height = 5, units = "in", dpi = 300)


### cl3
cl3<-(seqdata.seq[seqdata$hcm ==  "Players",])

cl3_gg <- cl3 %>%
  ggseqdplot() +
  theme_minimal() +
  theme(legend.position="none",
        text = element_text(size = 19),
        strip.text.x  = element_text(face = "bold"),
        axis.text.x=element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(title = "Cluster 3: Players",
       subtitle = paste("N = ", nrow(cl3)),
       x = NULL, 
       y = NULL) 

cl3_gg

ggsave(file.path(outDir, "sequences_cl3_gg.png"), cl3_gg, width = 5, height = 5, units = "in", dpi = 300)


### cl4
cl4<-(seqdata.seq[seqdata$hcm ==  "Errands",])

cl4_gg <- cl4 %>%
  ggseqdplot() +
  theme_minimal() +
  theme(legend.position="none",
        text = element_text(size = 19),
        strip.text.x  = element_text(face = "bold"),
        axis.text.x=element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(title = "Cluster 4: Errands",
       subtitle = paste("N = ", nrow(cl4)),
       x = NULL, 
       y = NULL) 

cl4_gg

ggsave(file.path(outDir, "sequences_cl4_gg.png"), cl4_gg, width = 5, height = 5, units = "in", dpi = 300)


### cl5
cl5<-(seqdata.seq[seqdata$hcm ==  "Work & CareTakers",])

cl5_gg <- cl5 %>%
  ggseqdplot() +
  theme_minimal() +
  theme(legend.position="none",
        text = element_text(size = 19),
        strip.text.x  = element_text(face = "bold"),
        axis.text.x=element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(title = "Cluster 5: Work & CareTakers",
       subtitle = paste("N = ", nrow(cl5)),
       x = NULL, 
       y = NULL) 

cl5_gg

ggsave(file.path(outDir, "sequences_cl5_gg.png"), cl5_gg, width = 5, height = 5, units = "in", dpi = 300)


### cl6
cl6<-(seqdata.seq[seqdata$hcm ==  "Caregivers",])

cl6_gg <- cl6 %>%
  ggseqdplot() +
  theme_minimal() +
  theme(legend.position="none",
        text = element_text(size = 19),
        strip.text.x  = element_text(face = "bold"),
        axis.text.x=element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(title = "Cluster 6: Caregivers",
       subtitle = paste("N = ", nrow(cl6)),
       x = NULL, 
       y = NULL) 

cl5_gg

ggsave(file.path(outDir, "sequences_cl6_gg.png"), cl6_gg, width = 5, height = 5, units = "in", dpi = 300)


# Figure 5 ---------------------------------------------------------------------

## We should consider doing regressions as described in Stuber 2013
seqdata$year <- as.factor(seqdata$year)
typodemo <- multinom(hcm  ~ year + sex + marstat + raceethnicity + edcat + employ + exfamdum + numhhchild + kidu2dum + kid2to5 + age + weekend,
                     data = seqdata, weight=wt20)

sexpp <- ggeffect(typodemo, terms = c("sex"))

# Rename/order the clusters
colnames(sexpp)[colnames(sexpp) == 'response.level'] <- 'class'
sexpp$class <- as.factor(sexpp$class)
levels(sexpp$class)[levels(sexpp$class)=="Paid.Workers"]       <- "Paid Workers"
levels(sexpp$class)[levels(sexpp$class)=="Work...CareTakers"]  <- "Work &\nCareTakers"
sexpp$class <- factor(sexpp$class, 
                      levels = c("Paid Workers", 
                                 "Houseworkers", 
                                 "Players", 
                                 "Errands",
                                 "Work &\nCareTakers",
                                 "Caregivers"), ordered = FALSE )

# Rename the gender variables
sexpp$x <- as.factor(sexpp$x)
levels(sexpp$x)[levels(sexpp$x)=="Man"] <- "Men"
levels(sexpp$x)[levels(sexpp$x)=="Woman"] <- "Women"

fig5 <- sexpp %>%
  ggplot(aes(x, predicted, fill = x, label = round(predicted, 2))) +
  geom_col() +
  facet_grid(~class) +
  geom_text(position=position_dodge(0.5), vjust=-0.25) + 
  #  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.2,
  #       position            = position_dodge(.9), color="grey") +
  theme_minimal() +
  scale_fill_discrete_diverging(palette = "Cyan-Magenta") +
  ylim(0, .35) +
  theme(legend.position     = "none",
        legend.title        = element_blank(),
        text                = element_text(size = 19),
        plot.subtitle       = element_text(vjust = 1),
        plot.caption        = element_text(vjust = 1, colour = "grey"), 
        strip.text.y.left   = element_text(angle = 0, face = "bold"),
        strip.text.x        = element_text(face = "bold"),
        strip.placement     = "outside") +
  labs(x        = NULL, 
       y        = NULL, 
       group    = NULL, 
       caption  = "Source: American Time Use Surveys \nDue to COVID-19 pandemic, data range: May through December in 2019 and 2020 \nModels control for education, race-ethnicity, marital status, extra adults,
       number of household kids, kids under 2, age, weekend diary day") 
#  ggtitle("Predicted proportion per cluster in 2019 and 2020")

fig5

ggsave(file.path(outDir, "sequences_fig5.png"), fig5, height = 6, width = 12, units = "in", dpi = 300)


# Figure 6 ---------------------------------------------------------------------
mendata <- seqdata %>%
  filter(sex == "Man")

femdata <- seqdata %>%
  filter(sex == "Woman")

typomen <- multinom(hcm  ~ year + marstat + raceethnicity + edcat + employ + exfamdum + numhhchild + kidu2dum + kid2to5 + age + weekend,
                     data = mendata, weight=wt20)

typofem <- multinom(hcm  ~ year + marstat + raceethnicity + edcat + employ + exfamdum + numhhchild + kidu2dum + kid2to5 + age + weekend,
                    data = femdata, weight=wt20)

menpp <- ggeffect(typomen, terms = c("year"))
fempp <- ggeffect(typofem, terms = c("year"))

fempp$group <- "Women"
menpp$group <- "Men"

gender <- merge(fempp, menpp, all = TRUE)

gender <- gender %>%
  select(response.level, x, predicted, group) %>%
  pivot_wider(names_from = x, values_from = predicted)

gender$value <- gender$`2020` - gender$`2019`

## Order and label activity factors
gender$typo <- as.factor(gender$response.level)

levels(gender$typo)[levels(gender$typo)=="Paid.Workers"]       <- "Paid Workers"
levels(gender$typo)[levels(gender$typo)=="Work...CareTakers"]  <- "Work &\nCareTakers"
gender$typo <- factor(gender$typo, 
                      levels = c("Paid Workers", 
                                 "Houseworkers", 
                                 "Players", 
                                 "Errands",
                                 "Work &\nCareTakers",
                                 "Caregivers"), ordered = FALSE )

fig6 <- gender %>%
  ggplot(aes(x = group, y = value, fill = group, label = round(value, 3))) +
  geom_bar(stat = "identity") + 
  facet_grid(cols = vars(typo)) +
  geom_text(. %>% filter(typo  == "Paid Workers"), 
             mapping  = aes(label   = group,
                            y       = -.02),
             size     = 5,
             colour  = c("#F79CD4", "#0FCFC0"),
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
# ggtitle("Figure 6. Change in Cluster") 

fig6

ggsave(file.path(outDir, "sequences_fig6.png"), fig6, width = 12, height = 6, units = "in", dpi = 300)
