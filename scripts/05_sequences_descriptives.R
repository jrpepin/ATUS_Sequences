######################################################################################################
# DESCRIBING SEQUENCES

## list of states present in the data set
alphabet(seqdata.seq)

## State distribution table
seqstatd(seqdata.seq) # This shows there are 1440 states (e.g., minutes)

# Calculate the probability of moving from one state (activity) to another
seqtrate(seqdata.seq)

# substitution-cost matrix
costgen=seqsubm(seqdata.seq, method="TRATE")
costgen

################################################################################################
# VISUALIZING SEQUENCES

## Create a legend as a separate graphic since several plots use the same color codes for the states
seqlegend(seqdata.seq, cex = 1.3, ncol = 2)

## seqdplot() for plotting the state distribution at each time point (tempograms)

levels(seqdata$sex)[levels(seqdata$sex)=="Man"]   <- "Fathers"
levels(seqdata$sex)[levels(seqdata$sex)=="Woman"] <- "Mothers"

### tempograms by gender
seqdplot(seqdata.seq, group = seqdata$sex, 
         border = NA, with.legend = F, ylab =NA, xtlab=as.character(1:1440), xtstep=1439, weighted=TRUE)


## seqfplot() for plotting the frequencies of the most frequent sequences
## the sequence frequency plot of the 10 most frequent sequences with bar width proportional to the frequencies
seqfplot(seqdata.seq, group = seqdata$sex, border = NA, with.legend = F, main = "Sequence frequency plot")

## Plot the entropy of the state distribution at each time point
seqHtplot(seqdata.seq, main = "Entropy index")

## seqiplot() for plotting all or a selection of individual sequences
#### seqIplot(seqdata.seq, with.legend = F, border = NA, main = "Index plot (All the sequences)") # This takes forever to run. use with caution
seqIplot(seqdata.seq, with.legend = F, border = NA, main = "Index plot (10 first sequences)") # The time sequence for the first 10 respondents
seqIplot(seqdata.seq, group = seqdata$sex, border=NA, with.legend = F)

## Mean time in each state
seqmtplot(seqdata.seq, group = seqdata$sex, border=NA, with.legend = F, weighted=FALSE) ## The weights create an error, so set them to false.

## Modal state plot
seqmsplot(seqdata.seq, group = seqdata$sex, border=NA, with.legend = F)

## the state distribution by time points
seqdplot(seqdata.seq, with.legend = F, border = NA, main = "State distribution plot")

## Which activities do people move between?
  ### define sequences of transitions (from user's guide)
  seqdatagen.seqe <- seqecreate(seqdata.seq)

  ### plot the transitions
  fsubseq <- seqefsub(seqdatagen.seqe, pmin.support = 0.05)
  plot(fsubseq[1:10])
  dev.off()

################################################################################################
# VISUALIZING CLUSTERS

# Figure 4. Tempograms of the 5 clusters -------------------------------------------
  
## 5	OMS	PAM	Ward https://rpubs.com/Kolpashnikova/sequenceAnalysis
clust5 <- cutree(ward.oms, k = 5)

  clust5 <- factor(clust5, labels = c("Strange", "Day Workers", "Houseworkers", "TV Viewers", "Caregivers"))
  
  # Method 1 (what's saved in outDir)
  png (file.path(outDir, "sequences_fig4.png"), width = 5, height = 5, units = 'in', res = 300)
  seqdplot(seqdata.seq, 
           group = clust5, 
           border = NA, 
           yaxis = FALSE, 
           ylab = "")
  dev.off()

  # Method 2 (what gets printed on plot window)
  keep <- as.clustrange(ward.oms, diss = dist.oms,
                             weights = seqdata$wt20, ncluster = 5)
  
  seqdplot(seqdata.seq, group = keep$clustering$cluster5, border = NA)
  # if error, increase the plot area window
  
################################################################################################
# MULTINOMIAL ANALAYSIS
  
## Assign respondents to a cluster and re-label the typologies -------------------------------
seqdata$hcm <- factor(keep$clustering$cluster5, 
                      levels = c(1, 2, 3, 4, 5), 
                      labels = c("Strange", "Day Workers", "Houseworkers", "TV Viewers", "Caregivers"))

## Save the data as a csv file.
write.csv(seqdata, file.path(outDir, "seqdata.csv"))

## If want to start from saved output file.
seqdata <-  read.csv(file.path(outDir, "seqdata.csv"), header = TRUE)

## I did the ASA paper graphs this way, but we should consider doing regressions as described in Stuber 2013 (see code below)
seqdata$year <- as.factor(seqdata$year)
typodemo <- multinom(hcm  ~ year + sex + marstat + raceethnicity + edcat + employ + exfamdum + numhhchild + kidu2dum + kid2to5 + age + weekend,
                    data = seqdata, weight=wt20)

# Figure 5 ------------------------------------------------------------------------------------
sexpp <- ggeffect(typodemo, terms = c("year", "sex"))

# Rename/order the clusters
colnames(sexpp)[colnames(sexpp) == 'response.level'] <- 'class'
sexpp$class <- as.factor(sexpp$class)
levels(sexpp$class)[levels(sexpp$class)=="Day.Workers"] <- "Day Workers"
levels(sexpp$class)[levels(sexpp$class)=="TV.Viewers"]  <- "TV Viewers"
sexpp$class <- factor(sexpp$class, 
                levels = c("Day Workers", 
                           "Caregivers", 
                           "Houseworkers", 
                           "TV Viewers",
                           "Strange"), ordered = FALSE )

# Rename the gender variables
sexpp$group <- as.factor(sexpp$group)
levels(sexpp$group)[levels(sexpp$group)=="1"] <- "Fathers"
levels(sexpp$group)[levels(sexpp$group)=="2"] <- "Mothers"

fig5 <- sexpp %>%
ggplot(aes(x, predicted, fill = x, label = round(predicted, 0))) +
  geom_col() +
  facet_grid(group~class, switch = "y") +
#  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.2,
#       position            = position_dodge(.9), color="grey") +
  theme_minimal() +
  scale_fill_manual(values  =c("#ec7014", "#7570b3")) +
  theme(legend.position     = "none",
        legend.title        = element_blank(),
        plot.subtitle       = element_text(size = 11, vjust = 1),
        plot.caption        = element_text(vjust = 1, size =8, colour = "grey"), 
        strip.text.y.left   = element_text(angle = 0, face = "bold"),
        strip.text.x        = element_text(face = "bold"),
        strip.placement     = "outside") +
  labs(x        = NULL, 
       y        = NULL, 
       group    = NULL, 
       subtitle = "Predicted proportion per cluster in 2019 and 2020",
       caption  = "Source: American Time Use Surveys \nDue to COVID-19 pandemic, data range: May through December in 2019 and 2020 \nModels control for education, race-ethnicity, marital status, extra adults,
       number of household kids, kids under 2, age, weekend diary day") +
  ggtitle("Figure 5. Sequence Cluster by Gender")
  
fig5

ggsave(file.path(outDir, "sequences_fig5.png"), fig5, height = 6, width = 8, dpi = 300)

## Figure 6 -------------------------------------------------------------------------------
racepp <- ggeffect(typodemo, terms = c("raceethnicity"))

colnames(racepp)[colnames(racepp) == 'response.level'] <- 'class'
racepp$class <- as.factor(racepp$class)

# Revalue the gender factors to be readable
racepp$x <- as.factor(racepp$x)
levels(racepp$x)[levels(racepp$x)=="1"] <- "Asian"
levels(racepp$x)[levels(racepp$x)=="2"] <- "Black"
levels(racepp$x)[levels(racepp$x)=="3"] <- "Hispanic"
levels(racepp$x)[levels(racepp$x)=="4"] <- "Other race"
levels(racepp$x)[levels(racepp$x)=="5"] <- "White"

fig6 <- racepp %>%
  ggplot(aes(x, predicted, fill = x, label = round(predicted, 0))) +
  geom_col() +
  facet_grid(~class) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.2,
                position=position_dodge(.9), color="grey") +
  theme_minimal() +
  ggtitle("Figure 6. Sequence Cluster by Race-ethnicity") +
  labs(x = NULL, y = NULL, subtitle = "Predicted proportion per cluster with model controls",
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

fig6

ggsave(file.path(outDir, "sequences_fig6.png"), fig6, height = 6, width = 8, dpi = 300)


# old <- wcKMedoids(dist.oms, k = 4, weights = seqdata$wt20, initialclust = ward.oms) 
# seqdplot(seqdata.seq, group = old$clustering, border = NA)



## Which transitions are distinctive for each cluster? (this plot needs formatting to be useful)
# discr4 <- seqecmpgroup(fsubseq, group = clust5$clustering)
# plot(discr5[1:5])

# discr5 <- seqecmpgroup(fsubseq, group = clust5)
# plot(discr5[1:5])
# dev.off()

## Plot sequences using ggplot -- FIGURE THIS OUT LATER
# http://databasefaq.com/index.php/answer/2236/r-ggplot2-traminer-sequence-index-plots-in-ggplot2-using-geom-tile-


# 3	Clusters OMS PAM	Ward
# clust3 <- wcKMedoids(dist.oms, k = 3, weights = seqdata$wt20,
# initialclust = ward.oms)
# Tempograms of the 3 clusters
# png ("figures/clust3.png", width = 5, height = 5, units = 'in', res = 300)
# seqdplot(seqdata.seq, group = clust3$clustering, border = NA) # Method 1
# dev.off()

# seqdplot(seqdata.seq, group = range.pam.ward.oms$clustering$cluster3, border = NA) # Method 2

################################################################################################
# REGRESSION ANALYSIS OF CLUSTERS (Struder, 2013)

# a bivariate test of the association
set.seed(1)
dsa <- dissassoc(dist.oms, seqdata$sex, weights = seqdata$wt20,
                 weight.permutation = "diss", R = 5000)
print(dsa$stat) # Interpret Pseudo R2

dsa <- dissassoc(dist.dhd, seqdata$sex, weights = seqdata$wt20,
                 weight.permutation = "diss", R = 5000)

setOutputLevel(Info)
report(Info, "End of 05_sequences_descriptives")     # Marks end of R Script
