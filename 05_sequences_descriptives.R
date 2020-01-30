######################################################################################################
# DESCRIBING SEQUENCES

## list of states present in the data set
alphabet(atus2017.seq)

## State distribution table
seqstatd(atus2017.seq) # This shows there are 1440 states (e.g., minutes)

# Calculate the probability of moving from one state (activity) to another
seqtrate(atus2017.seq)

# substitution-cost matrix
costgen=seqsubm(atus2017.seq, method="TRATE")
costgen

################################################################################################
# VISUALIZING SEQUENCES

## Create a legend as a separate graphic since several plots use the same color codes for the states
seqlegend(atus2017.seq, cex = 1.3, ncol = 2)

## seqdplot() for plotting the state distribution at each time point (tempograms)

levels(atus2017$sex)[levels(atus2017$sex)=="Man"]   <- "Fathers"
levels(atus2017$sex)[levels(atus2017$sex)=="Woman"] <- "Mothers"

### tempograms by gender
seqdplot(atus2017.seq, group = atus2017$sex, 
         border = NA, with.legend = F, ylab =NA, xtlab=as.character(1:1440), xtstep=1439, weighted=TRUE)


## seqfplot() for plotting the frequencies of the most frequent sequences
## the sequence frequency plot of the 10 most frequent sequences with bar width proportional to the frequencies
seqfplot(atus2017.seq, group = atus2017$sex, border = NA, with.legend = F, main = "Sequence frequency plot")

## Plot the entropy of the state distribution at each time point
seqHtplot(atus2017.seq, main = "Entropy index")

## seqiplot() for plotting all or a selection of individual sequences
#### seqIplot(atus2017.seq, with.legend = F, border = NA, main = "Index plot (All the sequences)") # This takes forever to run. use with caution
seqIplot(atus2017.seq, with.legend = F, border = NA, main = "Index plot (10 first sequences)") # The time sequence for the first 10 respondents
seqIplot(atus2017.seq, group = atus2017$sex, border=NA, with.legend = F)

## Mean time in each state
seqmtplot(atus2017.seq, group = atus2017$sex, border=NA, with.legend = F, weighted=FALSE) ## The weights create an error, so set them to false.

## Modal state plot
seqmsplot(atus2017.seq, group = atus2017$sex, border=NA, with.legend = F)

## the state distribution by time points
seqdplot(atus2017.seq, with.legend = F, border = NA, main = "State distribution plot")

## Which activities do people move between?
  ### define sequences of transitions (from user's guide)
  seqdatagen.seqe <- seqecreate(atus2017.seq)

  ### plot the transitions
  fsubseq <- seqefsub(seqdatagen.seqe, pmin.support = 0.05)
  plot(fsubseq[1:10])
  dev.off()

################################################################################################
# VISUALIZING CLUSTERS


# OR
# 4	OMS	HCM	Ward
clust4 <- cutree(ward.oms, k = 4)
  
# Tempograms of the 4 clusters
  png ("figures/clust4.png", width = 5, height = 5, units = 'in', res = 300)
  seqdplot(atus2017.seq, group = clust4, border = NA) # Method 1
  dev.off()

keep <- as.clustrange(ward.oms, diss = dist.oms, ### I think I'm using this one now?
                           weights = atus2017$wt06, ncluster = 4)
seqdplot(atus2017.seq, group = keep$clustering$cluster4, border = NA)

## Assign respondents to a cluster and re-label the typologies
atus2017$hcm <- factor(keep$clustering$cluster4, levels = c(1, 2,
                                                     3, 4), labels = c("Day Workers", "Other", "Homeworkers",
                                                                           "Night Workers"))

## Save the data as a csv file.
write.csv(atus2017, "data/atus17typo.csv")

## If want to start from saved output file.
atus2017 <-  read.csv("data/atus17typo.csv", header = TRUE)

## I did the ASA paper graphs this way, but we should consider doing regressions as described in Stuber 2013 (see code below)
library("nnet")
library("ggeffects")
typodemo <- multinom(hcm  ~ sex + marstat + raceethnicity + edcat + exfamdum + numhhchild + kidu2dum + kid2to5 + age + weekend,
                    data = atus2017, weight=wt06)

## Figure 4
sexpp <- ggeffect(typodemo, terms = c("sex"))

colnames(sexpp)[colnames(sexpp) == 'response.level'] <- 'class'
sexpp$class <- as.factor(sexpp$class)

# Revalue the gender factors to be readable
sexpp$x <- as.factor(sexpp$x)
levels(sexpp$x)[levels(sexpp$x)=="1"] <- "Fathers"
levels(sexpp$x)[levels(sexpp$x)=="2"] <- "Mothers"

fig4 <- sexpp %>%
ggplot(aes(x, predicted, fill = x, label = round(predicted, 0))) +
  geom_col() +
  facet_grid(~class) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.2,
                position=position_dodge(.9), color="grey") +
  theme_minimal() +
  ggtitle("Figure 4. Sequence Cluster by Gender") +
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

ggsave("figures/sequences_fig4.png", fig4, height = 6, width = 8, dpi = 300)


## Figure 4
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

fig5 <- racepp %>%
  ggplot(aes(x, predicted, fill = x, label = round(predicted, 0))) +
  geom_col() +
  facet_grid(~class) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.2,
                position=position_dodge(.9), color="grey") +
  theme_minimal() +
  ggtitle("Figure 5. Sequence Cluster by Race-ethnicity") +
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

fig5

ggsave("figures/sequences_fig5.png", fig5, height = 6, width = 8, dpi = 300)


# old <- wcKMedoids(dist.oms, k = 4, weights = atus2017$wt06, initialclust = ward.oms) 
# seqdplot(atus2017.seq, group = old$clustering, border = NA)



## Which transitions are distinctive for each cluster? (this plot needs formatting to be useful)
# discr4 <- seqecmpgroup(fsubseq, group = clust4$clustering)
# plot(discr5[1:5])

# discr5 <- seqecmpgroup(fsubseq, group = clust5)
# plot(discr5[1:5])
# dev.off()

## Plot sequences using ggplot -- FIGURE THIS OUT LATER
# http://databasefaq.com/index.php/answer/2236/r-ggplot2-traminer-sequence-index-plots-in-ggplot2-using-geom-tile-


# 3	Clusters OMS PAM	Ward
# clust3 <- wcKMedoids(dist.oms, k = 3, weights = atus2017$wt06,
# initialclust = ward.oms)
# Tempograms of the 3 clusters
# png ("figures/clust3.png", width = 5, height = 5, units = 'in', res = 300)
# seqdplot(atus2017.seq, group = clust3$clustering, border = NA) # Method 1
# dev.off()

# seqdplot(atus2017.seq, group = range.pam.ward.oms$clustering$cluster3, border = NA) # Method 2

################################################################################################
# REGRESSION ANALYSIS OF CLUSTERS (Struder, 2013)

# a bivariate test of the association
set.seed(1)
dsa <- dissassoc(dist.oms, atus2017$sex, weights = atus2017$wt06,
                 weight.permutation = "diss", R = 5000)
print(dsa$stat) # Interpret Pseudo R2

dsa <- dissassoc(dist.dhd, atus2017$sex, weights = atus2017$wt06,
                 weight.permutation = "diss", R = 5000)

