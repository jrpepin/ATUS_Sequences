setwd("C:/Users/Joanna/Dropbox/Repositories/ATUS_Sequences/docs")

# Libraries
library(TraMineR)
library(TraMineRextras)
library(WeightedCluster)
library(cluster)

# Create a subsample so it doesn't take forever to run
atus2017 <- atus2017[1:500,]

# Define the sequence
## Specify the columns that represent states (24:1463)
which( colnames(atus2017)=="1")
which( colnames(atus2017)=="1440")

## What the states are (in this case, the activity types) and what the labels are
###This is in alphabetical order.
atus2017.labels <- c("Carework", "Eating", "Housework", "Other", "Passive Leisure", "Sleep & Selfcare", "Work & Edu") # Create long labels.
atus2017.scode <- c("C", "E", "H", "O", "P", "S", "W") # Create alphabet list. 

## Define the sequence object
atus2017.seq <- seqdef(data = atus2017, var = 24:1463, states = atus2017.scode, labels = atus2017.labels, weights = atus2017$wt06)
### xtstep option sets the step between displayed tick-marks and labels on the x-axis of state sequence plots.

## Setting a user defined color palette
cpal(atus2017.seq) <- c("#7570b3", "#ec7014", "#1b9e77",  "#e6ab02", "#e7298a", "#e5d8bd", "#1f78b4") 

## Look at the sequence object
summary(atus2017.seq)
## Shows the number of sequences and unique sequences. 
## Sequences should be the number of observations, unique sequences will be the number of unique patterns of time use.
## in this case, we used a minute file, with an activity logged for every minute of the day. 
## No two respondents followed the exact same pattern to the minute, so the number of observations equals the number of sequence. 
## This also confirms that our min & max sequence length is 1440 minutes, equal to the number of minutes in a diary day.

atus2017.seq[1:5, 300:350] # Look at sequences of first 5 respondents, minutes 300-350
print(atus2017.seq[1:5, 300:350], format = "SPS") # more concise view of sequences with the SPS state-permanence representation.

######################################################################################################
# VISUALIZING SEQUENCES
## Create a legend as a separate graphic since several plots use the same color codes for the states
seqlegend(atus2017.seq, cex = 1.3)

## seqdplot() for plotting the state distribution at each time point (tempograms)
seqdplot(atus2017.seq, group = atus2017$sex, 
         border = NA, with.legend = F, ylab =NA, xtlab=as.character(1:1440), xtstep=1439, weighted=TRUE) ### tempograms by gender

## seqfplot() for plotting the frequencies of the most frequent sequences
seqfplot(atus2017.seq, group = atus2017$sex, border = NA, with.legend = F)

## seqiplot() for plotting all or a selection of individual sequences
seqIplot(atus2017.seq,idxs=1:10, border=NA, with.legend = F) # The time sequence for the first 10 respondents

######################################################################################################
# DESCRIBING SEQUENCES

## list of states present in the data set
alphabet(atus2017.seq)

## State distribution table
seqstatd(atus2017.seq) # This doesn't tell me anything

# Calculate the probability of moving from one state (activity) to another
seqtrate(atus2017.seq)

# substitution-cost matrix
costgen=seqsubm(atus2017.seq, method="TRATE")
costgen

# Using DHD allows for timing of the transition from one state to another to vary, which can be relevant in time use data.
# https://www.lives-nccr.ch/sites/default/files/pdf/publication/33_lives_wp_studer_sequencedissmeasures.pdf

distancegen=seqdist(atus2017.seq, method="DHD")
summary(distancegen)

# establish clusters based on the matrix created above

## using "cluster" rather than "hclust" can work if you use the "agnes" command
clustergen<- hclust(as.dist(distancegen), method = "ward.D", members = atus2017$wt06) 

## create a dendogram
plot(clustergen)


## plot from traminer manual - all 5 clusters together (yields 10 bars per cluster)




#All the sequences
png ("index.png")
seqIplot(seqdatagen,border=NA)
dev.off()

# The time sequence for the first respondent (can specify a range at idxs=1 command (idxs=1:5, for example))
png ("index1.png")
seqIplot(seqdatagen,idxs=1, border=NA)
dev.off()

### Which activities do people move between?
## define sequences of transitions (from user's guide)
seqdatagen.seqe <- seqecreate(seqdatagen)

## plot the transitions
fsubseq <- seqefsub(seqdatagen.seqe, pmin.support = 0.05)
png ("activitytransitions.png")
plot(fsubseq[1:10])
dev.off()

### Which transitions are distinctive for each cluster? (this plot needs formatting to be useful)
discr <- seqecmpgroup(fsubseq, group = facgen5clusters)
png ("distinctivetransitions.png")
plot(discr[1:5])
dev.off()

## plot from traminer manual - all 5 clusters together (5 tempograms)
png ("tempos5clustersgen2.png")
seqdplot(seqdatagen, group = facgen5clusters, border = NA)
dev.off()

## create a dataframe from the cluster information
as.data.frame(facgen5clusters)
clustersgen5.df<-as.data.frame(facgen5clusters)
View(clustersgen5.df)

## bind that variable with the cluster labels to the original dataset
datawclusgenw5.df<-cbind(clustersgen5.df, atusgenseq.df)
View(datawclusgenw5.df)

################################################################################################
# Visualizing sequences

## the index plot of the first 10 sequences
seqiplot(atus2017.seq, with.legend = F, border = NA, main = "Index plot (10 first sequences)")

## All the sequences
# seqIplot(atus2017.seq, with.legend = F, border = NA, main = "Index plot (All the sequences)") # This takes forever to run. use with caution

## the sequence frequency plot of the 10 most frequent sequences with bar width proportional to the frequencies
seqfplot(atus2017.seq, with.legend = F, border = NA, main = "Sequence frequency plot")

## the state distribution by time points
seqdplot(atus2017.seq, with.legend = F, border = NA, main = "State distribution plot")

## Plot the entropy of the state distribution at each time point
seqHtplot(atus2017.seq, main = "Entropy index")

## Compute the optimal matching distances using substitution costs based on transition rates
submat <- seqsubm(atus2017.seq, method = "TRATE")
dist.om1 <- seqdist(atus2017.seq, method = "OM", indel = 1, sm = submat) # This takes 3 1/2 hours to run on the full sample.

library(cluster)

clusterward1 <- agnes(dist.om1, diss = TRUE, method = "ward")

plot(clusterward1)

cl1.5 <- cutree(clusterward1, k = 5)

cl1.5fac <- factor(cl1.5, labels = paste("Type", 1:5))

seqdplot(atus2017.seq, group = cl1.5fac, border = NA)

seqfplot(atus2017.seq, group = cl1.5fac, border = NA)

## Plot sequences using ggplot
# http://databasefaq.com/index.php/answer/2236/r-ggplot2-traminer-sequence-index-plots-in-ggplot2-using-geom-tile-