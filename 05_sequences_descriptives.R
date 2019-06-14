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
seqlegend(atus2017.seq, cex = 1.3)

## seqdplot() for plotting the state distribution at each time point (tempograms)
seqdplot(atus2017.seq, group = atus2017$sex, 
         border = NA, with.legend = F, ylab =NA, xtlab=as.character(1:1440), xtstep=1439, weighted=TRUE) ### tempograms by gender

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

################################################################################################
# VISUALIZING CLUSTERS

# 3	Clusters OMS PAM	Ward
clust3 <- wcKMedoids(dist.oms, k = 3, weights = atus2017$wt06,
                           initialclust = ward.oms)
# OR
# 4	DHD	HCM	Beta
clust4 <- cutree(beta.dhd, k = 4)

# Tempograms of the 3 clusters
png ("figures/clust3.png", width = 5, height = 5, units = 'in', res = 300)
seqdplot(atus2017.seq, group = clust3$clustering, border = NA) # Method 1
dev.off()

seqdplot(atus2017.seq, group = range.pam.ward.oms$clustering$cluster3, border = NA) # Method 2

# Tempograms of the 4 clusters
png ("figures/clust4.png", width = 5, height = 5, units = 'in', res = 300)
seqdplot(atus2017.seq, group = clust4, border = NA) # Method 1
dev.off()

## Which transitions are distinctive for each cluster? (this plot needs formatting to be useful)
discr3 <- seqecmpgroup(fsubseq, group = clust3$clustering)
plot(discr3[1:5])

discr4 <- seqecmpgroup(fsubseq, group = clust4)
plot(discr4[1:5])

## Plot sequences using ggplot -- FIGURE THIS OUT LATER
# http://databasefaq.com/index.php/answer/2236/r-ggplot2-traminer-sequence-index-plots-in-ggplot2-using-geom-tile-


################################################################################################
# REGRESSION ANALYSIS OF CLUSTERS (Struder, 2013)

# a bivariate test of the association
set.seed(1)
dsa <- dissassoc(dist.oms, atus2017$sex, weights = atus2017$wt06,
                 weight.permutation = "diss", R = 5000)
print(dsa$stat) # Interpret Pseudo R2

dsa <- dissassoc(dist.dhd, atus2017$sex, weights = atus2017$wt06,
                 weight.permutation = "diss", R = 5000)

