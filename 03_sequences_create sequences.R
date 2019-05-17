setwd("C:/Users/Joanna/Dropbox/Repositories/ATUS_Sequences/docs")

#TraMinorR
library(TraMineR)

# Create a subsample so it doesn't take forever to run
atus2017 <- atus2017[1:500,]

atus2017.labels <- c("Carework", "Eating", "Housework", "Other", "Passive Leisure", "Sleep & Selfcare", "Work & Edu") #This is in alphabetical order

atus2017.scode <- c("Carework", "Eating", "Housework", "Other", "Passive Leisure", "Sleep & Selfcare", "Work & Edu") #This is in alphabetical order

which( colnames(atus2017)=="1" )
which( colnames(atus2017)=="1440" )

atus2017.seq <- seqdef(atus2017, 23:1462, states = atus2017.scode, labels = atus2017.labels) 

# Visualizing sequences
## Retrieving the color palette
cpal(atus2017.seq)

## Setting a user defined color palette
cpal(atus2017.seq) <- c("#a6761d", "#e6ab02", "#66a61e", "#d95f02",  "#7570b3", "#e7298a", "#1b9e77")

## the index plot of the first 10 sequences
seqiplot(atus2017.seq, with.legend = F, border = NA, main = "Index plot (10 first sequences)")

## All the sequences
# seqIplot(atus2017.seq, with.legend = F, border = NA, main = "Index plot (All the sequences)") # This takes forever to run. use with caution

## the sequence frequency plot of the 10 most frequent sequences with bar width proportional to the frequencies
seqfplot(atus2017.seq, with.legend = F, border = NA, main = "Sequence frequency plot")

## the state distribution by time points
seqdplot(atus2017.seq, with.legend = F, border = NA, main = "State distribution plot")

## the legend as a separate graphic since several plots use the same color codes for the states
seqlegend(atus2017.seq, fontsize = 1.3)

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