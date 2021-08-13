# Save output of this file
sink(file=file.path(outDir, "03output.txt"), append=FALSE, split=TRUE)  # for screen and log

# Libraries
#library(TraMineR)
#library(TraMineRextras)
#library(WeightedCluster)
#library(cluster)
#library(graphicsQC) 
#library(foreign)
#library(tictoc)

# Create a subsample so it doesn't take forever to run
  # seqdata <- data[seqdata(nrow(seqdata), 500), ]

# If want a sample for men and women separately
  # seqdata.M <- subset(seqdata, sex == "Man")
  # seqdata.W <- subset(seqdata, sex == "Woman")

#####################################################################
# Research Question: Are there patterns/regularities in the distribution of activity 
# transitions throughout the day?

# Goal: sort similar sequences into groups (clusters) to see which patterns 
# of a process exist and how prevalent they are
  # Identify order in sequences by analyzing the similarity of
  # sequences to one another and sorting them into groups of similar sequences

# Terminology
  # Sequence element: activity
  # Episode: list of identical elements
  # Sequence pattern: group of similar sequences

# Typical Steps in Optimal Matching Analysis
####################################################################
# 0. Define sequence: start, end, alphabet of states

## Identify the columns that represent states
minF <- which( colnames(seqdata)=="1") # 1 = minute 1 of the time diary 
minL <- which( colnames(seqdata)=="1440") #1440 = minute 1440 of the time diary

seqstatl(seqdata, minF:minL) #Lists the states of the sequences

## Define the labels for the states. (These are in alphabetical order.)
seqdata.labels <- c("Carework", "Eating", "Housework", "Other", "Passive Leisure", "Sleep & Selfcare", "Work & Edu") # Create long labels.
seqdata.scode <- c("C", "E", "H", "O", "P", "S", "W") # Create alphabet list. 

## Define the data as sequence data and create a sequence object
seqdata.seq <- seqdef(data = seqdata, var = minF:minL, states = seqdata.scode, labels = seqdata.labels, weights = seqdata$wt20)
### un-used xtstep option sets the step between displayed tick-marks and labels on the x-axis of state sequence plots.

## Setting a user defined color palette to be used in visuals
cpal(seqdata.seq) <- c("#7570b3", "#ec7014", "#1b9e77",  "#e6ab02", "#e7298a", "#e5d8bd", "#1f78b4") 

## Look at the sequence object
summary(seqdata.seq) # Overview of the sequence object
  ## Shows the number of sequences and unique sequences. 
  ## Sequences should be the number of observations, unique sequences will be the number of unique patterns of time use.
  ## in this case, we used a minute file, with an activity logged for every minute of the day. 
  ## No two respondents followed the exact same pattern to the minute, so the number of observations equals the number of sequences. 
  ## This also confirms that our min & max sequence length is 1440 minutes, equal to the number of minutes in a diary day.

stlab(seqdata.seq) # View the long labels of the states
alphabet(seqdata.seq) # View the short labels (alphabet list)
seqdata.seq[1:5, 300:350] # Look at sequences of first 5 respondents, minutes 300-350
print(seqdata.seq[1:5, 300:350], format = "SPS") # more concise view of sequences with the SPS state-permanence representation.

## Define separate sequence objects for men and women
 # seqdata.M.seq <- seqdef(data = seqdata.M,var = 34:1473, states = seqdata.scode, labels = seqdata.labels, weights = seqdata.M$wt06) # Men sequence object
 # seqdata.W.seq <- seqdef(data = seqdata.W,var = 34:1473, states = seqdata.scode, labels = seqdata.labels, weights = seqdata.W$wt06) # Women sequence object

####################################################################
# 1. Choose the measure of dissimilarity 
## used to quantify the differences between sequences.
## 'method' must be one of: OM, OMloc, OMslen, OMspell, OMstran, HAM, DHD, CHI2, EUCLID, LCS, LCP, RLCP, NMS, NMSMST, SVRspell, TWED
## Struder, Matthias and Gilbert Ritschard. 2014. "A Comparative Review of Sequence Dissimilarity Measures." LIVES Working Papers 33.
## Narrowed it down to two choices: A. DHD or B. OMstran

## A. Dynamic Hamming Distance (DHD) (driven by timing differences)
### Using DHD allows for timing of the transition from one state to another to vary, which can be relevant in time use data.
### data based time dependent substitution costs, no indel operations
### substitution cost different at each time. Determined by time-specific transition frequency in data (not by the researcher)
### https://www.lives-nccr.ch/sites/default/files/pdf/publication/33_lives_wp_studer_sequencedissmeasures.pdf
### Laurent Lesnard 2010, 2012
dist.dhd <- seqdist(seqdata.seq, method="DHD")
dist.dhd[1:5, 1:5]

## B. Optimal matching of transitions (OMstran) 
### Otto = Weight for controlling the trade-off between cost of origin state and cost of transition type.
### When otto = 1, OMstran is equivalent to classical OM.
### lower the w to give more importance to the transition type than to the origin states. I think w == otto
### sm must be specified. It can be "INDELS" or "INDELSLOG"

### Marking this out and using Sarah's instead!
# dist.oms <- readRDS("data/dist_oms.rds")
   tic("OMS Run Time:") #Let's time this long running function!
   dist.oms <- seqdist(seqdata.seq, method="OMstran", otto = .2, sm = "INDELS")
   toc(log = TRUE)

### Warning message: at least, one indel cost does not respect the triangle inequality.
dist.oms[1:5, 1:5]

####################################################################
# 2. Choose a clustering algorithm
### Need to see how dissimilar the sequences are from eachother

## A. Hierarchical Clustering Method 
## i. Ward - Minimization of residual variance (weighted)
ward.dhd<- hclust(as.dist(dist.dhd), method = "ward.D", members = seqdata$wt06)
ward.oms<- hclust(as.dist(dist.oms), method = "ward.D", members = seqdata$wt06)

plot(ward.dhd)
plot(ward.oms)

## ii. beta-flexible - good results in the presence of various forms of error in the data (unweighted)
beta.dhd <- agnes(dist.dhd, diss = TRUE, method = "flexible", par.method=0.625) # diss = this is a dissimiliarity matrix 
beta.oms <- agnes(dist.oms, diss = TRUE, method = "flexible", par.method=0.625) # diss = this is a dissimiliarity matrix 

plot(beta.dhd, which.plots = 2)
plot(beta.oms, which.plots = 2)

## B. Partitioning Around Medoids
#### advantage of maximizing a global criterion and not only a local criterion
#### medoid is defined as the observation of a group having the smallest weighted
    #### sum of distances from the other observations de this group

## i. predefined number k of groups
pam.dhd4 <- wcKMedoids(dist.dhd, k = 4, weights = seqdata$wt06) #This arbitrarily picks 4 clusters.
pam.oms4 <- wcKMedoids(dist.oms, k = 4, weights = seqdata$wt06) #This arbitrarily picks 4 clusters.

# display the medoid sequences of each group
print(seqdata.seq[unique(pam.dhd4$clustering), ], format = "SPS")

## ii. Combining the algorithms
pam.ward.dhd <- wcKMedoids(dist.dhd, k = 4, weights = seqdata$wt06,
                            initialclust = ward.dhd)

pam.ward.oms <- wcKMedoids(dist.oms, k = 4, weights = seqdata$wt06,
                           initialclust = ward.oms)

pam.beta.dhd <- wcKMedoids(dist.dhd, k = 4, weights = seqdata$wt06,
                           initialclust = beta.dhd)

pam.beta.oms <- wcKMedoids(dist.oms, k = 4, weights = seqdata$wt06,
                           initialclust = beta.oms)

####################################################################
# 3. Measure the quality of partition
### Look at PBC, ASWw, and HC test statistics
#### PBC- Point Biserial Correlation. Measures the strength of association of two variables 
    #### Correlation between the given distance matrice and a distance which equal to zero for individuals in the same cluster and one otherwise.
    #### a single measure ranging from -1 to +1, where -1 indicates a perfect negative association, 
    #### +1 indicates a perfect positive association and 0 indicates no association at all.)
#### HG- Hubert's Gamma. Measure of the capacity of the clustering to reproduce the distances (order of magnitude).
    #### [-1;1] Higher is better
#### ASWw - Average Silhouette width (weighted).
    #### Observations with a large Si (almost 1) are very well clustered
    #### SW Interpretation proposed
          #### 0.71???1.00 Strong structure identified.
          #### 0.51???0.70 Reasonable structure identified.
          #### 0.26???0.50 Structure is weak and could be artificial.Try other algorithms.
          #### ???0.25 No structure.
#### HC - Hubert's C coefficient.
    #### compares the partition obtained with the best partition that could be obtained with this number of groups and this distance matrix. 
    #### The index ranges between 0 and 1, with a small value indicating a good partition of the data.

## A. Ward -- Computing the quality of all these different possibilities 
range.ward.dhd <- as.clustrange(ward.dhd, diss = dist.dhd, weights = seqdata$wt06, ncluster = 15)
range.ward.oms <- as.clustrange(ward.oms, diss = dist.oms, weights = seqdata$wt06, ncluster = 15)

summary(range.ward.dhd, max.rank = 2) # the best number of groups according to each quality measure and the value of these statistics
summary(range.ward.oms, max.rank = 2) # the best number of groups according to each quality measure and the value of these statistics


## B. Beta -- Computing the quality of all these different possibilities 
range.beta.dhd <- as.clustrange(beta.dhd, diss = dist.dhd, weights = seqdata$wt06, ncluster = 15)
range.beta.oms <- as.clustrange(beta.oms, diss = dist.oms, weights = seqdata$wt06, ncluster = 15)

summary(range.beta.dhd, max.rank = 2) # the best number of groups according to each quality measure and the value of these statistics
summary(range.beta.oms, max.rank = 2) # the best number of groups according to each quality measure and the value of these statistics


# C. PAM
range.pam.ward.dhd <- wcKMedRange(dist.dhd, kvals = 2:15, weights = seqdata$wt06, initialclust = ward.dhd)
range.pam.ward.oms <- wcKMedRange(dist.oms, kvals = 2:15, weights = seqdata$wt06, initialclust = ward.oms)

range.pam.beta.dhd <- wcKMedRange(dist.dhd, kvals = 2:15, weights = seqdata$wt06, initialclust = beta.dhd)
range.pam.beta.oms <- wcKMedRange(dist.oms, kvals = 2:15, weights = seqdata$wt06, initialclust = beta.oms)


# save the quality measures as a dataframe
qm_ward_dhd <- as.data.frame(range.ward.dhd$stats)
qm_ward_oms <- as.data.frame(range.ward.oms$stats)

qm_beta_dhd <- as.data.frame(range.beta.dhd$stats)
qm_beta_oms <- as.data.frame(range.beta.oms$stats)

qm_pam_ward_dhd <- as.data.frame(range.pam.ward.dhd$stats) 
qm_pam_ward_oms <- as.data.frame(range.pam.ward.oms$stats) 
qm_pam_beta_dhd <- as.data.frame(range.pam.beta.dhd$stats) 
qm_pam_beta_oms <- as.data.frame(range.pam.beta.oms$stats) 

################
## Define objects that contains cluster memberships.
### cutree() cuts denodogram (tree) into groups of data
cl1.2 <- cutree(ward.dhd, k = 2)
cl1.3 <- cutree(ward.dhd, k = 3)
cl1.4 <- cutree(ward.dhd, k = 4)
cl1.5 <- cutree(ward.dhd, k = 5)
cl1.6 <- cutree(ward.dhd, k = 6)
cl1.7 <- cutree(ward.dhd, k = 7)
cl1.8 <- cutree(ward.dhd, k = 8)

## Quality of a cluster
### https://rdrr.io/rforge/WeightedCluster/f/inst/doc/WeightedCluster.pdf
### Look at PBC, ASW, and HC test statistics
  #### PBC- Point Biserial Correlation. (measures the strength of association of two variables) 
  #### Correlation between the given distance matrice and a distance which equal to zero for individuals in the same cluster and one otherwise.
      #### a single measure ranging from -1 to +1, where -1 indicates a perfect negative association, 
      #### +1 indicates a perfect positive association and 0 indicates no association at all.)
  #### ASWw - Average Silhouette width (weighted).
      #### Observations with a large Si (almost 1) are very well clustered
      #### SWInterpretation proposed
          #### 0.71???1.00 Strong structure identified.
          #### 0.51???0.70 Reasonable structure identified.
          #### 0.26???0.50 Structure is weak and could be artificial.Try other algorithms.
          #### ???0.25 No structure.
  #### HC - Hubert's C coefficient.
      #### compares the partition obtained with the best partition that could be obtained with this number of groups and this distance matrix. 
      #### The index ranges between 0 and 1, with a small value indicating a good partition of the data.
### Leaving the weights off for now, but need to decide about including them (weights = seqdata$wt06)

wcClusterQuality(dist.dhd, cl1.2)
wcClusterQuality(dist.dhd, cl1.3)
wcClusterQuality(dist.dhd, cl1.4)
wcClusterQuality(dist.dhd, cl1.5)
wcClusterQuality(dist.dhd, cl1.6)
wcClusterQuality(dist.dhd, cl1.7)
wcClusterQuality(dist.dhd, cl1.8)

####################################################################
# 4. Further analysis of groups/distances (visual, regression based methods)

####################################################################
# 5. Distances as direct indicators of dissimilarity (destandardization)


###################################################################
# Clean up
## Use these two lines if running the dist.oms and timer
  # tic.log(format = TRUE)
  # tic.clearlog() # rest the time log

sink() # Return output to the screen only
savehistory(file="data/03history.Rhistory") # Save the commnd history