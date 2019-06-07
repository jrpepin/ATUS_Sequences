# Libraries
library(TraMineR)
library(TraMineRextras)
library(WeightedCluster)
library(cluster)
library(graphicsQC) 
library(foreign)

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
# 1. Define sequence: start, end, alphabet of states

## Identify the columns that represent states (24:1463)
which( colnames(atus2017)=="1") # 1 = minute 1 of the time diary 
which( colnames(atus2017)=="1440") #1440 = minute 1440 of the time diary

seqstatl(atus2017, 24:1463) #Lists the states of the sequences

## Define the labels for the states. (These are in alphabetical order.)
atus2017.labels <- c("Carework", "Eating", "Housework", "Other", "Passive Leisure", "Sleep & Selfcare", "Work & Edu") # Create long labels.
atus2017.scode <- c("C", "E", "H", "O", "P", "S", "W") # Create alphabet list. 

## Define the data as sequence data and create a sequence object
atus2017.seq <- seqdef(data = atus2017, var = 24:1463, states = atus2017.scode, labels = atus2017.labels, weights = atus2017$wt06)
### un-used xtstep option sets the step between displayed tick-marks and labels on the x-axis of state sequence plots.

## Setting a user defined color palette to be used in visuals
cpal(atus2017.seq) <- c("#7570b3", "#ec7014", "#1b9e77",  "#e6ab02", "#e7298a", "#e5d8bd", "#1f78b4") 

## Look at the sequence object
summary(atus2017.seq) # Overview of the sequence object
stlab(atus2017.seq) # View the long labels of the states
alphabet(atus2017.seq) # View the short labels (alphabet list)

## Define separate sequence objects for men and women
atus2017.M <- atus2017 %>%  filter(sex == "Man") # Create men database
atus2017.W <- atus2017 %>%  filter(sex == "Woman") # Create women database

atus2017.M.seq <- seqdef(data = atus2017.M,var = 24:1463, states = atus2017.scode, labels = atus2017.labels, weights = atus2017.M$wt06) # Men sequence object
atus2017.W.seq <- seqdef(data = atus2017.W,var = 24:1463, states = atus2017.scode, labels = atus2017.labels, weights = atus2017.W$wt06) # Women sequence object

####################################################################
# 2. Calculate distances between sequences by applying the Dynamic Hamming Distance (DHD) algorithm & obtain distance matrix

## QUESTION: Choosing the measure of dissimilarity used to quantify the differences between sequences. Using DHD here. 

## Using DHD allows for timing of the transition from one state to another to vary, which can be relevant in time use data.
### data based time dependent substitution costs, no indel operations
### substitution cost different at each time. Determined by time-specific transition frequence in data (not by the researcher)
### https://www.lives-nccr.ch/sites/default/files/pdf/publication/33_lives_wp_studer_sequencedissmeasures.pdf
### Laurent Lesnard 2010, 2012
dhd.dist <- seqdist(atus2017.seq, method="DHD")
dhd.dist[1:5, 1:5]
seqiplot(atus2017.seq[1:5, ], with.legend = F, border = NA)

####################################################################
# 3. Further analysis of distances and cluster analysis

## Need to see how dissimilar the sequences are from eachother
dhd.clusterward <- agnes(dhd.dist, diss = TRUE, method = "ward") # diss = this is a dissimiliarity matrix 
plot(dhd.clusterward, which.plots = 2)

# OR
clustergen<- hclust(dhd.dist, method = "ward.D", members = atus2017$wt06) 
plot(clustergen)

## Define objects that contains cluster memberships.
### cutree() cuts denodogram (tree) into groups of data
cl1.2 <- cutree(dhd.clusterward, k = 2)
cl1.3 <- cutree(dhd.clusterward, k = 3)
cl1.4 <- cutree(dhd.clusterward, k = 4)
cl1.5 <- cutree(dhd.clusterward, k = 5)
cl1.6 <- cutree(dhd.clusterward, k = 6)
cl1.7 <- cutree(dhd.clusterward, k = 7)
cl1.8 <- cutree(dhd.clusterward, k = 8)

## Quality of a cluster  --- MAYBE TRY THIS SEPARATELY FOR MEN AND WOMEN
### https://rdrr.io/rforge/WeightedCluster/f/inst/doc/WeightedCluster.pdf
### Look at PBC, ASW, and HC test statistics
  #### PBC- Point Biserial Correlation. (measures the strength of association of two variables 
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
### Leaving the weights off for now, but need to decide about including them (weights = atus2017$wt06)

wcClusterQuality(dhd.dist, cl1.2)
wcClusterQuality(dhd.dist, cl1.3)
wcClusterQuality(dhd.dist, cl1.4)
wcClusterQuality(dhd.dist, cl1.5)
wcClusterQuality(dhd.dist, cl1.6)
wcClusterQuality(dhd.dist, cl1.7)
wcClusterQuality(dhd.dist, cl1.8)

####################################################################
# 4. Further analysis of groups/distances (visual, regression based methods)

####################################################################
# 5. Distances as direct indicators of dissimilarity (destandardization)