#------------------------------------------------------------------------------------
# ATUS SEQUENCE ANALYSIS
# 00_packages.R
# Joanna R. Pepin & Sarah Flood
#------------------------------------------------------------------------------------
#####################################################################################
## Install and load required packages
#####################################################################################

# dependencies include Rtools and devtools

# This file is for installing required packages for sequence and cluster analysis, 
# as well as further calculations. 

## for relative file paths
if(!require(here)){
  install.packages("here")
  library(here)
}

## to install IPUMS data
if(!require(ipumsr)){
  install.packages("ipumsr")
  library(ipumsr)
}

# report end of scripts
if(!require(reportr)){
  install.packages("reportr")
  library(reportr)
}


# use variable labels
if(!require(sjlabelled)){
  install.packages("sjlabelled")
  library(sjlabelled)
}


## for data processing
if(!require(tidyverse)){
  install.packages("tidyverse")
  library(tidyverse)
}

## for survey weights
if(!require(survey)){
  install.packages("survey")
  library(survey)
}

## create descriptive tables
if(!require(tableone)){
  install.packages("tableone")
  library(tableone)
}

## create marginal estimates
if(!require(ggeffects)){
  install.packages("ggeffects")
  library(ggeffects)
}


## for sequence analyses
if(!require(TraMineR)){
  install.packages("TraMineR")
  library(TraMineR)
}

if(!require(cluster)){
  install.packages("cluster")
  library(cluster)
}

if(!require(WeightedCluster)){
  install.packages("WeightedCluster")
  library(WeightedCluster)
}

if(!require(haven)){
  install.packages("haven")
  library(haven)
}

if(!require(foreign)){
  install.packages("foreign")
  library(foreign)
}

if(!require(dplyr)){
  install.packages("dplyr")
  library(dplyr)
}

if(!require(readr)){
  install.packages("readr")
  library(readr)
}

if(!require(TraMineRextras)){
  install.packages("TraMineRextras")
  library(TraMineRextras)
}

if(!require(graphicsQC)){
  install.packages("graphicsQC") # not available for this version of R......
  library(graphicsQC) # Consider using package ‘gdiff’ instead.
}

# report run time of R scripts
if(!require(tictoc)){
  install.packages("tictoc")
  library(tictoc)
}

# declare which package commands to use if same name function
if(!require(conflicted)){
  devtools::install_github("r-lib/conflicted")
  library(conflicted)
}

# Address any conflicts in the packages
conflict_scout() # Identify the conflicts
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("Position", "ggplot2")

