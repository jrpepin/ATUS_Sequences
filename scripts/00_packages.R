#------------------------------------------------------------------------------------
# ATUS SEQUENCE ANALYSIS
# 00_packages.R
# Joanna R. Pepin & Sarah Flood
#------------------------------------------------------------------------------------
  
# This file is for installing required packages for sequence and cluster analysis, 
# as well as further calculations. 
  
  
#####################################################################################
# Install and load required packages
#####################################################################################

# Download and install Rtools 4.2 from https://cran.r-project.org/bin/windows/Rtools/ 
# or https://www.r-project.org/nosvn/winutf8/ucrt3/. (only need to install 1X)
  
# install.packages("pacman")       # Install pacman package if not installed
library("pacman")                  # Load pacman package
  
# Install packages not yet installed & load them
pacman::p_load(
  here,       # relative file paths
  devtools,   # loading github packages
  conflicted, # declare which package commands
  ipumsr,     # load IPUMS data
  reportr,    # report end of scripts
  tictoc,     # report run time of R scripts
  sjlabelled, # use variable labels
  tidyverse,  # processing data
  haven,      # importing data
  readr,      # read rectangular data 
  foreign,    # reading and writing data
  flextable,  # reproducible tables
  officer,    # reproducible tables
  data.table, # for 04 script...can't remember why
  survey,     # use survey weights
  gtsummary,  # create descriptive tables
  effects,    # create marginal estimates
  ggeffects,  # create marginal estimates
  nnet,       # multinomial models
  colorspace, # color palettes of figures
  ## for sequence analyses
  TraMineR,
  cluster, 
  WeightedCluster, 
  TraMineRextras
)

# devtools::install_github("maraab23/ggseqplot")
library(ggseqplot) # use ggplot to graph TraMineR output


# if(!require(graphicsQC)){
  # install.packages("graphicsQC") # not available for this version of R......
  # library(graphicsQC) # Consider using package ‘gdiff’ instead.
# }

# Address any conflicts in the packages
conflict_scout() # Identify the conflicts
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("Position", "ggplot2")
