#########This file is for installing required packages for sequence and cluster analysis, as well as further calculations. 

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

if(!require(here)){
  install.packages("here")
  library(here)
}

if(!require(ipumsr)){
  install.packages("ipumsr")
  library(ipumsr)
}




if(!require(tidyverse)){
  install.packages("tidyverse")
  library(tidyverse)
}

if(!require(TraMineRextras)){
  install.packages("TraMineRextras")
  library(TraMineRextras)
}

if(!require(graphicsQC)){
  install.packages("graphicsQC")
  library(graphicsQC)
}

if(!require(tictoc)){
  install.packages("tictoc")
  library(tictoc)
}
