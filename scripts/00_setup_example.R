#------------------------------------------------------------------------------------
# ATUS SEQUENCE ANALYSIS
# 00_setup_environment.R
# Joanna R. Pepin & Sarah Flood
#------------------------------------------------------------------------------------

# Users should create a personal setup file. 
# Add the name of this setup file to the gitignore file.

#####################################################################################
# Set-up the environment

## Setup file
setupfile   <- "00_setup_yourname" # Name of your personal setup file (name of this R script)

## Data
data_atus   <- "atus_?????.xml"    # Name of the downloaded atusX data file

## Directories
projDir     <- here()                                          # File path to your master project folder (Project GitRepository)
dataDir     <- "../../Data/ATUS/ATUS19_20"                     # File path to where the data was downloaded

srcDir      <- file.path(projDir, "scripts")                   # File path to the R scripts
outDir      <- file.path(projDir, "output")                    # File path to processed tables/figures
docs        <- file.path(projDir, "docs")                      # File path to shared output

setOutputLevel(Info)
report(Info, "End of #{setupfile}")                            # Marks end of R Script
