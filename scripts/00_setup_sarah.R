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
setupfile   <- "00_setup_sarah" # Name of your personal setup file (name of this R script)

## Data
data_atus   <- "atus_00053.xml"    # Name of the downloaded atusX data file

## Directories
projDir     <- here("/pkg/ipums/personal/sarah/ATUS_sequences/")                                          # File path to your master project folder (Project GitRepository)
dataDir     <- file.path(projDir, "data")                    # File path to where the data was downloaded

srcDir      <- file.path(projDir, "scripts")                   # File path to the R scripts
outDir      <- file.path(projDir, "output")                    # File path to save tables/figures

setOutputLevel(Info)
report(Info, "End of #{setupfile}")                            # Marks end of R Script
