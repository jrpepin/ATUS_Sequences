# ATUS_Sequences
Using TraMineR to Sequence American Time Use Diary data

# Data
Create a data extract using [ATUS-X](https://www.atusdata.org).  
[Instructions](https://cran.r-project.org/web/packages/ipumsr/vignettes/ipums.html) for importing the data  
  
Sandra L. Hofferth, Sarah M. Flood, Matthew Sobek and Daniel Backman.  
American Time Use Survey Data Extract Builder: Version 2.8 [dataset].  
College Park, MD: University of Maryland and Minneapolis, MN: IPUMS, 2020.  
https://doi.org/10.18128/D060.V2.8  
  
  
### Samples  
  
2019-2020 - Respondents and Household Members  
  
### Variables  
  
RECTYPE       YEAR          CASEID       PERNUM         LINENO        LINENO_CPS8   PRESENCE     DAY      WT06     WT20  
AGE           SEX           RACE          HISPAN        MARST         RELATE        EDUC         EDUCYRS  EMPSTAT       
CLWKR         FULLPART      UHRSWORKT     SPOUSEPRES    HH_SIZE  
ACTIVITY      DURATION      ACTLINE       START         STOP  


# Environment  
Users will need to create a personal setup file before running these scripts. 
Use the 00_setup environment.R script as a template.  
