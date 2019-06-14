library(data.table)
library(tidyverse)
library(ggplot2)

# Data frames
# qm_ward_dhd
# qm_ward_oms
# qm_beta_dhd
# qm_beta_oms
# qm_pam_ward_dhd
# qm_pam_ward_oms
# qm_pam_beta_dhd
# qm_pam_beta_oms


# Transform row names (number of clusters) to a variable
qm_ward_dhd$clusters <- rownames(qm_ward_dhd)
qm_ward_oms$clusters <- rownames(qm_ward_oms)
qm_beta_dhd$clusters <- rownames(qm_beta_dhd)
qm_beta_oms$clusters <- rownames(qm_beta_oms)

qm_pam_ward_dhd$clusters <- rownames(qm_pam_ward_dhd)
qm_pam_ward_oms$clusters <- rownames(qm_pam_ward_oms)
qm_pam_beta_dhd$clusters <- rownames(qm_pam_beta_dhd)
qm_pam_beta_oms$clusters <- rownames(qm_pam_beta_oms)

## Add columns to dataframes that mark the dissimilarity measure
qm_ward_dhd$diss <- "DHD"
qm_beta_dhd$diss <- "DHD"
qm_pam_ward_dhd$diss <- "DHD"
qm_pam_beta_dhd$diss <- "DHD"

qm_ward_oms$diss <- "OMS"
qm_beta_oms$diss <- "OMS"
qm_pam_ward_oms$diss <- "OMS"
qm_pam_beta_oms$diss <- "OMS"

# Add columns to dataframes that mark the algorithm approach
qm_ward_dhd$algorithm <- "HCM"
qm_ward_oms$algorithm <- "HCM"
qm_pam_ward_dhd$algorithm <- "PAM"
qm_pam_ward_oms$algorithm <- "PAM"

qm_beta_dhd$algorithm <- "HCM"
qm_beta_oms$algorithm <- "HCM"
qm_pam_beta_dhd$algorithm <- "PAM"
qm_pam_beta_oms$algorithm <- "PAM"

# Add columns to dataframes that mark the algorithm approach
qm_ward_dhd$method <- "Ward"
qm_ward_oms$method <- "Ward"
qm_pam_ward_dhd$method <- "Ward"
qm_pam_ward_oms$method <- "Ward"

qm_beta_dhd$method <- "Beta"
qm_beta_oms$method <- "Beta"
qm_pam_beta_dhd$method <- "Beta"
qm_pam_beta_oms$method <- "Beta"

# Combine the dataframes
qmdata <- rbind(qm_ward_dhd, qm_beta_dhd, qm_ward_oms, qm_beta_oms, qm_pam_ward_dhd, qm_pam_beta_dhd, qm_pam_ward_oms, qm_pam_beta_oms)

## Number clusters -- integer
qmdata <- qmdata %>%
  mutate(
    clusters = case_when(
      clusters == "cluster2" ~ 2,
      clusters == "cluster3" ~ 3,
      clusters == "cluster4" ~ 4,
      clusters == "cluster5" ~ 5,
      clusters == "cluster6" ~ 6,
      clusters == "cluster7" ~ 7,
      clusters == "cluster8" ~ 8,
      clusters == "cluster9" ~ 9,
      clusters == "cluster10" ~ 10,
      clusters == "cluster11" ~ 11,
      clusters == "cluster12" ~ 12,
      clusters == "cluster13" ~ 13,
      clusters == "cluster14" ~ 14,
      clusters == "cluster15" ~ 15))

## Create and save a data table
qmtable <- qmdata %>%
  subset(clusters <= 6) %>%
  select("clusters", "diss", "algorithm", "method",
         "PBC", "HG", "ASWw", "HC") %>%
  arrange(clusters) %>%
  mutate_if(is.numeric, round, digits=3)

write.csv(qmtable, "data/qmtable.csv")

## Wrangle the data for graphing
## reshape the data from wide to long form
qmdata <- as_tibble(qmdata)

qmdata <- qmdata %>%
  gather(stat, value, -clusters, -diss, -algorithm, -method)

qmdata$stat <- as_factor(qmdata$stat)

qmdata <- qmdata %>%
  mutate(
    approach = case_when(
      diss == "DHD" & algorithm == "HCM" & method == "Ward" ~ "HCM-DHD-Ward",
      diss == "OMS" & algorithm == "HCM" & method == "Ward" ~ "HCM-OMS-Ward",
      diss == "DHD" & algorithm == "HCM" & method == "Beta" ~ "HCM-DHD-Beta",
      diss == "OMS" & algorithm == "HCM" & method == "Beta" ~ "HCM-OMS-Beta",
      diss == "DHD" & algorithm == "PAM" & method == "Ward" ~ "PAM-DHD-Ward",
      diss == "OMS" & algorithm == "PAM" & method == "Ward" ~ "PAM-OMS-Ward",
      diss == "DHD" & algorithm == "PAM" & method == "Beta" ~ "PAM-DHD-Beta",
      diss == "OMS" & algorithm == "PAM" & method == "Beta" ~ "PAM-OMS-Beta"))
      
# Let's graph it!
# Stats we care about
# "PBC", "HG", "ASWw", "HC"

qmplot <- qmdata %>%
  filter((stat == "PBC" | stat == "HG" | stat == "ASWw" | stat == "HC") & clusters <=10) %>%
    ggplot(aes(x = clusters, y = value, color = approach)) +
    facet_grid(vars(algorithm), vars(stat)) +
      geom_line(lwd = 1)

qmplot

ggsave("figures/qmplot.png", qmplot, width = 24, height = 16, units = "cm", dpi = 300)