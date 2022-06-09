#------------------------------------------------------------------------------------
# ATUS SEQUENCE ANALYSIS
# 04_sequences_quality analyses.R
# Joanna R. Pepin & Sarah Flood
#------------------------------------------------------------------------------------

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

# Add columns to dataframes that mark the method
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
         "HC", "ASWw", "PBC", "HG") %>%
  arrange(clusters) %>%
  mutate_if(is.numeric, round, digits=3)

write.csv(qmtable, file.path(outDir, "qmtable.csv"))

## Wrangle the data for graphing
## reshape the data from wide to long form
qmdata <- as_tibble(qmdata)

qmdata <- qmdata %>%
  gather(stat, value, -clusters, -diss, -algorithm, -method)

qmdata$stat <- forcats::as_factor(qmdata$stat)

qmdata <- qmdata %>%
  mutate(
    approach = case_when(
      diss == "DHD" & algorithm == "HCM" & method == "Ward" ~ "DHD-HCM-Ward",
      diss == "OMS" & algorithm == "HCM" & method == "Ward" ~ "OMS-HCM-Ward",
      diss == "DHD" & algorithm == "HCM" & method == "Beta" ~ "DHD-HCM-Beta",
      diss == "OMS" & algorithm == "HCM" & method == "Beta" ~ "OMS-HCM-Beta",
      diss == "DHD" & algorithm == "PAM" & method == "Ward" ~ "DHD-PAM-Ward",
      diss == "OMS" & algorithm == "PAM" & method == "Ward" ~ "OMS-PAM-Ward",
      diss == "DHD" & algorithm == "PAM" & method == "Beta" ~ "DHD-PAM-Beta",
      diss == "OMS" & algorithm == "PAM" & method == "Beta" ~ "OMS-PAM-Beta"))

## only use weighted sequences
qmdata <- qmdata %>%
  mutate(
    app2 = case_when(
      diss == "DHD" & algorithm == "HCM" & method == "Ward" ~ "DHD-HCM",
      diss == "OMS" & algorithm == "HCM" & method == "Ward" ~ "OMS-HCM",
      diss == "DHD" & algorithm == "PAM" & method == "Ward" ~ "DHD-PAM",
      diss == "OMS" & algorithm == "PAM" & method == "Ward" ~ "OMS-PAM"))

qmdata$stat <- factor(qmdata$stat,
                          levels = c("HC", "ASWw", "PBC", "HG",
                                     "HGSD", "ASW", "CH", "R2", "CHsq", "R2sq"), 
                          ordered = FALSE)  
    
# Let's graph it!
# Stats we care about
# "PBC", "HG", "ASWw", "HC"

qmdata$zscore <- ave(qmdata$value, qmdata$stat, qmdata$clusters, FUN=scale) # standardize values

fig3 <- qmdata %>%
  filter((stat == "PBC" | stat == "HG" | stat == "ASWw" | stat == "HC") & clusters >=3  & clusters <=10 & !(is.na(app2))) %>%
    ggplot(aes(x = clusters, y = zscore, color = app2)) +
  geom_line(lwd = 1) + 
  geom_point(data=qmdata %>% filter(clusters == 6 & app2 == "OMS-PAM" &
             (stat == "PBC" | stat == "HG" | stat == "ASWw" | stat == "HC")),
             pch=19,
             size=4,
             show.legend = FALSE) +
    facet_wrap(~stat, ncol = 4) +
  theme_minimal() +
  theme(text = element_text(size = 22),
        legend.title= element_blank()) +
  labs(x = "Number of clusters", y = "ZScore") +
  scale_color_discrete_diverging(palette = "Cyan-Magenta") 
#  ggtitle("Figure 3. Quality Measures for Sequence Analysis Approach") 
  
fig3

ggsave(file.path(outDir, "sequences_fig3.png"), fig3, width = 12, height = 5, units = "in", dpi = 300)

# for later -- shade just HC for min value indicator 
# https://stackoverflow.com/questions/9847559/conditionally-change-panel-background-with-facet-grid

setOutputLevel(Info)
report(Info, "End of 04_sequences_quality analyses")     # Marks end of R Script
