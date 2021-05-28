#Written by Cameron Wehrfritz
#Schilling Lab, Buck Institute
#May 25, 2021

# Ratio Plots

# Output: 
# i. Barchart

### Begin Script ###

#--------------------------------------------------------------------------------------------
#set working directory
# setwd("/Volumes/GibsonLab/users/Cameron/0521_2021_SCIEX/R_workspace") # mac
setwd("//bigrock/GibsonLab/users/Cameron/0521_2021_SCIEX/R_workspace") # windows
#--------------------------------------------------------------------------------------------


#--------------------------------------------------------------------------------------------
# PACKAGES #
packages = c("openxlsx", "readxl", "writexl", "reshape2", 
             "forcats", "ggplot2", "scales", "dplyr")

package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})
#--------------------------------------------------------------------------------------------


#--------------------------------------------------------------------------------------------
# LOAD DATA 

# sciex data - peptides 10+11
df.input <- read_xlsx("//bigrock/GibsonLab/users/Cameron/0521_2021_SCIEX/Input/Peptide10_11_Percentages_for_Bar_Plot_0528_2021_v1.xlsx", sheet=2) # sheet 2 contains tidy data
#--------------------------------------------------------------------------------------------


#--------------------------------------------------------------------------------------------
# clean data

df <- df.input %>%
  mutate(FRAGMENTATION = fct_relevel(FRAGMENTATION, "EAD KE 2", "EAD KE 5", "CID")) # relevel FRAGMENTATION so EAD are first
#--------------------------------------------------------------------------------------------


#--------------------------------------------------------------------------------------------
# barchart
# theoretical distribution

df %>%
  ggplot(aes(x=MIXTURE, y=PROPORTION_EXPECTED, fill=PEPTIDE)) +
  geom_bar(position="stack", stat="identity") +
  facet_wrap(~ FRAGMENTATION, ncol = 1) + # facet by FRAGMENTATION
  ggtitle("Theoretical Proportions") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) # remove grid lines
#--------------------------------------------------------------------------------------------


#--------------------------------------------------------------------------------------------
# barchart
# experimental distribution

df %>%
  ggplot(aes(x=MIXTURE, y=PROPORTION_EXPERIMENTAL, fill=PEPTIDE)) +
  geom_bar(position="stack", stat="identity") +
  facet_wrap(~ FRAGMENTATION, ncol = 1) + # facet by FRAGMENTATION
  ggtitle("Experimental Proportions") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) # remove grid lines
#--------------------------------------------------------------------------------------------


# #--------------------------------------------------------------------------------------------
# # barchart
# # experimental distribution
# # custom y-axis
# 
# df %>%
#   ggplot(aes(x=MIXTURE, y=PROPORTION_EXPERIMENTAL, fill=PEPTIDE)) +
#   geom_bar(position="stack", stat="identity") +
#   # scale_y_continuous(breaks=c(0, 0.333, 0.556, 0.889, 1, 1.25)) + # customize y-axis ticks # 1.25 limit since that is the combined max for 9_0_0 mixture
#   facet_wrap(~ FRAGMENTATION, ncol = 1) + # facet by FRAGMENTATION
#   # scale_fill_viridis_c() + # color scheme
#   ggtitle("Experimental Proportions") +
#   theme_bw() +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) # remove grid lines
# #--------------------------------------------------------------------------------------------

