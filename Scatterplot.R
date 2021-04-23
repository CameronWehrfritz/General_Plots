#Written by Cameron Wehrfritz
#Schilling Lab, Buck Institute
#April 22, 2021

# 2D scatterplot
# calculate pearson correlation coefficient (PCC)

# Output: 
# i. Scatterplot
# ii. Scatterplot w/ PCC

### Begin Script ###

#--------------------------------------------------------------------------------------------
#set working directory
setwd("/Volumes/GibsonLab/users/Cameron/2020_0701_CRUK_STORMing_Cancer_MASTER/Proteomic_Signature/R_workspace") # mac
# setwd("//bigrock/GibsonLab/users/Cameron/2020_0701_CRUK_STORMing_Cancer_MASTER/Proteomic_Signature/R_workspace") # windows
#--------------------------------------------------------------------------------------------


#--------------------------------------------------------------------------------------------
# PACKAGES #
packages = c("openxlsx", "readxl", "writexl", "reshape2", "ggplot2", "scales", "dplyr")

package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})
#--------------------------------------------------------------------------------------------


#--------------------------------------------------------------------------------------------
# LOAD DATA 

# first dataset
df.one.input <- read.csv("/Volumes/GibsonLab/users/Cameron/2020_0701_CRUK_STORMing_Cancer_MASTER/MASTER_Processed_Input_CSV_files/Esophagus/Candidates_PG24_E20_EN01TrueNormal_2021_0315_v3_processed_v1.csv", sep=",", stringsAsFactors=FALSE)
# df.one.input <- read.csv("//bigrock/GibsonLab/users/Cameron/2020_0701_CRUK_STORMing_Cancer_MASTER/Processed_input_csv_files/Esophagus/Candidates_PG24_E20_EN01TrueNormal_2021_0315_v3_processed_v1.csv", sep=",", stringsAsFactors=FALSE)

# second dataset
df.two.input <- read.csv("/Volumes/GibsonLab/users/Cameron/2020_0701_CRUK_STORMing_Cancer_MASTER/MASTER_Processed_Input_CSV_files/Esophagus/Candidates_PG24_E20_EN02TrueNormal_2021_0315_v4_processed_v1.csv", sep=",", stringsAsFactors=FALSE)
# df.two.input <- read.csv("//bigrock/GibsonLab/users/Cameron/2020_0701_CRUK_STORMing_Cancer_MASTER/Processed_input_csv_files/Esophagus/Candidates_PG24_E20_EN02TrueNormal_2021_0315_v4_processed_v1.csv", sep=",", stringsAsFactors=FALSE)
#--------------------------------------------------------------------------------------------


#--------------------------------------------------------------------------------------------
# clean data

df.one <- df.one.input %>%
  filter(Number.of.Unique.Total.Peptides>1) %>% # exclude one peptide wonders
  filter(Comparison..group1.group2.=="E20_bT / EN01_trueNorm") # select comparison

df.two <- df.two.input %>%
  filter(Number.of.Unique.Total.Peptides>1) %>% # exclude one peptide wonders
  filter(Comparison..group1.group2.=="E20_bT / EN02_trueNorm") # select comparison
#--------------------------------------------------------------------------------------------


#--------------------------------------------------------------------------------------------
# join datasets

# inner join
df.combined <- df.one %>%
  # grab necessary variables
  select(UniProtIds, Genes, ProteinDescriptions, x = AVG.Log2.Ratio, Qvalue.x = Qvalue, 
         ProteinGroups, ProteinNames, GO.Biological.Process, GO.Molecular.Function, GO.Cellular.Component, Organisms) %>%
  # join second dataset
  inner_join(df.two %>% 
               select(UniProtIds, y = AVG.Log2.Ratio, Qvalue.y = Qvalue), by="UniProtIds") %>%
  # rearrange variables
  select(UniProtIds, Genes, ProteinDescriptions, x, y, Qvalue.x, Qvalue.y, everything())
#--------------------------------------------------------------------------------------------


#--------------------------------------------------------------------------------------------
# scatterplot

df.combined %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(alpha=0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) # remove grid lines
#--------------------------------------------------------------------------------------------


#--------------------------------------------------------------------------------------------
# calculate pearson correlation coefficient for variables (x,y)
correlation <- cor(x = df.combined$x, y = df.combined$y, method = "pearson")
correlation
#--------------------------------------------------------------------------------------------


#--------------------------------------------------------------------------------------------
# scatterplot with correlation coefficient label

df.combined %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(alpha=0.5) +
  # this annotation, for some reason, doesn't include the equal sign ... may have to do with unicode/greek letters...
  # annotate("text", label = paste0(paste0("\u03C1", " = "), round(correlation, digits=3)), x = -8, y = 9, parse = TRUE) +
  annotate("text", label = round(correlation, digits=3), x = -8, y = 9, parse = TRUE) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) # remove grid lines
#--------------------------------------------------------------------------------------------


# END
