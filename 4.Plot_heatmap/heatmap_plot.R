library(dplyr)
library(reshape2)
library(ggplot2)
library(RColorBrewer)
library(scalesles)
library(gridExtra)

data <- read.csv("merged_dr.csv", sep = ",", header = T, check.names = F)
taxa_list <- read.csv("discriminative_otus_classification.csv", sep = ",", header = T)


df_abundance <- as.data.frame(data)
df_prevalence <- df_abundance[,c("OTU", "% Prevalence T (D)","% Prevalence NT (D)","% Prevalence T (R)","% Prevalence NT (R)")]
df_abundance <- df_abundance[,c("OTU","Mean Donors T","Mean Recipients T", "Mean Donors NT", "Mean Recipients NT")]



df_taxa <- as.data.frame(taxa_list)


df_melt <- melt(df_abundance)
df_melt_2 <- melt(df_prevalence)


hm.palette <- colorRampPalette((brewer.pal(9,"Greens")), space='Lab') 

p_abundance <- ggplot(df_melt, aes(x = variable , y = OTU, fill = value)) + geom_tile() + scale_fill_gradientn(colours = hm.palette(10)) + xlab(NULL) + ylab(NULL) + theme(axis.text=element_text(size=5)) 
p_abundance

p_prevalence <- ggplot(df_melt_2, aes(x = variable , y = OTU, fill = value)) + geom_tile() + scale_fill_gradientn(colours = hm.palette(10)) + xlab(NULL) + ylab(NULL) + theme(axis.text=element_text(size=5)) + geom_text(aes(label= value))
p_prevalence 

