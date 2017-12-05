library(dplyr)
library(reshape2)
library(ggplot2)
library(RColorBrewer)

data <- read.csv("merged_dr.csv", sep = ",", header = T)
taxa_list <- read.csv("discriminative_otus_classification2.csv", sep = ",", header = T)


df_otu <- as.data.frame(data)
df_taxa <- as.data.frame(taxa_list)
df_taxa$names = paste(df_taxa$Classification, df_taxa$Identity, sep= " ")

df_otu$Classification <- df_taxa$names


df_melt <- melt(df_otu)

hm.palette <- colorRampPalette((brewer.pal(11,"RdBu")), space='Lab')  
ggplot(df_melt, aes(x = variable , y = Classification, fill = value)) + geom_tile() + scale_fill_gradientn(colours = hm.palette(10)) + xlab(NULL) + ylab(NULL) + theme(axis.text=element_text(size=5))
