rm(list=ls())
library("randomForest")
library("plyr") # for "arrange" function
library("rfUtilities") # to test model significance
library("caret") # leave-one-out cross-validation accuracies and nearZeroVar function 

setwd("C:/Users/PhD/ml_R/ml_R")

otu_table <- read.table("OTUassociation.txt", sep="\t", header=T, row.names=1, stringsAsFactors=FALSE, comment.char="")  
metadata <- read.table("mapping_association.txt", sep="\t", header=T, row.names=1, stringsAsFactors=TRUE, comment.char="")

otu_nonzero_counts <- apply(otu_table, 1, function(y) sum(length(which(y > 0))))
hist(otu_nonzero_counts, breaks=100, col="grey", main="", ylab="Number of OTUs", xlab="Number of Non-Zero Values")