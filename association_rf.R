rm(list=ls())
library("randomForest")
library("plyr") # for "arrange" function
library("rfUtilities") # to test model significance
library("caret") # leave-one-out cross-validation accuracies and nearZeroVar function 
library(pROC)

setwd("C:/Users/PhD/ml_R/ml_R")



otu_table <- read.table("5wk_otu.csv", sep=",", header=T, row.names=1, stringsAsFactors=FALSE, comment.char="", check.names=FALSE)  
metadata <- read.table("metadata_5wk.csv", sep=",", header=T, row.names=1, stringsAsFactors=TRUE, comment.char="", check.names=FALSE)


otu_table_scaled <- scale(otu_table, center = TRUE, scale = TRUE)

otu_table_scaled_Phenotype <- data.frame(t(otu_table_scaled))  
otu_table_scaled_Phenotype$Phenotype <- metadata[rownames(otu_table_scaled_Phenotype), "Phenotype"]  

set.seed(42)

RF_phenotype_classify <- randomForest( x=otu_table_scaled_Phenotype[,1:(ncol(otu_table_scaled_Phenotype)-1)] , y=otu_table_scaled_Phenotype[ , ncol(otu_table_scaled_Phenotype)] , ntree=501, importance=TRUE, proximities=TRUE )
RF_phenotype_classify_sig <- rf.significance( x=RF_phenotype_classify ,  xdata=otu_table_scaled_Phenotype[,1:(ncol(otu_table_scaled_Phenotype)-1)] , nperm=1000 , ntree=501 )  

fit_control <- trainControl( method = "LOOCV" )    

RF_phenotype_classify_loocv <- train( otu_table_scaled_Phenotype[,1:(ncol(otu_table_scaled_Phenotype)-1)] , y=otu_table_scaled_Phenotype[, ncol(otu_table_scaled_Phenotype)] , method="rf", ntree=501 , tuneGrid=data.frame( mtry=25 ) , trControl=fit_control )

par(mfrow=c(1,2))
RF_phenotype_classify_importances <- as.data.frame( RF_phenotype_classify$importance )
RF_phenotype_classify_importances$features <- rownames( RF_phenotype_classify_importances )
RF_phenotype_classify_importances_sorted <- arrange( RF_phenotype_classify_importances  , desc(MeanDecreaseAccuracy)  )
barplot(RF_phenotype_classify_importances_sorted$MeanDecreaseAccuracy, ylab="Mean Decrease in Accuracy (Variable Importance)", main="RF Classification Variable Importance Distribution")

barplot(RF_phenotype_classify_importances_sorted[1:10,"MeanDecreaseAccuracy"], names.arg=RF_phenotype_classify_importances_sorted[1:10,"features"] , ylab="Mean Decrease in Accuracy (Variable Importance)", las=2, ylim=c(0,0.02), main="Classification RF") 

testing <- read.table('association_to_predict.csv', sep = ",",header=T, row.names=1, stringsAsFactors=FALSE, comment.char="", check.names=FALSE )


testing_scaled <- scale(testing, center = TRUE, scale = TRUE)

testing_scaled_Phenotype <- data.frame(t(testing_scaled))  
testing_scaled_Phenotype$Phenotype <- metadata[rownames(testing_scaled_Phenotype), "Phenotype"]  

pred <- predict(RF_phenotype_classify, newdata = testing_scaled_Phenotype)