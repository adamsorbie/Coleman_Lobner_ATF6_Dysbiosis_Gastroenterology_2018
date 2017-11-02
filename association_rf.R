rm(list=ls())
library("randomForest")
library("plyr") # for "arrange" function
library("rfUtilities") # to test model significance
library("caret") # leave-one-out cross-validation accuracies and nearZeroVar function 
library(pROC)

setwd("C:/Users/PhD/ml_R/ml_R")

remove_rare <- function( table , cutoff_pro ) {
row2keep <- c()
cutoff <- ceiling( cutoff_pro * ncol(table) )  
for ( i in 1:nrow(table) ) {
  row_nonzero <- length( which( table[ i , ]  > 0 ) ) 
  if ( row_nonzero > cutoff ) {
    row2keep <- c( row2keep , i)
  }
}
return( table [ row2keep , , drop=F ])
}


otu_table <- read.table("OTUassociation.txt", sep="\t", header=T, row.names=1, stringsAsFactors=FALSE, comment.char="", check.names=FALSE)  
metadata <- read.table("mapping_association.txt", sep="\t", header=T, row.names=1, stringsAsFactors=TRUE, comment.char="", check.names=FALSE)

otu_nonzero_counts <- apply(otu_table, 1, function(y) sum(length(which(y > 0))))
hist(otu_nonzero_counts, breaks=100, col="grey", main="", ylab="Number of OTUs", xlab="Number of Non-Zero Values")


otu_table_rare_removed <- remove_rare(table=otu_table, cutoff_pro=0.2)

otu_table_rare_removed_norm <- sweep(otu_table_rare_removed, 2, colSums(otu_table_rare_removed) , '/')*100

otu_table_scaled <- scale(otu_table_rare_removed_norm, center = TRUE, scale = TRUE)

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

