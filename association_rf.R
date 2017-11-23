library("randomForest")
library("plyr") 
library("rfUtilities") 
library("caret") 
library("pROC")

setwd("~/ml_R/ml_R")


# load data 
otu_table <- read.table("5wk_otu.csv", sep=",", header=T, row.names=1, stringsAsFactors=FALSE, comment.char="", check.names=FALSE)  
metadata <- read.table("metadata_5wk.csv", sep=",", header=T, row.names=1, stringsAsFactors=TRUE, comment.char="", check.names=FALSE)
metadata_test <- read.table("metadata_asc.csv", sep=",", header=T, row.names=1, stringsAsFactors=TRUE, comment.char="", check.names=FALSE)
testing <- read.table('association_to_predict.csv', sep = ",",header=T, row.names=1, stringsAsFactors=FALSE, comment.char="", check.names=FALSE )

# scale pre-preprocessed training data (normalised relative abundance with abundance cutoff of 0.5% in at least one sample) and merge phenotype column from metadata
otu_table_scaled <- scale(otu_table, center = TRUE, scale = TRUE)

otu_table_scaled_Phenotype <- data.frame(t(otu_table_scaled))  
otu_table_scaled_Phenotype$Phenotype <- metadata[rownames(otu_table_scaled_Phenotype), "Phenotype"] 

 
# # scale pre-preprocessed test data and merge phenotype column from metadata_test
testing_scaled <- scale(testing, center = TRUE, scale = TRUE)

testing_scaled_phenotype <- data.frame(t(testing_scaled)) 
testing_scaled_phenotype$problem_id <- metadata_test[rownames(testing_scaled_phenotype), "problem_id"] 

# set random seed to 42 
set.seed(42)

RF_phenotype_classify <- randomForest( x=otu_table_scaled_Phenotype[,1:(ncol(otu_table_scaled_Phenotype)-1)] , y=otu_table_scaled_Phenotype[ , ncol(otu_table_scaled_Phenotype)] , ntree=1001, importance=TRUE, proximities=TRUE )
RF_phenotype_classify_sig <- rf.significance( x=RF_phenotype_classify ,  xdata=otu_table_scaled_Phenotype[,1:(ncol(otu_table_scaled_Phenotype)-1)] , nperm=1000 , ntree=1001 )  

fit_control <- trainControl( method = "LOOCV" )    

RF_phenotype_classify_loocv <- train( otu_table_scaled_Phenotype[,1:(ncol(otu_table_scaled_Phenotype)-1)] , y=otu_table_scaled_Phenotype[, ncol(otu_table_scaled_Phenotype)] , method="rf", ntree=1001 , tuneGrid=data.frame( mtry=25 ) , trControl=fit_control )
RF_phenotype_classify_loocv$results   

par(mfrow=c(1,2))
RF_phenotype_classify_importances <- as.data.frame( RF_phenotype_classify$importance )
RF_phenotype_classify_importances$features <- rownames( RF_phenotype_classify_importances )
RF_phenotype_classify_importances_sorted <- arrange( RF_phenotype_classify_importances  , desc(MeanDecreaseAccuracy)  )
barplot(RF_phenotype_classify_importances_sorted$MeanDecreaseAccuracy, ylab="Mean Decrease in Accuracy (Variable Importance)", main="RF Classification Variable Importance Distribution")


barplot(RF_phenotype_classify_importances_sorted[1:10,"MeanDecreaseAccuracy"], names.arg=RF_phenotype_classify_importances_sorted[1:10,"features"] , ylab="Mean Decrease in Accuracy (Variable Importance)", las=2, ylim=c(0,0.02), main="Classification RF") 

#k <- dim()[1]
#predictions <- c()
#for (i in 1:k) {
 # model <- glmnet(x[-i,], y[-i], family="binomial")
#  predictions <- c(predictions, predict(model, newx=x[i,]))
#}

pred <- predict(RF_phenotype_classify, newdata = testing_scaled_phenotype)

table(pred, testing_scaled_phenotype$problem_id)
