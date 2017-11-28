packages <- c("randomForest", "plyr", "rfUtilities", "caret", "pROC")

InsPack <- function(pack)
{
  if ((pack %in% installed.packages()) == FALSE) {
    install.packages(pack,repos ="http://cloud.r-project.org/")
  } 
}


lapply(packages, InsPack)


lib <- lapply(packages, require, character.only = TRUE)

flag <- all(as.logical(lib))


if(!flag) { stop("
    It was not possible to install all required R libraries properly.
                 Please check the installation of all required libraries manually.\n
                 Required libaries: randomForest, plyr, rfUtilities, caret, pROC")
}

setwd("C:/Users/PhD/ml_R/ml_R")


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

# set x and y 

x <- otu_table_scaled_Phenotype[,1:(ncol(otu_table_scaled_Phenotype)-1)] 
y <- otu_table_scaled_Phenotype[ , ncol(otu_table_scaled_Phenotype)]

RF_phenotype_classify <- randomForest( x=x , y=y , ntree=1001, importance=TRUE, proximities=TRUE )
RF_phenotype_classify_sig <- rf.significance( x=RF_phenotype_classify ,  xdata=x , nperm=1000 , ntree=1001 )  

fit_control <- trainControl( method = "LOOCV", savePredictions = TRUE)    

RF_phenotype_classify_loocv <- train( x , y=y , method="rf", ntree=1001 , tuneGrid=data.frame( mtry=25 ) , trControl=fit_control )
RF_phenotype_classify_loocv$results   

par(mfrow=c(1,2))
RF_phenotype_classify_importances <- as.data.frame( RF_phenotype_classify$importance )
RF_phenotype_classify_importances$features <- rownames( RF_phenotype_classify_importances )
RF_phenotype_classify_importances_sorted <- arrange( RF_phenotype_classify_importances  , desc(MeanDecreaseAccuracy)  )
barplot(RF_phenotype_classify_importances_sorted$MeanDecreaseAccuracy, ylab="Mean Decrease in Accuracy (Variable Importance)", main="RF Classification Variable Importance Distribution")


barplot(RF_phenotype_classify_importances_sorted[1:10,"MeanDecreaseAccuracy"], names.arg=RF_phenotype_classify_importances_sorted[1:10,"features"] , ylab="Mean Decrease in Accuracy (Variable Importance)", las=2, ylim=c(0,0.02), main="Classification RF") 

k <- dim(otu_table_scaled_Phenotype)[1]
predictions <- c()
for (i in 1:k) {
  model <- RF_phenotype_classify <- randomForest( x=x , y=y , ntree=1001)
  predictions <- c(predictions, predict(model, newx=x[i,]))
}

pred <- predict(RF_phenotype_classify, newdata = testing_scaled_phenotype)
probs_pred <- predict(RF_phenotype_classify, newdata = testing_scaled_phenotype, type="prob")

table(pred, testing_scaled_phenotype$problem_id)

RF_test_classify <- randomForest(x=testing_scaled_phenotype[,1:(ncol(testing_scaled_phenotype)-1)], y = testing_scaled_phenotype[ , ncol(testing_scaled_phenotype)] , ntree=1001, importance = TRUE, proximities=TRUE )
RF_test_classify_importances <- as.data.frame( RF_test_classify$importance)
RF_test_classify_importances$features <- rownames( RF_test_classify_importances)
RF_test_classify_importances <- arrange( RF_test_classify_importances, desc(MeanDecreaseAccuracy))


write.csv(RF_test_classify_importances, file= "feature_importances_tumor.csv")

probs
probs_pred

