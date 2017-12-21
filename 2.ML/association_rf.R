# function to check if requirede packages are installed and if not install missing packages 
packages <- c("randomForest", "plyr", "rfUtilities", "caret", "ROCR")

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

####################################################################################################################################################
# Set working directory manually change accordingly 
home <- "C:/Users/adam-/ml_R/ml_R"
work <- "C:/Users/PhD/ml_R/ml_R"
user <- "C:/Users/user/folder"
setwd(home)

####################################################################################################################################################

# load data 
otu_table <- read.table("5wk_otu_0.25.csv", sep=",", header=T, row.names=1, stringsAsFactors=FALSE, comment.char="", check.names=FALSE)  
metadata <- read.table("metadata_5wk.csv", sep=",", header=T, row.names=1, stringsAsFactors=TRUE, comment.char="", check.names=FALSE)
metadata_test <- read.table("metadata_asc.csv", sep=",", header=T, row.names=1, stringsAsFactors=TRUE, comment.char="", check.names=FALSE)
testing <- read.table('association_to_predict_0.25.csv', sep = ",",header=T, row.names=1, stringsAsFactors=FALSE, comment.char="", check.names=FALSE )

# scale pre-preprocessed training data (normalised relative abundance filtered 1% abundance in at least one sample) and merge phenotype column from metadata

otu_table_scaled <- scale(otu_table, center = TRUE, scale = TRUE)

otu_table_scaled_Phenotype <- data.frame(t(otu_table_scaled))  
otu_table_scaled_Phenotype$Phenotype <- metadata[rownames(otu_table_scaled_Phenotype), "Phenotype"] 

 
# # scale pre-preprocessed test data and merge phenotype column from metadata_test

testing_scaled <- scale(testing, center = TRUE, scale = TRUE)

testing_scaled_phenotype <- data.frame(t(testing_scaled)) 
testing_scaled_phenotype$Phenotype <- metadata_test[rownames(testing_scaled_phenotype), "Phenotype"] 

# set random seed to 42 
set.seed(42)

# set x and y 

x <- otu_table_scaled_Phenotype[,1:(ncol(otu_table_scaled_Phenotype)-1)] 
y <- otu_table_scaled_Phenotype[ , ncol(otu_table_scaled_Phenotype)]

# classify training set and test significance
RF_phenotype_classify <- randomForest( x=x , y=y , ntree=500, mtry = 13, importance=TRUE, proximities=TRUE  )
RF_phenotype_classify_sig <- rf.significance( x=RF_phenotype_classify ,  xdata=x , nperm=1000 , ntree=500, mtry = 13) 
RF_phenotype_classify
RF_phenotype_classify_sig


# run leave-one out cross validation to test model accuracy
fit_control <- trainControl( method = "LOOCV", savePredictions = TRUE)    

RF_phenotype_classify_loocv <- train( x , y=y , method="rf", ntree=500 , tuneGrid=data.frame( mtry=c(5:26)) , trControl=fit_control )
RF_phenotype_classify_loocv$results  

# get feature importance, sort by mean decrease in accuracy a
RF_phenotype_classify_importances <- as.data.frame( RF_phenotype_classify$importance )
RF_phenotype_classify_importances$features <- rownames( RF_phenotype_classify_importances )
RF_phenotype_classify_importances_sorted <- arrange( RF_phenotype_classify_importances  , desc(MeanDecreaseAccuracy)  )


# predict test association data 
predict <- predict(RF_phenotype_classify, newdata = testing_scaled_phenotype)

# output confusion matrix
result <- confusionMatrix(predict, testing_scaled_phenotype$Phenotype)
precision <- result$byClass['Pos Pred Value']    
recall <- result$byClass['Sensitivity']
correct_predictions <- as.data.frame(result$table)
correct_predictions <- correct_predictions[-c(2,3), ]
accuracy <- sum(correct_predictions$Freq) / length(testing)



# predict probabilities and performance 
prob <- predict(RF_phenotype_classify, type="prob", newdata = testing_scaled_phenotype)[,2]
pred <- prediction(prob, testing_scaled_phenotype$Phenotype)
perf <- performance(pred, "tpr", "fpr")

# calculate AUC 
auc <- performance(pred,"auc")
auc <- auc@y.values[[1]]

# plot ROC and add annotations to plot showing AUC, accuracy and recall 
title <- "ROC Curve association" 

windows.options(width=10, height=10)
# increase size of axis numbers and text slightly and force square plot 
par(pty="s", cex.lab= 1.3, cex.axis=1.3)
plot(perf,main=title,col=2,lwd=2,asp=1) 
abline(a=0,b=1,lwd=2,lty=2,col="gray")
text(1,0.20,labels=paste("AUC = ",round(auc,digits=3), sep=""), cex=1.15,adj=1)
text(1,0.15,labels=paste("Accuracy = ",round(accuracy, digits = 3), sep=""), cex=1.15,adj=1)
text(1,0.10,labels=paste("Recall = ",round(recall,digits = 3), sep=""),cex=1.15 ,adj=1)

# get feature importances for test set
RF_test_classify <- randomForest(x=testing_scaled_phenotype[,1:(ncol(testing_scaled_phenotype)-1)], y = testing_scaled_phenotype[ , ncol(testing_scaled_phenotype)] , ntree=1001, importance = TRUE, proximities=TRUE )
RF_test_classify_importances <- as.data.frame( RF_test_classify$importance)
RF_test_classify_importances$features <- rownames( RF_test_classify_importances)
RF_test_classify_importances <- arrange( RF_test_classify_importances, desc(T))


write.csv(RF_test_classify_importances, file= "feature_importances_tumor.csv")




