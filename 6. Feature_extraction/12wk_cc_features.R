packages <- c("randomForest", "plyr", "rfUtilities", "caret", "ggplot2", "ROCR")

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
# Set working directory manually
home <- "C:/Users/adam-/ml_R/ml_R/feature_extraction"
work <- "C:/Users/PhD/ml_R/ml_R/feature_extraction/"
setwd(work)

####################################################################################################################################################

# load data 
otu_table <- read.table("otu_12_cc.csv", sep=",", header=T, row.names=1, stringsAsFactors=FALSE, comment.char="", check.names=FALSE)  
metadata <- read.table("metadata_12_cc.csv", sep=",", header=T, row.names=1, stringsAsFactors=TRUE, comment.char="", check.names=FALSE)

# scale pre-preprocessed training data (normalised relative abundance filtered 1% abundance in at least one sample) and merge phenotype column from metadata

otu_table_scaled <- scale(otu_table, center = TRUE, scale = TRUE)

otu_table_scaled_Phenotype <- data.frame(t(otu_table_scaled))  
otu_table_scaled_Phenotype$Phenotype <- metadata[rownames(otu_table_scaled_Phenotype), "Phenotype"] 

# set random seed to 42 
set.seed(42)

# set x and y 

x <- otu_table_scaled_Phenotype[,1:(ncol(otu_table_scaled_Phenotype)-1)] 
y <- otu_table_scaled_Phenotype[ , ncol(otu_table_scaled_Phenotype)]

# classify training set and test significance
RF_phenotype_classify <- randomForest( x=x , y=y , ntree=300, mtry = 14, importance=TRUE, proximities=TRUE  )
RF_phenotype_classify_sig <- rf.significance( x=RF_phenotype_classify ,  xdata=x , nperm=1000 , ntree=300) 
RF_phenotype_classify
RF_phenotype_classify_sig

# get feature importance, sort by mean decrease in accuracy a
RF_phenotype_classify_importances <- as.data.frame( RF_phenotype_classify$importance )
RF_phenotype_classify_importances$features <- rownames( RF_phenotype_classify_importances )
RF_phenotype_classify_importances_sorted <- arrange( RF_phenotype_classify_importances  , desc(MeanDecreaseAccuracy)  )

write.csv(RF_phenotype_classify_importances_sorted, file= "feature_importances_tumor_12cc.csv")

