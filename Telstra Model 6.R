########################################################################################
#Ryan Rigato copyright 2015 all rights reserved
#Telstra Kaggle Competition
#The goal of this script is to predict Telstra fault severity at any location.
#The response variable is 0(no fault) 1 (a few faults)  or 2 (many faults)
#
#Part six
#
#
#
#
#
#########################################################################################

#for knn model
library(class)

#used for glmnet models
library("glmnet")

#for examining classification and regression trees
library(caret)

#for neuralnetwork analysis
library(neuralnet)

#write to an xlsx file
library(xlsx)
#xgboost
library(DiagrammeR)
library(Ckmeans.1d.dp)
library(xgboost)
library(methods)
library(data.table)
library(magrittr)


#manipulating strings
library(stringr)

library(vcd)
library(plyr)
library(stats)

#sql queries
library(sqldf)

library(MASS)

#decision trees
library(tree)

library(ISLR)
#randomforests
library(randomForest)

library(foreign)
library(nnet)

#naive bayes
library(e1071)

#general boosting models
library(gbm)

#importing the datasets that were provided by Telstra
train <- read.csv("C:\\Users\\Randy\\Downloads\\Telstra Kaggle Competion\\train.csv")
test <- read.csv("C:\\Users\\Randy\\Downloads\\Telstra Kaggle Competion\\test.csv")
severity_type <- read.csv("C:\\Users\\Randy\\Downloads\\Telstra Kaggle Competion\\severity_type.csv")
resource_type <- read.csv("C:\\Users\\Randy\\Downloads\\Telstra Kaggle Competion\\resource_type.csv")
log_feature <- read.csv("C:\\Users\\Randy\\Downloads\\Telstra Kaggle Competion\\log_feature.csv")
event_type <- read.csv("C:\\Users\\Randy\\Downloads\\Telstra Kaggle Competion\\event_type.csv")






#merging the datasets
train = merge(train, severity_type, by='id')
test = merge(test, severity_type, by='id')









###############################################################################
#	This section of code takes the log_feature as the variable name and the
#	volume associated with that as the value of each of those 386 unique variables
#	
#	precondition:train has all 7381 observations with four variables:
#	id, log_feature, volume, and severity_type already in the data_frame
#
###############################################################################

	#initialize the matrix	
	 mtest2  = as.data.frame(matrix(nrow = nrow(train), ncol = 390))
	mtest2[,5:390] = 0
	mtest2[1:4] = train[1:4]
	mtest2 = rename(mtest2, c('V1' = 'id', 'V2' = 'log_feature',
	 'V3' = 'volume', 'V4' = 'severity_type'))




for(i in 1:386)
{

	#gets the value of each unique log_features
	#then uses those as a column name
	#starts at i+4 cause the first four columns of train are id,location
	#fault_severity and severity_type  
	colnames(mtest2)[i + 4] = 
	as.character(log_feature[unique(log_feature$log_feature),2][i])
}
head(mtest2)
str(mtest2)



