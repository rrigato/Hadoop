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


#gets the unique 386 log_feature names as strings
feature_name = as.character(log_feature[!duplicated(log_feature[,2]),2]) 
for(i in 1:386)
{

	#gets the value of each unique log_features
	#then uses those as a column name
	#starts at i+4 cause the first four columns of train are id,location
	#fault_severity and severity_type  
	colnames(mtest2)[i + 4] = 
	as.character(feature_name[i])
}
ncol(mtest2)


train_row = 0
column_num = 0

#puts the volume into the observation corresponding to the log_feature 
#variable name
for(z in 1:nrow(log_feature))
{
	#gets the row in train where the id corresponds to the id in log_feature
	train_row  = which(train$id == log_feature$id[z])

	#getting the column which corresponds to 'feature x' 
	column_num = which(colnames(mtest2) == log_feature$log_feature[z])
	
	#if it is length 0 then the observation corresponds to the test set
	#otherwise place it where it belongs in mtestm
	if(length(train_row) != 0)
	{
		mtest2[train_row,column_num] = 
		mtest2[train_row,column_num] + log_feature$volume[z]
	}
}


#tests to make sure the sum of the volume is equal to the sum of the volume
#for train observations
sum(mtest2[,5:390]) ==sum(log_feature[log_feature$id %in% train$id,3])






	#initialize the matrix	
	 mtest3  = as.data.frame(matrix(nrow = nrow(test), ncol = 390))
	mtest3[,5:390] = 0
	mtest3[1:4] = test[1:4]
	mtest3 = rename(mtest3, c('V1' = 'id', 'V2' = 'log_feature',
	 'V3' = 'volume', 'V4' = 'severity_type'))


#gets the unique 386 log_feature names as strings
feature_name = as.character(log_feature[!duplicated(log_feature[,2]),2]) 
for(i in 1:386)
{

	#gets the value of each unique log_features
	#then uses those as a column name
	#starts at i+4 cause the first four columns of test are id,location
	#fault_severity and severity_type  
	colnames(mtest3)[i + 4] = 
	as.character(feature_name[i])
}
ncol(mtest3)


test_row = 0
column_num = 0

#puts the volume into the observation corresponding to the log_feature 
#variable name
for(z in 1:nrow(log_feature))
{
	#gets the row in test where the id corresponds to the id in log_feature
	test_row  = which(test$id == log_feature$id[z])

	#getting the column which corresponds to 'feature x' 
	column_num = which(colnames(mtest3) == log_feature$log_feature[z])
	
	#if it is length 0 then the observation corresponds to the test set
	#otherwise place it where it belongs in mtestm
	if(length(test_row) != 0)
	{
		mtest3[test_row,column_num] = 
		mtest3[test_row,column_num] + log_feature$volume[z]
	}
}


#tests to make sure the 
sum(mtest3[,5:390]) ==sum(log_feature[log_feature$id %in% test$id,3])









