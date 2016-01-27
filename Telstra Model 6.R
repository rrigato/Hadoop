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
	mtest2[,1:4] = train[,1:4]
	mtest2 = rename(mtest2, c('V1' = 'id', 'V2' = 'location',
	 'V3' = 'fault_severity', 'V4' = 'severity_type'))


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

#set train equal to mtest2
train = mtest2




	#initialize the matrix	
	 mtest3  = as.data.frame(matrix(nrow = nrow(test), ncol = 389))
	mtest3[,4:389] = 0
	mtest3[,1:3] = test[,1:3]
	mtest3 = rename(mtest3, c('V1' = 'id', 'V2' = 'location',
	 'V3' = 'severity_type'))


#gets the unique 386 log_feature names as strings
feature_name = as.character(log_feature[!duplicated(log_feature[,2]),2]) 
for(i in 1:386)
{

	#gets the value of each unique log_features
	#then uses those as a column name
	#starts at i+3 cause the first three columns of test are id,location
	#and severity_type  
	colnames(mtest3)[i + 3] = 
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


#tests to make sure the volume for test is the same
sum(mtest3[,4:389]) ==sum(log_feature[log_feature$id %in% test$id,3])



#test to make sure total volume is the same
sum(mtest3[,4:389]) + sum(mtest2[,5:390]) ==sum(log_feature[,3])


#set test equal to mtest3
test = mtest3


#################################################################################
#	adds resource_type
#
#
#
#################################################################################
	



	#initialize the matrix	
	 mtest2  = train
	mtest2[,391:400] = 0




#gets the unique 10 resource_type names as strings
feature_name = as.character(resource_type[!duplicated(resource_type[,2]),2]) 
for(i in 1:10)
{

	#gets the value of each unique log_features
	#then uses those as a column name
	#starts at i+390 cause the first four columns of train are id,location
	#fault_severity and severity_type  
	colnames(mtest2)[i + 390] = 
	as.character(feature_name[i])
}
ncol(mtest2)


train_row = 0
column_num = 0

#puts the volume into the observation corresponding to the resource_type
#variable name
for(z in 1:nrow(resource_type))
{
	#gets the row in train where the id corresponds to the id in event_type
	train_row  = which(train$id == resource_type$id[z])

	#getting the column which corresponds to 'feature x' 
	column_num = which(colnames(mtest2) == resource_type$resource_type[z])
	
	#if it is length 0 then the observation corresponds to the test set
	#otherwise place it where it belongs in mtest2
	if(length(train_row) != 0)
	{
		mtest2[train_row,column_num] = 
		mtest2[train_row,column_num] + 1
	}
}


#tests to make sure the sum of the volume is equal to the sum of the resource_type
#for train observations
sum(mtest2[,391:400]) ==length(resource_type[resource_type$id %in% train$id,2]) 

#set train equal to mtest2
train = mtest2









#################################################################################
#	adds event_type to the model
#
#
#
#
##################################################################################
	#initialize the matrix	
	 mtest2  = train
	mtest2[,391:443] = 0




#gets the unique 53 event_type names as strings
feature_name = as.character(event_type[!duplicated(event_type[,2]),2]) 
for(i in 1:53)
{

	#gets the value of each unique log_features
	#then uses those as a column name
	#starts at i+390 cause the first four columns of train are id,location
	#fault_severity and severity_type  
	colnames(mtest2)[i + 390] = 
	as.character(feature_name[i])
}
ncol(mtest2)


train_row = 0
column_num = 0

#puts the volume into the observation corresponding to the event_type
#variable name
for(z in 1:nrow(event_type))
{
	#gets the row in train where the id corresponds to the id in event_type
	train_row  = which(train$id == event_type$id[z])

	#getting the column which corresponds to 'feature x' 
	column_num = which(colnames(mtest2) == event_type$event_type[z])
	
	#if it is length 0 then the observation corresponds to the test set
	#otherwise place it where it belongs in mtest2
	if(length(train_row) != 0)
	{
		mtest2[train_row,column_num] = 
		mtest2[train_row,column_num] + 1
	}
}


#tests to make sure the sum of the volume is equal to the sum of the event_type
#for train observations
sum(mtest2[,391:443]) ==length(event_type[event_type$id %in% train$id,2]) 

#set train equal to mtest2
train = mtest2









	#initialize the matrix	
	 mtest3  = test
	mtest3[,390:442] = 0




#gets the unique 53 event_type names as strings
feature_name = as.character(event_type[!duplicated(event_type[,2]),2]) 
for(i in 1:53)
{

	#gets the value of each unique log_features
	#then uses those as a column name
	#starts at i+389 cause the first three columns of test are id,location
	# severity_type  
	colnames(mtest3)[i + 389] = 
	as.character(feature_name[i])
}
ncol(mtest3)


test_row = 0
column_num = 0

#puts the volume into the observation corresponding to the event_type
#variable name
for(z in 1:nrow(event_type))
{
	#gets the row in test where the id corresponds to the id in event_type
	test_row  = which(test$id == event_type$id[z])

	#getting the column which corresponds to 'feature x' 
	column_num = which(colnames(mtest3) == event_type$event_type[z])
	
	#if it is length 0 then the observation corresponds to the test set
	#otherwise place it where it belongs in mtest3
	if(length(test_row) != 0)
	{
		mtest3[test_row,column_num] = 
		mtest3[test_row,column_num] + 1
	}
}


#tests to make sure the sum of the volume is equal to the sum of the event_type
#for test observations
sum(mtest3[,390:442]) ==length(event_type[event_type$id %in% test$id,2]) 

#tests total
sum(mtest3[,390:442]) + sum(mtest2[,391:443]) ==nrow(event_type)
#set test equal to mtest3
test = mtest3










################################################################
#	Splitting the train dataset into train2 and test2
#
#
#
#################################################################




#edit The percentage of the dataset in the train2 and test2, used to build a model 
size_of_train = floor(.9*nrow(train))
ran_num_test = 1:nrow(train)

#gets random numbers for train2 using a sample
ran_num_train = sample(1:nrow(train), size_of_train)

#numbers not randomly selected for train2 are included in test2
#this command gets the numbers not in ran_num_train
ran_num_test = ran_num_test[(!(ran_num_test %in% ran_num_train)) == TRUE]
train2 = train[ran_num_train,]
test2 = train[ran_num_test,]









################################################################
#	implementation of extreme gradient boosting algorithms(xgboost)
#
#location, severity_type, log_feature(368 variables), volume
#eta = .05, gamma = .05 subsample = .75  log_loss = .5899256
#
#location, severity_type, log_feature(368 variables), volume
#eta = .1, gamma = .1 subsample = .75  log_loss = .5368068
#
#
##location, severity_type, log_feature(368 variables), volume, event_type(53 variables)
#eta = .1, gamma = .1 subsample = .75  log_loss = .5791565
#
#
#
#
########################################################################



#extracts the location variable as a string
train2[,2] = as.numeric(str_sub(train2$location, start= 10))
test2[,2] = as.numeric(str_sub(test2$location, start= 10))


#extracts log_feature
train2[,5] = as.numeric(str_sub(train2$log_feature, start= 9))
test2[,5] = as.numeric(str_sub(test2$log_feature, start= 9))

#extracts severity_type
train2[,4] = as.numeric(str_sub(train2$severity_type, start= 15))
test2[,4] = as.numeric(str_sub(test2$severity_type, start= 15))


#extracts event_type
train2[,7] = as.numeric(str_sub(train2$event_type, start= 12))
test2[,7] = as.numeric(str_sub(test2$event_type, start= 12))

#extracts resource_type
train2[,8] = as.numeric(str_sub(train2$resource_type, start= 15))
test2[,8] = as.numeric(str_sub(test2$resource_type, start= 15))


#stores the ids in a vector and removes id from data frames
train2id = train2[,1]
train2 = train2[,-c(1)]

test3id = test2[,1]
test3 = test2[,-c(1)]

#checks that the number of ids in the vector is equal to the number of rows in 
#the data frames
length(train2id) == nrow(train2)
length(test3id) == nrow(test3)






#saves the outcome variable into a seperate vector
train2_response = train2[,2]
test3_response = test3[,2]

#removes outcome vector from the data_frame
test3 = test3[,-c(2)]
train2 = train2[,-c(2)]



length(train2_response) == nrow(train2)
length(test3_response) == nrow(test3)

train2[,2] = as.numeric(train2[,2])
train2Matrix = as.matrix(train2)

test3[,2] = as.numeric(test3[,2])
test3Matrix = as.matrix(test3)



#cross_validation parameters
numberOfClasses = 3
param = list( "objective" = "multi:softprob",
		"eval_metric" = "mlogloss",
		"num_class" = numberOfClasses
		)
cv.nround <- 1000
cv.nfold <- 3

#setting up cross_validation
bst.cv = xgb.cv(param=param, data = train2Matrix, label = train2_response, 
                nfold = cv.nfold, nrounds = cv.nround)

#test for optimal nround
bst.cv[which(min(bst.cv$test.mlogloss.mean) == bst.cv$test.mlogloss.mean),]

#sets the number of rounds based on the number of rounds determined by cross_validation
nround = which(min(bst.cv$test.mlogloss.mean) == bst.cv$test.mlogloss.mean)
#actual xgboost
bst = xgboost(param=param, data = train2Matrix, label = train2_response,
		gamma = .1, eta = .1, nrounds=nround,
		subsample = .75, max_delta_step = 15)







# Get the feature real names
names <- dimnames(train2Matrix)[[2]]

# Compute feature importance matrix
importance_matrix <- xgb.importance(names, model = bst); importance_matrix

# Nice graph for importance
xgb.plot.importance(importance_matrix[1:100,])



#the predictions are in a nrow(test3)*3 long vector
#bstPred[1:3] is the probability of 0,1,2 for fault_severity
#for the first observation of test2
#has to be a numeric matrix just like the training set
bstPred = predict(bst, test3Matrix)
is.vector(bstPred)
str(bstPred)


#initialize output frame
outputFrame = data.frame(matrix(nrow= nrow(test2), ncol=4))
outputFrame = rename(outputFrame, c("X1" = "id", "X2" = "predict_0", "X3" = "predict_1","X4" = "predict_2")) 

#Puts the ids for the observations into the first column of outputFrame[,1]
outputFrame[,1] = test2[,1]
#test to make sure ids are the same
sum(outputFrame[,1] != test2[,1])
z_element = 1
for (i in 1:nrow(test2))
{
	for (z in 1:3)
	{
		#the ith row of outputFrame is given observation z_element
		#probability of occuring from bstPred
		#column z+1 since id is in column 1
		outputFrame[i,z+1] = bstPred[z_element]
		z_element = z_element + 1
	}
}






#average the observations with the same ids
outputFrame[,5] = ave(outputFrame$predict_0, outputFrame$id, FUN=mean)
outputFrame[,6] = ave(outputFrame$predict_1, outputFrame$id, FUN=mean)
outputFrame[,7] = ave(outputFrame$predict_2,outputFrame$id, FUN=mean)
outputFrame = outputFrame[,-c(2,3,4)]
outputFrame = rename(outputFrame, c( "V5" = "predict_0", "V6" = "predict_1","V7" = "predict_2")) 



outputFrame = outputFrame[!duplicated(outputFrame$id),]



num_predict = 3
log_loss(outputFrame,num_predict)












##############################################################################
#write the results of importance matrix to a csv
#
#
#
#################################################################

#rows 1-133
write.xlsx(as.data.frame(importance_matrix),
	'C:/Users/Randy/Downloads/Telstra Kaggle Competion/importanceMatrix.xlsx'
	, append =TRUE)



write.xlsx(as.data.frame(importance_matrix),
	'C:/Users/Randy/Downloads/Telstra Kaggle Competion/importanceLogFeature.xlsx'
	, append =TRUE)


###############################################################
#
#The log_loss function takes two arguements, a data.frame and
#a vector of length 1 num_predict
#The data.frame has ncol = num_predict +1  with column 1 = id, column 2 = predict_0, 
#column 3 = predict_1 ... etc
#
#It will multiply the log of the predicted probability times 1 if the observation
#turned out to be that category, 0 otherwise
#It sums all of those up and divides by -N, where N is the number of 
#observations in data_frame
#
#################################################################
log_loss <- function(data_frame, num_predict)
{
	total = 0
	for( i in 1:nrow(data_frame))
	{
		
		for(j in 1: num_predict)
		{
			y=0
			#gets the id from the ith row of the data_frame
			#if the actual fault_severity == j-1 then that is when y is 1
			#This is the classification of the point
			#The [1] just takes the first in case their are duplicate observations
			#of the id
			if (test2[which(test2$id==data_frame$id[i])[1],3] == (j-1))
			{
				y=1;
				
			}

			#total is equal to total plus y times
			# the log of the ith row and the j+1 column
			#it is j+1 because predict_0 is in column 2
			total = total + y*log( max( min( data_frame[i,(j+1)], 1-10^(-15) ),  10^(-15) ) )
			

		}
		
	}
	print("Your logloss score is:")
	print(-total/nrow(data_frame))


}










