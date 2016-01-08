########################################################################################
#Ryan Rigato copyright 2015 all rights reserved
#Telstra Kaggle Competition
#The goal of this script is to predict Telstra fault severity at any location.
#The response variable is 0(no fault) 1 (a few faults)  or 2 (many faults)
#
#Part three
#
#
#
#
#
#########################################################################################
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

train = merge(train , log_feature, by='id')
test = merge(test , log_feature, by='id')







train = merge(train , event_type, by='id')
test = merge(test , event_type, by='id')




train = merge(train , resource_type, by='id')
test = merge(test , resource_type, by='id')




################################################################
#	Splitting the train dataset into train2 and test2
#
#
#
#################################################################




#edit The percentage of the dataset in the train2 and test2, used to build a model 
size_of_train = floor(.5*nrow(train))
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
#	log_loss of .6173927 for fault_severity ~ location + volume nround = 50
#
#	log_loss of .5663916 for fault_severity ~ location + volume +
#	  nround = 300
#
#
##	log_loss of .4738129 for fault_severity ~ all features
#	  nround = 300 eta =  .3
#
###	log_loss of .4677611 for fault_severity ~ all features
#	  nround = 300 eta =  .5
#
##
#
###	log_loss of .4882783 for fault_severity ~ all features
#	  nround = 300 eta =  .9
#
####	log_loss of .4740813 for fault_severity ~ all features
#	  nround = 300 eta =  .3 gamma = .05
#
#
#
####	log_loss of .4740813 for fault_severity ~ all features
#	 nround = 300 eta =  .3 subsample = .5
#
####	log_loss of .4825713 for fault_severity ~ all features - resource_type
#	  nround = 300 eta =  .3 subsample = .5

####	log_loss of .4663849 for fault_severity ~ all features
#	 nround = 348(optimal number of rounds by cross_validation eta =  .3 

####	log_loss of .4621409 for fault_severity ~ all features - resource_type
#	 nround = (optimal number of rounds by cross_validation) eta =  .3 

####	log_loss of .4635553 for fault_severity ~ all features - resource_type - event_type
#	 nround = (optimal number of rounds by cross_validation) eta =  .3 

####	log_loss of .5059381 for fault_severity ~ all features - resource_type - event_type - severity_type
#	 nround = (optimal number of rounds by cross_validation) eta =  .3 


##################################################################



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


#removes resource_type
test3 = test3[,-c(6)]
train2 = train2[,-c(6)]


#removes event_type
test3 = test3[,-c(5)]
train2 = train2[,-c(5)]

#removes severity_type
test3 = test3[,-c(2)]
train2 = train2[,-c(2)]


#removes volume
test3 = test3[,-c(3)]
train2 = train2[,-c(3)]

#adds a fourth variable which is the log_feature* volume
test3[,4] = test3[,3] * test3[,2]
train2[,4] = train2[,3] * train2[,2]

#adds a fifth variable which is log_feature*log_feature
test3[,5] = test3[,1] * test3[,1]
train2[,5] = train2[,1] * train2[,1]


test3[,6] = test3[,3] * test3[,3]
train2[,6] = train2[,3] * train2[,3]


length(train2_response) == nrow(train2)
length(test3_response) == nrow(test3)

train2[,2] = as.numeric(train2[,2])
train2Matrix = as.matrix(train2)

test3[,2] = as.numeric(test3[,2])
test3Matrix = as.matrix(test3)

#removes volume
test3 = test3[,-c(3)]
train2 = train2[,-c(3)]

#removes volume
test3 = test3[,-c(2)]
train2 = train2[,-c(2)]
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
bst = xgboost(param=param, data = train2Matrix, label = train2_response, nrounds=nround, max_delta_step = 10)







# Get the feature real names
names <- dimnames(train2Matrix)[[2]]

# Compute feature importance matrix
importance_matrix <- xgb.importance(names, model = bst)

# Nice graph for importance
xgb.plot.importance(importance_matrix[,])



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


#write the data frame to an excel file
write.xlsx(outputFrame,'C:/Users/Randy/Downloads/Telstra Kaggle Competion/Results.xlsx')

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














