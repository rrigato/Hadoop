########################################################################################
#Ryan Rigato copyright 2015 all rights reserved
#Telstra Kaggle Competition
#The goal of this script is to predict Telstra fault severity at any location.
#The response variable is 0(no fault) 1 (a few faults)  or 2 (many faults)
#
#Part two
#
#
#
#
#
#########################################################################################

install.packages("e1071")
library(vcd)
library(plyr)
library(stats)
library(sqldf)
library(MASS)
library(tree)
library(ISLR)
library(randomForest)
library(foreign)
library(nnet)
library(e1071)

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



##############################################################
#	Sums together the volume variable from log_feature
#	does not distinguish between log_feature
#
#
#
#############################################################
train = sqldf("select id, location, fault_severity, severity_type,
		 log_feature, sum(volume) as total_volume from train group by 1")

test = sqldf("select id, location, severity_type,
		 log_feature, sum(volume) as total_volume from test group by 1")





train = merge(train , event_type, by='id')
test = merge(test , event_type, by='id')




train = merge(train , resource_type, by='id')
test = merge(test , resource_type, by='id')



#tests to make sure I have some number of observations
all_vals <- sqldf("select count(distinct(id)) from train ")



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

#validates that every observation in the training set in accounted for
temp = numeric()
temp = c(train2[1:nrow(train2),1],test2[1:nrow(test2),1])
sum(temp %in% train[1:nrow(train),1])


distplot(cbind(train2$severity_type,train2$fault_severity))

#gives the distribution
goodfit(train2$fault_severity)

boxplot(train2$fault_severity~ train2$severity_type)
boxplot(train2$fault_severity~ train2$event_type)
plot(train2$fault_severity~train2$volume)



#sorts based on severity level type
level_1 = train2[which(train2[,4] == 'severity_type 1' ),]
nrow(level_1)
level_2 = train2[which(train2[,4] == 'severity_type 2'),]
nrow(level_2)

level_3 = train2[which(train2[,4] == 'severity_type 3' ),]
nrow(level_3)
level_4 = train2[which(train2[,4] == 'severity_type 4'),]
nrow(level_4)

level_5 = train2[which(train2[,4] == 'severity_type 5' ),]
nrow(level_5)


sum(train2[level_4,3] >1)
sum(train2[level_4,3])
sum(train2[level_3,3])
head(train2)



which(train[,4] == 'severity_type 3' )

#relative frequencies to use for level1 and 2
count(level_1, 'fault_severity')/nrow(level_1)
count(level_2, 'fault_severity')/nrow(level_2)

count(level_4, 'fault_severity')/nrow(level_4)

#good for two categoricals
one_table = table( level_1$total_volume,level_1$fault_severity)
prop.table(one_table,1)






################################################################
#	Naive Bayes implementation
#	Using the log_feature, where I average observations with multiple id's
#
#	log_loss for just fault_severity~log_feature: .7087696 
#
#
##################################################################


nb = naiveBayes(fault_severity ~ log_feature, data = train2)

nb.pred = predict(nb, newdata=test2, type="raw")
nb.pred = as.data.frame(nb.pred)
head(nb.pred)
nb.pred[,4] =test2[,1]
nb.pred = rename(nb.pred, c("0" = "predict_0", "1" = "predict_1","2" = "predict_2", "V4" = "id"))
nb.pred = nb.pred[c(4,1,2,3)]

num_predict = 3
log_loss(nb.pred,num_predict)
nb.pred[,5] = ave(nb.pred$predict_0, nb.pred$id, FUN=mean)
nb.pred[,6] = ave(nb.pred$predict_1, nb.pred$id, FUN=mean)
nb.pred[,7] = ave(nb.pred$predict_2,nb.pred$id, FUN=mean)
nb.pred = length(unique(nb.pred[,1:4]))
test = nb.pred
nb.pred = sqldf("select distinct id, V5 as predict_0, V6 as predict_1, V7 as predict_2 from test")


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



