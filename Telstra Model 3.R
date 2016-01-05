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

install.packages("e1071")
install.packages("gbm")
install.packages("xgboost")

library(xgboost)
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



















################################################################
#	Naive Bayes implementation part 2
#	Using the log_feature, where I average observations with multiple id's
#
#	log_loss for just fault_severity~log_feature: .7087696 
#	log_loss for fault_severity~log_feature + severity_type: .7417596
#	log_loss for fault_severity~severity_type: .8430985
#	log_loss for fault_severity~volume: .8910279
#	log_loss for fault_severity~ log_feature + volume: .745426
#	log_loss for fault_severity~ volume + severity_type: .8654359
#	log_loss for fault_severity~ event_type: .766821
#	log_loss for fault_severity~ location: .8121829
#	log_loss for fault_severity~ resource_type: .8068138

#	log_loss for fault_severity~ resource_type + event_type: .8164183
#	log_loss for fault_severity~ resource_type + location : .8298111
#	log_loss for fault_severity~ resource_type + log_feature : .7733385

##################################################################


nb2 = naiveBayes(fault_severity ~log_feature + event_type, data = train2)
nb.pred2 = predict(nb2, newdata=test2, type="raw")
nb.pred2 = as.data.frame(nb.pred2)
head(nb.pred2)
nb.pred2[,4] =test2[,1]
nb.pred2 = rename(nb.pred2, c("0" = "predict_0", "1" = "predict_1","2" = "predict_2", "V4" = "id"))
nb.pred2 = nb.pred2[c(4,1,2,3)]


#average the observations with the same ids
nb.pred2[,5] = ave(nb.pred2$predict_0, nb.pred2$id, FUN=mean)
nb.pred2[,6] = ave(nb.pred2$predict_1, nb.pred2$id, FUN=mean)
nb.pred2[,7] = ave(nb.pred2$predict_2,nb.pred2$id, FUN=mean)



test = nb.pred2
nb.pred2 = sqldf("select distinct id, V5 as predict_0, V6 as predict_1, V7 as predict_2 from test")


num_predict = 3
log_loss(nb.pred2,num_predict)












################################################################
#	Implementation of a classification tree
#	fault_severity~ severity_type log_loss: .8473691
#	fault_severity~ severity_type + log_loss: .8473691


	randomForest() for severity_type + resource_type + volume log_loss: 4.055598

	boosting log_loss = .9930778
	boosting log_loss = .7082097 # with shrinkage = .65, n.trees = 500, interaction.depth=4
	boosting log_loss = .6008061 # with shrinkage = .35, n.trees = 500, interaction.depth=4
	boosting log_loss = .6186485 # with shrinkage = .35, n.trees = 500, interaction.depth=4
	boosting log_loss = .7001604 # with shrinkage = .25, n.trees = 1000, interaction.depth=4
	boosting log_loss = .5851071 # with shrinkage = .25, n.trees = 250, interaction.depth=3

	boosting log_loss = .5804122 # with shrinkage = .1, n.trees = 300, interaction.depth = 2
##################################################################


#bTree = tree(factor(fault_severity) ~severity_type + resource_type,data = train2)
#bTree = randomForest(factor(fault_severity) ~severity_type + resource_type,data = train2)


#have to tell R that fault_severity is a factor
bTree = gbm(fault_severity ~. -id, distribution = "multinomial", n.trees = 300, shrinkage = .1,
		interaction.depth =2,  data = train2)
bTreeP = predict(bTree, newdata=test2, n.trees = 5000, type="response")
bTreeP = as.data.frame(bTreeP)
head(bTreeP)
bTreeP[,4] =test2[,1]
bTreeP = rename(bTreeP, c("0.300" = "predict_0", "1.300" = "predict_1","2.300" = "predict_2", "V4" = "id"))
bTreeP = bTreeP[c(4,1,2,3)]


#average the observations with the same ids
bTreeP[,5] = ave(bTreeP$predict_0, bTreeP$id, FUN=mean)
bTreeP[,6] = ave(bTreeP$predict_1, bTreeP$id, FUN=mean)
bTreeP[,7] = ave(bTreeP$predict_2,bTreeP$id, FUN=mean)



test = bTreeP
bTreeP = sqldf("select distinct id, V5 as predict_0, V6 as predict_1, V7 as predict_2 from test")


num_predict = 3
log_loss(bTreeP,num_predict)


sum(transform(bTreeP,sum=rowSums(bTreeP[,2:4]))[,5] >1.000001)




goodfit(train2$fault_severity)



################################################################
#
#	automate testing of gbm
#
#	interaction.depth is lowest at 2 or 3
#	.05 to .15 for shrinkage
#
#
################################################################

shrink_fact= seq(.01,.4, by=.01); shrink_fact
for (i in 1:40)
{

bTree = gbm(fault_severity ~. -id, distribution = "multinomial", n.trees = 250, shrinkage = .05,
		interaction.depth = 2,  data = train2)
bTreeP = predict(bTree, newdata=test2, n.trees = 5000, type="response")
bTreeP = as.data.frame(bTreeP)

bTreeP[,4] =test2[,1]
bTreeP = rename(bTreeP, c("0.250" = "predict_0", "1.250" = "predict_1","2.250" = "predict_2", "V4" = "id"))
bTreeP = bTreeP[c(4,1,2,3)]


#average the observations with the same ids
bTreeP[,5] = ave(bTreeP$predict_0, bTreeP$id, FUN=mean)
bTreeP[,6] = ave(bTreeP$predict_1, bTreeP$id, FUN=mean)
bTreeP[,7] = ave(bTreeP$predict_2,bTreeP$id, FUN=mean)



test = bTreeP
bTreeP = sqldf("select distinct id, V5 as predict_0, V6 as predict_1, V7 as predict_2 from test")
head(bTreeP)

num_predict = 3


print("log_loss for interaction depth:")
print(shrink_fact[i]) 

log_loss(bTreeP,num_predict)



}









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
