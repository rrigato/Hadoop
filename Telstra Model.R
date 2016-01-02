########################################################################################
#Ryan Rigato copyright 2015 all rights reserved
#Telstra Kaggle Competition
#The goal of this script is to predict Telstra fault severity at any location.
#The response variable is 0(no fault) 1 (a few faults)  or 2 (many faults)
#
#
#
#
#
#
#
#########################################################################################

#install.packages("vcd")
#install.packages("stats")
#install.packages("tree")
#install.packages("randomForest")
library(vcd)
library(plyr)
library(stats)
library(sqldf)
library(MASS)
library(tree)
library(ISLR)
library(randomForest)

#importing the datasets that were provided by Telstra
train <- read.csv("C:\\Users\\Randy\\Downloads\\Telstra Kaggle Competion\\train.csv")
test <- read.csv("C:\\Users\\Randy\\Downloads\\Telstra Kaggle Competion\\test.csv")
severity_type <- read.csv("C:\\Users\\Randy\\Downloads\\Telstra Kaggle Competion\\severity_type.csv")
resource_type <- read.csv("C:\\Users\\Randy\\Downloads\\Telstra Kaggle Competion\\resource_type.csv")
log_feature <- read.csv("C:\\Users\\Randy\\Downloads\\Telstra Kaggle Competion\\log_feature.csv")
event_type <- read.csv("C:\\Users\\Randy\\Downloads\\Telstra Kaggle Competion\\event_type.csv")







#Making sure the data was inputed correctly
head(train)
head(test)
head(severity_type)
head(resource_type)
head(log_feature)
head(event_type)



#merging the datasets
train = merge(train, severity_type, by='id')
test = merge(test, severity_type, by='id')

train = merge(train , log_feature, by='id')
test = merge(test , log_feature, by='id')



##############################################################
#	Sums together the volume variable from log_feature
#
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


#relative frequencies to use for level1 and 2
count(level_1, 'fault_severity')/nrow(level_1)
count(level_2, 'fault_severity')/nrow(level_2)



one_table = table(level_1$fault_severity, level_1$event_type)
prop.table(one_table,1)


level_1s = sqldf("	select * from level_1 where event_type in zz.large_sample
					(select z.event_type as large_sample from
						(select event_type,count(*) as obs from level_1 group by event_type)z
					 where obs >10
					)zz 
		      ")






###################################################
#
#	First algorithm with log_loss of ~.86
#	log_loss of .8638
#
#
####################################################


#initializing a data frame to have the predicted probabilities and id 
predict_obs = nrow(test2)
prediction = data.frame(matrix(ncol=4,nrow=predict_obs))
nrow(prediction)
colnames(prediction) = c("id", "predict_0", "predict_1", "predict_2") 
prediction


data_frame = prediction
data_frame[1:nrow(data_frame),1] = test2$id
data_frame[1:nrow(data_frame),2] = .64146
data_frame[1:nrow(data_frame),3] = .26449
data_frame[1:nrow(data_frame),4] = .094037

num_predict = 3
log_loss(data_frame,3)



#######################################################
#
#	second algorithm accounts for severity_type 4
#
#	if severity_type = severity_type 4
#	then predict_0 = .86, predict_1 = .14, predict_2 = 0
#	.8558934 
#
########################################################
data_frame2 = test2
data_frame2[1:nrow(test2),ncol(data_frame2) +1] = .64146
data_frame2[1:nrow(test2),ncol(data_frame2) +1] = .26449
data_frame2[1:nrow(test2),ncol(data_frame2) +1] = .094037

#rename the columns predict_0, predict_1 etc.
data_frame2 = rename(data_frame2, c("V5" = "predict_0", "V6" = "predict_1","V7" = "predict_2"))



#	if severity_type = severity_type 4
#	then predict_0 = .86, predict_1 = .14, predict_2 = 0
data_frame2[which(data_frame2$severity_type =="severity_type 4"),5] =.86
data_frame2[which(data_frame2$severity_type =="severity_type 4"),6] =.14
data_frame2[which(data_frame2$severity_type =="severity_type 4"),7] = 0

#check to make sure proabilities are adjusted
nrow(data_frame2[which(data_frame2$severity_type =="severity_type 4"),])



#	if severity_type = severity_type 4
#	then predict_0 = .86, predict_1 = .14, predict_2 = 0
data_frame2[which(data_frame2$severity_type =="severity_type 3"),5] =.86
data_frame2[which(data_frame2$severity_type =="severity_type 3"),6] =.14
data_frame2[which(data_frame2$severity_type =="severity_type 3"),7] = 0



#have to drop the columns that are not used in the log_loss testing function
keep<-c("id", "predict_0", "predict_1", "predict_2") 
data_frame2 = data_frame2[,keep]

#number of outcome variables
num_predict = 3
log_loss(data_frame2,3)


#adds a column 5 which is the sum of predict_0, predict_1, predict_2
#for my implementation of the solution, this needs to be less than or equal to one
#This will show how many rows have predictions greater than 1 
sum(transform(data_frame2,sum=rowSums(data_frame2[,2:4]))[,5] >1)















#######################################################
#
#	third algorithm accounts for all severity
#
#	if severity_type = severity_type 4
#	then predict_0 = .86, predict_1 = .14, predict_2 = 0
#	.8558934 
#	if "severity_type 1", then .5282 for 0
#	.3308 for 1 and .1409 for 2
#	if "severity_type 2" that .74716 for 0,
#	.18877 for 1 and .06405 for 2
#	
# 	log_loss of .8084057
#
########################################################
data_frame3 = test2
data_frame3[1:nrow(test2),ncol(data_frame3) +1] = .64146
data_frame3[1:nrow(test2),ncol(data_frame3) +1] = .26449
data_frame3[1:nrow(test2),ncol(data_frame3) +1] = .094037

#rename the columns predict_0, predict_1 etc.
data_frame3 = rename(data_frame3, c("V5" = "predict_0", "V6" = "predict_1","V7" = "predict_2"))



#	if severity_type = severity_type 4
#	then predict_0 = .86, predict_1 = .14, predict_2 = 0
data_frame3[which(data_frame3$severity_type =="severity_type 4"),5] =.86
data_frame3[which(data_frame3$severity_type =="severity_type 4"),6] =.14
data_frame3[which(data_frame3$severity_type =="severity_type 4"),7] = 0

#check to make sure proabilities are adjusted
nrow(data_frame3[which(data_frame3$severity_type =="severity_type 4"),])



#	if severity_type = severity_type 3
#	then predict_0 = .86, predict_1 = .14, predict_2 = 0
data_frame3[which(data_frame3$severity_type =="severity_type 3"),5] =.86
data_frame3[which(data_frame3$severity_type =="severity_type 3"),6] =.14
data_frame3[which(data_frame3$severity_type =="severity_type 3"),7] = 0



#	if severity_type = severity_type 2
#	then predict_0 = .74716, predict_1 = .18877, predict_2 = .06405
data_frame3[which(data_frame3$severity_type =="severity_type 2"),5] = .74716
data_frame3[which(data_frame3$severity_type =="severity_type 2"),6] = .18877
data_frame3[which(data_frame3$severity_type =="severity_type 2"),7] = .06405



#	if severity_type = severity_type 1
#	then predict_0 = .5282, predict_1 = .3308, predict_2 = .1409
data_frame3[which(data_frame3$severity_type =="severity_type 1"),5] = .5282
data_frame3[which(data_frame3$severity_type =="severity_type 1"),6] = .3308
data_frame3[which(data_frame3$severity_type =="severity_type 1"),7] = .1409



#have to drop the columns that are not used in the log_loss testing function
keep<-c("id", "predict_0", "predict_1", "predict_2") 
data_frame3 = data_frame3[,keep]

#number of outcome variables
num_predict = 3
log_loss(data_frame3,3)


#adds a column 5 which is the sum of predict_0, predict_1, predict_2
#for my implementation of the solution, this needs to be less than or equal to one
#This will show how many rows have predictions greater than 1 
sum(transform(data_frame3,sum=rowSums(data_frame3[,2:4]))[,5] >1)
























#########################################################
#
#		tests that the log-loss function works
#	Only works if the first 10 obs from test2 have a y variable
#		of 0. 
#
#
##########################################################
prediction2 = data.frame(matrix(ncol=4,nrow=3))
colnames(prediction2) = c("id", "predict_0", "predict_1", "predict_2") 
data_frame = prediction2
data_frame[1:10,1] = test2$id[1:10]
data_frame[1:10,2] = 1
data_frame[1:10,3] = 0
data_frame[1:10,4] = 0
data_frame
test2[1:10,3]

#this answer should be equilvalent to log(1 - 10^(-15)) * 10
#if the first 10 observations of test2 have fault_severity =0
log_loss(data_frame,3)
which(test2$id == data_frame$id[5])
#############################################################
#end of log_loss test



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
			#gets the id from the ith row of the data_fame
			#if the actual fault_severity == j-1 then that is when y is 1
			#This is the classification of the point
			if (test2[which(test2$id==data_frame$id[i]),3] == (j-1))
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


















