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
install.packages("stats")
library(vcd)
library(plyr)
library(stats)



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
#train = merge(train , resource_type, by='id')
test = merge(test, severity_type, by='id')



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


count(level_1, 'fault_severity')/nrow(level_1)
count(level_2, 'fault_severity')/nrow(level_2)

sum(level_2 ==2)




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


















