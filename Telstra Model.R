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
#install.packages("foreign")
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


#doesn't work
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
















#######################################################
#
#	Fourth algorithm clusters based on total_volume
#	log_loss of 22.44389
########################################################
data_frame4 = test2
data_frame4[1:nrow(test2),ncol(data_frame4) +1] = .64146
data_frame4[1:nrow(test2),ncol(data_frame4) +1] = .26449
data_frame4[1:nrow(test2),ncol(data_frame4) +1] = .094037

#rename the columns predict_0, predict_1 etc.
data_frame4 = rename(data_frame4, c("V7" = "predict_0", "V8" = "predict_1","V9" = "predict_2"))


data_frame4[which(data_frame4[,6] <= 20),9] = 0
data_frame4[which(data_frame4[,6] <= 20),8] = 1
data_frame4[which(data_frame4[,6] <= 20),7] = 0

data_frame4[which(data_frame4[,6] %in% 20:44),9] = 0
data_frame4[which(data_frame4[,6] %in% 20:44),8] = 0
data_frame4[which(data_frame4[,6] %in% 20:44),7] = 1

data_frame4[which(data_frame4[,6] >45),9] = 1
data_frame4[which(data_frame4[,6] > 45),8] = 0
data_frame4[which(data_frame4[,6] > 45),7] = 0

#have to drop the columns that are not used in the log_loss testing function
keep<-c("id", "predict_0", "predict_1", "predict_2") 
data_frame4 = data_frame4[,keep]





#number of outcome variables
num_predict = 3
log_loss(data_frame4,3)



#adds a column 5 which is the sum of predict_0, predict_1, predict_2
#for my implementation of the solution, this needs to be less than or equal to one
#This will show how many rows have predictions greater than 1 
sum(transform(data_frame4,sum=rowSums(data_frame4[,2:4]))[,5] >1)









#######################################################
#
#	Fifth algorithm qda of fault_severity ~ log_loss
#	log_loss of .9454182 for qda
#	log_los of .8423054 for lda
########################################################
data_frame5 = test2
#qda.fit = qda(fault_severity~total_volume, data= train2)
qda.fit = lda(fault_severity~total_volume, data= train2)
qda.pred = predict(qda.fit, test2)
qda.pred = as.data.frame(qda.pred)

head(data_frame5)
data_frame5[,7:11] = qda.pred 
head(data_frame5)


#rename the columns predict_0, predict_1 etc.
data_frame5 = rename(data_frame5, c("posterior.0" = "predict_0", "posterior.1" = "predict_1","posterior.2" = "predict_2"))


#have to drop the columns that are not used in the log_loss testing function
keep<-c("id", "predict_0", "predict_1", "predict_2") 
data_frame5 = data_frame5[,keep]



num_predict = 3
log_loss(data_frame5,num_predict)

#This will show how many rows have predictions greater than 1 
sum(transform(data_frame5,sum=rowSums(data_frame5[,2:4]))[,5] >1.00001)





################################################################
#	algorithm 6: lda for each category of severity_type
#
#	log_loss: .8053
#
#
#
################################################################



#linear discriminant analysis
lda.fit1 = lda(fault_severity~total_volume , data= level_1)
lda.pred1 = predict(lda.fit1, test2[which(test2[,4] == 'severity_type 1'),])
lda.pred1 = as.data.frame(lda.pred1)
head(lda.pred1)
lda.pred1[,6]= test2[which(test2[,4] == 'severity_type 1'),1]
lda.pred1 = rename(lda.pred1, c("posterior.0" = "predict_0", "posterior.1" = "predict_1","posterior.2" = "predict_2", "V6" = "id"))

#reorders the variables and drops not needed variables.
lda.pred1 = lda.pred1[c(6,2,3,4)]




#linear discriminant analysis for severity_type 2 bin
lda.fit2 = lda(fault_severity~total_volume , data= level_2)
lda.pred2 = predict(lda.fit2, test2[which(test2[,4] == 'severity_type 2'),])
lda.pred2 = as.data.frame(lda.pred2)
head(lda.pred2)
lda.pred2[,6]= test2[which(test2[,4] == 'severity_type 2'),1]
lda.pred2 = rename(lda.pred2, c("posterior.0" = "predict_0", "posterior.1" = "predict_1","posterior.2" = "predict_2", "V6" = "id"))

#reorders the variables and drops not needed variables.
lda.pred2 = lda.pred2[c(6,2,3,4)]



#####uncomment if enough obs for lda

#linear discriminant analysis for severity_type 3 bin
#lda.fit3 = lda(fault_severity~total_volume , data= level_3)
#lda.pred3 = predict(lda.fit3, test2[which(test2[,4] == 'severity_type 3'),])
#lda.pred3 = as.data.frame(lda.pred3)
#head(lda.pred3)
#lda.pred3[,6]= test2[which(test2[,4] == 'severity_type 3'),1]
#lda.pred3 = rename(lda.pred3, c("posterior.0" = "predict_0", "posterior.1" = "predict_1","posterior.2" = "predict_2", "V6" = "id"))

#reorders the variables and drops not needed variables.
#lda.pred3 = lda.pred3[c(6,2,3,4)]

#####uncomment if enough obs for lda



#fix severity_type 3
lda.pred3 = test2[which(test2[,4] == 'severity_type 3'),]
lda.pred3 = lda.pred3[c(1,2,3,4)]
lda.pred3[,2]= .86
lda.pred3[,3] = .14
lda.pred3[,4] = 0
lda.pred3 = rename(lda.pred3, c("location" = "predict_0", "fault_severity" = "predict_1","severity_type" = "predict_2"))






#linear discriminant analysis for severity_type 4 bin
lda.fit4 = lda(fault_severity~total_volume , data= level_4)
lda.pred4 = predict(lda.fit4, test2[which(test2[,4] == 'severity_type 4'),])
lda.pred4 = as.data.frame(lda.pred4)
head(lda.pred4)
lda.pred4[,5] = 0
lda.pred4[,6]= test2[which(test2[,4] == 'severity_type 4'),1]
lda.pred4 = rename(lda.pred4, c("posterior.0" = "predict_0", "posterior.1" = "predict_1","V5" = "predict_2", "V6" = "id"))

#reorders the variables and drops not needed variables.
lda.pred4 = lda.pred4[c(6,2,3,5)]






#linear discriminant analysis for severity_type 5 bin
lda.fit5 = lda(fault_severity~total_volume , data= level_5)
lda.pred5 = predict(lda.fit5, test2[which(test2[,4] == 'severity_type 5'),])
lda.pred5 = as.data.frame(lda.pred5)
head(lda.pred5)
lda.pred5[,5] = 0
lda.pred5[,6]= test2[which(test2[,4] == 'severity_type 5'),1]
lda.pred5 = rename(lda.pred5, c("posterior.0" = "predict_0", "posterior.1" = "predict_1","V5" = "predict_2", "V6" = "id"))

#reorders the variables and drops not needed variables.
lda.pred5 = lda.pred5[c(6,2,3,5)]











#bind severity type 1 and 2
data_frame6 = rbind(lda.pred1, lda.pred2)


data_frame6 = rbind(data_frame6, lda.pred3)
data_frame6 = rbind(data_frame6, lda.pred4)
data_frame6 = rbind(data_frame6, lda.pred5)




head(data_frame6)
nrow(data_frame6)


names(data_frame6)

num_predict = 3
log_loss(data_frame6,num_predict)

#This will show how many rows have predictions greater than 1 
sum(transform(data_frame6,sum=rowSums(data_frame6[,2:4]))[,5] >1.00001)





















################################################################
#	algorithm 7: multinomial for each category of severity_type
#
#	log_loss = .7961833
#
#	logistic regression was used on severity_type 4 and 5
#	This is because they have no observations of response 2
#	
################################################################



#multinomial regression
multi.fit1 = multinom(fault_severity~total_volume , data= level_1)
multi.pred1 = predict(multi.fit1, newdata=test2[which(test2[,4] == 'severity_type 1'),], "probs")
multi.pred1 = as.data.frame(multi.pred1)
head(multi.pred1)
multi.pred1[,4]= test2[which(test2[,4] == 'severity_type 1'),1]
multi.pred1 = rename(multi.pred1, c("0" = "predict_0", "1" = "predict_1","2" = "predict_2", "V4" = "id"))

#reorders the variables and drops not needed variables.
multi.pred1 = multi.pred1[c(4,1,2,3)]




#multinomial regression
multi.fit2 = multinom(fault_severity~total_volume , data= level_2)
multi.pred2 = predict(multi.fit2, newdata=test2[which(test2[,4] == 'severity_type 2'),], "probs")
multi.pred2 = as.data.frame(multi.pred2)
head(multi.pred2)
multi.pred2[,4]= test2[which(test2[,4] == 'severity_type 2'),1]
multi.pred2 = rename(multi.pred2, c("0" = "predict_0", "1" = "predict_1","2" = "predict_2", "V4" = "id"))

#reorders the variables and drops not needed variables.
multi.pred2 = multi.pred2[c(4,1,2,3)]



#####uncomment if enough obs for lda

#multinomial regression
#multi.fit3 = multinom(fault_severity~total_volume , data= level_3)
#multi.pred3 = predict(multi.fit3, newdata=test2[which(test2[,4] == 'severity_type 3'),], "probs")
#multi.pred3 = as.data.frame(multi.pred3)
#head(multi.pred3)
#multi.pred3[,4]= test2[which(test2[,4] == 'severity_type 3'),1]
#multi.pred3 = rename(multi.pred3, c("0" = "predict_0", "1" = "predict_1","2" = "predict_2", "V4" = "id"))

#reorders the variables and drops not needed variables.
#multi.pred3 = multi.pred3[c(4,1,2,3)]

#####uncomment if enough obs for lda



#fix severity_type 3
multi.pred3 = test2[which(test2[,4] == 'severity_type 3'),]
multi.pred3 = multi.pred3[c(1,2,3,4)]
multi.pred3[,2]= .86
multi.pred3[,3] = .14
multi.pred3[,4] = 0
multi.pred3 = rename(multi.pred3, c("location" = "predict_0", "fault_severity" = "predict_1","severity_type" = "predict_2"))






#logistic regression
multi.fit4 = glm(fault_severity~total_volume , data= level_4, family = "binomial")
#gets the probability of predict_1
multi.pred4 = predict(multi.fit4, newdata=test2[which(test2[,4] == 'severity_type 4'),], type= "response")
multi.pred4 = as.data.frame(multi.pred4)
head(multi.pred4)

#sets column two as the probability of 1 - predict_1
#since prob(2) ==0 
multi.pred4[,2] = 1 - multi.pred4[,1]
multi.pred4[,3]=0
multi.pred4[,4]= test2[which(test2[,4] == 'severity_type 4'),1]
multi.pred4 = rename(multi.pred4, c("V2" = "predict_0", "multi.pred4" = "predict_1","V3" = "predict_2", "V4" = "id"))

#reorders the variables and drops not needed variables.
multi.pred4 = multi.pred4[c(4,2,1,3)]





#logistic regression
multi.fit5 = glm(fault_severity~total_volume , data= level_5, family = "binomial")
#gets the probability of predict_1
multi.pred5 = predict(multi.fit5, newdata=test2[which(test2[,4] == 'severity_type 5'),], type= "response")
multi.pred5 = as.data.frame(multi.pred5)
head(multi.pred5)

#sets column two as the probability of 1 - predict_1
#since prob(2) ==0 
multi.pred5[,2] = 1 - multi.pred5[,1]
multi.pred5[,3]=0
multi.pred5[,4]= test2[which(test2[,4] == 'severity_type 5'),1]
multi.pred5 = rename(multi.pred5, c("V2" = "predict_0", "multi.pred5" = "predict_1","V3" = "predict_2", "V4" = "id"))

#reorders the variables and drops not needed variables.
multi.pred5 = multi.pred5[c(4,2,1,3)]










#bind severity type 1 and 2
data_frame7 = rbind(multi.pred1, multi.pred2)


data_frame7 = rbind(data_frame7, multi.pred3)
data_frame7 = rbind(data_frame7, multi.pred4)
data_frame7 = rbind(data_frame7, multi.pred5)




head(data_frame7)
nrow(data_frame7)
str(data_frame7)


names(data_frame7)

num_predict = 3
log_loss(data_frame7,num_predict)

#This will show how many rows have predictions greater than 1 
sum(transform(data_frame7,sum=rowSums(data_frame7[,2:4]))[,5] >1.00001)






















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
			#gets the id from the ith row of the data_frame
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


















