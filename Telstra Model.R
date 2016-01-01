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

#install.packages('vcd')
library(vcd)



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
level_1 = which(train2[,4] == 'severity_type 1' )
length(level_1)
level_2 = which(train2[,4] == 'severity_type 2')
length(level_2)

level_3 = which(train2[,4] == 'severity_type 3' )
length(level_3)
level_4 = which(train2[,4] == 'severity_type 4')
length(level_4)

level_5 = which(train2[,4] == 'severity_type 5' )
length(level_5)



sum(train2[level_4,3])
sum(train2[level_3,3])

head(train2)











