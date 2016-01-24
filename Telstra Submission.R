########################################################################################
#Getting it into a submission after the model has been trained
#
#
#
#
#########################################################################################









################################################################
#	implementation of extreme gradient boosting algorithms(xgboost)
#
#
##################################################################


#extracts the location variable as a string
test[,2] = as.numeric(str_sub(test$location, start= 10))


#extracts log_feature
test[,4] = as.numeric(str_sub(test$log_feature, start= 9))

#extracts severity_type
test[,3] = as.numeric(str_sub(test$severity_type, start= 15))


#extracts event_type
test[,6] = as.numeric(str_sub(test$event_type, start= 12))

#extracts resource_type
test[,7] = as.numeric(str_sub(test$resource_type, start= 15))

test3id = test[,1]
test3 = test[,-c(1)]

#checks that the number of ids in the vector is equal to the number of rows in 
#the data frames

length(test3id) == nrow(test3)





test3[,2] = as.numeric(test3[,2])
test3Matrix = as.matrix(test3)





#the predictions are in a nrow(test3)*3 long vector
#bstPred[1:3] is the probability of 0,1,2 for fault_severity
#for the first observation of test
#has to be a numeric matrix just like the training set
bstPred = predict(bst, test3Matrix)
is.vector(bstPred)
str(bstPred)


#initialize output frame
outputFrame = data.frame(matrix(nrow= nrow(test), ncol=4))
outputFrame = rename(outputFrame, c("X1" = "id", "X2" = "predict_0", "X3" = "predict_1","X4" = "predict_2")) 

#Puts the ids for the observations into the first column of outputFrame[,1]
outputFrame[,1] = test[,1]
#test to make sure ids are the same
sum(outputFrame[,1] != test[,1])
z_element = 1
for (i in 1:nrow(test))
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


#write the data frame to an excel file
write.xlsx(outputFrame,'C:/Users/Randy/Downloads/Telstra Kaggle Competion/Results.xlsx')







































