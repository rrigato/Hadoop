train <- read.csv("C:\\Users\\Randy\\Downloads\\Telstra Kaggle Competion\\train.csv")
test <- read.csv("C:\\Users\\Randy\\Downloads\\Telstra Kaggle Competion\\test.csv")
severity_type <- read.csv("C:\\Users\\Randy\\Downloads\\Telstra Kaggle Competion\\severity_type.csv")
resource_type <- read.csv("C:\\Users\\Randy\\Downloads\\Telstra Kaggle Competion\\resource_type.csv")
log_feature <- read.csv("C:\\Users\\Randy\\Downloads\\Telstra Kaggle Competion\\log_feature.csv")
event_type <- read.csv("C:\\Users\\Randy\\Downloads\\Telstra Kaggle Competion\\event_type.csv")

head(train)
head(test)
head(severity_type)
head(resource_type)
head(log_feature)
head(event_type)


plot(train$fault_severity, train$location)
class(train$location)




nrow(train)
.5*7381
ran_num_train = sample(1:nrow(train),3690)
test = 1:10; test2 = 1:5

train2 = train[ran_num,]
