train = read.csv("train2016.csv")
test = read.csv("test2016.csv")

# fix Age (NAs and illogical values)
train$Age = 2016 - train$YOB
summary(train$Age)
median(train$Age[is.na(train$Age) == FALSE])
train$Age[is.na(train$Age) == TRUE] = median(train$Age[is.na(train$Age) == FALSE])
train$Age[train$Age < 18] = median(train$Age[is.na(train$Age) == FALSE])
train$Age[train$Age > 75] = median(train$Age[is.na(train$Age) == FALSE])
summary(train$Age)

# fix Gender NAs
summary(train$Gender)
train$Gender = as.character(train$Gender)
train$Gender[train$Gender == ""] = "Unspecified"
train$Gender = as.factor(train$Gender)
summary(train$Gender)

#fix for test data

# fix Age (NAs and illogical values)
test$Age = 2016 - test$YOB
summary(test$Age)
median(test$Age[is.na(test$Age) == FALSE])
test$Age[is.na(test$Age) == TRUE] = median(test$Age[is.na(test$Age) == FALSE])
test$Age[test$Age < 18] = median(test$Age[is.na(test$Age) == FALSE])
test$Age[test$Age > 75] = median(test$Age[is.na(test$Age) == FALSE])
summary(test$Age)

# fix Gender NAs
summary(test$Gender)
test$Gender = as.character(test$Gender)
test$Gender[test$Gender == ""] = "Unspecified"
test$Gender = as.factor(test$Gender)
summary(test$Gender)

# Load CART packages
library(rpart)
library(rpart.plot)

tree = rpart(Party ~ . -USER_ID -YOB, data = train, method = "class", cp = 0.02)
prp(tree)

tree.pred = predict(tree, newdata = test)
threshold = 0.35
PredTestLabels = as.factor(ifelse(tree.pred[,1] > threshold, "Democrat", "Republican"))

MySubmission = data.frame(USER_ID = test$USER_ID, Predictions = PredTestLabels)
write.csv(MySubmission, "SubCART5.csv", row.names=FALSE)
