library(reshape)
library(ggplot2)
library(mice)
library(caTools)
library(rpart)
library(rpart.plot)
library(caret)
library(e1071)
library(randomForest)

# Read data
Train_ALL = read.csv("Train2016.csv", na.strings=c("", "NA"))
Test_ALL = read.csv("Test2016.csv", na.strings=c("", "NA"))

# Correct YOB (incorrect years)
Train_ALL$YOB[Train_ALL$YOB < 1930 | Train_ALL$YOB > 2004] = NA

Train = Train_ALL[,c("USER_ID", "YOB", "Gender", "Income", "HouseholdStatus", "EducationLevel", "Party", "Q98059", "Q98197", "Q99480", "Q106272", "Q107869", "Q108343", "Q108617", "Q108754", "Q109244", "Q109367", "Q112270", "Q113181", "Q114386", "Q115195", "Q115602", "Q115611", "Q116441", "Q116881", "Q116953", "Q120650", "Q122771", "Q123621", "Q124122")]

# Multiple Imputation for demographics
set.seed(128)

simple = Train[,c(2:6,8:30)]
summary(simple)
Train_imputed = complete(mice(simple))

Train$YOB = Train_imputed$YOB
Train$Gender = Train_imputed$Gender
Train$Income = Train_imputed$Income
Train$HouseholdStatus = Train_imputed$HouseholdStatus
Train$EducationLevel = Train_imputed$EducationLevel

Train$Q98059 = Train_imputed$Q98059
Train$Q98197 = Train_imputed$Q98197
Train$Q99480 = Train_imputed$Q99480
Train$Q106272 = Train_imputed$Q106272
Train$Q107869 = Train_imputed$Q107869
Train$Q108343 = Train_imputed$Q108343
Train$Q108617 = Train_imputed$Q108617
Train$Q108754 = Train_imputed$Q108754
Train$Q109244 = Train_imputed$Q109244
Train$Q109367 = Train_imputed$Q109367
Train$Q112270 = Train_imputed$Q112270
Train$Q113181 = Train_imputed$Q113181
Train$Q114386 = Train_imputed$Q114386
Train$Q115195 = Train_imputed$Q115195
Train$Q115602 = Train_imputed$Q115602
Train$Q115611 = Train_imputed$Q115611
Train$Q116441 = Train_imputed$Q116441
Train$Q116881 = Train_imputed$Q116881
Train$Q116953 = Train_imputed$Q116953
Train$Q120650 = Train_imputed$Q120650
Train$Q122771 = Train_imputed$Q122771
Train$Q123621 = Train_imputed$Q123621
Train$Q124122 = Train_imputed$Q124122

summary(Train)

# Question Manual Imputation, Training set
for (i in names(Train[,8:30])) {
  Train[[i]] = as.character(Train[[i]])
}

Train$Q96024[is.na(Train$Q96024)] = "No"
Train$Q98059[is.na(Train$Q98059)] = "Unanswered"
Train$Q98078[is.na(Train$Q98078)] = "No"
Train$Q98197[is.na(Train$Q98197)] = "No"
Train$Q98578[is.na(Train$Q98578)] = "No"
Train$Q98869[is.na(Train$Q98869)] = "Unanswered"
Train$Q99480[is.na(Train$Q99480)] = "Unanswered"
Train$Q99581[is.na(Train$Q99581)] = "No"
Train$Q99716[is.na(Train$Q99716)] = "Yes"
Train$Q99982[is.na(Train$Q99982)] = "Nope"
Train$Q100010[is.na(Train$Q100010)] = "Unanswered"
Train$Q100562[is.na(Train$Q100562)] = "Unanswered"
Train$Q100680[is.na(Train$Q100680)] = "Yes"
Train$Q100689[is.na(Train$Q100689)] = "Unanswered"
Train$Q101162[is.na(Train$Q101162)] = "Unanswered"
Train$Q101163[is.na(Train$Q101163)] = "Unanswered"
Train$Q101596[is.na(Train$Q101596)] = "No"
Train$Q102089[is.na(Train$Q102089)] = "Unanswered"
Train$Q102289[is.na(Train$Q102289)] = "No"
Train$Q102674[is.na(Train$Q102674)] = "Unanswered"
Train$Q102687[is.na(Train$Q102687)] = "No"
Train$Q102906[is.na(Train$Q102906)] = "Unanswered"
Train$Q103293[is.na(Train$Q103293)] = "No"
Train$Q104996[is.na(Train$Q104996)] = "No"
Train$Q105655[is.na(Train$Q105655)] = "Unanswered"
Train$Q105840[is.na(Train$Q105840)] = "No"
Train$Q106042[is.na(Train$Q106042)] = "No"
Train$Q106272[is.na(Train$Q106272)] = "Unanswered"
Train$Q106388[is.na(Train$Q106388)] = "Unanswered"
Train$Q106389[is.na(Train$Q106389)] = "Unanswered"
Train$Q106993[is.na(Train$Q106993)] = "Unanswered"
Train$Q106997[is.na(Train$Q106997)] = "Unanswered"
Train$Q107491[is.na(Train$Q107491)] = "No"
Train$Q107869[is.na(Train$Q107869)] = "Unanswered"
Train$Q108342[is.na(Train$Q108342)] = "Unanswered"
Train$Q108343[is.na(Train$Q108343)] = "Unanswered"
Train$Q108617[is.na(Train$Q108617)] = "No"
Train$Q108754[is.na(Train$Q108754)] = "No"
Train$Q108855[is.na(Train$Q108855)] = "Umm..."
Train$Q108856[is.na(Train$Q108856)] = "Space"
Train$Q108950[is.na(Train$Q108950)] = "Cautious"
Train$Q109244[is.na(Train$Q109244)] = "No"
Train$Q109367[is.na(Train$Q109367)] = "Unanswered"
Train$Q110740[is.na(Train$Q110740)] = "Unanswered"
Train$Q111220[is.na(Train$Q111220)] = "No"
Train$Q111580[is.na(Train$Q111580)] = "Unanswered"
Train$Q111848[is.na(Train$Q111848)] = "No"
Train$Q112270[is.na(Train$Q112270)] = "Unanswered"
Train$Q112478[is.na(Train$Q112478)] = "Unanswered"
Train$Q112512[is.na(Train$Q112512)] = "No"
Train$Q113181[is.na(Train$Q113181)] = "Unanswered"
Train$Q113583[is.na(Train$Q113583)] = "Unanswered"
Train$Q113584[is.na(Train$Q113584)] = "Unanswered"
Train$Q113992[is.na(Train$Q113992)] = "Unanswered"
Train$Q114152[is.na(Train$Q114152)] = "No"
Train$Q114386[is.na(Train$Q114386)] = "Unanswered"
Train$Q114517[is.na(Train$Q114517)] = "Unanswered"
Train$Q114748[is.na(Train$Q114748)] = "Unanswered"
Train$Q114961[is.na(Train$Q114961)] = "Unanswered"
Train$Q115195[is.na(Train$Q115195)] = "Unanswered"
Train$Q115390[is.na(Train$Q115390)] = "Unanswered"
Train$Q115602[is.na(Train$Q115602)] = "Unanswered"
Train$Q115610[is.na(Train$Q115610)] = "Unanswered"
Train$Q115611[is.na(Train$Q115611)] = "Unanswered"
Train$Q115777[is.na(Train$Q115777)] = "Unanswered"
Train$Q115899[is.na(Train$Q115899)] = "Circumstances"
Train$Q116197[is.na(Train$Q116197)] = "Unanswered"
Train$Q116441[is.na(Train$Q116441)] = "Unanswered"
Train$Q116448[is.na(Train$Q116448)] = "Unanswered"
Train$Q116601[is.na(Train$Q116601)] = "Unanswered"
Train$Q116797[is.na(Train$Q116797)] = "No"
Train$Q116881[is.na(Train$Q116881)] = "Unanswered"
Train$Q116953[is.na(Train$Q116953)] = "Unanswered"
Train$Q117186[is.na(Train$Q117186)] = "Unanswered"
Train$Q117193[is.na(Train$Q117193)] = "Unanswered"
Train$Q118117[is.na(Train$Q118117)] = "Unanswered"
Train$Q118232[is.na(Train$Q118232)] = "Unanswered"
Train$Q118233[is.na(Train$Q118233)] = "No"
Train$Q118237[is.na(Train$Q118237)] = "No"
Train$Q118892[is.na(Train$Q118892)] = "No"
Train$Q119334[is.na(Train$Q119334)] = "No"
Train$Q119650[is.na(Train$Q119650)] = "Unanswered"
Train$Q119851[is.na(Train$Q119851)] = "No"
Train$Q120012[is.na(Train$Q120012)] = "Unanswered"
Train$Q120014[is.na(Train$Q120014)] = "Unanswered"
Train$Q120194[is.na(Train$Q120194)] = "Unanswered"
Train$Q120379[is.na(Train$Q120379)] = "No"
Train$Q120472[is.na(Train$Q120472)] = "Unanswered"
Train$Q120650[is.na(Train$Q120650)] = "Unanswered"
Train$Q120978[is.na(Train$Q120978)] = "No"
Train$Q121011[is.na(Train$Q121011)] = "No"
Train$Q121699[is.na(Train$Q121699)] = "Unanswered"
Train$Q121700[is.na(Train$Q121700)] = "Unanswered"
Train$Q122120[is.na(Train$Q122120)] = "Unanswered"
Train$Q122769[is.na(Train$Q122769)] = "No"
Train$Q122770[is.na(Train$Q122770)] = "Unanswered"
Train$Q122771[is.na(Train$Q122771)] = "Unanswered"
Train$Q123464[is.na(Train$Q123464)] = "Unanswered"
Train$Q123621[is.na(Train$Q123621)] = "No"
Train$Q124122[is.na(Train$Q124122)] = "Unanswered"
Train$Q124742[is.na(Train$Q124742)] = "Unanswered"

for (i in names(Train[,8:30])) {
  Train[[i]] = as.factor(Train[[i]])
}

# Age & AgeRange calculation
Train$Age = 2016 - Train$YOB
Train$AgeRange = cut(Train$Age, 
                     breaks = c(-Inf, 20, 30, 40, 50, 60, Inf), 
                     labels = c("-20", "20-29", "30-39", "40-49", "50-59", "60+"),
                     right = FALSE)
Train$YOB = NULL
Train$Age = NULL




# Make the same changes on test data

# Correct YOB (incorrect years)
Test_ALL$YOB[Test_ALL$YOB < 1930 | Test_ALL$YOB > 2004] = NA

Test = Test_ALL[,c("USER_ID", "YOB", "Gender", "Income", "HouseholdStatus", "EducationLevel", "Q98059", "Q98197", "Q99480", "Q106272", "Q107869", "Q108343", "Q108617", "Q108754", "Q109244", "Q109367", "Q112270", "Q113181", "Q114386", "Q115195", "Q115602", "Q115611", "Q116441", "Q116881", "Q116953", "Q120650", "Q122771", "Q123621", "Q124122")]

simpleTest = Test[,2:29]
summary(simpleTest)
Test_imputed = complete(mice(simpleTest))

Test$YOB = Test_imputed$YOB
Test$Gender = Test_imputed$Gender
Test$Income = Test_imputed$Income
Test$HouseholdStatus = Test_imputed$HouseholdStatus
Test$EducationLevel = Test_imputed$EducationLevel

Test$Q98059 = Test_imputed$Q98059
Test$Q98197 = Test_imputed$Q98197
Test$Q99480 = Test_imputed$Q99480
Test$Q106272 = Test_imputed$Q106272
Test$Q107869 = Test_imputed$Q107869
Test$Q108343 = Test_imputed$Q108343
Test$Q108617 = Test_imputed$Q108617
Test$Q108754 = Test_imputed$Q108754
Test$Q109244 = Test_imputed$Q109244
Test$Q109367 = Test_imputed$Q109367
Test$Q112270 = Test_imputed$Q112270
Test$Q113181 = Test_imputed$Q113181
Test$Q114386 = Test_imputed$Q114386
Test$Q115195 = Test_imputed$Q115195
Test$Q115602 = Test_imputed$Q115602
Test$Q115611 = Test_imputed$Q115611
Test$Q116441 = Test_imputed$Q116441
Test$Q116881 = Test_imputed$Q116881
Test$Q116953 = Test_imputed$Q116953
Test$Q120650 = Test_imputed$Q120650
Test$Q122771 = Test_imputed$Q122771
Test$Q123621 = Test_imputed$Q123621
Test$Q124122 = Test_imputed$Q124122

# Question Manual Imputation, Testing set
for (i in names(Test[,7:29])) {
  Test[[i]] = as.character(Test[[i]])
}

Test$Q96024[is.na(Test$Q96024)] = "No"
Test$Q98059[is.na(Test$Q98059)] = "Unanswered"
Test$Q98078[is.na(Test$Q98078)] = "No"
Test$Q98197[is.na(Test$Q98197)] = "No"
Test$Q98578[is.na(Test$Q98578)] = "No"
Test$Q98869[is.na(Test$Q98869)] = "Unanswered"
Test$Q99480[is.na(Test$Q99480)] = "Unanswered"
Test$Q99581[is.na(Test$Q99581)] = "No"
Test$Q99716[is.na(Test$Q99716)] = "Yes"
Test$Q99982[is.na(Test$Q99982)] = "Nope"
Test$Q100010[is.na(Test$Q100010)] = "Unanswered"
Test$Q100562[is.na(Test$Q100562)] = "Unanswered"
Test$Q100680[is.na(Test$Q100680)] = "Yes"
Test$Q100689[is.na(Test$Q100689)] = "Unanswered"
Test$Q101162[is.na(Test$Q101162)] = "Unanswered"
Test$Q101163[is.na(Test$Q101163)] = "Unanswered"
Test$Q101596[is.na(Test$Q101596)] = "No"
Test$Q102089[is.na(Test$Q102089)] = "Unanswered"
Test$Q102289[is.na(Test$Q102289)] = "No"
Test$Q102674[is.na(Test$Q102674)] = "Unanswered"
Test$Q102687[is.na(Test$Q102687)] = "No"
Test$Q102906[is.na(Test$Q102906)] = "Unanswered"
Test$Q103293[is.na(Test$Q103293)] = "No"
Test$Q104996[is.na(Test$Q104996)] = "No"
Test$Q105655[is.na(Test$Q105655)] = "Unanswered"
Test$Q105840[is.na(Test$Q105840)] = "No"
Test$Q106042[is.na(Test$Q106042)] = "No"
Test$Q106272[is.na(Test$Q106272)] = "Unanswered"
Test$Q106388[is.na(Test$Q106388)] = "Unanswered"
Test$Q106389[is.na(Test$Q106389)] = "Unanswered"
Test$Q106993[is.na(Test$Q106993)] = "Unanswered"
Test$Q106997[is.na(Test$Q106997)] = "Unanswered"
Test$Q107491[is.na(Test$Q107491)] = "No"
Test$Q107869[is.na(Test$Q107869)] = "Unanswered"
Test$Q108342[is.na(Test$Q108342)] = "Unanswered"
Test$Q108343[is.na(Test$Q108343)] = "Unanswered"
Test$Q108617[is.na(Test$Q108617)] = "No"
Test$Q108754[is.na(Test$Q108754)] = "No"
Test$Q108855[is.na(Test$Q108855)] = "Umm..."
Test$Q108856[is.na(Test$Q108856)] = "Space"
Test$Q108950[is.na(Test$Q108950)] = "Cautious"
Test$Q109244[is.na(Test$Q109244)] = "No"
Test$Q109367[is.na(Test$Q109367)] = "Unanswered"
Test$Q110740[is.na(Test$Q110740)] = "Unanswered"
Test$Q111220[is.na(Test$Q111220)] = "No"
Test$Q111580[is.na(Test$Q111580)] = "Unanswered"
Test$Q111848[is.na(Test$Q111848)] = "No"
Test$Q112270[is.na(Test$Q112270)] = "Unanswered"
Test$Q112478[is.na(Test$Q112478)] = "Unanswered"
Test$Q112512[is.na(Test$Q112512)] = "No"
Test$Q113181[is.na(Test$Q113181)] = "Unanswered"
Test$Q113583[is.na(Test$Q113583)] = "Unanswered"
Test$Q113584[is.na(Test$Q113584)] = "Unanswered"
Test$Q113992[is.na(Test$Q113992)] = "Unanswered"
Test$Q114152[is.na(Test$Q114152)] = "No"
Test$Q114386[is.na(Test$Q114386)] = "Unanswered"
Test$Q114517[is.na(Test$Q114517)] = "Unanswered"
Test$Q114748[is.na(Test$Q114748)] = "Unanswered"
Test$Q114961[is.na(Test$Q114961)] = "Unanswered"
Test$Q115195[is.na(Test$Q115195)] = "Unanswered"
Test$Q115390[is.na(Test$Q115390)] = "Unanswered"
Test$Q115602[is.na(Test$Q115602)] = "Unanswered"
Test$Q115610[is.na(Test$Q115610)] = "Unanswered"
Test$Q115611[is.na(Test$Q115611)] = "Unanswered"
Test$Q115777[is.na(Test$Q115777)] = "Unanswered"
Test$Q115899[is.na(Test$Q115899)] = "Circumstances"
Test$Q116197[is.na(Test$Q116197)] = "Unanswered"
Test$Q116441[is.na(Test$Q116441)] = "Unanswered"
Test$Q116448[is.na(Test$Q116448)] = "Unanswered"
Test$Q116601[is.na(Test$Q116601)] = "Unanswered"
Test$Q116797[is.na(Test$Q116797)] = "No"
Test$Q116881[is.na(Test$Q116881)] = "Unanswered"
Test$Q116953[is.na(Test$Q116953)] = "Unanswered"
Test$Q117186[is.na(Test$Q117186)] = "Unanswered"
Test$Q117193[is.na(Test$Q117193)] = "Unanswered"
Test$Q118117[is.na(Test$Q118117)] = "Unanswered"
Test$Q118232[is.na(Test$Q118232)] = "Unanswered"
Test$Q118233[is.na(Test$Q118233)] = "No"
Test$Q118237[is.na(Test$Q118237)] = "No"
Test$Q118892[is.na(Test$Q118892)] = "No"
Test$Q119334[is.na(Test$Q119334)] = "No"
Test$Q119650[is.na(Test$Q119650)] = "Unanswered"
Test$Q119851[is.na(Test$Q119851)] = "No"
Test$Q120012[is.na(Test$Q120012)] = "Unanswered"
Test$Q120014[is.na(Test$Q120014)] = "Unanswered"
Test$Q120194[is.na(Test$Q120194)] = "Unanswered"
Test$Q120379[is.na(Test$Q120379)] = "No"
Test$Q120472[is.na(Test$Q120472)] = "Unanswered"
Test$Q120650[is.na(Test$Q120650)] = "Unanswered"
Test$Q120978[is.na(Test$Q120978)] = "No"
Test$Q121011[is.na(Test$Q121011)] = "No"
Test$Q121699[is.na(Test$Q121699)] = "Unanswered"
Test$Q121700[is.na(Test$Q121700)] = "Unanswered"
Test$Q122120[is.na(Test$Q122120)] = "Unanswered"
Test$Q122769[is.na(Test$Q122769)] = "No"
Test$Q122770[is.na(Test$Q122770)] = "Unanswered"
Test$Q122771[is.na(Test$Q122771)] = "Unanswered"
Test$Q123464[is.na(Test$Q123464)] = "Unanswered"
Test$Q123621[is.na(Test$Q123621)] = "No"
Test$Q124122[is.na(Test$Q124122)] = "Unanswered"
Test$Q124742[is.na(Test$Q124742)] = "Unanswered"

for (i in names(Test[,7:29])) {
  Test[[i]] = as.factor(Test[[i]])
}

# Age & AgeRange calculation
Test$Age = 2016 - Test$YOB
Test$AgeRange = cut(Test$Age, 
                     breaks = c(-Inf, 20, 30, 40, 50, 60, Inf), 
                     labels = c("-20", "20-29", "30-39", "40-49", "50-59", "60+"),
                     right = FALSE)
Test$YOB = NULL
Test$Age = NULL



# Create holdout for Testing
set.seed(128)
split = sample.split(Train$Party, SplitRatio = 0.70)

# Split up the data using subset
Train_model = subset(Train, split==TRUE)
Train_holdout = subset(Train, split==FALSE)


# Random Forest Model
set.seed(128)
#ModelForest = randomForest(Party ~ . - USER_ID, data = Train_model, ntree=500, nodesize=2)
#pred.rforest = predict(ModelForest)
#pred.rforest.holdout = predict(ModelForest, newdata = Train_holdout)

#train_model_table = table(Train_model$Party, pred.rforest)
#train_model_table
#(train_model_table[1,1] + train_model_table[2,2]) / nrow(Train_model)

#train_holdout_table = table(Train_holdout$Party, pred.rforest.holdout)
#train_holdout_table
#(train_holdout_table[1,1] + train_holdout_table[2,2]) / nrow(Train_holdout)

# Full training set randomForest
ModelForestFull = randomForest(Party ~ . - USER_ID, 
                               data = Train, 
                               ntree = 1500, 
                               nodesize = 200)

pred.rforest.full = predict(ModelForestFull)

train_table = table(Train$Party, pred.rforest.full)
train_table
(train_table[1,1] + train_table[2,2]) / nrow(Train)

# Predict Testing Data
pred.rforest.Test = predict(ModelForestFull, newdata = Test)

# Submission
MySubmissionForest = data.frame(USER_ID = Test$USER_ID, Predictions = pred.rforest.Test)
write.csv(MySubmissionForest, "Submissions/SubForest7.csv", row.names=FALSE)


# CART with CV
numFolds = trainControl(method = "cv", number = 10)
cpGrid = expand.grid(.cp = seq(0.001,0.1,0.001))

train(Party ~ . - USER_ID, 
      data = Train, 
      method = "rpart", 
      trControl = numFolds, 
      tuneGrid = cpGrid)

tree = rpart(Party ~ . -USER_ID, data = Train, method = "class", cp = 0.002)
prp(tree)

pred.tree = predict(tree, newdata = Test)
threshold = 0.5
PredTestLabels = as.factor(ifelse(pred.tree[,1] > threshold, "Democrat", "Republican"))


# Submission
MySubmission = data.frame(USER_ID = Test$USER_ID, Predictions = PredTestLabels)
write.csv(MySubmission, "Submissions/SubCART10.csv", row.names=FALSE)


# GLM
ModelGLM = glm(Party ~ . -USER_ID, data = Train_model, family = binomial)

pred.glm = predict(ModelGLM, type = "response")

train_glm_table = table(Train_model$Party, pred.glm > threshold)
train_glm_table
(train_glm_table[1,1] + train_glm_table[2,2]) / nrow(Train_model)

pred.glm.ho = predict(ModelGLM, newdata = Train_holdout, type = "response")
train_glm_ho_table = table(Train_holdout$Party, pred.glm.ho > threshold)
train_glm_ho_table
(train_glm_ho_table[1,1] + train_glm_ho_table[2,2]) / nrow(Train_holdout)

ModelGLMFull = glm(Party ~ . -USER_ID, data = Train, family = binomial)
pred.glm.full = predict(ModelGLMFull, newdata = Test, type = "response")

PredTestLabelsGLM = as.factor(ifelse(pred.glm.full <= threshold, "Democrat", "Republican"))

# Submission
MySubmission3 = data.frame(USER_ID = Test$USER_ID, Predictions = PredTestLabelsGLM)
write.csv(MySubmission3, "Submissions/SubGLM2.csv", row.names=FALSE)
