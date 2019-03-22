quality = read.csv("quality.csv")
str(quality)

library(caTools)
set.seed(88)
split = sample.split(quality$PoorCare, SplitRatio  = 0.75)
split
qualityTrain = subset(quality, split == TRUE)
qualityTest = subset(quality, split == FALSE)
nrow(qualityTest)
nrow(qualityTrain)
#glm for generalize linear model
QualityLog = glm(PoorCare ~ OfficeVisits  + Narcotics, data = qualityTrain, family = binomial)
summary(QualityLog)
#predict on training  set
predictTrain = predict(QualityLog, type = "response")
summary(predictTrain)

tapply(predictTrain, qualityTrain$PoorCare, mean)

#quiz
model1 = glm(PoorCare ~ StartedOnCombination + ProviderCount, data = qualityTrain, family = binomial)
summary(model1)

#threshold
table(qualityTrain$PoorCare, predictTrain > 0.5)
#sensitivity 10/25 = 0.4
#specificity 70/74 = 0.9459

table(qualityTrain$PoorCare, predictTrain > 0.7)
#sensitivity = 8/25 = 0.32 
#specificity = 73/74 = 0.9864

table(qualityTrain$PoorCare, predictTrain > 0.2)
#sensitivity = 16/25 = 0.64 
#specificity = 54/74 = 0.7297

library(ROCR)

#we need to call the prediction first 
ROCRpred = prediction(predictTrain, qualityTrain$PoorCare)
#performance function before plot
ROCRperf = performance(ROCRpred, "tpr", "fpr")
#plot the function
plot(ROCRperf, colorize = TRUE, print.cutoffs.at = seq(0,1,0.1), text.adj=c(-0.2,1.7))

#quize ROC
#compute the test predictions
predictTest = predict(QualityLog, type = "response", newdata = qualityTest)

#compute the test set AUC
ROCRpredTest = prediction(predictTest, qualityTest$PoorCare)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
