polling = read.csv("PollingData.csv")
str(polling)
table(polling$Year)
summary(polling)

#handle missing value
library("mice")
#Choosing only the column that has independent variable
simple = polling[c("Rasmussen", "SurveyUSA", "PropR", "DiffCount")]
set.seed(144)
#use complete function to perform muntiple imputation
imputed = complete(mice(simple))
#assign the value back to the original dataframe
polling$Rasmussen = imputed$Rasmussen
polling$SurveyUSA = imputed$SurveyUSA

#make the training data from 2004 and 2008 while 2012 was use as prediction
Train = subset(polling, Year == 2004 | Year == 2008)
Test = subset(polling, Year == 2012)
table(Train$Republican)

#The normal base line model is not good as the model will
#alwyas predict repblicant on the state that democrat will win for sure.

#So we will make a smart baseline model that will use only Rasmussen and predict
#So if the democrat is leading it will predict democrat and if replubcant 
#leading it will predict republicant

table(sign(Train$Rasmussen))

#test our baseline model
table(Train$Republican, sign(Train$Rasmussen))

#multicollinearity may course the problem we want to find it.
cor(Train[c("Rasmussen", "SurveyUSA", "PropR","DiffCount", "Republican")])

#we would add the model which has the most correlation with our dependent variable
mod1 = glm(Republican ~ PropR, data = Train, family = "binomial")
#AIC = 19.772
summary(mod1)

#training set prediction of mod1
pred1 = predict(mod1, type = "response")
table(Train$Republican, pred1 >= 0.5)
#make 4 mistake which is almost the same as baseline model

#now we look back to the corrleration of independeat variable
#and choose two independent variables that have low correlation
mod2 = glm(Republican ~ SurveyUSA + DiffCount, data = Train, family = "binomial")
#training set prediction of mod2
pred2 = predict(mod2, type = "response")
table(Train$Republican, pred2 >= 0.5)
#AIC = 18.439 (lower mean better)
summary(mod2)

#-----------
#Test set prediction
#smart baseline
#4 mistage and 2 inconclusive model
table(Test$Republican, sign(Test$Rasmussen))

TestPrediciton = predict(mod2, newdata = Test, type ="response")
table(Test$Republican, TestPrediciton >= 0.5)

#pull out the state that we predict wrong as all poll are suggest republicant will win
subset(Test, TestPrediciton >= 0.5 & Republican == 0)
