#RANDOM FOREST AND CLASSIFICATION AND REGRESSION TREES(CART)
# @Author : Radhika Kini
# Data Set: 
# Cases from 1994 and 2001 used by Martin 

#===============================================================
# JUSTICE STEVEN
# 
#===============================================================
steven <- read.csv("stevens.csv")
str(steven)

#Builds a trees <- 
#cart creates splits
#creates subsets 
# minbucket parameter
# output : affirm or reverse
# reverse ==1 affirm== 0
library(caTools)

set.seed(3000)     # Initializes the the random number generator
spl <- sample.split(steven$Reverse, SplitRatio = 0.70) # caTools package (randomly splits)
# Outcome variable , % of data you want| outcome variable is balanced| Test and training are balanced
split
stevenTrain <- subset( steven, spl == TRUE) # split is true to take the values of split
stevenTest  <- subset( steven, spl == FALSE)

# rpart rpart plotting installation
# Load the library used to create CART Function
install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)

stevenTree <- rpart(Reverse ~ Circuit + Issue + 
                              Petitioner + Respondent +
                              LowerCourt + Unconst,
                    data = stevenTrain,
                    method = "class",   # Classification model
                    minbucket = 25)

prp(stevenTree)

predictCart <- predict(stevenTree, newdata = stevenTest, type = "class")
table(stevenTest$Reverse,  predictCart)

# Accuracy
(41+71)/ (41+71+22+36)
#Load ROC
library(ROCR)

predictROC <- predict(stevenTree, newdata = stevenTest)
predictROC

predROC <- prediction(predictROC[,2], stevenTest$Reverse )
perfROC <- performance(predROC)

typeof(predROC)
class(predROC)
#prp(predictCart)

install.packages("caret")
library(caret)

install.packages("e1071")
library(e1071)

numFolds <- trainControl(method = "cv", number = 10)# How many folds
# trainControl cv -- cross validation number is the number of folds

cpGrid  <- expand.grid(.cp = seq(0.01,0.5,0.01))
# this tells how to set the cp value

train(Reverse ~ Circuit + Issue + 
              Petitioner + Respondent +
              LowerCourt + Unconst,
      data = stevenTrain,
       method = "rpart",
      trControl = numFolds,
      tuneGrid = cpGrid)

#The final value used for the model was cp = 0.01.

stevenTrainCV <- rpart(Reverse ~ Circuit + Issue + 
                                Petitioner + Respondent +
                                LowerCourt + Unconst,
                       data = stevenTrain,
                       method = "class",   # Classification model
                       cp = 0.18)

predictCV <- predict(stevenTrainCV, newdata = stevenTest, type = "class")
table(stevenTest$Reverse,  predictCV)

# ACCURACY = 0.7235294
(59+64)/(59+64+29+18)



#===================================================================
# RANDOM FOREST
#==================================================================
install.packages("randomForest")
library(randomForest)

stevenRForest <- randomForest(Reverse ~ Circuit + Issue + 
                              Petitioner + Respondent +
                              LowerCourt + Unconst,
                              data = stevenTrain,
                    #method = "class",   # Classification model
                    nodesize = 25,
                    ntree = 200)

# Random Forests dont have a method
# so it cant determine whether we must use classification or regression
# By default its regression
# for classification the data must be  factor

# Convert into factor
stevenTrain$Reverse <- as.factor(stevenTrain$Reverse)
stevenTest$Reverse  <- as.factor(stevenTest$Reverse)

stevenRForest <- randomForest(Reverse ~ Circuit + Issue + 
                                Petitioner + Respondent +
                                LowerCourt + Unconst,
                              data = stevenTrain,
                              #method = "class",   # Classification model
                              nodesize = 25,
                              ntree = 200)

predictForest <- predict(stevenRForest, newdata = stevenTest)

#CONFUSION MATRIX
table(stevenTest$Reverse, predictForest)

#Accuracy = 67.65%
(40+75)/(40+37+18+75)

# KFold cross validation
#1) split the set into k folds
#2) k-1 to estimate the 1 fold
#3) Repeat for the other folds

#parameter is small or too large then accuracy is small
# cp <- complexity parameter small -- big tree

