qualityFile <- read.csv("quality.csv")
str(qualityFile)

table(qualityFile$PoorCare)
# Package used to create random samples
install.packages("caTools")
library(caTools)                       #LOad the package

# ==================================================================
#      SAMPLE THE DATA INTO TEST AND TRAIN DATA SETS
#===================================================================
set.seed(88)     # Initializes the the random number generator
split <- sample.split(qualityFile$PoorCare, SplitRatio = 0.75) # caTools package (randomly splits)
# Outcome variable , % of data you want| outcome variable is balanced| Test and training are balanced
split
qualityTrain <- subset( qualityFile, split == TRUE) # split is true to take the values of split
qualityTest  <- subset( qualityFile, split == FALSE)

nrow(qualityTrain)        # 
nrow(qualityTest)
# GENRALIZED LINEAR MODEL
qualityLog <- glm(PoorCare~ OfficeVisits + Narcotics, 
                  data = qualityTrain, 
                  family = binomial) # this tells to build the logistic regression model

summary(qualityLog) # Higher values of officeVists and Narcotics indicate Poor care
                    # They are significant
                    # AIC : evaluates models . Lower AIC is good

predictTrain <- predict(qualityLog, type = 'response') # type = response tells the predict function to give probabilities
summary(predictTrain)

tapply(predictTrain, qualityTrain$PoorCare, mean)
  
table(qualityTrain$PoorCare , predictTrain > 0.5)

# SESITIVITY

table(qualityTrain$PoorCare , predictTrain > 0.7)

table(qualityTrain$PoorCare , predictTrain > 0.3)
  #============================================================================
# MODEL 2 : StartedOnCombination and ProviderCount
#============================================================================
qualityLog <- glm(PoorCare~ StartedOnCombination + ProviderCount, 
                  data = qualityTrain, 
                  family = binomial)

summary(qualityLog)

#============================================================================
# MODEL 2 : StartedOnCombination and ProviderCount
#============================================================================
install.packages("ROCR")
library(ROCR)

ROCRPredict <- prediction(predictTrain, qualityTrain$PoorCare)
ROCRPerform <- performance(ROCRPredict, "True Positive", "False Positive")
plot(POCRPerform)
