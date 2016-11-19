install.packages("bigdata")
wine <- read.csv("wine.csv")
str(wine)
summary(wine)

# LINEAR MODEL ONE VARIABLE
model1 <- lm(Price~AGST, data = wine)
summary(model1)
plot(model1)

model1$residuals

SSE <- sum(model1$residuals^2)
SSE

#LINEAR MODEL TWO VARIABLES
model2 <- lm(Price~AGST + HarvestRain +WinterRain + Age + FrancePop, data = wine)
summary(model2)

#LINEAR WITH SIGNIFICANCE
model3 <- lm(Price~AGST + HarvestRain +WinterRain + FrancePop , data = wine)
summary(model3)

# ASSIGNMENT
modelAsg <- lm(Price~AGST + HarvestRain +WinterRain , data = wine)
summary(modelAsg)

plot(x = wine$Age, xlab = "Age in years", y = wine$Year, ylab = "FrancePop")
cor(wine)

#
model4 <- lm(Price~AGST + HarvestRain +WinterRain, data = wine)
summary(model4)
