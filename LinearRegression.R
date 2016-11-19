
# Load the data of climate into a data Frame climateChange
climateChange <- read.csv("climate_change.csv")
str(climateChange)    # Display the elements of the data set

#    BIFURCATION INTO TEST AND TRAINING DATA SETS
climateChangeTrainingSet <- subset(climateChange, climateChange$Year<=2006)
str(climateChangeTrainingSet)

climateChangeTestSet <- subset(climateChange, climateChange$Year >2006)
str(climateChangeTestSet)

climateChangeModel1 <- lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, 
                          data = climateChangeTrainingSet )
summary(climateChangeModel1)
plot(climateChangeModel1, colors = 1:6)

cor(climateChangeTrainingSet)
plot(climateChangeTrainingSet$N2O, climateChangeTrainingSet$CFC.11)

climateChangeModel2 <- lm(Temp ~ MEI + TSI + Aerosols + N2O, data = climateChangeTrainingSet)
summary(climateChangeModel2)

stepModel1 <- step(climateChangeModel1)
summary(stepModel1)
stepModel1$anova
stepModel1$model

?step

summary(climateChangeTestSet)
str(climateChangeTestSet)
climateChangeTestSetModel <- predict(climateChangeModel1, newdata = climateChangeTestSet)
summary(climateChangeTestSetModel)
