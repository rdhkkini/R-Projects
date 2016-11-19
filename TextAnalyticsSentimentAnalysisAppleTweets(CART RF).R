# Working with Text
# Data file : tweets.csv
# SENTIMENT ANALYSIS OF APPLE TWEETS

install.packages("RSentiment")
library('RSentiment')

#=================================================================
# Reading the text csv file into a data frame tweetsDataFile
#=================================================================

appleTweetsDataFile <- read.csv("tweets.csv", stringsAsFactors = FALSE) # strings shouldnt be stored as factors to avoid discrepancy

str(appleTweetsDataFile) # Check what are the contents of the datafile

#1181 records 2 varibles
# Tweet : Real tweets
# Avg : Average sentiment score

# Define a variable to classify the tweets into negative and positive
# If the Avg is less than 1 then files are negative
appleTweetsDataFile$NegPosTweet <- as.factor(appleTweetsDataFile$Avg <= -1)

table(appleTweetsDataFile$NegPosTweet)


#============================================================================
# Pre- processing
#============================================================================

install.packages("tm")
install.packages("SnowballC") # Helps to use the tm package
library(tm)
library(SnowballC)

# Corpus is a collection of documents

# creation of a corpus

appleCorpus <- Corpus(VectorSource(appleTweetsDataFile$Tweet))
appleCorpus

# the corpus contents
appleCorpus[[1]]

# want the tweets in lower case
appleCorpus <- tm_map(appleCorpus, tolower)
appleCorpus[[1]]

# remove punctuation
appleCorpus <- tm_map(appleCorpus, removePunctuation)
appleCorpus[[1]]

# check for the stop words
stopwords(kind = "english")                            # Entire corpus
stopwords(kind = "english")[1:20]                      # selected corpus

# Remove stop words
appleCorpus <- tm_map(appleCorpus, removeWords, c("apple", stopwords(kind = "english")))
appleCorpus[[1]]

#stemming concept
appleCorpus <- tm_map(appleCorpus, stemDocument)
appleCorpus[[1]]

#====================================================================================
# BAG OF WORD
#====================================================================================

appleCorpus <-tm_map(appleCorpus, PlainTextDocument) # converts into plain text document
# matrix creation
appleCorpusMatrix <- DocumentTermMatrix(appleCorpus)
appleCorpusMatrix

inspect(appleCorpusMatrix[1000:1005, 500:515])

findFreqTerms(appleCorpusMatrix, lowfreq = 20)

# There are many useless terms that appear
# They have low frequencies
# remove the low frequency terms

appleCorpusMinusLowFrequencyTerms <- removeSparseTerms(appleCorpusMatrix, 0.995)
appleCorpusMinusLowFrequencyTerms

appleCorpusDataFrame <- as.data.frame(as.matrix(appleCorpusMinusLowFrequencyTerms))

# Variable names are appropriate
colnames(appleCorpusDataFrame) <- make.names(colnames(appleCorpusMinusLowFrequencyTerms))

appleCorpusDataFrame$NegPos <- appleTweetsDataFile$NegPosTweet
str(appleCorpusDataFrame)
table(appleCorpusDataFrame$NegPos)

# ==================================================================
#      SAMPLE THE DATA INTO TEST AND TRAIN DATA SETS
#===================================================================

library(caTools)                       #LOad the package


set.seed(123)     # Initializes the the random number generator
splitVector <- sample.split(appleCorpusDataFrame$NegPos, SplitRatio = 0.7) # caTools package (randomly splits)
# Outcome variable , % of data you want| outcome variable is balanced| Test and training are balanced
splitVector

appleTrain <- subset( appleCorpusDataFrame, splitVector == TRUE) # split is true to take the values of split
appleTest  <- subset( appleCorpusDataFrame, splitVector == FALSE)

table(appleTrain$NegPos)


#=============================================================
# CART MODEL
#=============================================================

library(rpart)
library(rpart.plot)

str(appleTrain)

appleCARTModel <- rpart(NegPos~., data = appleTrain, method = "class")
prp(appleCARTModel) # plot the model

appleCARTPredict <- predict(appleCARTModel, appleTest, type = "class")

# CONFUSION MATRIX
table(appleTest$NegPos, appleCARTPredict)

# ACCURACY = 89.02%
(295+21)/(295+21+34+5)


#=============================================================
# RANDOM MODEL
#=============================================================

library(randomForest)
# time consuming wrt to CART 
appleRFModel <- randomForest(NegPos~., data = appleTrain)
#ggplot(appleRFModel)

appleRFPredict <- predict(appleRFModel, appleTest, type = "class")

# CONFUSION MATRIX
table(appleTest$NegPos, appleRFPredict)

# ACCURACY = 89.577%
(294+24)/(294+24+6+31)


#=============================================================
#  PLOT SENTIMENTS
#=============================================================
#sentiments_apple = cbind(calculate_sentiment(appleTrain$NegPos))

wordcloud(appleCorpus, max.words = 100,colors=brewer.pal(6,"Dark2"))
