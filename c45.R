library(RWeka)
library(caret)
library(partykit)
moviedb = read.csv("moviedb.csv")
#function for creating test and train sets
divideDataset <- function(data) {
  set.seed(0375)
  split <- floor(0.8 * nrow(data))
  index <- sample(seq_len(nrow(data)), size = split)
  train <- data[index, ]
  test <- data[-index, ]
  list(trainSet=train, testSet=test)
}
movieSplit <- divideDataset(moviedb)
#Train for C45
movieC45fit <- J48(genre~., data=movieSplit$trainSet)
#Test for C45
prediction <- predict(movieC45fit, movieSplit$testSet, type="class")
cm <- confusionMatrix(prediction, movieSplit$testSet$genre)
accuracy <- cm$overall['Accuracy']
means <- colMeans(cm$byClass)
precision <- means[3]
recall <- means[1]
fmeasure <- 2*precision*recall/(precision+recall)
movieC45row <- list("movie C45", accuracy, precision, recall, fmeasure)
print(movieC45row)