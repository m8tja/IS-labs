install.packages("CORElearn")
install.packages(c("tm", "SnowballC", "wordcloud", "proxy", "kernlab", "NLP", "openNLP"))
install.packages("openNLPmodels.en", repos="http://datacube.wu.ac.at/", type="source")
install.packages("ipred")

library(CORElearn)
library(ggplot2)
library(tm)
library(wordcloud)
library(class)
library(caret)
library(e1071)
library(rpart)
library(randomForest)
library(ipred)

test <- read.table(file="test_data.tsv", sep="\t", header=TRUE, comment.char = "", quote = "")

test_corpus <- Corpus(VectorSource(test$text_a))
#removeC <- function(x) gsub("c\\(\\\"", "", x)
#test_corpus <- tm_map(test_corpus, content_transformer(removeC))
test_corpus <- tm_map(test_corpus, content_transformer(tolower))
test_corpus <- tm_map(test_corpus, removePunctuation)
test_corpus <- tm_map(test_corpus, removeNumbers)
test_corpus <- tm_map(test_corpus, removeWords, stopwords('english'))
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
test_corpus <- tm_map(test_corpus, content_transformer(removeURL))
test_corpus <- tm_map(test_corpus, stemDocument)
removeSymbols <- function(x) gsub('[^ -~]', '', x)
test_corpus <- tm_map(test_corpus, content_transformer(removeSymbols))
test_corpus <- tm_map(test_corpus, stripWhitespace)

#content(test_corpus)

train <- read.table(file="train_data.tsv", sep="\t", header=TRUE, comment.char = "", quote = "")

train_corpus <- Corpus(VectorSource(train$text_a))
#removeC <- function(x) gsub("c\\(\\\"", "", x)
#train_corpus <- tm_map(train_corpus, content_transformer(removeC))
train_corpus <- tm_map(train_corpus, content_transformer(tolower))
train_corpus <- tm_map(train_corpus, removePunctuation)
train_corpus <- tm_map(train_corpus, removeNumbers)
train_corpus <- tm_map(train_corpus, removeWords, stopwords('english'))
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
train_corpus <- tm_map(train_corpus, content_transformer(removeURL))
train_corpus <- tm_map(train_corpus, stemDocument)
removeSymbols <- function(x) gsub('[^ -~]', '', x)
train_corpus <- tm_map(train_corpus, content_transformer(removeSymbols))
train_corpus <- tm_map(train_corpus, stripWhitespace)

test_tdm <- TermDocumentMatrix(test_corpus)
rownames(test_tdm)
findFreqTerms(test_tdm, lowfreq=300)
termFrequency <- rowSums(as.matrix(test_tdm))
termFrequency <- subset(termFrequency, termFrequency >= 300)
qplot(seq(length(termFrequency)),sort(termFrequency), xlab = "index", ylab = "Freq")

mat <- as.matrix(test_tdm)
wordFreq <- sort(rowSums(mat), decreasing=TRUE)
grayLevels <- gray((wordFreq+10) / (max(wordFreq)+10))
wordcloud(words=names(wordFreq), freq=wordFreq, min.freq=100, random.order=F, colors=grayLevels)

train_tdm2 <- TermDocumentMatrix(train_corpus)
rownames(train_tdm2)
findFreqTerms(train_tdm2, lowfreq=300)
termFrequency2 <- rowSums(as.matrix(train_tdm2))
termFrequency2 <- subset(termFrequency, termFrequency >= 300)
qplot(seq(length(termFrequency2)),sort(termFrequency2), xlab = "index", ylab = "Freq")

mat2 <- as.matrix(train_tdm2)
wordFreq2 <- sort(rowSums(mat2), decreasing=TRUE)
grayLevels2 <- gray((wordFreq2+10) / (max(wordFreq2)+10))
wordcloud(words=names(wordFreq2), freq=wordFreq2, min.freq=100, random.order=F, colors=grayLevels2)

CA <- function(observed, predicted)
{
  t <- table(observed, predicted)
  
  sum(diag(t)) / sum(t)
}

train_dtm1.tfidf <- DocumentTermMatrix(train_corpus, control = list(weighting = weightTfIdf))
#dim(train_dtm1.tfidf)

train_dtmS1.tfidf <- removeSparseTerms(train_dtm1.tfidf, 0.90)
#dim(train_dtmS1.tfidf)
mat1.mat <- as.matrix(train_dtmS1.tfidf)

test_dtm2.tfidf <- DocumentTermMatrix(test_corpus, control = list(dictionary=Terms(train_dtmS1.tfidf), weighting = weightTfIdf))
#dim(test_dtm2.tfidf)
mat2.mat <- as.matrix(test_dtm2.tfidf)

#dim(mat2.mat)
#dim(mat1.mat)

test_label <- test$label
#length(test_label)
train_label <- train$label
#length(train_label)

test_data <- cbind(mat2.mat, test_label)
names(test_data)[ncol(test_data)] <- "Label"
#dim(test_data)

train_data <- cbind(mat1.mat, train_label)
names(train_data)[ncol(train_data)] <- "Label"
#dim(train_data)

r1 <- which(names(test_data)=="Label")
r2 <- which(names(train_data)=="Label")

predictedKNN <- knn(train_data[, -r1], test_data[, -r2], train_data[, r1])
observed <- test_label
resultKNN <- CA(observed, predictedKNN)
resultKNN

train_dtm1.tfidf <- DocumentTermMatrix(train_corpus, control = list(weighting = weightTfIdf))
#dim(train_dtm1.tfidf)

train_dtmS1.tfidf <- removeSparseTerms(train_dtm1.tfidf, 0.99)
#dim(train_dtmS1.tfidf)
mat1.mat <- as.matrix(train_dtmS1.tfidf)

test_dtm2.tfidf <- DocumentTermMatrix(test_corpus, control = list(dictionary=Terms(train_dtmS1.tfidf), weighting = weightTfIdf))
#dim(test_dtm2.tfidf)
mat2.mat <- as.matrix(test_dtm2.tfidf)

#dim(mat2.mat)
#dim(mat1.mat)

test_label <- test$label
#length(test_label)
train_label <- train$label
#length(train_label)

test_data <- cbind(mat2.mat, test_label)
names(test_data)[ncol(test_data)] <- "Label"
#dim(test_data)

train_data <- cbind(mat1.mat, train_label)
names(train_data)[ncol(train_data)] <- "Label"

svm.train <- train_data
svm.test <- test_data
dim(svm.train)
dim(svm.test)

model.svm <- train(as.factor(train_label) ~., data = svm.train, method = "svmLinear")
predictedSVM <- predict(model.svm, svm.test, type = "raw")
resultSVM <- CA(observed, predictedSVM)
resultSVM

rf <- randomForest(as.factor(train_label) ~., data = train_data)
predictedRF <- predict(rf, newdata = test_data)
resultRF <- CA(observed, predictedRF)
resultRF

bayes <- CoreModel(as.factor(train_label) ~., data = as.data.frame(train_data), model = "bayes")
predictedNB <- predict(bayes, as.data.frame(test_data), type = "class")
resultNB <- CA(observed, predictedNB)
resultNB

bag <- bagging(as.factor(train_label) ~., data = as.data.frame(train_data), nbagg=15)
predictedBAG <- predict(bag, test_data, type="class")
resultBAG <- CA(test_label, predictedBAG)
resultBAG

actual <- test_label
#length(actual)
#length(predictedRF)

confusionMatrixKNN <- confusionMatrix(factor(predictedKNN), factor(actual), mode = "everything", positive = "1")
confusionMatrixSVM <- confusionMatrix(factor(predictedSVM), factor(actual), mode = "everything", positive = "1")
confusionMatrixRF <- confusionMatrix(factor(predictedRF), factor(actual), mode = "everything", positive = "1")
confusionMatrixNB <- confusionMatrix(factor(predictedNB), factor(actual), mode = "everything", positive = "1")
confusionMatrixBAG <- confusionMatrix(factor(predictedBAG), factor(actual), mode = "everything", positive = "1")

knnF1 <- confusionMatrixKNN$byClass['F1']
svmF1 <- confusionMatrixSVM$byClass['F1']
rfF1<- confusionMatrixRF$byClass['F1']
nbF1 <- confusionMatrixNB$byClass['F1']
bagF1 <- confusionMatrixBAG$byClass['F1']

barplot(cbind(resultKNN, resultSVM, resultRF, resultNB, resultBAG))
barplot(cbind(knnF1, svmF1, rfF1, nbF1, bagF1))

library(knitr)
#tab <- matrix(c('KNN', knnCA, knnF1, 'SVM', svmCA, svmF1, 'Random Forest', rfCA, rfF1, 'Bayes', nbCA, nbF1, 'Bagging', bagCA, bagF1), ncol=3, byrow=TRUE)
#colnames(tab) <- c('Model', 'Accuracy', 'F1')
#rownames(tab) <- c('','','','','')
#tab <- as.table(tab)

tab <- matrix(c(resultKNN, knnF1, resultSVM, svmF1, resultRF, rfF1, resultNB, nbF1, resultBAG, bagF1), ncol=2, byrow=TRUE)
colnames(tab) <- c('Accuracy', 'F1')
rownames(tab) <- c('KNN','SVM','Random Forest','Bayes','Bagging')
tab <- as.table(tab)

kable(tab)
