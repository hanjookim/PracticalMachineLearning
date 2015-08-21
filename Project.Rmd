#Practical Machine Learning Course Project

###Background
####Six participants used accelerometers on the belt, forearm, arm and
####dumbell and collected data.  They were asked to perform barbell
####lifts correctly and incorrectly in 5 different ways.

setwd("C:/My Documents/Company Work/LA County/Practical Machine Learning")
getwd()

install.packages('caret', dependencies = TRUE)
library("caret")

###Load the data
```{r 1, echo=TRUE}
raw_training <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
training <- read.csv(url(raw_training), na.strings=c("","NA","<NA>"),
                     header=TRUE)

raw_testing <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
testing <- read.csv(url(raw_testing), na.strings=c("","NA","<NA>"),
                    header=TRUE)
```
###Perform some exploratory analysis
```{r 2}
head(training)
```
```{r 3}
str(training)
```
```{r 4}
summary(training)
```
###Split training data into smaller train and validation
```{r 5}
set.seed(10)
split <- createDataPartition(y=training$classe, p=0.7, list=F)
train_t <- training[split,]
train_v <- training[-split,]
```
###Remove near zero variance
```{r 6}
nzv <- nearZeroVar(train_t)
train_t <- train_t[, -nzv]
train_v <- train_v[, -nzv]
```
###Remove variables with mostly NAs
```{r 7}
nas <- sapply(train_t, function(x) mean(is.na(x)))>0.95
train_t <- train_t[, nas==F]
train_v <- train_v[, nas==F]
```
###Remove first 5 variables that do not make sense in prediction
```{r 8}
train_t <- train_t[, -(1:5)]
train_v <- train_v[, -(1:5)]
```
###Perform Random Forest model for a better accuracy
###Use 3-fold cross-validation to select optimal tuning parameters
```{r 9}
cross <- trainControl(method="cv", number=3, verboseIter=F)
model <- train(classe ~ ., data=train_t, method="rf", trControl=cross)
model$finalModel
####The results were using 500 trees and
####trying 27 variables at each split.
```
###Predict the label (classe) in the validation data,
###using the fitted model, and get estimate of out-of-sample error
###by confusion matrix
```{r 10}
preds <- predict(model, newdata=train_v)
confusionMatrix(train_v$classe, preds)
```
####The result was that the accuracy was 0.998.
####The predicted accuracy for the out-of-sample error is 0.2%.
####Since the result is good, Random Forest model will be used
####for the test set.

##Clean the test set also
###Remove near zero variance
```{r 11}
nzv_test <- nearZeroVar(testing)
testing <- testing[, -nzv_test]
```
###Remove variables with mostly NAs
```{r 12}
nas_test <- sapply(testing, function(x) mean(is.na(x)))>0.95
testing <- testing[, nas_test==F]
```
###Remove first 5 variables that do not make sense in prediction
```{r 13}
testing <- testing[, -(1:5)]
```

###Predict the test set
####Predict on test set
```{r 14}
preds <- predict(model, newdata=testing)
```
####Convert predictions to character variables
```{r 15}
preds <- as.character(preds)
```
####Create a function to write predictions to files
```{r 16}
files <- function(x) {
  n <- length(x)
  for (i in 1:n) {
    filename <- paste0("problem_id_", i, ".txt")
    write.table(x[i], file=filename, quote=F, row.names=F, col.names=F)
  }
}
```
####Create predction files for submission
```{r 17}
files(preds)
```
