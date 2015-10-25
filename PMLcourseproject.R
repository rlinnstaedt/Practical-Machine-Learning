library(caret)
library(AppliedPredictiveModeling)
library(rattle)
library(randomForest)
library(rpart)
library(rpart.plot)

## Load the data


if(!file.exists("/r programming")){dir.create("/r programming")}
fileUrl1 = "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
fileUrl2 = "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
setInternet2(use = TRUE)
download.file(fileUrl1, destfile="/r programming/pml-training.csv", method="internal")
download.file(fileUrl2, destfile="/r programming/pml-testing.csv", method = "internal")
training <- read.csv("/r programming/pml-training.csv", header= TRUE, na.strings = c("#DIV/0!", "NA", ""))
testing <- read.csv("/r programming/pml-testing.csv", header= TRUE, na.strings = c("#DIV/0!", "NA", ""))

## The goal is to predict the manner in which they did the exercise, based on the
## variable "classe".  Class A: Exact, Class B: Elbows forward, class C: lifting 
## dumbbell halfway, Class D: lowering dumbbell halfway, Class E: throwing hips 
## forward

##  Describe how we made the model
##  How you used cross validation
##  Expected out of sample error 
##  why you made the choices we did

## Remove NA columns
notNA <- function(x) {
    as.vector(apply(x, 2, function(x) length(which(!is.na(x)))))
    
}

colCount <- notNA(training)
drop <- c()
for (count in 1:length(colCount)){
    if(colCount[count] < nrow(training)){
        drop <- c(drop, colnames(training)[count])
    }
}

training <- training[, !(names(training) %in% drop)]
training <- training[, 8:length(colnames(training))]

testing <- testing[, !(names(testing) %in% drop)]
testing <- testing[, 8:length(colnames(testing))]

colnames(training)
colnames(testing)

## Test for variables with zero variance

nZeroVar <- nearZeroVar(training, saveMetrics=TRUE)
nZeroVar


## split into training/testing
set.seed(2468)
inTrain <- createDataPartition(y=training$classe, p = 0.6, list=FALSE)
myTraining <- training[inTrain,]
myTesting <- training[-inTrain,]

## Use rpart ML

modRpart <- rpart(classe ~., data=myTraining, method="class")

fancyRpartPlot(modRpart)

predRpart <- predict(modRpart, myTesting, type="class")
confusionMatrix(predRpart, myTesting$classe)

## Use RF ML

modRF <- randomForest(classe ~., data=myTraining, method="class")

predRF <- predict(modRF, myTesting)

confusionMatrix(predRF, myTesting$classe)


## Error = 1 - Accuracy

## Run through 20 problem thing

predTest <- predict(modRF, testing)

pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
        filename = paste0("problem_id_",i,".txt")
        write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
}

pml_write_files(predTest)