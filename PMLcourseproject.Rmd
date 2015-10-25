---
output: html_document
---
## Practical Machine Learning:  Predicting Exercise Correctness
```{r setup, include=FALSE}
knitr::opts_chunk$set(cache=TRUE)
```
### Background

Devices such as Jawbone Up, Nike FuelBand, and Fitbit collect data from accelerometers on on the belt, forearm, arm, and dumbbell of 6 participants.  

Using this data we will try to predict whether an exercise was performed "correctly."  The goal is to predict the manner in which they did the exercise, based on the variable "classe".  Class A: Exact, Class B: Elbows forward, class C: lifting dumbbell halfway, Class D: lowering dumbbell halfway, Class E: throwing hips forward

### Loading Packages and Data:

Packages:
```{r, message=FALSE}
library(caret)
library(AppliedPredictiveModeling)
library(rattle)
library(randomForest)
library(rpart)
library(rpart.plot)
```

Data:
```{r}
if(!file.exists("/r programming")){dir.create("/r programming")}
fileUrl1 = "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
fileUrl2 = "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
setInternet2(use = TRUE)
download.file(fileUrl1, destfile="/r programming/pml-training.csv", method="internal")
download.file(fileUrl2, destfile="/r programming/pml-testing.csv", method = "internal")
training <- read.csv("/r programming/pml-training.csv", header= TRUE, na.strings = c("#DIV/0!", "NA", ""))
testing <- read.csv("/r programming/pml-testing.csv", header= TRUE, na.strings = c("#DIV/0!", "NA", ""))
```

### Cleaning out NA's

Using a pair of functions to select out the columns without a large number of NAs:
```{r}
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
```

Remaking data frames to remove NA columns and id/timestamp columns since they would have no impact on exercise correctness.
```{r}
training <- training[, !(names(training) %in% drop)]
training <- training[, 8:length(colnames(training))]

testing <- testing[, !(names(testing) %in% drop)]
testing <- testing[, 8:length(colnames(testing))]

colnames(training)
colnames(testing)
```

### Testing that none of the variables selected have zero variance:

```{r}
nZeroVar <- nearZeroVar(training, saveMetrics=TRUE)
nZeroVar
```

None of the variables are covariate.

### Cross Validation:  Subsetting 60%

```{r}
set.seed(2468)
inTrain <- createDataPartition(y=training$classe, p = 0.6, list=FALSE)
myTraining <- training[inTrain,]
myTesting <- training[-inTrain,]
```

### ML: rpart

testing rpart's accuracy in predicting classe
```{r}
modRpart <- rpart(classe ~., data=myTraining, method="class")
predRpart <- predict(modRpart, myTesting, type="class")
confusionMatrix(predRpart, myTesting$classe)
```

plotting the prediction:
```{r}
knitr::opts_chunk$set(fig.path='Figs/', echo=FALSE)
fancyRpartPlot(modRpart)
```

### ML: randomForest

testing randomForest's accuracy in predicting classe
```{r}
modRF <- randomForest(classe ~., data=myTraining, method="class")
predRF <- predict(modRF, myTesting)
confusionMatrix(predRF, myTesting$classe)
```

randomForest modelling shows a significant improvement in accuracy compared to rpart.  Specifically, 0.9955 to 0.7261 respectively.  

### Out of sample Error:

Out of sample Error using our best model, randomForest, is one minus the model's accuracy.  Thus, 0.0045

### Preparing the submission part:

Using the randomForest model to predict the test dataset.  
```{r}
predTest <- predict(modRF, testing)

pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
        filename = paste0("problem_id_",i,".txt")
        write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
}

pml_write_files(predTest)
```

Answers for problem ids 1 through 20:
B A B A A E D B A A B C B A E E A B B B
