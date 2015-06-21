### Miscellaneaous #############################################################

# Import packages
library(knitr)
library(caret)

# Working directory
wd <- getwd()

# Reproducability
set.seed(42)

### Data #######################################################################

## Download the csv files in the data folder.
## The data will only be loaded once.


# Create folder
if(!file.exists(file.path(wd,"data"))){
  dir.create(file.path(wd,"data"))
}

# Download csv files
url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"

if(!file.exists(file.path(wd,"data","training.csv"))){ 
  download.file(url, file.path(wd,"data","training.csv")) 
}

url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

if(!file.exists(file.path(wd,"data","testing.csv"))){ 
  download.file(url, file.path(wd,"data","testing.csv")) 
}

# Check data
list.files(file.path(wd,"data"))

### Import data ################################################################

data.training <- read.table("data\\training.csv", sep=",", header = TRUE)
data.coursera  <- read.table("data\\testing.csv",  sep=",", header = TRUE)

dim(data.training)
dim(data.coursera)
colnames(data.training)

### Clean data #################################################################

sum(complete.cases(data.training))

data.training <- data.training[,-c(1,3,4,5,6,7)]
#data.coursera <- data.coursera[,-c(1,3,4,5,6,7)]

sum(colSums(is.na(data.training)) == 0)


#tmp <- apply(data.training, 2, function(x) length(which(is.na(x))))
data.training <- data.training[, colSums(is.na(data.training)) == 0] 

nzv <- nearZeroVar(data.training, saveMetrics = TRUE)
nzv[nzv$nzv == TRUE, ]

nzv <- nearZeroVar(data.training)
data.training <- data.training[, -nzv]
dim(data.training)

### Split data #################################################################

inTrain <- createDataPartition(y=data.training$classe,
                               p=0.7, list=FALSE)
data.testing <-  data.training[-inTrain,]
data.training <- data.training[inTrain,]

### Build Model ################################################################

fitControl <- trainControl(
  method = "cv",
  number = 5)
fitMod <- train(classe ~ ., 
                method = "rf", 
                trainControl = fitControl, 
                data = data.training, 
                ntree = 50)

### Evaluate Model #############################################################

evalMod <- predict(fitMod, newdata = data.testing)
confusionMatrix(data.testing$classe, evalMod)

accuracy <- postResample(evalMod, data.testing$classe)
accuracy
eoo.sampleerror <- 1 - as.numeric(accuracy[1])
eoo.sampleerror

### Prediction of the 20 test cases ############################################

pred.coursera <- predict(fitMod, newdata = data.coursera)
pred.coursera

### Variable Importance ########################################################

plot(varImp(fitMod))

### Accuracy vs #randomly selected predictors ##################################

plot.train(fitMod)

### Coursera Submission ########################################################

# Create submission folder
if(!file.exists(file.path(wd,"submission"))){
  dir.create(file.path(wd,"submission"))
}

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("submission\\problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

# Predict the 20 observations
pred.coursera <- predict(fitMod, newdata = data.coursera)
pred.coursera

# Write submission files
pml_write_files(pred.coursera)
