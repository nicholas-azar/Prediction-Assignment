#import necessary libraries
library(caret)
library(rpart)
library(rattle)
library(randomForest)
library(rpart.plot)
library(RColorBrewer)


# set seed
set.seed(1)

# get the data from online and set it as training and testing
training <- read.csv(url("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"), na.strings=c("NA","#DIV/0!",""))
testing <- read.csv(url("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"), na.strings=c("NA","#DIV/0!",""))

# create training data and testing data withinthe training set
inTrain <- createDataPartition(y=training$classe, p=0.6, list=FALSE)
myTraining <- training[inTrain, ]; myTesting <- training[-inTrain, ]
dim(myTraining); dim(myTesting)

#collects the near zero values
myDataNZV <- nearZeroVar(myTraining, saveMetrics=TRUE)

# now lets remove the compenents we don't want.
#these are the compenents we don't want:
myNZVvars <- names(myTraining) %in% c("new_window", "kurtosis_roll_belt", "kurtosis_picth_belt",
                                      "kurtosis_yaw_belt", "skewness_roll_belt", "skewness_roll_belt.1", "skewness_yaw_belt",
                                      "max_yaw_belt", "min_yaw_belt", "amplitude_yaw_belt", "avg_roll_arm", "stddev_roll_arm",
                                      "var_roll_arm", "avg_pitch_arm", "stddev_pitch_arm", "var_pitch_arm", "avg_yaw_arm",
                                      "stddev_yaw_arm", "var_yaw_arm", "kurtosis_roll_arm", "kurtosis_picth_arm",
                                      "kurtosis_yaw_arm", "skewness_roll_arm", "skewness_pitch_arm", "skewness_yaw_arm",
                                      "max_roll_arm", "min_roll_arm", "min_pitch_arm", "amplitude_roll_arm", "amplitude_pitch_arm",
                                      "kurtosis_roll_dumbbell", "kurtosis_picth_dumbbell", "kurtosis_yaw_dumbbell", "skewness_roll_dumbbell",
                                      "skewness_pitch_dumbbell", "skewness_yaw_dumbbell", "max_yaw_dumbbell", "min_yaw_dumbbell",
                                      "amplitude_yaw_dumbbell", "kurtosis_roll_forearm", "kurtosis_picth_forearm", "kurtosis_yaw_forearm",
                                      "skewness_roll_forearm", "skewness_pitch_forearm", "skewness_yaw_forearm", "max_roll_forearm",
                                      "max_yaw_forearm", "min_roll_forearm", "min_yaw_forearm", "amplitude_roll_forearm",
                                      "amplitude_yaw_forearm", "avg_roll_forearm", "stddev_roll_forearm", "var_roll_forearm",
                                      "avg_pitch_forearm", "stddev_pitch_forearm", "var_pitch_forearm", "avg_yaw_forearm",
                                      "stddev_yaw_forearm", "var_yaw_forearm")

#remove the named compenents above from our training set
myTraining <- myTraining[!myNZVvars]

#creating another subset of our training set that  we can iterate through
trainingTemp <- myTraining

for(i in 1:length(myTraining)) { 
  if( sum( is.na( myTraining[, i] ) ) /nrow(myTraining) >= .6 ) { #if the nas are more than 60% of the data
    for(j in 1:length(trainingTemp)) {
      if( length( grep(names(myTraining[i]), names(trainingTemp)[j]) ) ==1)  { #if the columns are the same:
        trainingTemp <- trainingTemp[ , -j] #Remove that column
      }   
    } 
  }
}

# now set our modified temporary training set to be our real training set
myTraining <- trainingTemp
#then get rid of the temporary data set
rm(trainingTemp)

# we also do the same 3 transformations for myTesting and testing
trainNames <- colnames(myTraining)
testNames <- colnames(myTraining[, -59]) 
myTesting <- myTesting[trainNames]
testing <- testing[testNames]

#Now we make all the data the same type
for (i in 1:length(testing) ) {
  for(j in 1:length(myTraining)) {
    if( length( grep(names(myTraining[i]), names(testing)[j]) ) ==1)  {
      class(testing[j]) <- class(myTraining[i])
    }      
  }      
}

# then we remove the last row since we don't need it
testing <- rbind(myTraining[2, -59] , testing)
testing <- testing[-1,]


# now we'll use a random forest to make predictions
modFit <- randomForest(classe ~. , data=myTraining)
predictions <- predict(modFit, myTesting, type = "class")

# then we test results using a confusion Matrix
confusionMatrix(predictions, myTesting$classe)

# now we apply it to the test set
testPredict <- predict(modFit, testing, type = "class")


# function to write the files to turn in
writeFile = function( pred){
  n = length(pred)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(pred[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

writeFile(testPredict)


