#################Code for Decision Tree##################
#Download the dataset
library(rpart)
temp <- tempfile()
url <- "https://raw.githubusercontent.com/GeorgetownMcCourt/data-science/master/homework_data/accel.Rda"
download.file(url, temp)
load(temp)

#Assign test group in train dataset
k.fold <- function(data, k){
  #randomly assign numbers to a new vector based on the number of observations
  rand <- runif(nrow(train))
  #divide the vector into equal intervals (k groups)
  folds <- cut(rand, k, labels = FALSE)
  return(folds)
}
#divide train into 3 groups
train$folds <- k.fold(train, 3)

#Calculate mean F1 score
meanf1 <- function(actual, predicted){
  # Desc: 
  #   Weighted mean F1 score function
  #
  # Args:
  #   actual = a vector of actual labels
  #   predicted = predicted labels
  #
  # Returns:
  #   Single value F1 score
  #
  
  classes <- unique(actual)
  results <- data.frame()
  
  #Loop through all classes
  for(k in classes){
    prec <- sum(predicted == k & actual == k)/sum(predicted == k)
    rec <- sum(predicted == k & actual == k)/sum(actual == k)
    results <- rbind(results, 
                     data.frame(class.name = k,
                                weight = sum(actual == k)/length(actual),
                                precision = prec, 
                                recall = rec))
  }
  
  #Calculate weighted mean
  score <- results[,2] * 2 * (results[,3] * results[,4]) / (results[,3] + results[,4]) 
  return(sum(score))
}

#Rpart model for meanf1 calculation
#create a placeholder for loop results
placeholder <- data.frame()
dec.treeModel <- function(spec, data){
  #cross validation
  for(i in 1:3) {
    #decision tree model from train group - (k-1)folds and folds != i
    model <- rpart(spec, data = train[train$folds != i,], method = "class", cp = 0)
    
    #extract the optimal cp value
    cptable <- as.data.frame(printcp(model)) #build a dataframe of cptable
    xerror_min <- cptable[cptable$xerror == min(cptable$xerror),"xerror"] #extract the xerror_min
    xstd_min <- cptable[cptable$xerror == min(cptable$xerror),"xstd"] #extract the corresponding xstd
    errorplussd <- sum(xerror_min, xstd_min) # add two extracted value
    opt.cp.row <- cptable[cptable$xerror == max(cptable$xerror[cptable$xerror <= errorplussd]),] #get the row of optimal cp
    opt.cp <- opt.cp.row[1,"CP"] #extract the value of optimal cp
    model <- rpart(spec1, data = train[train$folds != i ,], method = "class", cp = opt.cp) #fit the model with optical cp
    
    #predict when folds = i
    testdata <- train[train$folds == i,]
    testdata$pred <- predict(model, newdata = testdata, type = "class")
    
    #calculate meanf1 with meanf1 function and put values of each loop into the placeholder
    mean.f1<- meanf1(testdata$activity, testdata$pred)
    placeholder <- rbind(placeholder, mean.f1)
  }
  
  #calculate the mean of meanf1
  colnames(placeholder) <- "MEANF1"
  place.holder <- data.frame(mean = mean(placeholder$MEANF1))
  return(place.holder)
}

#Model specification and cross-validate the model with meanf1
spec1 <- as.formula("activity ~ user_acc_x.G. + user_acc_y.G. + user_acc_z.G.+ avg50 + sd50")
cv.f1 <- dec.treeModel(spec1,train)

#Build the regression model with the specification using the whole train group
Dec.tree <- rpart(spec1,data = train, cp = 0, method = "class") #assume cp = 0
cptable.train <- as.data.frame(printcp(Dec.tree)) #build a dataframe of cptable
minxerror.train <- cptable.train[cptable.train$xerror == min(cptable.train$xerror),"xerror"] #extract the xerror_min
minxstd.train <- cptable.train[cptable.train$xerror == min(cptable.train$xerror),"xstd"] #extract the corresponding xstd
errorplussd.train <- sum(minxerror.train, minxstd.train) # add two extracted value
opt.cp.row.train <- cptable.train[cptable.train$xerror == max(cptable.train$xerror[cptable.train$xerror <= errorplussd.train]),] #get the row of optimal cp
opt.cp.train <- opt.cp.row.train[1,"CP"] #extract the value of optimal cp
#fit the model with optimal cp
Dec.tree <- rpart(spec1,data = train, method = "class", cp = opt.cp.train) 

#Predict the test group and build the dataframe for submission
test$pred.activity <- predict(Dec.tree, newdata = test, type = "class")
myPredictions <- data.frame(id = test$id, yhat = test$pred.activity)