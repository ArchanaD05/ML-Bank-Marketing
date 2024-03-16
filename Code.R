###Scatterplots

#Age vs Balance
ggplot(data = bank_sample, aes(x = age, y = balance, color = y)) +
  geom_point() + ggtitle("Age vs Balance")

#Age vs Duration 
ggplot(data = bank_sample, aes(x = age, y = duration, color = y)) +
  geom_point() + ggtitle("Age vs Duration")

#Balance vs Duration
ggplot(data = bank_sample, aes(x = balance, y = duration, color = y)) +
  geom_point() + ggtitle("Balance vs Duration")


ggplot(data = bank_sample, aes(x = balance, y = y, color = y)) +
  geom_point() + ggtitle("Balance vs Dependent Variable")

###Descriptive Stats
summary(bank)

prop.table(table(bank$job))*100
prop.table(table(bank$marital))*100
prop.table(table(bank$education))*100
prop.table(table(bank$default))*100
prop.table(table(bank$housing))*100
prop.table(table(bank$loan))*100
prop.table(table(bank$contact))*100
prop.table(table(bank$month))*100
prop.table(table(bank$poutcome))*100



#Binary and continuous variables
list1 <- c("age","default","balance","housing","loan",
      "day","duration","campaign","pdays","previous","y")
bankDF <- bank[,list1]




###VIF Analysis
lmOut <- lm(y~., data = bankDF)
summary(lmOut)

library(car)
viftable <- vif(lmOut)
sorttable <- sort(viftable, decreasing=TRUE)
sorttable[sorttable < 10]
sorttable


#Set the seed for constant results in every run 
set.seed(7)


###Perceptron
PercBankDF <- bankDF

#Convert y variable to -1 and 1
PercBankDF$y <- lapply(PercBankDF$y, function(x){
  if(x==1)
    PercBankDF$y <- 1
  else
    PercBankDF$y <- -1
})

#create training and testing data
PercBankDF_index <- sample(nrow(PercBankDF), 0.7*nrow(PercBankDF))
PercBankDF_train <- PercBankDF[PercBankDF_index,]
PercBankDF_test <- PercBankDF[-PercBankDF_index,]

#Separate the data into input and output variables 
PercX <- PercBankDF_train[, c("age","default","balance","housing","loan","day","duration","campaign","pdays","previous")]
PercY <- PercBankDF_train$y


#Create the perceptron learning algorithm
perceptron <- function(X, y, numEpochs) {
  results <- list()
  w <- runif(ncol(X), -10, 10) #Initalize weights
  
  # For loop - number of generations(epochs) - number of times dataset is ran through
  for(j in 1:numEpochs) {
    predictedResult <- numeric(length=100) # Initalize predictedResult vector
    numIncorrect = 0 # Keeps track of # of missclassified points
    
    # For loop - loop throught dataset
    for(i in 1:length(y)) {
      xi = as.numeric(unlist(X[i,])) # Convert dataframe to vector
      predictedResult[i] = sign(w %*% xi) # Predict the point
      
      # If predicted point is incorrect - change weight
      if(predictedResult[i] != y[i]) {
        numIncorrect = numIncorrect + 1 # Add one to # of missclassified points
        w <- w + as.numeric(y[i]) * xi # Update the weight w <- w + WiXi
      } 
    } 
    # Print results of this generation(epoch)
    cat("\nEpoch #: ", j)
    cat("\nNumber Incorrect: ", numIncorrect)
    cat("\nFinal Weight: ", w)
  } 
} 


#Run the perceptron algorithm
perceptron(PercX,PercY, 15)


#Take the weights from the lowest incorrect epoch 
weight <- c(-49996.69, -1277.829, -1788.731, -16258.32, -5813.784, -47640.93, 3496.342, -33237.28, 3662.17, 13762.51)

#Multiply weights accordingly
PercBankDF_test$Wage <- PercBankDF_test$age*weight[1]
PercBankDF_test$Wdefault <- PercBankDF_test$default*weight[2]
PercBankDF_test$Wbalance <- PercBankDF_test$balance*weight[3]
PercBankDF_test$Whousing <- PercBankDF_test$housing*weight[4]
PercBankDF_test$Wloan <- PercBankDF_test$loan*weight[5]
PercBankDF_test$Wday <- PercBankDF_test$day*weight[6]
PercBankDF_test$Wduration <- PercBankDF_test$duration*weight[7]
PercBankDF_test$Wcampaign <- PercBankDF_test$campaign*weight[8]
PercBankDF_test$Wpdays <- PercBankDF_test$pdays*weight[9]
PercBankDF_test$Wprevious <- PercBankDF_test$previous*weight[10]


#Add all weights
PercBankDF_test$predict <- rowSums(PercBankDF_test[, c("Wage","Wdefault","Wbalance","Whousing",
                                                       "Wloan","Wday","Wduration","Wcampaign",
                                                       "Wpdays","Wprevious")])


#Evaluate model performance
percpredict <- table(PercBankDF_test$y == 1, PercBankDF_test$predict > 0) + 
                table(PercBankDF_test$y == -1, PercBankDF_test$predict < 0)
percpredict

#Get the accuracy rate
sum(diag(percpredict)) / sum(percpredict)





####Support Vector Machines
set.seed(7)

#Load the library
library(e1071)

#Define dataset for SVM
svmDF <- bankDF

#Convert target variable into factor
svmDF$y <- as.factor(svmDF$y)


#Create training and testing dataset
svm_index <- sample(nrow(svmDF), 0.7*nrow(svmDF))
svm_train <- svmDF[svm_index, ]
svm_test <- svmDF[-svm_index, ]

#Run the SVM model
bankSVM <- svm(y ~ ., data = svm_train)

#Summarize the model
summary(bankSVM)


#Plot the model
plot(bankSVM, svm_train, age~balance)


#Use the model for prediction
library(caret)
bankpredict <- predict(bankSVM, newdata = svm_test[,-11], type = "response")
confusionMatrix(bankpredict, svm_test$y)



###Neural Networks
library(neuralnet)
set.seed(7)

#Create training and testing data
bankSampleDF <- bank[,list1]
bankNN <- bankSampleDF
NN_index <- sample(nrow(bankNN), 0.7*nrow(bankNN), replace = FALSE)
NN_train <- bankNN[NN_index, ]
NN_test <- bankNN[-NN_index, ]

temp_test <- NN_test[,c("age","default","balance","housing","loan","day","duration","campaign","pdays","previous")]


#Create Neural Network
banknet <- neuralnet(y ~ age+default+balance+housing+loan+day+duration+campaign+pdays+previous,NN_train, hidden = 4, lifesign="minimal",
                     linear.output = FALSE, threshold = 0.1)



#Plot the model
plot(banknet)


#Test the model
banknet.results <- compute(banknet, temp_test)
results <- data.frame(actual=NN_test$y, prediction = banknet.results$net.result)
results$prediction <- round(results$prediction)
actual <- as.factor(NN_test$y)
neural_result <- as.factor(round(results$prediction))

#Build Confusion Matrix
predicttable <- table(neural_result, actual)
predicttable

#Calculate accuracy
sum(diag(predicttable)) / sum(predicttable)




#### K-nearest Neighbours
set.seed(7)

#Define dataset
knnDF <- bankSampleDF

#Z-score transformation
bank_z <- as.data.frame(scale(knnDF[-11]))
bank_z$y <- knnDF$y


#Convert output variable to factor
bank_z$y <- as.factor(bank_z$y)


#Create training and testing data
knn_index <- sample(nrow(bank_z), 0.7*nrow(bank_z))
knn_train <- bank_z[knn_index, ]
knn_test <- bank_z[-knn_index, ]

#Create a vector with output variable
bank_train_labels <- bank_z[knn_index, 11, drop=TRUE]
bank_test_labels <- bank_z[-knn_index, 11, drop=TRUE ]


#Run knn algorithm
library(class)
library(gmodels)

bank_test_pred <- knn(train = knn_train, test = knn_test, cl = bank_train_labels, k=30)


#Evaluate model
banktable <- CrossTable(x=bank_test_labels, y=bank_test_pred, prop.chisq = FALSE)


#Get predictive accuracy
sum(diag(banktable$prop.tbl))





###Naive-Bayes
library(e1071)
set.seed(7)

#Define dataset
bankNB <- bankSampleDF

#Convert output variable to a factor
bankNB$y <- as.factor(bankNB$y)

#Create training and testing data
NB_index <- sample(nrow(bankNB), 0.7*nrow(bankNB))
NB_train <- bankNB[NB_index, ]
NB_test <- bankNB[-NB_index, ]


#Run Naive-Bayes Classifier
NB_Model <- naiveBayes(y ~ ., data=NB_train, laplace=1)
NB_Model


#Evaluate the model
bank_pred <- predict(NB_Model, NB_test, type="class")
bank_pred_table <- table(NB_test$y, bank_pred)
bank_pred_table

#Calculate the accuracy
sum(diag(bank_pred_table)) / sum(bank_pred_table)
