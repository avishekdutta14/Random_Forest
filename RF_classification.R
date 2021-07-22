#######required libraries#######################
library(randomForest)
library(caret)
library(e1071)
library(dplyr)

##################preparing data and running RF#########################

#reading the main input file
rf_data1 <- read.csv("RF_input_674_phase.csv", header=TRUE, row.name=1)

#removing columns having columnSUM = 0 

a = rf_data1[,-1]

b = a[, which(colSums(a) != 0)]

phase = rf_data1[,1]

rf_data = cbind(phase,b)

#saving the filtered dataset
write.csv(rf_data, "RF_input_674_phase_colsum0.csv")

#changing a particular variable to factor since the variable is categorical
data <- transform(rf_data,phase=as.factor(phase))

# Data Partition in 70% training dataset and 30% test dataset
set.seed(123)
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
train <- data[ind==1,]
test <- data[ind==2,]

#running rf

set.seed(4444)
rf <- randomForest(phase~., #looking at phase while compared to all the columns so dot (.) is given after telda (~)
                   data=train, #train data is used to train
                   ntree = 300, #number of trees to run
                   importance = TRUE, #evaluates importance of a predictor
                   proximity = TRUE) #calculates the proximity measure among the rows

#plotting error-shows the error with growing number of trees in a forest

plot(rf)

#saving the parameters used in running RF
sink("RF_parameters_phase.txt")
rf
sink()

#prediction of model training set

rf$predicted

################For training accuracy######################
# Prediction for training data
p1 <- predict(rf, train)

# writing the prediction to check which observation are predicted incorrectly in the training dataset
write.csv(p1,"training_phase.csv")

data1 <- read.csv("RF_input_674_phase.csv", header=TRUE)

df2 <- data1 %>%  select(1, phase)
colnames(df2) <- c("SampleID", "Actual_phase")

#for training accuracy

tr <- read.csv("training_phase.csv")

#Changing the column names 
colnames(tr) <- c("SampleID", "Predicted_phase")

total1 <- merge(tr,df2,by="SampleID")
write.csv(total1,"predicted_vs_actual_in_training.csv", row.names = FALSE)

#writing confusion matrix for understanding accuracies and other parameters
sink("training_phase.txt")
confusionMatrix(p1, train$phase)
sink()


################For validaiton accuracy######################
# Prediction for validation data

p2 <- predict(rf, test)

# writing the prediction to check which observation are predicted incorrectly in the validaiton dataset
write.csv(p2,"validation_phase.csv")

pr <- read.csv("validation_phase.csv")

#Changing the column names 
colnames(pr) <- c("SampleID", "Predicted_value")

total2 <- merge(pr,df2,by="SampleID")
write.csv(total2,"predicted_vs_actual_in_validation.csv", row.names = FALSE)

#writing confusion matrix for understanding accuracies and other parameters
sink("validation_phase.txt")
confusionMatrix(p2, test$phase)
sink()

######################variable importance########################

#plotting top 10 variable which are important for decision making in decision making tree

varImpPlot(rf,
           sort = T,
           n.var = 10,
           main = "Top 10 - Variable Importance")


#writing all the variable importance
write.csv(importance(rf), "variable_importance_phase.csv")


