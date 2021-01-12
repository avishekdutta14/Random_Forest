#######required libraries#######################
library(randomForest)
library(dplyr)
library(ggplot2)

x= "_593"
y= ""
z= ""
##################preparing data and running RF#########################

#reading the main input file
rf_data1 <- read.csv(paste0("RF_input",x,y,z,".csv"), header=TRUE, row.name=1)

#removing columns having columnSUM = 0 

a = rf_data1[,-1]

b = a[, which(colSums(a) != 0)]

Sulfide = rf_data1[,1]

rf_data = cbind(Sulfide,b)

#saving the filtered dataset
write.csv(rf_data, paste0("RF_input",x,y,z,"_removed_column0.csv"))

#Data Partition 70:30 ratio (70% data for training and 30% data for validation)

set.seed(123)
ind <- sample(2, nrow(rf_data), replace = TRUE, prob = c(0.7, 0.3))
train <- rf_data[ind==1,]
test <- rf_data[ind==2,]

#running rf
set.seed(4444)
rf <- randomForest(Sulfide~., data=train, #here Sulfide is the dependent variable and the rest of the variables are independent variables
                   ntree = 300, #number of trees will depend on the error plot
                   importance = TRUE, #evaluates importance of a predictor
                   proximity = TRUE) #calculates the proximity measure among the rows


#plotting error-shows the error with growing number of trees in a forest

plot(rf)

#saving the parameters used in running RF

sink(paste0("RF_parameter",x,y,z,".txt"))
rf
sink()

#predicting the dependent variable in the training set using generated RF model

p1 <- predict(rf, train)
write.csv(p1,paste0("training",x,y,z,".csv"))


#predicting the dependent variable in the validation set using generated RF model

p2 <- predict(rf, test)
write.csv(p2,paste0("validation",x,y,z,".csv"))

################For training accuracy######################
data <- read.csv(paste0("RF_input",x,y,z,".csv"), header=TRUE)

df2 <- data %>%  select(1, Sulfide)
colnames(df2) <- c("SampleID", "Actual_value")

#for training accuracy

tr <- read.csv(paste0("training",x,y,z,".csv"))

#Changing the column names 
colnames(tr) <- c("SampleID", "Predicted_value")

total1 <- merge(tr,df2,by="SampleID")
write.csv(total1,"predicted_vs_actual_in_training.csv", row.names = FALSE)

#plotting predicted vs actual value
pdf(paste0("Training_plot",x,y,z,".pdf"),8,4)
ggplot(total1, aes(y=Actual_value, x=Predicted_value)) +  geom_point() + geom_smooth(method=lm)+
  ggtitle(paste0("Model RM",x,y,z," training plot")) +
  xlab("Predicted sulfide concentration") + ylab("Actual sulfide concentration") 
dev.off()

#running linear model for training
fit <- lm(Actual_value ~ Predicted_value, data = total1)

sink(paste0("training",x,y,z,"_accuracy.txt"))
print(summary(fit))
sink()

###############for validation accuracy##############

pr <- read.csv(paste0("validation",x,y,z,".csv"))

#Changing the column names 
colnames(pr) <- c("SampleID", "Predicted_value")

total2 <- merge(pr,df2,by="SampleID")
write.csv(total2,"predicted_vs_actual_in_validation.csv", row.names = FALSE)

#plotting predicted vs actual value

pdf(paste0("Validation_plot",x,y,z,".pdf"),8,4)
ggplot(total2, aes(y=Actual_value, x=Predicted_value)) +  geom_point() + geom_smooth(method=lm)+  
  ggtitle(paste0("Model RM",x,y,z," validation plot")) +
  xlab("Predicted sulfide concentration ") + ylab("Actual sulfide concentration ") 
dev.off()

#running linear model for validation
fit <- lm(Actual_value ~ Predicted_value, data = total2)

sink(paste0("validation",x,y,z,"_accuracy.txt"))
print(summary(fit))
sink()

######################variable importance########################
#plotting top 10 variable which are important for decision making in decision making tree

varImpPlot(rf,
           sort = T,
           n.var = 10,
           main = "Top 10 - Variable Importance")

#writing all the variable importance
write.csv(importance(rf), paste0("variable_importance_",x,y,z,".csv"))
