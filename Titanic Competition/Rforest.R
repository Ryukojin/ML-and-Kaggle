library(rpart)

train = read.csv("train.csv", stringsAsFactors = F)
test = read.csv("test.csv", stringsAsFactors = F)
#Combine
all_data <- rbind(train,test)

#Replace missing embarks with mode
all_data$Embarked[c(62, 830)] <- "S"
# Factorize embarkment codes
all_data$Embarked <- factor(all_data$Embarked)
# Passenger on row 1044 has an NA Fare value. Let's replace it with the median fare value.
all_data$Fare[1044] <- median(all_data$Fare, na.rm = TRUE)

# How to fill in missing Age values?
# We make a prediction of a passengers Age using the other variables and a decision tree model.
# This time you give method = "anova" since you are predicting a continuous variable.
predicted_age <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + family_size, data = all_data[!is.na(all_data$Age),], method = "anova")
all_data$Age[is.na(all_data$Age)] <- predict(predicted_age, all_data[is.na(all_data$Age),])

# Split the data back into a train set and a test set
train <- all_data[1:891,]
test <- all_data[892:1309,]

str(train)
str(test)
set.seed(111)

# Apply the Random Forest Algorithm
my_forest <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title,
                          data = train, 
                          importance = TRUE, 
                          ntree = 1000)
#plot importance (set importance to true). the accuracy plot shows how much worse the model would perform without the included variables. The second plot is the Gini coefficient. The higher the variable scores here, the more important it is for the model.
varImpPlot(my_forest)
# Make your prediction using the test set
my_prediction <- predict(my_forest, test)
# Create a data frame with two columns: PassengerId & Survived. Survived contains your predictions
my_solution <- data.frame(PassengerId = test$PassengerId, Survived = my_prediction)
# Write your solution away to a csv file with the name my_solution.csv
write.csv(my_solution, file = "my_solution.csv", row.names = FALSE)

#Submission Score (.79245)
