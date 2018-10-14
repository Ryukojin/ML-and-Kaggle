#Importing of relevant ML libraries
library(rpart) 
library(randomForest)
library(caret)

# Set to ensure reproducable results
set.seed(111)
train = read.csv('ks-train.csv', stringsAsFactors = FALSE)
test = read.csv('ks-test.csv', stringsAsFactors = FALSE)

# Removing rows with other unwanted labels such as canceled, suspended and live
train = train[-which(train$status == "canceled"), ]
train = train[-which(train$status == "suspended"), ]
train = train[-which(train$status == "live"), ]

# Encoding variables into factors
train$category = as.factor(train$category)
train$subcategory = as.factor(train$subcategory)
train$levels = as.factor(train$levels)
train$status = as.factor(train$status)

test$category = as.factor(test$category)
test$subcategory = as.factor(test$subcategory)
test$levels = as.factor(test$levels)

# Dropping unwanted variables from both datasets
myvars <- names(train) %in% c("project.id", "name", "url", "location", "funded.date","reward.levels") 
train <- train[!myvars]
myvars <- names(test) %in% c("name", "url", "location", "funded.date","reward.levels") 
test <- test[!myvars]

# Inspecting data before building the model
summary(train)
str(train)
table(train$status)

# Finding variable importance by taking useable variables
fit = rpart(status ~., data = train, method = "class") 
varImp(fit)

# Building the model with important variables and plotting it
fitted_forest = randomForest(status ~ pledged + funded.percentage + backers + updates + comments, data = train, importance = TRUE, ntree = 1000) 
varImpPlot(fitted_forest, type = 2)

# Predicting on test data
my_predictions = predict(fitted_forest, test)
View(my_predictions)

# Creating the final scored predictions dataset ready for submission
results = data.frame(test$project.id, my_predictions)

# Naming the columns according to the requirements
colnames(results) <- c("project id", "status")
View(results)

# Finally writing the predicted scores to output csv file
write.csv(results, file="score.csv", row.names = FALSE)
#====================================================================================================
