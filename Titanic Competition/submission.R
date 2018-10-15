library(randomForest)
library(rpart)
train <- read.csv("train.csv")
test <- read.csv("test.csv")

# Join together the test and train sets for easier feature engineering
test$Survived <- NA
combi <- rbind(train, test)
str(combi)

#                                       FEATURE ENGINEERING
# Convert name to a string
combi$Name <- as.character(combi$Name)
# Engineered variable: Title
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title)

# Combine small title groups
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
# Convert to TITLE to factor
combi$Title <- factor(combi$Title)

# Engineered variable: Family size
combi$FamilySize <- combi$SibSp + combi$Parch + 1


#                                       REMOVE MISSING VALUES
summary(combi$Age)
# How to fill in missing Age values?
# We make a prediction of a passengers Age using the other variables and a decision tree model.
# This time you give method = "anova" since you are predicting a continuous variable.
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize, 
                data=combi[!is.na(combi$Age),], method="anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])

# Fill in Embarked
summary(combi$Embarked)
which(combi$Embarked == '')
combi$Embarked[c(62,830)] = "S"
combi$Embarked <- factor(combi$Embarked)

# Fill in Fare NAs
summary(combi$Fare)
which(is.na(combi$Fare))
combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE)

# Split back into test and train sets
train <- combi[1:891,]
test <- combi[892:1309,]

#                                       TRAIN MODEL
# Build Random Forest Ensemble
set.seed(415)
forest <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                    data=train, 
                    importance=TRUE, 
                    ntree=1000)
# Look at variable importance
varImpPlot(forest)
# Now let's make a prediction and write a submission file
Prediction <- predict(forest, test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "nuforest.csv", row.names = FALSE)

#Final Submission score of 0.78468
