library(readr)
titanic=read_csv('https://raw.githubusercontent.com/datasciencedojo/datasets/refs/heads/master/titanic.csv')

library(dplyr)
titanic_data <- titanic %>% select(-c(Cabin, PassengerId, Ticket, Name,Embarked))
## Converting "Survived","Pclass","Sex" to factors

titanic_data$Pclass=as.factor(titanic_data$Pclass)
titanic_data$Survived=as.factor(titanic_data$Survived)
titanic_data$Sex=as.factor(titanic_data$Sex)


titanic_data=na.omit(titanic_data)

#train:30% de los datos
train <- titanic_data[1:214,]
test <- titanic_data[215:714,]

## Model Creation
model <- glm(Survived ~.,family=binomial(link='logit'),data=train)

## Model Summary
summary(model)


## Using anova() to analyze the table of devaiance
#anova(model, test="Chisq")

## Predicting Test Data
result <- predict(model,newdata=test,type='response')
result <- as.factor(ifelse(result > 0.8,1,0))


## Confusion matrix and statistics
library(caret)
confusionMatrix(data=result, reference=test$Survived,
                positive = "1")


# ROC Curve and calculating the area under the curve(AUC)
library(ROCR)
predictions <- predict(model, newdata=test, type="response")
ROCRpred <- prediction(predictions, test$Survived)
ROCRperf <- performance(ROCRpred, measure = "tpr", x.measure = "fpr")

plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7), print.cutoffs.at = seq(0,1,0.1))
auc <- performance(ROCRpred, measure = "auc")
auc <- auc@y.values[[1]]
auc


