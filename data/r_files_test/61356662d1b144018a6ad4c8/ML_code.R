#Machine learning code

library(DescTools)
library(caret)

set.seed(412)

###Representative code for one study outcome (process will be repeated for each outcome)

#Randomly select 50% of data and create training and test datasets
indices<-createDataPartition(data$outcome1, p=.5, list = F)
train_data<-data[indices,]
test_data<-data[-indices,]

#Main effects model
predict_model<-train(outcome1~X1+X2, data=data, method="lm")

#Interaction model
predict_model_int<-train(outcome1~X1*X2, data=data, method="lm")

#Evaluate prediction accuracy of main effect and interaction models
predictions<-predict.lm(predict_model, test_data)
predictions_int<-predict.lm(predict_model_int, test_data)

r.main<-cor(predictions, test_data$outcome1)
r.int<-cor(predictions_int, test_data$outcome1)

q<-FisherZ(r.int)-FisherZ(r.main) #Cohen's q 

#Mean absolute error (MAE) and Root mean square error (RMSE)
MAE(predictions, test_data$outcome1)
MAE(predictions_int, test_data$outcome1) 

RMSE(predictions, test_data$outcome1)
RMSE(predictions_int, test_data$outcome1)