#Including the Libraries
library(forecast)
library(fpp)
library(smooth)
library(readxl)
library(rmarkdown)

#Loading the dataset to which forecasting is to be done
plastics <- read.csv(file.choose())
View(plastics)
windows()
plot(plastics$Sales,type = "o")

X<-data.frame(outer(rep(month.abb,length=60),month.abb,"==") + 0 )
View(X)
colnames(X)<-month.abb
View(X)
pl_data <- cbind(plastics,X)
View(pl_data)
colnames(plastics)


pl_data["t"]<-1:60
View(pl_data)
pl_data["log_Sales"]<-log(pl_data["Sales"])
pl_data["t_sq"]<-pl_data["t"]*pl_data["t"]
attach(pl_data)

#Creating the necassary train and test data sets.
train<-pl_data[1:48,]
test<-pl_data[49:60,]

########################### LINEAR MODEL #############################
linear_mod <- lm(Sales~t,data=train)
summary(linear_mod)

linear_pred<-data.frame(predict(linear_mod,interval = "predict",newdata = test))
View(linear_pred)
rmse_linear<-sqrt(mean((test$Sales-linear_pred$fit)^2,na.rm = T))
rmse_linear #260.9378 and Adjusted R2 Value = 31.59

######################### Exponential #################################
exp_mod <- lm(log_Sales~t,data=train)
summary(exp_mod)

exp_pred<-data.frame(predict(exp_mod,interval = "predict",newdata = test))
View(exp_pred)
rmse_exp<-sqrt(mean((test$Sales-exp_pred$fit)^2,na.rm = T))
rmse_exp

######################### Quadratic ####################################
Quad_model<-lm(Sales~t+t_sq,data=train)
summary(Quad_model)

Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_Quad<-sqrt(mean((test$Sales-Quad_pred$fit)^2,na.rm=T))
rmse_Quad # 297.4067 and Adjusted R2 - 30.48%

######################### Additive Seasonality #########################
sea_add_model<-lm(Sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data=train)
summary(sea_add_model)

sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$Sales-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add # 235.6027 and Adjusted R2 Value = 69.85

######################## Additive Seasonality with Linear #################
Add_sea_Linear_model<-lm(Sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data=train)
summary(Add_sea_Linear_model)

Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test))
rmse_Add_sea_Linear<-sqrt(mean((test$Sales-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear # 135.5536 and Adjusted R2 - 96.45%

######################## Additive Seasonality with Quadratic #################
Add_sea_Quad_model<-lm(Sales~t+t_sq+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data=train)
summary(Add_sea_Quad_model)

Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$Sales-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad # 218.1939 and Adjusted R2 - 97.68 %

######################## Multiplicative Seasonality #########################
multi_sea_model<-lm(log_Sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data = train)
summary(multi_sea_model)

multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$Sales-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea # 239.6543
######################## Multiplicative Seasonality Linear trend ##########################
multi_add_sea_model<-lm(log_Sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data = train)
summary(multi_add_sea_model) 

multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((test$Sales-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea # 160.6833 and Adjusted R2 - 97.51%

table_rmse<-data.frame(c("rmse_linear","rmse_exp","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),c(rmse_linear,rmse_exp,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

# Multiplicative Seasonality Linear trend  has least RMSE value
new_model<-lm(log_Sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data = pl_data)
new_model_pred<-data.frame(predict(new_model,newdata=pl_data,interval='predict'))

new_model_fin <- exp(new_model$fitted.values)
View(new_model_fin)
Month <- as.data.frame(pl_data$Month)
Final <- as.data.frame(cbind(Month,pl_data$Sales, new_model_fin))
colnames(Final) <-c("Month","Sales","New_Pred_Value")
plot(Final$Sales,main = "ActualGraph", xlab="Sales(Actual)", ylab="Months",
     col.axis="blue",type="o") 
plot(Final$New_Pred_Value, main = "PredictedGraph", xlab="Sales(Predicted)", ylab="Months",
     col.axis="Green",type="s")
View(Final)
