library(readxl)

covid <- read_excel(file.choose()) # read the Covid-19 data

attach(covid)
train <- covid[1:53, ]
test <- covid[54:75, ]

#Linear Model

linear_model <- lm(Confirmed ~ T, data = train)
summary(linear_model)
linear_pred <- data.frame(predict(linear_model, interval = 'predict', newdata = test))
mape_linear <- mean(abs(test$Confirmed - linear_pred$fit)/(test$Confirmed))
mape_linear

#Exponential Model

expo_model <- lm(LogConfirmed ~ T, data = train)
summary(expo_model)
expo_pred <- data.frame(predict(expo_model, interval = 'predict', newdata = test))
mape_expo <- mean(abs(test$Confirmed - exp(expo_pred$fit))/(test$Confirmed))
mape_expo

#Quadratic Model

quad_model <- lm(Confirmed ~ T + T2, data = train)
summary(quad_model)
quad_pred <- data.frame(predict(quad_model, interval = 'predict', newdata = test))
mape_quad <- mean(abs(test$Confirmed - quad_pred$fit)/(test$Confirmed))
mape_quad

#Linear Trend + Daily seasonality

lintrendsea <- lm(Confirmed ~ T + Sin + Cos, data = train)
summary(lintrendsea)
lintrendsea_pred <- data.frame(predict(lintrendsea, newdata=test, interval = 'predict'))
mape_lintrendsea <- mean(abs(test$Confirmed - lintrendsea_pred$fit)/(test$Confirmed))
mape_lintrendsea

#Expotential Trend + Daily seasonality

exptrendsea <- lm(LogConfirmed ~ T + Sin + Cos, data = train)
summary(exptrendsea)
exptrendsea_pred <- data.frame(predict(exptrendsea, newdata=test, interval = 'predict'))
mape_exptrendsea <- mean(abs(test$Confirmed - exp(exptrendsea_pred$fit))/(test$Confirmed))
mape_exptrendsea

#Linear Trend + Daily seasonality + Lag

lintrendsealag <- lm(Confirmed ~ T + LagConfirmed + Sin + Cos, data = train)
summary(lintrendsealag)
lintrendsealag_pred <- data.frame(predict(lintrendsealag, newdata=test, interval = 'predict'))
mape_lintrendsealag <- mean((abs(test$Confirmed - lintrendsealag_pred$fit))/(test$Confirmed))
mape_lintrendsealag


#Preparing model table with RMSE
table_mape <- data.frame(c("mape_linear", "mape_expo", "mape_quad", "mape_lintrendsea", "mape_exptrendsea", "mape_lintrendsealag"), c(mape_linear, mape_expo, mape_quad, mape_lintrendsea, mape_exptrendsea, mape_lintrendsealag))
colnames(table_mape) <- c("model", "RMSE")
View(table_mape)


#Combining Train & Test data to build Additive seasonality using Quadratic Trend

finalmodel <- lm(Confirmed ~ T + LagConfirmed + Sin + Cos, data = covid)
confint(finalmodel, level = 0.95)
summary(finalmodel)


#Predicting new data
new_data <- read_excel(file.choose())
View(new_data)
pred_new <- predict(finalmodel, newdata = new_data, interval = 'predict')
pred_new
