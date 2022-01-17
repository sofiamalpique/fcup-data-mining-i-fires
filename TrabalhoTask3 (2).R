library(tidyverse)
library(caret)
library(naivebayes)

fires_test <- read_csv("fires_test.csv", na=c("-","NA"))
fires_test <- fires_test %>% select(-c(id,region,municipality,parish,alert_source,lat,
                                 lon,firstInterv_date,firstInterv_hour,village_veget_area,
                                 total_area))
#fires_test <- fires_test %>% filter(origin!="false_alarm")
fires_test <- fires_test %>% mutate(alert_date=as.Date(alert_date),
                              extinction_date=as.Date(extinction_date))
#fires_test <- fires_test %>% drop_na()
fires_test <- fires_test %>% mutate(date=alert_date)
test_data <- merge(temp_data,fires_test,c("district","date"), all.y = TRUE) %>%
  as.tibble()
help <- test_data %>% subset(is.na(tmax)) %>% 
  select(district,date,tmax)
temp_data <- temp_data %>% add_row(help) %>% 
  arrange(district,date) %>% 
  fill(tmax) %>%
  unique()
test_data <- test_data %>% select(-c(tmax))
test_data <- merge(temp_data,test_data,c("district","date"))
test_data <- as.tibble(test_data)
test_data <- test_data %>% rename(season=date)
test_data$season <- getSeason(test_data$season)
test_data <- test_data %>% mutate(season = as.factor(season))

test_data %>% find_na(index=FALSE)
test_data %>% filter(!complete.cases(.))


train_norm <- final_data
test_norm <- test_data



normalize <- function(x) {
  return ((x - min(x,na.rm=TRUE)) / (max(x,na.rm=TRUE) - min(x,na.rm=TRUE)))
}


train_norm <- train_norm %>% 
  mutate(alert_hour=gsub("[: -]", "" , alert_hour),
         extinction_hour=gsub("[: -]", "" , extinction_hour),
         alert_date=gsub("[: -]", "" , alert_date),
         extinction_date=gsub("[: -]", "" , extinction_date)) %>% 
  mutate(alert_hour=as.numeric(alert_hour),
         extinction_hour=as.numeric(extinction_hour),
         alert_date=as.numeric(alert_date),
         extinction_date=as.numeric(extinction_date)) %>%
  mutate(tmax=normalize(tmax),
         farming_area=normalize(farming_area),
         village_area=normalize(village_area),
         vegetation_area=normalize(vegetation_area),
         alert_hour=normalize(alert_hour),
         alert_date=normalize(alert_date),
         extinction_date=normalize(extinction_date),
         extinction_hour=normalize(extinction_hour)) %>% 
  mutate(firepit=ifelse(origin=="firepit",1,0),
         fire=ifelse(origin=="fire",1,0),
         agriculture=ifelse(origin=="agriculture",1,0),
         agric_burn=ifelse(origin=="agric_burn",1,0),
         false_alarm=ifelse(origin=="false_alarm",1,0)) %>% 
  mutate(winter=ifelse(season=="Winter",1,0),
         spring=ifelse(season=="Spring",1,0),
         sommer=ifelse(season=="Sommer",1,0),
         fall=ifelse(season=="Fall",1,0)) %>%
  select(-c(origin,season))


test_norm <- test_norm %>% 
  mutate(alert_hour=gsub("[: -]", "" , alert_hour),
         extinction_hour=gsub("[: -]", "" , extinction_hour),
         alert_date=gsub("[: -]", "" , alert_date),
         extinction_date=gsub("[: -]", "" , extinction_date)) %>% 
  mutate(alert_hour=as.numeric(alert_hour),
         extinction_hour=as.numeric(extinction_hour),
         alert_date=as.numeric(alert_date),
         extinction_date=as.numeric(extinction_date)) %>%
  mutate(tmax=normalize(tmax),
         farming_area=normalize(farming_area),
         village_area=normalize(village_area),
         vegetation_area=normalize(vegetation_area),
         alert_hour=normalize(alert_hour),
         alert_date=normalize(alert_date),
         extinction_hour=normalize(extinction_hour),
         extinction_date=normalize(extinction_date)) %>% 
  mutate(firepit=ifelse(origin=="firepit",1,0),
         fire=ifelse(origin=="fire",1,0),
         agriculture=ifelse(origin=="agriculture",1,0),
         agric_burn=ifelse(origin=="agric_burn",1,0),
         false_alarm=ifelse(origin=="false_alarm",1,0)) %>% 
  mutate(winter=ifelse(season=="Winter",1,0),
         spring=ifelse(season=="Spring",1,0),
         sommer=ifelse(season=="Sommer",1,0),
         fall=ifelse(season=="Fall",1,0)) %>%
  select(-c(origin,season))


test_norm <- test_norm %>% select(-c(district,extinction_date,extinction_hour))
train_norm <- train_norm %>% select(-c(district,extinction_date,extinction_hour))


inTrain_norm <- createDataPartition(y = train_norm$intentional_cause, p = 0.7, list = FALSE) 
train70_norm <- train_norm %>% slice(inTrain_norm) 
test30_norm <- train_norm %>% slice(-inTrain_norm)
test30_causes <- test30_norm$intentional_cause 
test30_norm <- test30_norm %>% select(-intentional_cause)

knn3_model_70.30 <- knn3(intentional_cause ~ .,data=train70_norm, k=3)
knn3_preds_70.30 <- predict(knn3_model_70.30, test30_norm, type = "class")
knn3_confM_70.30 <- confusionMatrix(test30_causes, knn3_preds_70.30)
knn3_confM_70.30

knn5_model_70.30 <- knn3(intentional_cause ~ .,data=train70_norm, k=5)
knn5_preds_70.30 <- predict(knn5_model_70.30, test30_norm, type = "class")
knn5_confM_70.30 <- confusionMatrix(test30_causes, knn5_preds_70.30)
knn5_confM_70.30

knn7_model_70.30 <- knn3(intentional_cause ~ .,data=train70_norm, k=7)
knn7_preds_70.30 <- predict(knn7_model_70.30, test30_norm, type = "class")
knn7_confM_70.30 <- confusionMatrix(test30_causes, knn7_preds_70.30)
knn7_confM_70.30

nb_model_70.30 <- naive_bayes(intentional_cause ~ .,data=train70_norm)
nb_preds_70.30 <- predict(nb_model_70.30, test30_norm)
nb_confM_70.30 <- confusionMatrix(test30_causes, nb_preds_70.30)
nb_confM_70.30

nbLaP_model_70.30 <- naive_bayes(intentional_cause ~ .,data=train70_norm,laplace = 1)
nbLaP_preds_70.30 <- predict(nbLaP_model_70.30, test30_norm)
nbLaP_confM_70.30 <- confusionMatrix(test30_causes, nbLaP_preds_70.30)
nbLaP_confM_70.30




knn3.model <- knn3(intentional_cause ~ .,data=train_norm, k=3)
knn3.preds <- predict(knn3.model, test_norm, type = "class")

knn5.model <- knn3(intentional_cause ~ .,data=train_norm, k=5)
knn5.preds <- predict(knn5.model, test_norm, type = "class")

knn7.model <- knn3(intentional_cause ~ .,data=train_norm, k=7)
knn7.preds <- predict(knn7.model, test_norm, type = "class")

nb.model <- naive_bayes(intentional_cause ~ .,data=train_norm)
nb.preds <- predict(nb.model, test_norm)

nbLaP.model <- naive_bayes(intentional_cause ~ .,data=train_norm,laplace = 1)
nbLaP.preds <- predict(nb.model, test_norm)



inTrain <- createDataPartition(y = final_data$intentional_cause, p = 0.7, list = FALSE) 
train70 <- final_data %>% slice(inTrain) 
test30 <- final_data %>% slice(-inTrain)
test30_causes2 <- test30$intentional_cause 
test30 <- test30 %>% select(-intentional_cause)

test30 <- test30 %>% select(-c(district,extinction_date,extinction_hour))
train70 <- train70 %>% select(-c(district,extinction_date,extinction_hour))



train_lm <- train70 %>% mutate(intentional_cause=as.numeric(intentional_cause)-1)
lm_70.30 <- lm(intentional_cause ~ ., train_lm) 
lm_70.30
summary(lm_70.30)

predslm <- predict(lm_70.30, test30) 
lm_confM_70.30 <- confusionMatrix(test30_causes2, predslm)
lm_confM_70.30
RMSE(predslm, as.numeric(test30_causes2)-1)
R2(predslm, test30_causes2)
ggplot(data.frame(x = as.numeric(test30_causes2)-1, y = predslm), aes(x = x, y = y)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x) + ggtitle("Linear Regression: True vs Predicted Values ") + xlab("True Values") + ylab("Predicted Values")

x <- as.matrix(train70_norm %>% select(-intentional_cause))
y <- train_lm$intentional_cause
# ridge regression alpha=1 is the lasso penalty, and alpha=0 the ridge penalty
lambda_seq <- 10^seq(2, -2, by = -0.5)
lmRidge_70.30 <- glmnet(x, y, alpha = 0, lambda = lambda_seq) 
lmRidge_70.30
coef(lmRidge_70.30)
lmRidge_70.30$lambda

predsRidge <- predict(lmRidge_70.30, as.matrix(test30_norm), s = 0)
RMSE(predsRidge, as.numeric(test30_causes2)-1) 
R2(predsRidge, as.numeric(test30_causes2)-1) 
predsRidge <- predict(lmRidge_70.30, as.matrix(test30_norm), s = 8)
RMSE(predsRidge, as.numeric(test30_causes2)-1) 
R2(predsRidge, as.numeric(test30_causes2)-1) 
ggplot(data.frame(x = as.numeric(test30_causes2)-1, y = predsRidge[, 1]), aes(x = x, y = y)) +
  geom_point() + geom_smooth(method = "lm", formula = y ~ x) + ggtitle("Ridge Linear Regression: True vs Predicted Values ") + xlab("True Values") + ylab("Predicted Values")
lmlasso_70.30 <- glmnet(x, y, alpha = 1, lambda = lambda_seq) 
lmlasso_70.30
coef(lmlasso_70.30)
lmlasso_70.30$lambda

predsLasso <- predict(lmlasso_70.30, as.matrix(test30_norm), s = 8)
RMSE(predsLasso, as.numeric(test30_causes2)-1) 
R2(predsLasso, as.numeric(test30_causes2)-1) 
ggplot(data.frame(x = as.numeric(test30_causes2)-1, y = predsLasso[, 1]), aes(x = x, y = y)) +
  geom_point() + geom_smooth(method = "lm", formula = y ~ x) + ggtitle("Lasso Linear Regression: True vs Predicted Values ") + xlab("True Values") + ylab("Predicted Values")
plot(lmRidge_70.30, xvar = "lambda")
plot(lmlasso_70.30, xvar = "lambda")





tree_70.30 <- rpart(intentional_cause ~ ., train70) 
rpart.plot(tree_70.30)
predsTree <- predict(tree_70.30, test30, type = "class") 
tree_confM_70.30 <- confusionMatrix(test30_causes2, predsTree)
tree_confM_70.30





