library(tidyverse)
library(caret)
library(naivebayes)

test_ds <- read_csv("fires_test.csv", na=c("-","NA"))
test_ds <- test_ds %>% select(-c(id,region,municipality,parish,alert_source,lat,lon,))
test_ds <- test_ds %>% filter(origin!="false_alarm")
test_ds <- test_ds %>% mutate(alert_date=as.Date(alert_date),extinction_date=as.Date(extinction_date),firstInterv_date=as.Date(firstInterv_date))
test_ds <- test_ds %>% mutate(alert_time=as.POSIXct(paste(test_ds$alert_date, test_ds$alert_hour), format="%Y-%m-%d %H:%M:%S"))
test_ds <- test_ds %>% select(-c(alert_date,alert_hour))
test_ds <- test_ds %>% mutate(extinction_time=as.POSIXct(paste(test_ds$extinction_date, test_ds$extinction_hour), format="%Y-%m-%d %H:%M:%S"))
test_ds <- test_ds %>% select(-c(extinction_date,extinction_hour))
test_ds <- test_ds %>% mutate(firstInterv_time=as.POSIXct(paste(test_ds$firstInterv_date, test_ds$firstInterv_hour), format="%Y-%m-%d %H:%M:%S"))
test_ds <- test_ds %>% select(-c(firstInterv_date,firstInterv_hour))
test_ds <- test_ds %>% relocate(c(district, alert_time, extinction_time, firstInterv_time ,origin, village_area, vegetation_area, farming_area, village_veget_area, total_area))
test_ds <- test_ds %>% select(-c(firstInterv_time))
test_ds <- test_ds %>% drop_na()
test_ds <- test_ds %>% mutate(date=as.Date(alert_time))
test_ds <- merge(novo,test_ds,c("district","date"), all.y = TRUE)
test_ds <- as.tibble(test_ds)
help <- test_ds %>% subset(is.na(tmax))
help <- help %>% select(district,date,tmax)
novo <- novo %>% add_row(help)
novo <- novo %>% arrange(district,date)
novo <- novo %>% fill(tmax)
novo <- unique(novo)
test_ds <- test_ds %>% select(-c(tmax))
test_ds <- merge(novo,test_ds,c("district","date"))
test_ds <- as.tibble(test_ds)
test_ds <- test_ds %>% select(-c(date))



train <- final %>% select(-c(district,village_area,vegetation_area))
test <- test_ds %>% select(-c(district,village_area,vegetation_area))

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

train$tmax <- normalize(train$tmax)
train$farming_area <- normalize(train$farming_area)
train$village_veget_area <- normalize(train$village_veget_area)
train$total_area <- normalize(train$total_area)

test$tmax <- normalize(test$tmax)
test$farming_area <- normalize(test$farming_area)
test$village_veget_area <- normalize(test$village_veget_area)
test$total_area <- normalize(test$total_area)

train <- train %>% mutate(alert_time=gsub("[: -]", "" , as.ITime(alert_time)))
train <- train %>% mutate(extinction_time=gsub("[: -]", "" , as.ITime(extinction_time)))
test <- test %>% mutate(alert_time=gsub("[: -]", "" , as.ITime(alert_time)))
test <- test %>% mutate(extinction_time=gsub("[: -]", "" , as.ITime(extinction_time)))
test <- test %>% mutate(alert_time=as.numeric(alert_time),extinction_time=as.numeric(extinction_time))
test$alert_time <- normalize(test$alert_time)
test$extinction_time <- normalize(test$extinction_time)
train <- train %>% mutate(alert_time=as.numeric(alert_time),extinction_time=as.numeric(extinction_time))
train$alert_time <- normalize(train$alert_time)
train$extinction_time <- normalize(train$extinction_time)


help <- train %>% select(origin)
unique(help)
train <- train %>% mutate(firepit=ifelse(origin=="firepit",1,0))
train <- train %>% mutate(fire=ifelse(origin=="fire",1,0))
train <- train %>% mutate(agriculture=ifelse(origin=="agriculture",1,0))
train <- train %>% mutate(agric_burn=ifelse(origin=="agric_burn",1,0))
train <- train %>% select(-origin)
test <- test %>% mutate(firepit=ifelse(origin=="firepit",1,0))
test <- test %>% mutate(fire=ifelse(origin=="fire",1,0))
test <- test %>% mutate(agriculture=ifelse(origin=="agriculture",1,0))
test <- test %>% mutate(agric_burn=ifelse(origin=="agric_burn",1,0))
test <- test %>% select(-origin)


knn3.model_fire <- knn3(intentional_cause ~ .,data=train, k=3)
knn3.preds_fire <- predict(knn3.model_fire, test, type = "class")

knn5.model_fire <- knn3(intentional_cause ~ .,data=train, k=5)
knn5.preds_fire <- predict(knn5.model_fire, test, type = "class")

knn7.model_fire <- knn3(intentional_cause ~ .,data=train, k=7)
knn7.preds_fire <- predict(knn7.model_fire, test, type = "class")

nb.model <- naive_bayes(intentional_cause ~ .,data=train)
nb.preds <- predict(nb.model, test)

nbLaP.model <- naive_bayes(intentional_cause ~ .,data=train,laplace = 1)
nbLaP.preds <- predict(nb.model, test)







