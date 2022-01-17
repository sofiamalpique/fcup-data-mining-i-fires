library(tidyverse)
library(dplyr)
library(dlookr)
library(rnoaa)
library(lubridate)
require(devtools)
library(zoo)
library(data.table)
library(ggplot2)
library(cluster)
library(fpc)
library(factoextra)
library(caret)
library(naivebayes)
library(rpart)
library(rpart.plot)
library(e1071)
library(performanceEstimation)
library(adabag)
library(randomForest)
#library(xgboost)



#-------------------------------------------------------------------------------
#---------Data importing, cleaning, pre-processing------------------------------
#-------------------------------------------------------------------------------

# Read the data.
fires_train <- read_csv("fires_train.csv", na=c("-","NA"))

# understanding  and analysie of the data.
fires_train
summary(fires_train)
spec(fires_train)
fires_train %>% find_na(index=FALSE)
fires_train %>% select(find_na(.)) %>% diagnose()
unique(select(fires_train,district))
# We have a duplicate value: "Viana do Castelo" and "Viana Do Castelo".
# Given that "intentional_cause" is, in fact, a Boolean, it must be changed.
# Change date information into date format

fires_train <- fires_train %>% 
  select(-c(id,region,municipality,parish,alert_source,lat,lon,
            village_veget_area,firstInterv_date,firstInterv_hour,total_area))%>% 
  mutate(district=replace(district,
                          district=="Viana Do Castelo","Viana do Castelo")) %>% 
  mutate(intentional_cause=as.factor(intentional_cause)) %>% 
  mutate(alert_date=as.Date(alert_date),extinction_date=as.Date(extinction_date))

# Second diagnose of NA.
fires_train %>% find_na(index=FALSE)
fires_train %>% select(find_na(.)) %>% diagnose()
#fires_train %>% drop_na()

# So in order to merge another data set ("temp_data") with this one, and create a new
# one - called "fires_train" - we needed to have something in common, which, in this 
# case, is the district and the date of occurrence.

# So we only have one question now: where did "temp_data" come from? What consists 
# the "temp_data" data set?




#-------------------------------------------------------------------------------
#------------Collect the data with the temperature for each district------------
#-------------------------------------------------------------------------------

# First load "station_data.Rdata".
load("station_data.Rdata")


# We are only interested in he main-land of Portugal, that's why Azores and 
# Madeira are not accounted. So we restrained to the coordinates of Portugal.

# Also, we have element == "TMAX", because we are interested in using the 
# maximum temperature registered on that day. We believe it influences the risk 
# of natural fires, so it might help deciding the intentional cause.

# Now we select "id" and "name" because we want to check if there is any name 
# duplicated.

# We decided to remove "COIMBRA/CERNACHE" and "TAVIRA" because we are crossing 
# districts and dates.
# "COIMBRA/CERNACHE" was removed because it already has "COIMBRA"- which is a 
# district - and "TAVIRA" was removed because it isn't a district.
station_data <- filter(station_data,str_starts(id,"PO"), latitude < 42.2,
                       latitude > 36.8, longitude > -9.6,longitude < -6.1,
                       element=="TMAX") %>% 
  select(id,name) %>% 
  filter(name!="COIMBRA/CERNACHE",name!="TAVIRA")  %>% 
  filter(name!=is.na(name))


# Here we restrained the dates. We're only interested in 2014 and 2015.
# All this information is going to a new data set called "weather_data".
weather_data <- ghcnd_search(station_data$id, var = c("TMAX"), 
                             date_min = "2014-01-01", date_max = "2015-12-31")

# Here, the table is being copied to "weather_data".

# We maintain "id" so we are able to join tables later.

# We sort first by id and then by date, so we guarantee to first search by 
#district.
weather_data <- weather_data$tmax %>% 
  select(id,tmax,date) %>% 
  arrange(id,date) %>% 
  fill(tmax)


# Here we want to find out which stations in Portugal have data on tmax
merge(weather_data,station_data,"id") %>% distinct(id, .keep_all = TRUE) %>% 
  select(id,name)

# As we don't have all the Portuguese districts in "station_data" we need to 
# add/create a column called "nearest_district" containing the districts 
# available in "weather_data".
# With this we can put the values of maximum temperature to districts that don't 
# appear in "weather_data" using the values of the nearest district.

station_data <- station_data %>% mutate(nearest_district=c("Lisboa","Porto",
                                                           "Coimbra","Beja",
                                                           "Bragança","Faro",
                                                           "Évora",
                                                           "Castelo Branco")) %>%
  add_row(nearest_district="Viseu",id="PO000008575")  %>% 
  add_row(nearest_district="Aveiro",id="PO000008575") %>% 
  add_row(nearest_district="Braga",id="PO000008575")  %>% 
  add_row(nearest_district="Viana do Castelo",id="PO000008575") %>% 
  add_row(nearest_district="Vila Real",id="PO000008575") %>% 
  add_row(nearest_district="Guarda",id="POM00008570") %>% 
  add_row(nearest_district="Leiria",id="PO000008535") %>% 
  add_row(nearest_district="Santarém",id="PO000008535") %>% 
  add_row(nearest_district="Portalegre",id="POM00008558") %>% 
  add_row(nearest_district="Setúbal",id="PO000008535")


station_data$id[2] <- "PO000008575"
station_data$id[3] <- "POM00008570"
station_data$id[4] <- "POM00008558"

# "temp_data" is a new table, created by merging "station_data" and 
# "weather_data" 
temp_data <- merge(station_data,weather_data,"id",all.x = TRUE) %>% 
  as_tibble(.) %>% 
  select(nearest_district,tmax,date) %>% 
  rename(district=nearest_district) %>% 
  mutate(tmax= (tmax/10))


# "fires_train" is a table with the original and relevant data adding the 
# maximum temperature at the day of the fire.
# To merge temp_data and fires_train we rename alert_date to date so that we can 
# match it according to this column
fires_train <- fires_train %>% 
  mutate(date=alert_date) %>% 
  merge(temp_data,.,c("district","date"), all.y = TRUE) %>%
  as_tibble(.)

# Observe if there are any Na that came with the merge
fires_train %>% find_na(index=FALSE)

# Create a help dataset with all the rows where tmax is NA
help <- fires_train %>% subset(is.na(tmax)) %>% 
  select(district,date,tmax)
# add the rows to the temp_data where there are no rows with missing NA
# and order it by district and date so that we can fill the tmax that is missing
# with the previous tmax value
# drop dublicated rows
temp_data <- temp_data %>% add_row(help) %>% 
  arrange(district,date) %>% 
  fill(tmax) %>%
  unique(.)


# now temp_data has temperatures for all dates in fires train so we merge again
# the two datasets
fires_train <- fires_train %>% 
  select(-c(tmax)) %>%
  merge(temp_data,.,c("district","date")) %>%
  as_tibble(.)

# We think it might be useful to have the season as a variable. In order to do 
# that, we've created the following function, so it converts a date into the 
# corresponding season.

getSeason <- function(DATES) {
  WS <- as.Date("2012-12-21", format = "%Y-%m-%d") # Winter Solstice
  SE <- as.Date("2012-3-21",  format = "%Y-%m-%d") # Spring Equinox
  SS <- as.Date("2012-6-21",  format = "%Y-%m-%d") # Summer Solstice
  FE <- as.Date("2012-9-23",  format = "%Y-%m-%d") # Fall Equinox
  
  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(DATES, format="2012-%m-%d"))
  
  ifelse (d >= WS | d < SE, "Winter",
          ifelse (d >= SE & d < SS, "Spring",
                  ifelse (d >= SS & d < FE, "Summer", "Fall")))
}
fires_train <- fires_train %>% 
  rename(season = date) %>%
  mutate(season = getSeason(season), weekday = as.factor(weekdays(alert_date))) %>%
  mutate(weekday = ordered(weekday, levels=c("Monday","Tuesday","Wednesday",
                                             "Thursday","Friday","Saturday","Sunday")))



#-------------------------------------------------------------------------------
#-------------------------TASK 2 data exploratory analysis----------------------
#-------------------------------------------------------------------------------


summary(fires_train)

# How many intentional and non intentional fires do we have?
fires_train %>% group_by(intentional_cause) %>% count()


# What's the distribution of the fires according to the season?
ggplot(fires_train, aes(x=season,fill= intentional_cause)) + geom_bar() +
  ggtitle("Relationship between season and intentional cause")


# What's the distribution of the fires according to the weekday?
ggplot(fires_train, aes(x=weekday,fill= intentional_cause)) + geom_bar() + 
  ggtitle("Relationship between weekdays and intentional cause")


# Could there be a relationship between "intentional_cause" and the maximum 
# temperature?   É MELHOR POR EM PERCENTAGEM
ggplot(fires_train, aes(x=tmax, fill= intentional_cause)) + geom_histogram(binwidth = 2) + 
  ggtitle("Relationship between maximum temperature and intentional cause")


# Could there be a relationship between "intentional_cause" and the time it was 
# alerted?
ggplot(fires_train, aes(x=as.ITime(alert_hour), fill= intentional_cause)) + 
  geom_histogram(binwidth = 3600) + 
  scale_x_time()


# What can we say about the "origin" and the "intentional_cause"?
fires_train %>% group_by(origin,intentional_cause) %>% count() %>% 
  arrange(desc(n))

ggplot(fires_train, aes(x=intentional_cause)) + geom_bar() + facet_wrap(~origin)
# We can see in the graph that the most fires with "intentional_cause" have 
# origin "firepit".


# Relationship between "tmax", "origin" and "intentional_cause"
ggplot(fires_train, aes(x=tmax, fill= intentional_cause)) + 
  geom_histogram(binwidth = 2) + 
  facet_grid(~origin)




# What is the correlation coefficient between all numeric attributes?
fires_train %>% select(tmax,village_area, farming_area, 
                 vegetation_area) %>% cor()

# Is there any monotonic relationship?
fires_train %>% select(tmax,village_area, farming_area, 
                 vegetation_area) %>% cor(method = "spearman")


#-------------------------------------------------------------------------------
#-------------------------------Task 3------------------------------------------
#-------------------------------------------------------------------------------
litoral <- c("Viana do Castelo","Braga","Porto","Aveiro","Coimbra","Leiria",
             "Lisboa","Setúbal","Faro")

fires_train$district <- ifelse(fires_train$district %in% litoral, "litoral","interior")

time <- as.POSIXct(strptime(fires_train$alert_hour,"%H:%M:%S"),"UTC")
x=as.POSIXct(strptime(c("000000","060000","120000","180000","235959"),"%H%M%S"),
             "UTC")

labs=c("dawn","morning","afternoon","night")
fires_train$alert_hour <- labs[findInterval(time,x)]

fires_train <- fires_train %>% select(-c(alert_date,extinction_date,extinction_hour))


inTrain <- createDataPartition(y = fires_train$intentional_cause, p = 0.7, list = FALSE)
fires_train_partition <- fires_train %>% slice(inTrain)
fires_test_partition <- fires_train %>% slice(-inTrain)
fires_test_intentional_cause <- fires_test_partition$intentional_cause
fires_test <- fires_test_partition %>% select(-intentional_cause)


knn3.model <- knn3(intentional_cause ~ ., data = fires_train_partition, k = 3)
knn3.preds <- predict(knn3.model, fires_test_partition, type = "class")
knn.confM <- confusionMatrix(fires_test_intentional_cause, knn3.preds)
knn.confM

fires_train <- drop_na(fires_train)
fires_train <- fires_train %>% mutate(tmax=scale(tmax),village_area=scale(village_area),
                                      vegetation_area=scale(vegetation_area),
                                      farming_area=scale(farming_area))


fires_train6 <- fires_train %>% select(season,district,weekday,tmax,village_area,vegetation_area,
                                      farming_area,intentional_cause)



fires_train8 <- fires_train %>% 
  mutate(origin2 = ifelse(origin=="agric_burn","agricburn",ifelse(origin=="false_alarm","falsealarm",ifelse(origin=="fire","f",ifelse(origin=="firepit","fp",ifelse(origin=="agriculture","a",0))))))



fires_train8 <- fires_train8 %>% select(-origin)

fires_train10 <- fires_train %>% select(alert_hour,season,district,weekday,
                                        tmax,village_area,vegetation_area,
                                       farming_area,intentional_cause)




res <- performanceEstimation(PredTask(intentional_cause ~ ., fires_train10),
                             Workflow(learner="knn3", predictor.pars=list(type="class")), 
                             EstimationTask(metrics = c("acc","F","rec","prec")))





#-------------------------------------------------------------------------------


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


train_norm <- fires_train
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



inTrain <- createDataPartition(y = fires_train$intentional_cause, p = 0.7, list = FALSE) 
train70 <- fires_train %>% slice_(inTrain) 
test30 <- fires_train %>% slice_(-inTrain)
test30_causes2 <- test30$intentional_cause 
test30 <- test30 %>% select(-intentional_cause)

test30 <- test30 %>% select(-c(district,extinction_date,extinction_hour))
train70 <- train70 %>% select(-c(district,extinction_date,extinction_hour))



tree_70.30 <- rpart(intentional_cause ~ ., train70) 
rpart.plot(tree_70.30)
predsTree <- predict(tree_70.30, test30, type = "class") 
tree_confM_70.30 <- confusionMatrix(test30_causes2, predsTree)
tree_confM_70.30


res <- performanceEstimation(PredTask(intentional_cause ~ ., fires_train), 
                             Workflow(learner = "naiveBayes"),
                             EstimationTask(metrics = c("acc", "F", "rec", "prec")))
res
summary(res)

res <- performanceEstimation(PredTask(intentional_cause ~ ., fires_train), 
                             c(Workflow(learner = "naiveBayes"),
                               workflowVariants(learner = "rpart",
                                                learner.pars = list(maxdepth = 1:10), 
                                                predictor.pars = list(type = "class")),
                               Workflow(learner = "rpart", predictor.pars = list(type = "class"))), 
                             EstimationTask(metrics = c("acc",
                                                                                                                                                                            "F", "rec", "prec")))
plot(res)

rankWorkflows(res, max = TRUE)

pres <- pairedComparisons(res, baseline = "naiveBayes")
signifDiffs(pres)

m <- bagging(intentional_cause ~.,train70,mfinal=100)
