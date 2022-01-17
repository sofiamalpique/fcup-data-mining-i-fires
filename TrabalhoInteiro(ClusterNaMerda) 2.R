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
library(randomForest)
library(e1071)
library(nnet)
library(neuralnet)
library(caret)
library(dplyr)
library(adabag)
#library(xgboost)
library(performanceEstimation)



#-------------------------------------------------------------------------------
#---------Data importing, cleaning, pre-processing------------------------------
#-------------------------------------------------------------------------------


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

###############################################################################

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


data_preprocessing <- function(dataset){
  dataset <- dataset %>% 
    select(-c(region,municipality,parish,alert_source,lat,lon,
              village_veget_area,firstInterv_date,firstInterv_hour,total_area,
              extinction_date,extinction_hour)) %>% 
    mutate(district=replace(district,
                            district=="Viana Do Castelo","Viana do Castelo")) %>% 
    mutate(alert_date=as.Date(alert_date)) %>% 
    mutate(date=alert_date) %>% 
    merge(temp_data,.,c("district","date"), all.y = TRUE) %>%
    as_tibble(.)
  # So in order to merge another data set ("temp_data") with this one, and create a new
  # one - called "fires_train" - we needed to have something in common, which, in this 
  # case, is the district and the date of occurrence.
  # So we only have one question now: where did "temp_data" come from? What consists 
  # the "temp_data" data set?
  # "fires_train" is a table with the original and relevant data adding the 
  # maximum temperature at the day of the fire.
  # To merge temp_data and fires_train we rename alert_date to date so that we can 
  # match it according to this column
  # Observe if there are any Na that came with the merge
  dataset %>% find_na(index=FALSE)
  # Create a help dataset with all the rows where tmax is NA
  help <- dataset %>% subset(is.na(tmax)) %>% 
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
  dataset <- dataset %>% 
    select(-c(tmax)) %>%
    merge(temp_data,.,c("district","date")) %>%
    as_tibble(.) %>% 
    rename(season = date) %>%
    mutate(season = getSeason(season), weekday = as.factor(weekdays(alert_date))) %>%
    mutate(weekday = ordered(weekday, levels=c("Monday","Tuesday","Wednesday",
                                               "Thursday","Friday","Saturday","Sunday")))
  litoral <- c("Viana do Castelo","Braga","Porto","Aveiro","Coimbra","Leiria",
               "Lisboa","Setúbal","Faro")
  dataset$district <- ifelse(dataset$district %in% litoral, "litoral","interior")
  time <- as.POSIXct(strptime(dataset$alert_hour,"%H:%M:%S"),"UTC")
  x=as.POSIXct(strptime(c("000000","060000","120000","180000","235959"),"%H%M%S"),
               "UTC")
  labs=c("dawn","morning","afternoon","night")
  dataset$alert_hour <- labs[findInterval(time,x)]
  dataset <- dataset %>% select(-alert_date)
  return(dataset)
}


fires_train <- data_preprocessing(fires_train) %>% 
  mutate(intentional_cause=as.factor(intentional_cause))

# Second diagnose of NA.
fires_train %>% find_na(index=FALSE)
fires_train %>% select(find_na(.)) %>% diagnose()
#fires_train %>% drop_na()






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
ggplot(fires_train, aes(x=alert_hour, fill= intentional_cause)) + 
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


fires_test <- read_csv("fires_test.csv", na=c("-","NA"))

fires_test <- data_preprocessing(fires_test)

normalize <- function(x) {
  return ((x - min(x,na.rm=TRUE)) / (max(x,na.rm=TRUE) - min(x,na.rm=TRUE)))
}

normalize_data <- function(dataset){
  dataset <- dataset %>% 
    mutate(tmax=normalize(tmax),
           farming_area=normalize(farming_area),
           village_area=normalize(village_area),
           vegetation_area=normalize(vegetation_area)) %>% 
    mutate(firepit=ifelse(origin=="firepit",1,0),
           fire=ifelse(origin=="fire",1,0),
           agriculture=ifelse(origin=="agriculture",1,0),
           agric_burn=ifelse(origin=="agric_burn",1,0),
           false_alarm=ifelse(origin=="false_alarm",1,0)) %>% 
    mutate(winter=ifelse(season=="Winter",1,0),
           spring=ifelse(season=="Spring",1,0),
           sommer=ifelse(season=="Sommer",1,0),
           fall=ifelse(season=="Fall",1,0)) %>%
    mutate(Monday=ifelse(weekday=="Monday",1,0),
           Tuesday=ifelse(weekday=="Tuesday",1,0),
           Wednesday=ifelse(weekday=="Wednesday",1,0),
           Thursday=ifelse(weekday=="Thursday",1,0),
           Friday=ifelse(weekday=="Friday",1,0),
           Saturday=ifelse(weekday=="Saturday",1,0),
           Sunday=ifelse(weekday=="Sunday",1,0)) %>%
    mutate(district=ifelse(district=="litoral",1,0)) %>%
    mutate(dawn=ifelse(alert_hour=="dawn",1,0),
           afternoon=ifelse(alert_hour=="afternoon",1,0),
           night=ifelse(alert_hour=="night",1,0),
           morning=ifelse(alert_hour=="morning",1,0)) %>%
    select(-c(origin,season,weekday,alert_hour))
  return(dataset)
}

fires_train_norm <- normalize_data(fires_train)
fires_test_norm <- normalize_data(fires_test)
id <- fires_test %>% select(id)
fires_test_norm <- fires_test_norm %>% select(-id)
fires_train_norm <- fires_train_norm %>% select(-id)

model_testing <- function(){
  in_Model_testing <- createDataPartition(y = fires_train_norm$intentional_cause, p = 0.7, list = FALSE) 
  model_testing_train <- fires_train_norm %>% slice(in_Model_testing) 
  model_testing_test <- fires_train_norm %>% slice(-in_Model_testing)
  model_testing_causes <- model_testing_test$intentional_cause 
  model_testing_test <- model_testing_test %>% select(-intentional_cause)
  
  knn3_preds_testing <- knn3(intentional_cause ~ .,data=model_testing_train, k=3) %>% 
    predict(.,model_testing_test, type = "class")
  knn3_confM_testing <- confusionMatrix(model_testing_causes, knn3_preds_testing)
  
  knn5_preds_testing <- knn3(intentional_cause ~ .,data=model_testing_train, k=5) %>% 
    predict(., model_testing_test, type = "class")
  knn5_confM_testing <- confusionMatrix(model_testing_causes, knn5_preds_testing)
  
  knn7_preds_testing <- knn3(intentional_cause ~ .,data=model_testing_train, k=7) %>% 
    predict(., model_testing_test, type = "class")
  knn7_confM_testing <- confusionMatrix(model_testing_causes, knn7_preds_testing)
  
  nb_preds_testing <- naive_bayes(intentional_cause ~ .,data=model_testing_train) %>% 
    predict(., model_testing_test, type = "class")
  nb_confM_70.30 <- confusionMatrix(model_testing_causes, nb_preds_testing)
  
  nbLaP_preds_testing <- naive_bayes(intentional_cause ~ .,data=model_testing_train,laplace = 1) %>%
    predict(., model_testing_test, type = "class")
  nbLaP_confM_testing <- confusionMatrix(model_testing_causes, nbLaP_preds_testing)
  
  tree_preds_testing <- rpart(intentional_cause ~ ., fires_train_norm) %>% 
    predict(., model_testing_test, type = "class") 
  tree_confM_testing <- confusionMatrix(model_testing_causes, tree_preds_testing)
}

res <- performanceEstimation(PredTask(intentional_cause ~ ., fires_train), 
                             c(Workflow(learner = "naiveBayes"),   
                               workflowVariants(learner = "rpart",learner.pars = list(maxdepth = c(3,5)),predictor.pars = list(type = "class")), 
                               Workflow(learner = "rpart", predictor.pars = list(type = "class")),
                               Workflow(learner = "randomForest",learner.pars = list(na.action=na.omit,importance=T)),
                               Workflow(learner = "nnet", learner.pars = list(size = 5), predictor.pars = list(type = "class"))
                               #Workflow(learner="bagging",predictor.pars=list(type="class"))                                   #!!!!!!nunca na vida vai acabar
                               #Workflow(learner="boosting",learner.pars=list(mfinal = 5, control = rpart.control(maxdepth = 3)),predictor.pars=list(type="class"))   #!!!!! também nao acaba
                               #Workflow(learner="xgboost", learner.pars=list(data = data.matrix(fires_train[,-9]), label = as.integer(fires_train_norm[,9])-1,nrounds = 10,objective = "binary:logistic", eval_metric = "logloss"),predictor.pars=list(type="class")) #!!!! nao funciona um caralho
                               ), 
                             EstimationTask(metrics = c("acc", "F", "rec", "prec")))
res_norm <- performanceEstimation(PredTask(intentional_cause ~ ., fires_train_norm), 
                                  # c(Workflow(learner = "naiveBayes"),   
                                  #   workflowVariants(learner = "knn3",learner.pars = list(k=c(3,5,7)),predictor.pars = list(type = "class")),    #!!!!só funciona norm
                                  #   workflowVariants(learner = "rpart",learner.pars = list(maxdepth = c(3,5)),predictor.pars = list(type = "class")), 
                                  #   Workflow(learner = "rpart", predictor.pars = list(type = "class")),
                                  #   Workflow(learner = "randomForest",learner.pars = list(na.action=na.omit,importance=T)),
                                  #   workflowVariants(learner = "svm", learner.pars = list(kernel = "radial"),predictor.pars = list(type = "class")),    #!!!!só funciona norm
                                  #   Workflow(learner = "nnet", learner.pars = list(size = 5), predictor.pars = list(type = "class"))
                                    Workflow(learner="bagging", learner.pars=list(mfinal = 1, control = rpart.control(maxdepth = 3)), predictor.pars=list(type="class"))                                   #!!!!!!nunca na vida vai acabar
                                    #Workflow(learner="boosting",learner.pars=list(mfinal = 5, control = rpart.control(maxdepth = 3)),predictor.pars=list(type="class"))   #!!!!! também nao acaba
                                    #Workflow(learner="xgboost", learner.pars=list(data = data.matrix(fires_train[,-9]), label = as.integer(fires_train_norm[,9])-1,nrounds = 10,objective = "binary:logistic", eval_metric = "logloss"),predictor.pars=list(type="class")) #!!!! nao funciona um caralho
                                  ), 
                                  EstimationTask(metrics = c("acc", "F", "rec", "prec")))


in_Model_testing <- createDataPartition(y = fires_train$intentional_cause, p = 0.7, list = FALSE) 
model_testing_train <- fires_train %>% slice(in_Model_testing) 
model_testing_test <- fires_train %>% slice(-in_Model_testing)
model_testing_causes <- model_testing_test$intentional_cause 
model_testing_test <- model_testing_test %>% select(-intentional_cause)

m <- bagging(intentional_cause ~ ., model_testing_train, mfinal = 1, control = rpart.control(maxdepth = 3))
preds <- predict(m, model_testing_test)

#plot(res)
#summary(res)
#rankWorkflows(res_norm, max = TRUE)
#pres <- pairedComparisons(res, baseline = "naiveBayes") 
#signifDiffs(pres)



#result <- id %>% mutate(intentional_cause=predsTree) %>% arrange(id)
#write.csv(result,file="result.csv",row.names = FALSE)


