library(tidyverse)
library(dplyr)
library(dlookr)
library(rnoaa)
library(lubridate)
require(devtools)
library(zoo)
library(data.table)
library(ggplot2)

# Read the data.
ds <- read_csv("fires_train.csv", na=c("-","NA"))

# First understanding of the data.
ds
summary(ds)
spec(ds)

# First diagnose of NA.
ds %>% find_na(index=FALSE)
ds %>% select(find_na(.)) %>% diagnose()

# Remove unnecessary columns and alert_source which has only Na as entry.
ds <- ds %>% select(-c(id,region,municipality,parish,alert_source,lat,lon,village_veget_area))



#---------------------------inspecting columns----------------------------------

unique(select(ds,district))
# We have a duplicate value: "Viana do Castelo" and "Viana Do Castelo".
ds <- ds %>% mutate(district=replace(district,district=="Viana Do Castelo",
                                     "Viana do Castelo"))


# Now we are inspecting the different origins for the fire.
unique(select(ds,origin))

# Get to know which ones were "false-alarm", because they don't matter.
ds %>% filter(origin=="false_alarm")

# 8 with false alarm and no area damage so we can remove them
ds <- ds %>% filter(origin!="false_alarm")

# Given that "intentional_cause" is, in fact, a Boolean, it must be changed.
ds <- ds %>% mutate(district=as.factor(district), origin=as.factor(origin), 
                    intentional_cause=as.factor(intentional_cause))

# Now we are interested in checking how many cases exist with "total_area"==0:
ds %>% filter(total_area==0)

# We conclude that there's no cases with total_area==0.


# Merge date with hour columns: 
ds <- ds %>% mutate(alert_date=as.Date(alert_date),
                    extinction_date=as.Date(extinction_date),
                    firstInterv_date=as.Date(firstInterv_date))

ds <- ds %>% mutate(firstInterv_time=as.POSIXct(paste(ds$firstInterv_date, 
                                                      ds$firstInterv_hour), 
                                                format="%Y-%m-%d %H:%M:%S"))

ds <- ds %>% select(-c(firstInterv_date,firstInterv_hour))

ds <- ds %>% mutate(date=alert_date)

ds <- ds %>% relocate(c(district, alert_date, alert_hour, extinction_date, extinction_hour, firstInterv_time ,origin, village_area, vegetation_area, farming_area, total_area, intentional_cause))



# Second diagnose of NA.
ds %>% find_na(index=FALSE)
ds %>% select(find_na(.)) %>% diagnose()
ds %>% filter(!complete.cases(.))

# We decided to keep the rows with NA in extinction_time and firstInterv_time, 
# but just for now.

ds <- ds %>% select(-c(firstInterv_time))
ds <- ds %>% drop_na()

# So in order to merge another data set ("novo") with this one, and create a new
# one - called "final" - we needed to have something in common, which, in this 
# case, is the district and the date of occurrence.

# So we only have one question now: where did "novo" come from? What consists 
# the "novo" data set?

#------------------------------Creating "novo"----------------------------------

# First load "station_data.Rdata".
load("station_data.Rdata")


sd <- as_tibble(station_data)


# We are only interested in he main-land of Portugal, that's why Azores and 
# Madeira are not accounted. So we restrained to the coordinates of Portugal.

# Also, we have element == "TMAX", because we are interested in using the 
# maximum temperature registered on that day. We believe it influences the risk 
# of natural fires, so it might help deciding the intentional cause.
sd <- filter(sd,str_starts(id,"PO"), latitude < 42.2,latitude > 36.8, longitude 
             > -9.6,longitude < -6.1,element=="TMAX")

# Now we select "id" and "name" because we want to check if there is any name 
# duplicated.
sd <- sd %>% select(id,name)

# We decided to remove "COIMBRA/CERNACHE" and "TAVIRA" because we are crossing 
# districts and dates.
# "COIMBRA/CERNACHE" was removed because it already has "COIMBRA"- which is a 
# district - and "TAVIRA" was removed because it isn't a district.
sd <- sd %>% filter(name!="COIMBRA/CERNACHE",name!="TAVIRA")

sd <- sd %>% filter(name!=is.na(name))

# Here we restrained the dates. We're only interested in 2014 and 2015.
# All this information is going to a new data set called "weather_data".
weather_data <- ghcnd_search(sd$id, var = c("TMAX") , date_min = "2014-01-01", 
                             date_max = "2015-12-31")

# Here, the table is being copied to "weather_data".
weather_data <- weather_data$tmax

# We maintain "id" so we are able to join tables later.
weather_data <- weather_data %>% select(id,tmax,date)

# We sort first by id and then by date, so we guarantee to first search by 
#district.
weather_data <- weather_data %>% arrange(id,date)
weather_data <- weather_data %>% fill(tmax)

# Finding out which stations in Portugal have tmax
help <- weather_data %>% drop_na() %>% select(id)
help <- merge(help,sd,"id")
help <- help %>% distinct(id, .keep_all = TRUE)

# As we don't have all the Portuguese districts in "sd" we need to add/create a 
# column called "nearest_district" containing the districts available in 
# "weather_data".
# With this we can put the values of maximum temperature to districts that don't 
# appear in "weather_data" using the values of the nearest district.

sd <- sd %>% mutate(nearest_district=c("Lisboa","Porto","Coimbra","Beja",
                                       "Bragança","Faro","Évora",
                                       "Castelo Branco"))
sd <- sd %>% add_row(nearest_district="Viseu",id="PO000008575")
sd <- sd %>% add_row(nearest_district="Aveiro",id="PO000008575")
sd <- sd %>% add_row(nearest_district="Braga",id="PO000008575")
sd <- sd %>% add_row(nearest_district="Viana do Castelo",id="PO000008575")
sd <- sd %>% add_row(nearest_district="Vila Real", id="PO000008575")
sd <- sd %>% add_row(nearest_district="Guarda", id="POM00008570")
sd <- sd %>% add_row(nearest_district="Leiria", id="PO000008535")
sd <- sd %>% add_row(nearest_district="Santarém", id="PO000008535")
sd <- sd %>% add_row(nearest_district="Portalegre", id="POM00008558")
sd <- sd %>% add_row(nearest_district="Setúbal", id="PO000008535")
sd$id[2] <- "PO000008575"
sd$id[3] <- "POM00008570"
sd$id[4] <- "POM00008558"

# "novo" is a new table, created by merging "sd" and "weather_data" 
novo <- merge(sd,weather_data,"id",all.x = TRUE)
novo <- as_tibble(novo)
novo <- novo %>% select(nearest_district,tmax,date)
novo <- novo %>% rename(district=nearest_district)
novo$tmax <- ((novo$tmax)/10)

# "final" is a table with the original and relevant data adding the maximum
# temperature at the day of the fire.
final <- merge(novo,ds,c("district","date"), all.y = TRUE)
final <- as_tibble(final)

# Observe if there are any Na that came with the merge
final %>% find_na(index=FALSE)

help <- final %>% subset(is.na(tmax))
help <- help %>% select(district,date,tmax)
novo <- novo %>% add_row(help)
novo <- novo %>% arrange(district,date)
novo <- novo %>% fill(tmax)
novo <- unique(novo)

final <- final %>% select(-c(tmax))
final <- merge(novo,ds,c("district","date"))
final <- as_tibble(final)

final <- final %>% rename(season = date)


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

final$season <- getSeason(final$season)

final <- final %>% mutate(season = as.factor(season))


#----------------------------------TASK 2---------------------------------------

#


summary(final)

# How many intentional and non intentional fires do we have?
final %>% group_by(intentional_cause) %>% count()
ggplot(final, aes(x=intentional_cause)) + geom_bar() +
  ggtitle("How many fires were intentional and non-intentional?")

# What's the distribution of the fires according to the season?
# É MELHOR POR EM PERCENTAGEM
ggplot(final, aes(x=season)) + geom_bar() + facet_wrap(~intentional_cause) +
  ggtitle("Relationship between season and intentional cause")

# Could there be a relationship between "intentional_cause" and the maximum 
# temperature?   É MELHOR POR EM PERCENTAGEM
ggplot(final, aes(x=tmax)) + geom_histogram(binwidth = 2) + 
  facet_wrap(~intentional_cause) + 
  ggtitle("Relationship between maximum temperature and intentional cause")


# Could there be a relationship between "intentional_cause" and the time it was 
# alerted?
ggplot(final, aes(x=as.ITime(alert_hour))) + geom_histogram(binwidth = 3600) + 
  scale_x_time() + facet_wrap(~intentional_cause)


# What can we say about the "origin" and the "intentional_cause"?
final %>% group_by(origin,intentional_cause) %>% count() %>% arrange(desc(n))

ggplot(final, aes(x=intentional_cause)) + geom_bar() + facet_wrap(~origin)
# We can see in the graph that the most fires with "intentional_cause" have 
# origin "firepit".


# Relationship between "tmax", "origin" and "intentional_cause"
ggplot(final, aes(x=tmax)) + geom_histogram(binwidth = 2) + 
  facet_grid(intentional_cause~origin)


# TODO
# descriptive Modeling (clustering - regiões, temp)
# Ver em que dia da semana ocorrem os intencionais - só é 
# interessante se for diferente dos acidentais


#is there a relationship between the area damaged and intentional_cause
final %>% group_by(intentional_cause) %>% 
  summarise(total_area_mean=mean(total_area), 
            total_area_median=median(total_area))

ggplot(final, aes(x=village_area)) + geom_histogram(binwidth = 200) + 
  facet_wrap(~intentional_cause)

ggplot(final, aes(x=vegetation_area)) + geom_histogram(binwidth = 200) + 
  facet_wrap(~intentional_cause)

ggplot(final, aes(x=farming_area)) + geom_histogram(binwidth = 200) + 
  facet_wrap(~intentional_cause)

ggplot(final, aes(x=village_veget_area)) + geom_histogram(binwidth = 200) + 
  facet_wrap(~intentional_cause)

ggplot(final, aes(x=total_area)) + geom_histogram(binwidth = 200) + 
  facet_wrap(~intentional_cause)


#what is the correlation coefficient between all numeric attributes
final %>% select(tmax,village_area, farming_area, 
                 vegetation_area, total_area) %>% cor()

#is there any monotonic relationship
final %>% select(tmax,village_area, village_veget_area, farming_area, 
                 vegetation_area, total_area) %>% cor(method = "spearman")





