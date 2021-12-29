library(tidyverse)
library(rnoaa)
library(lubridate)
require(devtools)
library(zoo)

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

# Now we select id and name because we want to check if there are any name 
# duplicates.
sd <- sd %>% select(id,name)

# We decided to remove "COIMBRA/CERNACHE" and "TAVIRA" because we are crossing 
# districts and dates.
# "COIMBRA/CERNACHE" was removed because it already has "COIMBRA"- which is a 
# district - and "TAVIRA" was removed because it isn't a district.
sd <- sd %>% filter(name!="COIMBRA/CERNACHE",name!="TAVIRA")


# Here we restrained the dates. We're only interested in 2014 and 2015.
# All this information is going to a new data set called "weather_data".
weather_data <- ghcnd_search(sd$id, var = c("TMAX") , date_min = "2014-01-01", date_max = "2015-12-31")

# Here, the table is being copied to "weather_data".
weather_data <- weather_data$tmax

# We maintain id so we are able to join tables later.
weather_data <- weather_data %>% select(id,tmax,date)

# We sort first by id and then by date, so we guarantee to first search by 
#district.
weather_data <- weather_data %>% arrange(id,date)
weather_data <- weather_data %>% fill(tmax)

# find out which stations in Portugal have tmax
help <- weather_data %>% drop_na() %>% select(id)
help <- merge(help,sd,"id")
help <- help %>% distinct(id, .keep_all = TRUE)
help


# As we don't have all the Portuguese districts in "sd" we need to add/create a 
# column called "nearest_district" containing the districts available in 
# "weather_data".
# With this we can put the values of maximum temperature to districts that don't 
# appear in "weather_data" using the values of the nearest district.

sd <- sd %>% mutate(nearest_district=c("Lisboa","Porto","Coimbra","Beja","Braganaca","Faro","Evora","Castelo Branco"))
sd <- sd %>% add_row(nearest_district="Viseu",id="PO000008575")
sd <- sd %>% add_row(nearest_district="Aveiro",id="PO000008575")
sd <- sd %>% add_row(nearest_district="Braga",id="PO000008575")
sd <- sd %>% add_row(nearest_district="Viana do Castelo",id="PO000008575")
sd <- sd %>% add_row(nearest_district="Vila Real", id="PO000008575")
sd <- sd %>% add_row(nearest_district="Guarda", id="POM00008570")
sd <- sd %>% add_row(nearest_district="Leiria", id="PO000008535")
sd <- sd %>% add_row(nearest_district="Santarem", id="PO000008535")
sd <- sd %>% add_row(nearest_district="Portalegre", id="POM00008558")
sd <- sd %>% add_row(nearest_district="Setubal", id="PO000008535")
sd$id[2] <- "PO000008575"
sd$id[3] <- "POM00008570"
sd$id[4] <- "POM00008558"

# "novo" is a new table, created by merging "sd" and "weather_data" 
novo <- merge(sd,weather_data,"id",all.x = TRUE)
novo <- as.tibble(novo)
novo <- novo %>% select(nearest_district,tmax,date)
novo <- novo %>% rename(district=nearest_district)
novo$tmax <- ((novo$tmax)/10)

