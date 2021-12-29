library(tidyverse)
library(dplyr)
library(dlookr)

# Read the data
ds <- read_csv("fires_train.csv", na=c("-","NA"))

# first understanding of the data
ds
summary(ds)
spec(ds)

# first diagnose of Na
ds %>% find_na(index=FALSE)
ds %>% select(find_na(.)) %>% diagnose()

# remove unnecessary columns and alert_source which has only NA as entry
ds <- ds %>% select(-c(id,region,municipality,parish,alert_source))



#-----------------inspect columns:----------------------------------------------

unique(select(ds,district))
#Duas vezes Viana do Castelo
ds <- ds %>% mutate(district=replace(district,district=="Viana Do Castelo","Viana do Castelo"))


unique(select(ds,origin))
ds %>% filter(origin=="false_alarm")

# 8 with false alarm and no area damage so we can remove them
ds <- ds %>% filter(origin!="false_alarm")


dsHelp <- ds %>% select(lat,lon)
print(dsHelp,n=200)
ds <- ds %>% mutate(lat=gsub("[:,?,',.]*",'',lat),lon=gsub("[:,?,',.]*",'',lon))
ds <- ds %>% mutate(lat=gsub("[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]",'',lat))
ds <- ds %>% mutate(lat=sub("^0+", "", lat),lon=sub("^0+", "", lon))
ds <- ds %>% mutate(lat=gsub("\\s", "0", format(lat, width=max(6))),lon=gsub("\\s", "0", format(lon, width=max(5))))
ds <- ds %>% mutate(lat=substr(lat,1,6),lon=substr(lon,1,5))
ds <- ds %>% mutate(lat=gsub('^(.{2})(.*)$', '\\1.\\2', lat),lon=gsub('^(.{1})(.*)$', '\\1.\\2', lon))
ds <- ds %>% mutate(lat=as.numeric(lat),lon=as.numeric(lon))
ds <- ds %>% mutate(lon=-lon)


ds <- ds %>% mutate(intentional_cause=as.factor(intentional_cause))


ds %>% filter(total_area==0)
# dont exists more total area damage equal to 0


# merge date with hour columns
ds <- ds %>% mutate(alert_date=as_date(alert_date),extinction_date=as_date(extinction_date),firstInterv_date=as_date(firstInterv_date))
ds <- ds %>% mutate(alert_time=as.POSIXct(paste(ds$alert_date, ds$alert_hour), format="%Y-%m-%d %H:%M:%S"))
ds <- ds %>% select(-c(alert_date,alert_hour))
ds <- ds %>% mutate(extinction_time=as.POSIXct(paste(ds$extinction_date, ds$extinction_hour), format="%Y-%m-%d %H:%M:%S"))
ds <- ds %>% select(-c(extinction_date,extinction_hour))
ds <- ds %>% mutate(firstInterv_time=as.POSIXct(paste(ds$firstInterv_date, ds$firstInterv_hour), format="%Y-%m-%d %H:%M:%S"))
ds <- ds %>% select(-c(firstInterv_date,firstInterv_hour))
ds <- ds %>% relocate(c(district, lat, lon, alert_time, extinction_time, firstInterv_time ,origin, village_area, vegetation_area, farming_area, village_veget_area, total_area, intentional_cause))



# second diagnose of Na
ds %>% find_na(index=FALSE)
ds %>% select(find_na(.)) %>% diagnose()
ds %>% filter(!complete.cases(.))





