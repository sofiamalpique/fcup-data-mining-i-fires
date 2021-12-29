library(tidyverse)
library(dplyr)
library(dlookr)

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
ds <- ds %>% select(-c(id,region,municipality,parish,alert_source,lat,lon,))



#---------------------------inspecting columns----------------------------------

unique(select(ds,district))
# We have a duplicate value: "Viana do Castelo" and "Viana Do Castelo".
ds <- ds %>% mutate(district=replace(district,district=="Viana Do Castelo","Viana do Castelo"))


# Now we are inspecting the different origins for the fire.
unique(select(ds,origin))

# Get to know which ones were "false-alarm", because they don't matter.
ds %>% filter(origin=="false_alarm")

# 8 with false alarm and no area damage so we can remove them
ds <- ds %>% filter(origin!="false_alarm")

# Given that "intentional_cause" is, in fact, a Boolean, it must be changed.
ds <- ds %>% mutate(intentional_cause=as.factor(intentional_cause))

# Now we are interested in checking how many cases exist with "total_area"==0:
ds %>% filter(total_area==0)

# We conclude that there's no cases with total_area==0.


# Merge date with hour columns: 
ds <- ds %>% mutate(alert_date=as.Date(alert_date),extinction_date=as.Date(extinction_date),firstInterv_date=as.Date(firstInterv_date))
ds <- ds %>% mutate(alert_time=as.POSIXct(paste(ds$alert_date, ds$alert_hour), format="%Y-%m-%d %H:%M:%S"))
ds <- ds %>% select(-c(alert_date,alert_hour))
ds <- ds %>% mutate(extinction_time=as.POSIXct(paste(ds$extinction_date, ds$extinction_hour), format="%Y-%m-%d %H:%M:%S"))
ds <- ds %>% select(-c(extinction_date,extinction_hour))
ds <- ds %>% mutate(firstInterv_time=as.POSIXct(paste(ds$firstInterv_date, ds$firstInterv_hour), format="%Y-%m-%d %H:%M:%S"))
ds <- ds %>% select(-c(firstInterv_date,firstInterv_hour))
ds <- ds %>% relocate(c(district, alert_time, extinction_time, firstInterv_time ,origin, village_area, vegetation_area, farming_area, village_veget_area, total_area, intentional_cause))



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
# case, is the district and the date.
ds <- ds %>% mutate(date=as.Date(alert_time))

# So we only have one question now: where did "novo" come from? What consists 
# the "novo" data set?

# This is the part where we run Weather.R script and so we create and get "novo"


# "final" is a table with the original and relevant data adding the maximum
# temperature at the day of the fire.

final <- merge(novo,ds,c("district","date"), all.y = TRUE)
final <- as.tibble(final)

# observe if there are any Na that came with the merge
final %>% find_na(index=FALSE)

help <- final %>% subset(is.na(tmax))
help <- help %>% select(district,date,tmax)
novo <- novo %>% add_row(help)
novo <- novo %>% arrange(district,date)
novo <- novo %>% fill(tmax)
novo <- unique(novo)

final <- final %>% select(-c(tmax))
final <- merge(novo,ds,c("district","date"))
final <- as.tibble(final)
final <- final %>% select(-c(date))

