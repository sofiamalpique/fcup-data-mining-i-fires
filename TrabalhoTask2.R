library(data.table)

summary(final)

# How many intentional and non intentional fires do we have?
final %>% group_by(intentional_cause) %>% count()


# Could there be a relationship between intentional_cause and the maximum 
# temperature?
final %>% group_by(intentional_cause) %>% summarise(tmax_mean=mean(tmax), 
                  tmax_mode=median(tmax))

ggplot(final, aes(x=tmax)) + geom_histogram() + facet_wrap(~intentional_cause)
# After seeing the graph, it seems not, however we can say that there generally 
# seems to be no "intentional_cause" when the temperatures are low.

# Could there be a relationship between "intentional_cause" and the time it was 
# alerted?
ggplot(final, aes(x=as.ITime(alert_time))) + geom_histogram() + scale_x_time() 
  + facet_wrap(~intentional_cause)
# We can see that fires that are not intentional often started or were alerted 
# between 10h and 22h, however intentional caused fires occur more often during 
# the day although the difference isn't that big.

# What can we say about the "origin" and the "intentional_cause"?
final %>% group_by(origin,intentional_cause) %>% count() %>% arrange(desc(n))

ggplot(final, aes(x=intentional_cause)) + geom_bar() + facet_wrap(~origin)
# We can see in the graph that the most fires with "intentional_cause" have 
# origin "firepit".

# Relationship between "tmax" origin and "intentional_cause"
ggplot(final, aes(x=tmax)) + geom_histogram() + 
  facet_grid(intentional_cause~origin)

# Are there any relationships between the damaged areas?
ggplot(final, aes(x = village_area, y = vegetation_area)) + geom_point() + 
  facet_wrap(~intentional_cause)

ggplot(final, aes(x = village_area, y = farming_area)) + geom_point() + 
  facet_wrap(~intentional_cause)

ggplot(final, aes(x = village_area, y = village_veget_area)) + geom_point() + 
  facet_wrap(~intentional_cause)

ggplot(final, aes(x = vegetation_area, y = farming_area)) + geom_point() + 
  facet_wrap(~intentional_cause)

ggplot(final, aes(x = vegetation_area, y = village_veget_area)) + geom_point() + 
  facet_wrap(~intentional_cause)


# "vegetation_area" and "village_veget_area" seems to have a linear relationship
ggplot(final, aes(x = farming_area, y = village_veget_area)) + geom_point() + 
  facet_wrap(~intentional_cause)


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
final %>% select(tmax,village_area, village_veget_area, farming_area, 
                 vegetation_area, total_area) %>% cor()

#is there any monotonic relationship
final %>% select(tmax,village_area, village_veget_area, farming_area, 
                 vegetation_area, total_area) %>% cor(method = "spearman")
