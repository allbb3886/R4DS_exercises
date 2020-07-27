############## Section 13 Relational Data
## common to combine multiple types of data we are interested in.
## multiple tables of data are called relational data.

# Mutating joins, add new variables to one data frame from matching obsevations in another
# Filtering joins, which filter obseravations from one data frame based on whether or not they match an observation in the other table
# set operatoins, which treat observations as if there were set elements.

### RRelational database management system (RDBMS), encompasses almost all modern databases.

#new
### Sectional 13.1.1 Prerequisites

library(tidyverse)
library(nycflights13)
library(viridis)
library(datamodelr)
airlines
airports

## goal here is to draw relationships between different tables/ tibbles
# each relation referes to a pair of tables, the chain of relations between tables are our main interest.



#Exercise 13.2.1
## note that in nycslfights13: 
flights connects to planes via a single varaible, tailnum
# flgihts connects to airlines through carrier
# flights connects to airports, through origin and dest
# flights also connects to weather via origin(location, year month and day and hour)
## Drawing routes reqiures latitude and longitude of origina and destination airports.
# this requires flights and airports tables.
#flights table has origin and destination airport for each flgiht.
# need latitude and longitude for origin and destinantion of each flight.
# use an inner join to drop any flights missing airports since they will not have a longitude or latitude.
# this requires two joins for flights to airports.

flights_latlong <- flights%>%
  inner_join(select(airports,origin=faa, origin_lat=lat, origin_lon=lon),
             by="origin") %>%
  inner_join(select(airports,dest=faa,dest_lat=lat, dest_lon = lon),
             by = "dest"
  )

# this will plot the approximate flight paths for the first 100 flights in the flights dataset.

flights_latlon %>%
  slice(1:100) %>%
  ggplot(aes(
    x=origin_lon,xend=dest_lon,
    y=origin_lat, yend=dest_lat
  )) +
  borders("state") + geom_segment(arrow=arrow(length=unit(0.1, "cm"))) +
  coord_quickmap() + 
  labs(y="lattitude",x = "logitude")


## exercise 13.2.2
## how to draw the relationship between weather and airports.
## column airports$faa is a foreign key of weather$origin
# following drawing updates the one insectoin 13.2.
# the line represent the new relation between weather and airports is black.

## Exercise 13.2.3
#Weather only contains information for origin (NYC) airports. If it contained weather records for all airports in the USA, what additional relation would it define with flights.
# if weather was available for all airports, then it would provide the weather for the destination of each flight.
#weather data frame columns (year,month,day,hour,origin) are a foreign key for the flights data frame columns (year,month,day,hour,dest).
#This would provide information about the weather at the destination airport at the time of the flight take off, unless arrival date-time were calculatd.

# Why was this not a relationship prior to adding additional rows to the weather table?
# in a foreign key relatoinship, colection columns of columns in the child table must refer to a unique collection of columns in the parent table.
# so, if the weather table only contained New York airpots, there were many values of (year, month, day, hour, dest) in flights that did not appear in the weather table.
###
### Exercise 13.2.4 #######
## add a table of special dates
special_days <- tribble(
  ~year, ~month, ~day, ~holiday,
  2013, 01, 01, "New Years Day",
  2013, 07, 04, "Independence Day",
  2013, 11, 29, "Thanksgiving Day",
  2013, 12, 25, "Christmas Day"
)
# primary key of the table would be year, month day columns. And the year month day columns would be used to join special_days with other tables.


####### Section 13.3 Keys ##########
# variables used to conncet each pair of tables are called keys.
# a key is a variable (or set of variables) that uniquely identifies an observation.
# example each plane is uniquely identified by its tailnumber, but multiple variables are needed to identify something like weather. These key variables would be year,month,day,hour, and origin.

## Two types of keys:

## a primary key - uniquely identifies an observation in its own table. 
#Example, planes$tailnum is a primary key because it uniquely identifies each plane in the planes table.

# a foreign key identifies an observatino in another table, for example, flights$tailnum is a foreign key because it appears in the flights table where it matches each flight to a unique plane.

# A variable can be both primary and foreign, an example of this is origin. This is a part of the weather primary key, and is also a foreign key for the airport table.

## good practice to verify the primary keys, and that they verify that they are indeed unique to identify each observation.
## a good way to do this is count(), the primary keys and look for entries where n>1

planes%>% 
  count(tailnum) %>%
  filter(n>1)
weather%>% 
  count(year,month,day,hour,origin) %>%
  filter(n>1)

# if a table lacks a primary key, useful to add one with mutate() and row_number(). That makes it eaier to match observatoins if you've done some filtering and want to check back with original date.
# this is called surrogate key.

# a primary key and coressponding foreign key in another table form a relatoin.
# relations are typically one-to-many.
# each flight has one plane, but each plane has many flights.

## Exercise 13.3.1 ##########

## add a surrogate key to flights
## add the column flight_id as a surrogate key.
# sor the data prior to making the key, even though it is not stricly necessary, so the order of the rows has some meaning.

flights%>% arrange(year,monht,day,sched_dep_time, carrier, flight) %>%
  mutate(flight_id=row_number()) %>%
  glimpse()

### eXERCISE 13.3.2 ##3

## primary key for Lahman:: Batting is (playerID, yearID, stint). The columns (playerID, year ID) are not a primary key since playesr can play on different teams.


Lahman::Batting %>%
  count(playerID, yearID, stint) %>%
  filter(n > 1) %>%
  nrow()

### Primary key for babynamess::babynames is (year, sex, name). 
# The columns (year,name) are not a primary key since there are separate counts for each name for each sex.

babynames::babynames %>%
  count(year,sex,name) %>%
  filter(n>1) %>%
  nrow()
# which proves n=0, unique

## primary key for nasaweather::atmos is (lat,long,year,month).
# the primary key reprsents the location and time that the measurement was taken.

nasaweather::atmos%>%
  count(lat,long,year,month) %>%
  filter(n>1) %>%
  nrow()

####
fueleconomy::vehicles %>%
  count(id) %>%
  filter(n > 1) %>%
  nrow()

#######

### No primary key for ggplot2::diamonds, since no combination of variables that uniquely identifies each observation.

ggplot2::diamonds %>%
  distinct() %>%
  nrow()
#> [1] 53794
nrow(ggplot2::diamonds)
#> [1] 53940

## can add a unique identifier for our analysis, by using a surrogate key
diamonds <- mutate(ggplot2::diamonds, id = row_number())


############# Section 13.4 Mutating joins ###############
## look at combining a pair of tables is the mutating join.
#this lets us combine variables from two tables.
# it matches observations by their keys, then copies across variables from one table to another.

## similar to mutate(), join function adds variables to the right, so if we have a lot of varibles, new variables wont get printed.

flights2 <-  flights %>%
  select(year:day, hour, origin, dest, tailnum,carrier)
flights2
# lets say we want to add the full airline name to flights 2 data
## combine airlines and flights 2 with left_join()
flights2%>%
  select(-origin,-dest) %>%
  left_join(airlines, by ="carrier")

# As a result, joining airlines to flights 2 is an additional variable, name:
# this is why i call this type of join a mutating join.
## can use the same place using mutate()

flights2 %>%
  select(-origin, -dest) %>% 
  mutate(name = airlines$name[match(carrier, airlines$carrier)])


#### Sectoin 13.4.1 Understanding Joins ##########

# To help learn how joins work, use a visual representation

x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  3, "x3"
)
y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2",
  4, "y3"
)
# the coloured column represnts the "key" variable, these are used to match rows between the tables.
# In this case it is 1,2,3 and 1,2,4.
# value columns are on the right.
# a join is a way of connecting each row in x, to zero, one, or more rows in y.
# Joins simply match based on key, the value is just carried along for the ride.

# what happens in an actual join is that matches will be indicated with dots. the number of dots = number of mathces = number of rows in the output.
# So what happens here is in row 1, there will be a match to 1:1, which means the value is x1 and value of y is y1
## similarly, 2 is matched and val_x = 2 and val_y = y2


##Sectoin 13.4.2 Inner Join

# Simplest type of join the inner join. An inner join matches pairs of observations whenever their keys are equal!
# the output of an inner join is a new data frame that contains the key, x values and y values.
# we use by to tell dplyr which variable is the key:

x %>% 
  inner_join(y,by="key")
## The most important property of an inner join is that unmatched rows are not included.
# generally inner joins are not appropriate as it loses obesrvations.


### Sectoin 13.4.3 Outer Joins ##############

# So recall an inner join keeps observations that appear in both tables.
# An outer join keeps observations that appear in at least one of the tables.
# Three types of outer joins :
# 1) left join keeps all observations in x.
#2) right join keeps all observations in y.
# 3) a full join keeps all observations in x and y.
# These joins work by adding an additional "virtual" observation to each table.
# This observation has a key that always matches (if no other key matches), and a value with NA.

## Consider the previos example.
# if we did a left join, they key rows kept are 1,2,3, the value of x and value of y respectively, x1,x2,x3, y1,y2,na
# if we did a right join, we keep rows 1,2,4. X value and y value - x1,x2, NA, y1, y2,y3
# if we did a full join. Keep both key values, 1,2,3,4 and respective value of x and y are : x1 x2 x3, NA, y1 y2 NA, y3

# most common used join is the left join.
# we should use this whenver you look up additional data from another table, because it preserves the original observations even when there isnt a match.
# left join is default choice.

################## Section 13.4.4 Duplicate Keys #################


# so far, we assumed keys are unique, but not always true.
# two possibilities when keys are not unique:
# 1) one table has duplicate keys, this is useful when we want to add in more informatoin as there is typically a one to many relationship

x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  2, "x3",
  1, "x4"
)
y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2"
)
left_join(x, y, by = "key")
# in this case, key is a primary key in y and a foreign key in x.
# so if we were to join.


## Case 2) when both tables have duplicate keys. This is an error because in neither table do keys uniquely identify an observation, when we join duplicated keys, we get all possible combinations, cartesian product:

x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  2, "x3",
  3, "x4"
)
y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2",
  2, "y3",
  3, "y4"
)

##### Section 13.4.5 Defining the key columns ###############

#pairs of tables have been joined by single variable, and that varialbe has the same name in both tables.
#this was encoded by by="key". Can also use other values for by to connect the table in other ways:

# by default, by=NULL, so it uses all variables that appear in both tables, natural join.
## for example the flights and weathers tables match on their common variables: year, month, day, hour and origin.

flights2%>%
  left_join(weather)
## this will match common variables, year,month, day, hour , and origin.

## A character vecot, by ="x". This is like a natural join, but uses only some of the common variables.
# For example flights and planes have year variables, but they mean different when we want to join by tailnum.

flights2 %>% 
  left_join(planes, by = "tailnum")
## note that year variables (appear in both input data frames, but are not constrainted to be equal are disambiguated)
## another case to consider : by=c("a" = "b"). This will match variable a in table x to variable b in table y.
# The vriables from x will be used.

## suppose we want to draw a map we need to combine the flights data with airports data which contain the location (lat and lon) of each airport.
# each flight has an origin and destination airpor, so w need to specify which one we want to join to:
flights2%>% 
  left_join(airports,c("dest"="faa"))
flights2 %>% 
  left_join(airports, c("origin" = "faa"))

left_join(x, y, by = "key")


##### Exercises 13.4.6 ############
##computing average delay by destination, then join on airports data frame to show spatial distribution

airports %>%
  semi_join(flights, c("faa"="dest")) %>%
  ggplot(aes(lon,lat))+ 
  borders("state") +
  geom_point() +
  coord_quickmap()

avg_dest_delays <- 
  flights %>%
  group_by(dest) %>%
  # arrival delay NA's are canelled flights
  summarise(delay =mean(arr_delay, na.rm=TRUE)) %>%
  inner_join(airports, by =c (dest="faa"))

avg_dest_delays %>%
  ggplot(aes(lon,lat,colur=delay)) + 
  borders ("state") + 
  geom_point() +
  coord_quickmap()


## Exercise 13.4.2, Add the location of the origin and destinatoin to flights

airport_locations <-  airports %>%
  select(faa,lat,lon)
flights %>%
  select(year:day, hour, origin, dest) %>%
  left_join(airport_locations, by=c("origin" = "faa")) %>%
  left_join(airport_locations, by =c("dest"="faa"))
####### suffix argument overrides this default behaviour, good practice to have clear varialbe names.
airport_locations <- airports %>%
  select(faa, lat, lon)

flights %>%
  select(year:day, hour, origin, dest) %>%
  left_join(
    airport_locations,
    by = c("origin" = "faa")
  ) %>%
  left_join(
    airport_locations,
    by = c("dest" = "faa"),
    suffix = c("_origin", "_dest")
    # existing lat and lon variables in tibble gain the _origin suffix
    # new lat and lon variables are given _dest suffix
  )

#### Exercise 13.4.3
# relationship between age of a plane and its delays?
## merge flights with the planes, which contains a variable plane_year
view(planes)
#calculate average arrival and delay for each age of a flight.
33 few planes older than 25 years, so truncate age at 25 years.

plane_cohorts <- inner_join(flights,select(planes,tailnum,plane_year=year), by ="tailnum") %>%
  mutate(age=year-plane_year) %>%
  filter(!is.na(age)) %>%
  mutate(age=if_else(age>25, 25L, age)) %>%
  group_by(age) %>%
  summarise(dep_delay_mean=mean(dep_delay,na.rm=TRUE),
            dep_delay_sd=sd(dep_delay,na.rm=TRUE),
            arr_delay_mean=mean(arr_delay,na.rm=TRUE),
            arr_delay_sd=sd(arr_delay,na.rm=TRUE),
            n_arr_delay = sum(!is.na(arr-delay)),
            n_dep_delay = sum(!is.na(dep_delay))
  )

ggplot(plane_cohorts, aes(x = age, y = dep_delay_mean)) +
  geom_point() +
  scale_x_continuous("Age of plane (years)", breaks = seq(0, 30, by = 10)) +
  scale_y_continuous("Mean Departure Delay (minutes)")
ggplot(plane_cohorts, aes(x = age, y = arr_delay_mean)) +
  geom_point() +
  scale_x_continuous("Age of Plane (years)", breaks = seq(0, 30, by = 10)) +
  scale_y_continuous("Mean Arrival Delay (minutes)")


### Exercise 13.4.4 ########

flight_weather <-
  flights %>%
  inner_join(weather, by = c(
    "origin" = "origin",
    "year" = "year",
    "month" = "month",
    "day" = "day",
    "hour" = "hour"
  ))

flight_weather %>%
  group_by(precip) %>%
  summarise(delay = mean(dep_delay, na.rm = TRUE)) %>%
  ggplot(aes(x = precip, y = delay)) +
  geom_line() + geom_point()

flights %>%
  filter(year == 2013, month == 6, day == 13) %>%
  group_by(dest) %>%
  summarise(delay = mean(arr_delay, na.rm = TRUE)) %>%
  inner_join(airports, by = c("dest" = "faa")) %>%
  ggplot(aes(y = lat, x = lon, size = delay, colour = delay)) +
  borders("state") +
  geom_point() +
  coord_quickmap() +
  scale_colour_viridis()


######### Section 13.5 Filtering joins ##############

#filtering joins match obervations in the same way as mutating join, but affect observations not variables.
semi_join(x, y) keeps all observations in x that have a match in y.
anti_join(x, y) drops all observations in x that have a match in y.

## suppose we found most popular destinations:
top_dest <- flights%>%
  count(dest,sort=TRUE) %>%
  head(10)
## now we want to find each flight that went to one of those destinations. Construct a filter

flights %>%
  filter(dest %in% tpo_dest$dest)

## difficult to extend that approach to multiple variables. For example, imagine that you'd found the 10 days with higest average delays.
# how do we construct the filter statement that used year, month, and day to match it back to flights?
## use a semi-join, to connect two tables, but instead of adding new columns, only keep the rows in x that match y.

flights %>%
  semi_join(top_dest)
## this wll create a tibble to keep all observations in top destination that match flights.

## filtering joins never duplicate rows like mutating joins.
## similarly, an anti join keeps rows that don't match.

## connecting flights and planes, might be interested to know that there are many flights that dont have a match in planes:
flights %>%
  anti_join(planes,by= "tailnum") %>%
  count(tailnum, sort= TRUE)

################ Exercises 13.5.1 

#flights with a missing tailnum all have missing values of arr_time, cancelled flights !
flights %>% filter(is.na(tailnum), !is.na(arr_time)) %>%
  nrow()

# many of tail numbers that dont have a matching value in planes are registerd to AA or envoy airlines.


### Excerise 13.5.2 ##
#Filter flights to only show flights with planes that have flown at least 100 flights.

planes_gte100 <-  flights %>%
  filter(!is.na(tailnum)) %>%
  group_by(tailnum) %>%
  count() %>%
  filter(n>=100)

%% semi join data frame of planes that have flown at least 100 flights

flights %>% 
  semi_join(planes_gte100, by="tailnum")

## here is a mutated group

flights %>%
  filter(!is.na(tailnum)) %>%
  group_by(tailnum) %>%
  mutate(n = n()) %>%
  filter(n >= 100)

### Exercise 13.5.3
#combine fueleconomy::vehicles and fueleconomy::common to find only the records for the most common models.

fueleconomy::vehicles %>%
  semi_join(fueleconomy::common, by =c("make","model"))

## why join on make and model and not just possbile? Possible for car brands (make) to prduce a car witht eh same name

fueleconomy::vehicles %>%
  distinct(model,make) %>%
  group_by(model) %>%
  filter(n() >1) %>%
  arrange(model)
#####

flights %>% anti_join(planes,by="tailnum") %>%
  count(carrier,sort=TRUE) %>%
  mutate(p=n/ sum(n))


### Exercise 13.5.4

worst_hours <- flights %>%
  mutate(hour = sched_dep_time %/% 100) %>%
  group_by(origin, year, month, day, hour) %>%
  summarise(dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(dep_delay)) %>%
  slice(1:48)

## use semi join to get weather for these hours.

weather_most_delayed <-  semi_join(weather,worst_hours, by = c("origin", "year", "month", "day", "hour"))

## focus on precipitation, wind speed, temperature

select(weather_most_delayed,temp,wind_speed,precip)%>%
  print(n=48)

ggplot(weather_most_delayed, aes(x = precip, y = wind_speed, color = temp)) +
  geom_point()

### Exercise 13.5.5 ###
anti_join(flights, airpots, by=c("dest" = "faa"))?
  This returns the flights that went to an imporant that is not on the list of FAA destinations.

anti_join(airports, flights, by=c("faa"="dest"))
# returns the US airports that were not the destination of any flight in data. Since data contains all flights from new york city.

##3 Exercise 13.5.6
Check all dinstnct airline, plane combos
plane_carriers <-  flights %>%
  filter(!is.na(tailnum)) %>%
  distinct(tailnum, carrier)
# PLanes that have flown for more than one airline are those tailnum that appear more than once in the planes_carriers data

planes_carriers%>%
  count(tailnum) %>%
  filter(n>1) %>% 
  nrow()

## lets look at planes which have flown for more than one airline

carrier_transfer_tbl <-  planes_carriers %>%
  group_by(tailnum) %>%
  filter(n()>1) %>%
  #lets join the airlines to get the names 
  left_join(airlines, by= "carrier") %>%
  arrange(tailnum, carrier)

#get the carriers and tailnum by carrier and tailnum


### section 13.6 Join problems
## data is typically not as tidy as this section, so a few things to do to make sure data joins are smooth.
#1) start by identifying vairables that form primary key in each each.
# example, altitude and longitude uniquely identify each airport, but not good identifiers.
airpots %>% count(alt,lon) %>% filter (n>1) 
##2) check that none of the variables in the primary key ar emissing, if a value is missing then it cant identify an observation
## 3) check that foreign keys match primary keys in another table. # best way to do this is anti_join().
#check the rows before and after the join is not sufficient to ensure that your join has gone smooth. #if you have an iner join with duplicate keys in both tables, might get unluck as the number of dropped rows might equal
# exactly the number of duplicated rows.

####### Section 13.7 Set Operations ############

intersect(x, y): return only observations in both x and y.
union(x, y): return unique observations in x and y.
setdiff(x, y): return observations in x, but not in y.

df1 <- tribble(
  ~x, ~y,
  1,  1,
  2,  1
)
df2 <- tribble(
  ~x, ~y,
  1,  1,
  1,  2
)




intersect(df1, df2)
#> # A tibble: 1 x 2
#>       x     y
#>   <dbl> <dbl>
#> 1     1     1

# Note that we get 3 rows, not 4
union(df1, df2)
#> # A tibble: 3 x 2
#>       x     y
#>   <dbl> <dbl>
#> 1     1     1
#> 2     2     1
#> 3     1     2

setdiff(df1, df2)
#> # A tibble: 1 x 2
#>       x     y
#>   <dbl> <dbl>
#> 1     2     1

setdiff(df2, df1)
#> # A tibble: 1 x 2
#>       x     y
#>   <dbl> <dbl>
#> 1     1     2


