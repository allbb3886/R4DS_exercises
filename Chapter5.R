##########SECTIOn 5 Data Transformation ##################

## often need to rename / re roder observations in order to make data easier to work.

library(tidyverse)
library(nycflights13)
library(Lahman)
## tibbles

nycflights13::flights

## notice that data frame is different structure
## dont worry about tibbles

## int <- integers, dbl <- doubles/real numbers
## chr <- character/vectors
## dttm <- data-times (a date + time)

## also common to see lgl <-  logical
## fctr <-  categorical variables with fixed possible values
## date <-  dates.

### Section 5.1.3 dply basics ############

## five key dplyr functions to do data manipulation:

## PICK OBSERVATIONS BY THEIR VALUES (filter())
## Reorder rows (arrange())
##pick variables by their names( select())
## Create new variables with functions of existing variables (mutate())
## collapse many values down to single summary (summarise())

#These can all be used in conjunction with group_by() which changes the scope of each function from operating on the entire dataset to operating on it group-by-group.
#These six functions provide the verbs for a language of data manipulation.

## First argument is data frame, subsequent arguments describe what to do with data frame, using variable names
# result is a new data frame.

########## Filtering rows with filter()


## allows to subset observations 

## filtering to see all flights on january first

filter(flights, month==1,day==1)

## dplyr executes the filtering operation and returns a new data frame, dplyr functions never modify their insputs, so need to save results using assignemnt

jan1 <- filter(flights,month==1,day==1)

## R either prints out the results or saves them to a variable, if you want to do both, wrap assignment in parentheses

(dec25 <- filter(flights,month==12,day==25))


####### Section 5.2.1 Comparisons #########

## when computing sqrt(2)^2 == 2, this results in false, since it needs floating numbers
## similarly for 1/49* 49 == 1.

## use near() function instead.
## near(sqrt(2)^2,2)
## <- True
##near(1/49*49,1)
## true


######### SEction 5.22 Logical operators

## using in filter() argument, & represents "and", | represents "or", and ! represents "not"

filter(flights, month == 11 | month == 12)

## a common problem is you cant write filter(flights,month==(11|12)), this translates to find all flights departed in november or december.
#-> usefull short hand for this is x %in% y. This will select every row where x is one of the values in y

## if we want all flights in novemeber or december 
nov_dec <- filter(flights,month%in% c(11,12))


## using Demorgan's law !(x&y) == !x | !y, and !(x|y) is same as !x&!y.

## if we wanted flights that werent delayed (on arrival or departure) by more than two hours
# use the folowing two filters:

delayed_arr_dep <- filter(flights, !(arr_delay >120 | dep_delay >120))
## or (filter(flights,arr_delay<-120, dep_delay <= 120))


############
########## 5.2.3 Missing Values ######################

##NA represents unknown value, so missing values are "contagious".

#NA > 5
#> [1] NA
#10 == NA
##> [1] NA
#NA + 10
##> [1] NA
#NA / 2
##> [1] NA

## and even NA == NA => leads to NA output

## why? consider x to be marys age, x is unknown, x <-  NA
## let y be johns age, y <- NA.

## then are mary and john the same age? x == y?, its unknown.

## To determine whether a value is missing, use is.na(),
## is.na(x)
#True

##Filter() only includes rows where condition is true, it excludes both false and NA values, if you want to preserve missing values, ask explicitly.

df <- tibble(x=c(1,NA,3))
filter(df,x>1)
filter(df, is.na(x) | x > 1)

################# Exercises, 5.2.4 ##########

## find all flights that had an arrival delay of two or more hours
delay_2_hours <-filter(flights,arr_delay > 120)

july_aug_sept_dep <- filter(flights, month %in% c(6,7,8))
arr_late_not_leave_late <- filter(flights,arr_delay >120,dep_delay<= 0)

## between() function looks at values greater than left and values greater than right.
num_of_NA <-  filter(flights, is.na(dep_time)) # Number of NAs for depart_time

### 5.3 Arranging rows with arrange ###

#######
## arrange() works simarly to filter(), except instead of select rows, it changes order.
## takes a data frame and a set of column names to order by.
## if provding more than one column name, each additional column will be used to break ties in values of preceding columns


arrange(flights,year,month,day)

## sorting dep_delay using desc() -> descending order

arrange(flights,desc(dep_delay))
## sort missing values at the end:
df <- tibble(x=c(5,2,NA))
arrange(df,desc(x))


## how to arrange() to sort all missing values to the start?
# arraange(flights,is.na(x))?
arrange(flights,desc(dep_delay))
arrange(flights,order(dep_delay))
f <- arrange(flights,desc(distance))


######### Section5.4 Select columns with Select() ####################
## useful to help narrow in on variables of interest. 
select(flights,year,month,day)

##select all columns between year and day (inclusive)
select(flights,year:day)

##select all columns except those from year to day (inclusive)
asdas <- select(flights,-(year:day))


##### Can use help functions within select():, 
starts_with("abc") : matches names that begin with "abc"
ends_with("xyz") : matches names that end with "xyz"
contains("ijk"): mathces names that contain "ijk"
matches("(.)\\1"): select varialbes that amtch a regular expression
# this one matches any variables that contain repeated characters. 
# num_range("x", 1:3) : matches x1, x2 and x3

select() can be used to rename variables, but tis not useful since its drop all variables not mentioned,
# instead use rename(), which is a varaint of select() that keeps all the variables that aren't explitcilty mentioned:

# Example for renaming:
rename(flights,tail_num=tailnum)

## if interested in moving variables to the start of the data frame:
# using select() in conjuction with everything()

## example:
select(flights,time_hour,air_time,everything())

select(flights,year,year,year,month,day)

## one of function helps match variables name in a character, so we are interested in one of th following year,month,day,dep,arr)
# using a select with the name of a variable multiple times wiull just read it intially the first time

select(flights, contains("TIME")) 
# The above will print out the tibble for all observations related to time! and drops everything else.

############ Section 5.5, Add new variables with mutate() ##################

# useful to add new columns that are functions of existing columns!
#mutate() will walys add new columns at the end of datset, so we can see new variables of narrow dataset.
## to se all columns, use view()

flights_sml <- select(flights,year:day,ends_with("delay"),distance,air_time) ## creating a table of year, month, day, all variables with "delay", distance, air_time

mutate(flights_sml,gain=dep_delay-arr_delay,speed= distance/air_time * 60) ## mutating the above table by adding new varaibldes, gain and speed at the end.

## Can also refer to columns just created:
mutate(flights_sml,gain=dep_delay-arr_delay,hours=air_time/60, gain_per_hour=gain/hours)
## if we want to see only the mutated variables, use transmute() function!
transmutedstuff <- transmute(flights,gain=dep_delay-arr_delay,hours=air_time/60, gain_per_hour=gain/hours)
view(transmutedstuff)


################# Section 5.5.1  Useful creation functions #######################
## key property is that function must be vectorised: it must take a vector of values as input, return a vector with same number of values as output
##arithmetic operators: +,- ,*, / , ^. All are vectorised. 
# if one parameter is shorter than the other, it will be automatically exteneded to be same length.
# most useful when one of the arguments is a single number: air_time/60, hours*60 +minute, etc
## useful in aggregate functions:
## x/sum(x), calculates proportion of total, y-mean(y) = difference from mean

## modular arithmetic %/% (integer divison) and %% remainder, where x==y*(x%/%y)+(x%%y)


## can compute hour and minute from dep_time with : 
transmute(flights,
          dep_time,
          hour = dep_time %/% 100,
          minute = dep_time %% 100
)

## Logs are also helpful, log(), log2(), log10(), to deal with data ranging across different magnitudes.
# converts multiplicative relationships to additive.
## recommend using log2(), easy to intrepret , a differnce of 1 on log scale, corresponds to doubling on original scale and a difference of -1 coressponds to halving.


## offsets: lead() and lag() allow one to refer to leading/lagging values.
# this allows one to compute running diferences (x-lag(x)), or find when values change (x!=lag(x)). 
# useful in conjunction with group_by().

## Example
(x <- 1:10)
##>  [1]  1  2  3  4  5  6  7  8  9 10
#lag(x)
##>  [1] NA  1  2  3  4  5  6  7  8  9
#lead(x)
##>  [1]  2  3  4  5  6  7  8  9 10 NA

## Cumulative and rolling aggregates, R provides functions for running sums, products, mins/maxes:
# cumsum(), cumprod(), cummin(), cummax(), and dplyr provides cummean().
x
#>  [1]  1  2  3  4  5  6  7  8  9 10
cumsum(x)
#>  [1]  1  3  6 10 15 21 28 36 45 55
cummean(x)
#>  [1] 1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5 5.0 5.5

## tehre are a number of ranking functions, min_rank(), its does the most usual type of ranking.
# use desc(x) to give largest values the smallest ranks
yasdasd <-  c(1,2,2,NA,3,4)
min_rank(yasdasd)
min_rank(desc(yyasdasd))
## If min_rank() doesn't do what you need, look at the variants row_number(), dense_rank(), percent_rank(), cume_dist(), ntile(). See their help pages for more details.

############### Exercises 5.5.2 ################

## convert dep_time and sched_dep_time to more conveneint represetation of numbers

transmute(flights,dep_time, sched_dep_time, change_dep=dep_time%/% 100, change_sched=sched_dep_time %%100)

#
#transmute(flights,air_time, change_air=arr_time-dep_time)
mutate(flights, air_time, gaindiff=arr_time-dep_time) ## creating table with flights, and adding new variables
transmute(flights, air_time, gaindiff=arr_time-dep_time)

flights_dep <- select(flights,dep_time,sched_dep_time,dep_delay)
mutate(flights_dep)## Notice dep delay is the difference.

##
#1:3 + 1:10
#object length, 1,2,3 + 1,2,3,4,5,..,10. not enough elements


##################### 5.6 Grouped Summaries with summarise() ###############
### last key verb is summarise(), it collapses an entire data frame to a single row:

summarise(flights,delay=mean(dep_delay,na.rm=TRUE))

## not a useful function, unless we pair it with group_by()

## this changes the unit of analysis from compelte dataset to individual groups.
## Using dplyr verbs on grouped data frame, they'll be automatically applied "by group".
## If we applied exactly the same code to a data frame grouped by date, we get average delay per date.

by_day <- group_by(flights,year,month,day)
summarise(by_day,delay=mean(dep_delay,na.rm=TRUE))
## this provdies average delay per date.

# together group_by() and summarise(), provide one of the tools that you'll use most. must introduce
# a new idea, the pipe.

###### Section 5.6.1, combining multiple operations with the pipe)###################
#suppose we are interested in relationship between distance and average delay for each location.

by_dest <- group_by(flights,dest)
delays <- summarise(by_dest,count=n(),dist=mean(distance,na.rm=TRUE),delay=mean(arr_delay,na.rm=TRUE))

delays <- filter(delays,count>20, dest!="HNL")

ggplot(data=delays,mapping=aes(x=dist,y=delay))+geom_point(aes(size=count),alpha=1/3)+ geom_smooth(se=FALSE)

## Group flights by destiation, summarise to compute distance, average delay and number of flights,
#filter to remove noisy points, and honolulu airport, almost tiwce as far as the next closest airport.

## different way to give each intermediate data frame a name. using pipe##

delays <- flights%>%group_by(dest)%>% summarise(count=n(),dist=mean(distance,na.rm=TRUE),delay=mean(arr_delay,na.rm=TRUE)) %>% filter(count>20,dest!="HNL")
## focuses on transformation, not whats being transformed.
## read it as aseries of statements, group, summarise, then filter.
# %>% can be read in english as "then".


## Behind the scenes x%>% f(y), turns into f(x,y) and x%>% f(y) %>% g(z), g(f(x,y),z)

######### 5.6.2 Missing Values ##################

## what was na.rm??
flights%>% group_by(year,month,day) %>% summarise(mean=mean(dep_delay))

### WE get a a lot of missing values, because aggregation functions obey the usual rule of missing values.
#he output will be a missing value. Fortunately, all aggregation functions have an na.rm argument which removes the missing values prior to computation:

flights %>% 
  group_by(year, month, day) %>% 
  summarise(mean = mean(dep_delay, na.rm = TRUE))

## missing values in this case represent cancelled flights, could solve this problem by removing cancelled flights.
## save this data set. removing cancelled flgihts first!
##
not_cancelled <- flights%>% filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled %>% group_by(year,month,day)%>%summarise(mean=mean(dep_delay))


########## 5.6.3 Counts ###############################
## always a good diea to include either a count(n()), or a couint of non missing values (sum(!is.na(x))

## looking at planes identified by tail number with high delays
delays <- not_cancelled %>% group_by(tailnum)%>% summarise(delay=mean(arr_delay))
ggplot(data=delays,mapping=aes(x=delay)) +geom_freqpoly(binwidth=10)

## notice some planes have average delay of 5 hours
## a bit more nuanced,. draw scaterplot isntead of numer of flights vs average delay

delays <- not_cancelled %>% group_by(tailnum) %>% summarise(delay=mean(arr_delay,na.rm=TRUE),n=n())
ggplot(data=delays,mapping=aes(x=n,y=delay)) + geom_point(alpha=1/10)

## much greater variation in average delay when there are fewer flights
## shape of plot is very characterstics, whjenever you plot a mean (or other summary)
## vs group size, you'll see variation decreases as sample size increases

## lets focus in on n> 25
delays%>% filter(n>25) %>% ggplot(mapping=aes(x=n,y=delay))+geom_point(alpha=1/10)

######### Consider common variation, how average performance of batters in baseball is related to number of times at bat.
## Using Lahman package ##### 
#batting average = num of hits/ num of attempts

## converting to tibble

batting <-  as_tibble(Lahman::Batting)

batters <- batting %>% group_by(playerID) %>% summarise(ba=sum(H,na.rm=TRUE)/sum(AB,na.rm=TRUE),ab=sum(AB,na.rm=TRUE))

batters%>% filter(ab>100)%>%ggplot(mapping=aes(x=ab,y=ba))+geom_point()+geom_smooth(se=FALSE)

## note that this has implications for ranking, if you desc(ba) the people with batting averages are lucky, not skilled.!!

batters %>% arrange(desc(ba)) ### looking at the batters, and arrange descending order of BA)

########## SECTION 5.6.4 USEFUL SUMAMRY FUNCTIONS ############

## just using means, counts, and sum can get you a long way, but can use other methods
## measures of location: mean(x) has been used, now consider median(x)
# median(x) tells us where 50% of x lies and 50% below.

## 
not_cancelled%>% group_by(year,month,day)%>%summarise(avg_delay1=mean(arr_delay),avg_delay2=mean(arr_delay[arr_delay>0])) ## looking at position delay!

###3 avrg_delay 2 looks at average positive delay !!



## Measures of spreds: sd(x), IQR(x), mad(x), the root mean squared deviation, is standard measure of spread.
## IQR and median absolute deivation are robust equivalents if we have outliers
### 
##why is distance to some destintations mroe variable than others.
#Measures of rank: min(x), quantile(x, 0.25), max(x). Quantiles are a generalisation of the median. For example, quantile(x, 0.25) will find a value of x that is greater than 25% of the values, and less than the remaining 75%.

# Why is distance to some destinations more variable than to others?
not_cancelled %>% 
  group_by(dest) %>% 
  summarise(distance_sd = sd(distance)) %>% 
  arrange(desc(distance_sd))

## can also look at measures of rank, min(x), quantile(x,0.25), max(x). 
# Quantiles are generalistions of the median. ##
# quantile(x,0.25) will find x that is greater than 25% and less than remaining 75%.

## what time do the first and lasst flight leave each day?

not_cancelled %>% group_by(year,month,day) %>% summarise(first=min(dep_time),last=max(dep_time))

## measures of position: first(x), nth(x,2), last(x). 
# these work simiarly to x[1], x[2], and x[length(x)], but let you set a default value
# if that positions does not exist. (trying to get the 3rd element in a 2 element list)

## get data set>
not_cancelled %>% group_by(year,month,day) %>% mutate(r=min_rank(desc(dep_time))) %>% filter (r %in% range(r))

## these functions are complementary to filtering on ranks.

## Recall counts function, n(), tkaes no arguments, and returns size of the current group
## to count the number of non-missing valuies, use sum(!is.na(x)). 
# to count number of distinct(unique) values, can use n_distinct(x)

#
not_cancelled %>% group_by(dest) %>% summarise(carriers = n_distinct (carrier)) %>%
  arrange(desc(carriers))

### counts are useful, dplyr provides a simple header if we want a count:

not_cancelled%>% count(dest)

## Which destintations ahve the most carriers?

## can provide a weight variable, for example, you could use this "count" (sum) the total number of miles a plane flew
not_cancelled%>% count(tailnum,wt=distance)

## counts and proportions of logical values: sum(x>10), mean(y==0), when used with numeric functions.
#True is converted to 1 and false to 0. This makes sum() and mean() very useful:
# sum(x) gives number of trues in x, and mean(x) gives the proportion.


## How many flights left before 5 am??

not_cancelled%>% group_by(year,month,day)%>%summarise(n_early=sum(dep_time<500))

## what proportion of flights are delayed by more than an hour??

not_cancelled%>% group_by(year,month,day)%>% summarise(delaying_hour=mean(arr_delay>60))

########### Grouping by multiple Variables #########################

# each summary peels off one level of the grouping, making it easy to roll up a dataset

daily <- group_by(flights,year,month,day)
(per_day <- summarise(daily,flights=n()))

(per_month <- summarise(per_day,flights=sum(flights)))

## and per year
(per_year  <- summarise(per_month, flights = sum(flights)))


#### WHen progressively rolling up summies, its ok for sum/counts, but need to think about weighting means and variances.
# not possible to do it exactly for rank based stat like median.
# sum of groupwise sums is the overall sum, but median of the group wise median is not the overall median.



############# Section 5.6.6 Ungrouping ###################

## if we need to remove group and return to operatons on ungrouped data.
daily %>% ungroup () %>% summarise (flights = n())
## we no longer group by date(month,day,year) and summarise all flights)
#
############### Exercises 5.6.7 ##################

## cost function? flight delays are costly, and want to know how to use this information 


### Exercise 5.6.2 ###########
## Come up with another approach that will give you the same output as 
#not_cancelled %>% count(dest) and not_cancelled %>% count(tailnum, wt = distance) (without using count()).
not_cancelled <-  flights %>% filter(!is.na(dep_delay),!is.na(arr_delay))

## count function counts the number of isntances within each group of variables,
# instead of using count, we can combine group_by() and summarise verbs

## firs tnotice that 
not_cancelled %>% count(dest) #evalutes the number of instances within the group, so it finds all the non cancelled destintation flights.

not_cancelled %>% group_by(dest) %>% summarise(n=n())
## this will give us all obersations in our group data frame.

## Another alternative to count() is to use the combination of the group_by() and tally() verbs. In fact, count() is effectively a short-cut for group_by() followed by tally().

# use group_by() and tally, coun() is a shortcut to group and tally

not_cancelled %>% group_by(tailnum)%>% tally()

## to address the weight argument, can replicate count(), with group_by and summarise.

not_cancelled %>%
  group_by(tailnum) %>%
  summarise(n = sum(distance))


not_cancelled %>% group_by(tailnum) %>% tally(distance)

##
###############
### which is more important? not available dep delay or arriveal delay?
## if it never departs, it wont arrive, arrival delay is imporatn
filter(flights, !is.na(dep_delay), is.na(arr_delay)) %>% select(dep_time, arr_time,sched_arr_time,dep_delay,arr_delay)



######### Exercise 5.6.4 ##############

## number of cancelled flights per day?

canceled_per_day <- flights %>% mutate(cancelled = (is.na(arr_delay) | is.na(dep_delay))) %>% group_by(year,month,day) %>% summarise(cancelled_num=sum(cancelled), flights_num=n(),)


##Our definition of cancelled flights (is.na(dep_delay) | is.na(arr_delay)) is slightly suboptimal. Why? Which is the most important column?

not_cancelled%>% group_by(dest)%>% summarise(n=length(dest))
## number of destinations grouped by non cancelled flights
## recall to get number of observations in a data frame is the function n

## now let us plot flights_nums against canelld to show number of flights cancelled

ggplot(canceled_per_day) + geom_point(aes(x=flights_num,y=cancelled_num))

## 

## lets look at the average departure delay and 

cancelled_and_delays <-  flights %>% mutate(cancelled=(is.na(arr_delay) | is.na(dep_delay)))%>% group_by(year,month,day)%>%summarise(cancelled_pro=mean(cancelled),avg_dep_delay = mean(dep_delay,na.rm=TRUE),avrg_arr_delay=mean(arr_delay,na.rm=TRUE))%>% ungroup()
ggplot(cancelled_and_delays)+ geom_point(aes(x=avg_dep_delay,y=cancelled_pro))

########### Worst delays??
worst_delays <- flights %>% group_by(carrier) %>% summarise(worst_delays_flight=mean(arr_delay,na.rm=TRUE)) %>% arrange(desc(worst_delays_flight))
worst_delays

## what airline is F9?

filter(airlines,carrier =="F9")


## how to disentangle the effects of airpots versus bad carriers?
## compare the average delay of each carrier en route to a destination

#### Excercise 5.6.6 ########
##What does count() do?
## sort arugment to count() sorts the results in order of n. Use this anytime you would run count() followed by arrange()


########## 5.7 Grouped Mutates (and filters) ##################

# Grouping helps the most in conjuction with summarise() but can do operations with mutate() and filter()
## finding the worst members of each group:

flights_sml %>% group_by(year,month,day) %>% filter(rank(desc(arr_delay))<10)

## find all groups bigger than some threshold.

popular_dests <-  flights %>% group_by(dest) %>% filter(n() > 365)
popular_dests

popular_dests %>% 
  filter(arr_delay > 0) %>% 
  mutate(prop_delay = arr_delay / sum(arr_delay)) %>% 
  select(year:day, dest, arr_delay, prop_delay)
### grouped filter is a grouped mutate follwed by an ungrouped filter.
