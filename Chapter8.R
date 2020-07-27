############ Section 8 Exploratory Data Analysis############ library(dplyr)
library(dplyr)
library(tidyverse)
library(nycflights13)
library("ggstance")
library("hexbin")
library(faithful)
library(modelr)
## Questions to consider when analysis data?
## What type of variation occurs within my variables?
## what type of covaraitions occurs?


### Section 7.3.1 Visualising Distributions ###############

## Categorical variable?? 
## when it can only take one of a small set of values, typically factors or character vectors.

ggplot(data=diamonds) + geom_bar(mapping=aes(x=cut))

diamonds %>% count(cut) ## count the number of observations in each category for cut

## Continous Variable ########
## can take any infinite set of rodered values. numbers and date times are two exmaples of continous variables
## use a histogram to examine distribution of continous variables

ggplot(data=diamonds) + geom_histogram(mapping = aes(x=carat),binwidth=0.5)
## by combinding dplyr::count() and ggplot2::cut_width()

diamonds %>% count(cut_width(carat,0.5))

## histograms divides x axis into equally spaced bins and uses height to display number of observations in each bin.

ggplot(data=smaller,mapping = aes(x=carat,colour=cut)) + geom_freqpoly(binwidth=0.1)



################ 7.3.3 Unusual Values #######################

# unusual values :
ggplot(diamonds) + geom_histogram(mapping=aes(x=y),binwidth=0.5)

## need to examine the smaller values so zoom in on coordiates

ggplot(diamonds) + geom_histogram(mapping =aes(x=y), binwidth = 0.5) + coord_cartesian(ylim=c(0,50))

#(coord_cartesian() also has an xlim() argument for when you need to zoom into the x axis.)

## Three outleirs, 0 ,30 ,60. Let us examine is more thoroughly.

unusual <-  diamonds %>% filter(y<3 | y>20) %>% select(price,x,y,z) %>% arrange(y)
unusual

## examine to see the discrepancies, notice that y measures diamonds in mm, 
# the price of these diamonds cost way too low for large diamonds.


## Exercise 7.3.1 ################
summary(select(diamonds,x,y,z))

ggplot(diamond) + geom_histogram(mapping=aes(x=x),binwidth=0.01)
ggplot(diamond) + geom_histogram(mapping=aes(x=y),binwidth=0.01)
ggplot(diamond) + geom_histogram(mapping=aes(x=z),binwidth=0.01)

## check for diamonds that are potential outliers = 0.

filter(diamonds, x==0|y==0|z==0)

diamonds %>% arrange(desc(y)) %>% head()

## consider multivariate outliers

ggplot(diamonds, aes(x=x,y=y)) + geom_point()
ggplot(diamonds, aes(x=x,y=z)) + geom_point()


### Removing the outliers from x,y,z we can see the distribution.

filter(diamonds,x>0,x<10) %>% ggplot() + geom_histogram(mapping = aes(x=x), binwidth = 0.01) +
  scale_x_cotinous (breaks=1:10)


####### Exercise 7.3.2 #################################
## examine distribution of price.

ggplot(filter(diamonds, price<3000),aes(x=price)) + geom_histogram(binwidth=100)

### Exercise 7.3.3 #########################

diamonds %>% filter(carat>=0.99, carat<=1) %>% count(carat)



################# 7.4 Missing Values ############################

## can drop the entire row of strange values :
diamonds2 <- diamonds %>% filter(between(y,3,20))
## not a good idea, measurements are important if only 1 outlier.

# replace unusual values with missing values instead using mutate()

diamonds2 <- diamonds %>% mutate(y=ifelse(y<3|y>20,NA,y))

## three arguments to ifelse(), the test in first argument or logical argument, result for the 2nd argument (NA), third is the argument for when it is false.

ggplot(data=diamonds2,mapping=aes(x=x,y=y)) + geom_point()


## can surpress the warning about 9 rows missing values using na.rm=TRUE

ggplot(data=diamonds2,mapping=aes(x=x,y=y)) + geom_point(na.rm=TRUE)

# what makes observations with missing values different to observations with recorded values?

## want to compare cancelled flights with non canclled times.

nycflights13::flights%>% mutate(cancelled=is.na(dep_time),sched_hour=sched_dep_time %/% 100, sched_min=sched_dep_time %% 100, sched_dep_time=sched_hour+sched_min/60) %>% ggplot(mapping=aes(sched_dep_time))+geom_freqpoly(mapping=aes(colour=cancelled),binwidth=1/4)

#Examing the cases where flights are cancelled over true. However, this difference is too large for non cancelled flights.


## Exercise 7.4.1 #######

#mising values in a histogram are removed in each bin, in geom_bar, treated as a nother category.

## what does na.rm= TRUE do in mean() and sum()? It removes NA values before caluclating.






#####################7.5 Covariation ############################

## variation between variables is called covariations, how variables move between each other.
# look at the relationship between two or more variables.

##
## A categorical and continous variable
## common to explore distribution of continous variable broken down
# by a categorical variable.  # geom_freqpoly() not that useful for this comparison, 
#since height is given by count.

ggplot(data=diamonds,mapping=aes(x=price))+geom_freqpoly(mapping=aes(colour=cut),binwidth=500)

## how does price of a diamond varie with quality.
## count is too high.

ggplot(diamonds)+geom_bar(mapping=aes(x=cut))

## display density instead of count

ggplot(data=diamonds,mapping=aes(x=price,y=..density..)) + geom_freqpoly(mapping=aes(colour=cut),binwidth =500)
## notice fair diamonds have the highest average price.

## can consider a boxplot, a visual distribution of values.

## boxplots consists of a box that strethces 25th percentile to 75th, known as the IQR. middle of the box is a line that displays median.

## visual points that display observations that fall more than 1.5 times the IQR, typically outliers.
# line that extends form each end of the box goes to furthest non-outlier point.
ggplot(data=diamonds,mapping=aes(x=cut,y=price))+geom_boxplot()
## less info on distribution, but can compare more easily.
## vbetter quality diamonds are cheaper on average! why?

## note cut is an ordered factor, so we can reorder() it

ggplot(data=mpg,mapping=aes(x=class,y=hwy))+geom_boxplot()

## now we can see how highway mileage varies across classes

## let us reorder this based on median values

ggplot(data=mpg)+geom_boxplot(mapping=aes(x=reorder(class,hwy,FUN=median),y=hwy))

## notice long names, can do a coord(flip)

ggplot(data=mpg)+geom_boxplot(mapping=aes(x=reorder(class,hwy,FUN=median),y=hwy)) + coord_flip()



########## Excercises 7.5.1.1###################


nycflights13::flights%>% mutate(cancelled = is.na(dep_time), sched_hour=sched_dep_time %/% 100, sched_min=sched_dep_time %%100,sched_dep_time = sched_hour+sched_min / 60) %>% ggplot() + geom_boxplot(mapping=aes(y=sched_dep_time, x=cancelled))

############# Exercise 7.5.1.2 ################

ggplot(diamonds,aes(x=carat,y=price))+geom_point()

## not helpful with so many points

ggplot(data=diamonds,mapping=aes(x=carat,y=price)) + geom_boxplot(mapping=aes(group=cut_width(carat,0.1)))

## relationship between color and price!

diamonds %>% mutate(color=fct_rev(color)) %>% ggplot(aes(x=color,y=price)) + geom_boxplot()
#weak relationship between price and clarity (negative)

## carat seems to be the best indicator for prices

ggplot(diamonds,aes(x=cut,y=carat)) + geom_boxplot()


## larger diamonds have a poor cut ("fair"), smaller diamonds reuqires a better cut.


## Excercise 7.5.1.3 ###
ggplot(data = mpg) +
  geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy)) +
  coord_flip()

ggplot(data = mpg) +## same thing but x and y are flipped.
  geom_boxploth(mapping = aes(y = reorder(class, hwy, FUN = median), x = hwy))



########## Excerise 7.5.1.4 ########
#problem with box plots is that they were developed in an era where small data is common, so larger numbers may have outlying values. Consdier a letter value plot.

ggplot(diamonds, aes(x = cut, y = price)) +
  geom_lv()


### Exercise 7.5.1.5 ###########
## geom_freqpoly() is better for looking up data, so given a price, it is easy to tell which cut has highest density. #overlapping lines make iot difficult to distinguish




########Excerise 7.5.1.6 ############

## small dataset, useful to use geom_jitter() to see the relatioship between continous and categorical
#geom_quasirandom() produces plots that are a mix of jitter and violin plots. There are several different methods that determine exactly how the random location of the points is generated.
#geom_beeswarm() produces a plot similar to a violin plot, but by offsetting the points.


########### Section 7.5.2 Two Categorical Variables ##################
#To visualise the covariation between categorical variables, 
#you'll need to count the number of observations for each combination. One way to do that is to rely on the built-in geom_count():

ggplot(data=diamonds)+ geom_count(mapping=aes(x=cut,y=color))

## size of each circle in plot shows observations occured at each observations

## covaritain will appear as a strong correlation between speciufic x and y values.

## can also count with dplyr:
diamonds %>% count(color,cut)

# then visualise with geom_tile() and fill aesthetic

diamonds %>% count(color,cut) %>% ggplot(mapping=aes(x=color,y=cut)) + geom_tile(mapping=aes(fill=n))




############# Section 7.5.3 Two Continuous Variables ###############
## scatterplots with geom_point() are good to see covaraition.

ggplot(data = diamonds) +
  geom_point(mapping = aes(x = carat, y = price))

## scatterplots become less useful as size of data grows, since points overplot

# how to address the overlapping points and pile up?

## using the alpha aesthetic to add transparency
ggplot(data=diamonds)+geom_point(mapping=aes(x=carat,y=price),alpha=1/100)
# challening for very large datasets. Could use bins as geom_histogram() and geom_freqpoly().

## now learn to use geom_bin23() and geom_hex() to bin two dimesnsions
## geom_bin2d() and geom_hex() divide the coordinate plane into 2d bins and then use a fill color to display how many points fall into each bin. geom_bin2d() creates rectangular bins. geom_hex() creates hexagonal bins. You will need to install the hexbin package to use geom_hex().

##
ggplot(data = smaller) +
  geom_bin2d(mapping = aes(x = carat, y = price))

ggplot(data = smaller) +
  geom_hex(mapping = aes(x = carat, y = price))




############### Patterns and Models Section 7.6 ################

ggplot(data = faithful) + 
  geom_point(mapping = aes(x = eruptions, y = waiting))
## understanding covaraition important to identify the values of each variable.
## use models to extract patterns out of data.
##
mod <-  lm(log(price)~log(carat),data=diamonds)

diamonds2 <- diamonds%>%add_residuals(mod)%>%mutate(resid=exp(resid))

ggplot(data=diamonds2) + geom_point(mapping=aes(x=carat,y=resid))

ggplot(data=diamonds2) + geom_boxplot(mapping=aes(x=cut,y=resid))



############ Section 7.7 ggplot2 calls ##########

# prior we used the full expression for ggplot function
ggplot(data = faithful, mapping = aes(x = eruptions)) + 
  geom_freqpoly(binwidth = 0.25)

## where the first two arugments are important, data, mapping = aes(x, or y)
## we will now refrain from typing the mapping portion and data,

ggplot(faithful, aes(eruptions)) + 
  geom_freqpoly(binwidth = 0.25)

# similar to above, and much faster
## transition from %>% to +

diamonds %>% count(cut,clarity) %>% ggplot(aes(clairty,cut,fill=n)) + geom_tile()

######### end of chapter ###
