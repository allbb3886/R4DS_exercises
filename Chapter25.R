#### Section 25 Many Models ###############

Using many simple models to better understand complex datasets.

Using list-columns to store arbitrary data structures in a data frame. For example, this will allow you to have a column that contains linear models.

Using the broom package, by David Robinson, to turn models into tidy data. This is a powerful technique for working with large numbers of models because once you have tidy data, you can apply all of the techniques that you've learned about earlier in the book.


library(modelr)
library(tidyverse)
library(gapminder)
##In this case study, we're going to focus on just three variables to answer the question "How does life expectancy (lifeExp) change over time (year) for each country (country)?". A good place to start is with a plot:
  
  gapminder%>%
  ggplot(aes(year,lifeExp,group=country)) + geom_line(alpha=1/3)

##small dataset, lets fit a model with linear trend

nz <- filter(gapminder,country=="New Zealand")
nz%>%
  ggplot(aes(year,lifeExp)) + geom_line() +
  ggtitle("Full data=")

## modeling nz
nz_mod <- lm(lifeExp~year,data=nz)
nz %>% 
  add_predictions(nz_mod) %>%
  ggplot(aes(year, pred)) + 
  geom_line() + 
  ggtitle("Linear trend + ")

nz %>% 
  add_residuals(nz_mod) %>% 
  ggplot(aes(year, resid)) + 
  geom_hline(yintercept = 0, colour = "white", size = 3) + 
  geom_line() + 
  ggtitle("Remaining pattern")




### Section 25.2.1 Nested Data #########

## extract ouit the common code with functino and repeat using map function.
## lets create a new data structure: nested data frame.

by_country <- gapminder %>% 
  group_by(country, continent) %>% 
  nest()

### creates data frame that has one row per group (per country) and a data column,
# data is a lsit of data frames (or tibbles).

by_country$data[[1]]

# to seee a data column from a single element.
# difference between grouped data frame and nested data frame,
#in a GDF, each row is a observation, NDF, each row is a group.
# a nested data set is a emta-observation, a row that represents the complete time course fora  country, rather than a single point in time.

### Section 25.2.2 List Columns ############

country_model <- function(df) {
  lm(lifeExp ~ year, data = df)
}

## now we apply it to every data frame, using map()
models <- map(by_country$data,country_model)

by_country <- by_country %>% 
  mutate(model = map(data, country_model))
by_country


## this stores the objects together.
by_country%>%
  filter(continent=="Europe")

by_country %>% 
  arrange(continent, country)


#### Unnesting Section 25.2.3

## computed the residuals of a single model with a singel data set, we ahve 142 data frames and 142 cmodels.

by_country <- by_country %>% 
  mutate(
    resids = map2(data, model, add_residuals)
  )
# this adds residuals to each model-data pair

## how to plot a list of data frames?
#instead of struggling to answer that question, turn the list of data frames back into regular data frame
# we used nest() to turn regular into nested, now we do opposite with unnest():

resids <- unnest(by_country, resids)
resids

##each regular column is repeated one for each row in teh nested column.


# we have a regular data frame, now plot residuals.

resids %>% 
  ggplot(aes(year, resid)) +
  geom_line(aes(group = country), alpha = 1 / 3) + 
  geom_smooth(se = FALSE)


resids %>% 
  ggplot(aes(year, resid, group = country)) +
  geom_line(alpha = 1 / 3) + 
  facet_wrap(~continent)
## large residuals in Africa, model doesn't explaion it well.


#Section 25.2.4 Model Quality

#using Broom package, provides general set of functions to turn models into tidy data.
#instead of looking at residuals.

##applying broom to a single model we get a data frame with a single row.

#use mutate() and unest() to create a data frame with a row for each country.
broom::glance(nz_mod)

by_country%>%
  mutate(glance=map(model,broom::glance)) %>%
  unest(glance)

# still includes list columns, default behaviour of unnest(), use .drop=TRUE

glance <- by_country %>% 
  mutate(glance = map(model, broom::glance)) %>% 
  unnest(glance, .drop = TRUE)

##look for models that don't fit well.

glance %>% 
  arrange(r.squared)
#worst model appears to be africa.
## small observations and discrete variable, use geom_jitter()

glance %>% 
  ggplot(aes(continent, r.squared)) + 
  geom_jitter(width = 0.5)


##plotting a country with bad R squared value and plot the data.

bad_fit <- filter(glance, r.squared < 0.25)

gapminder %>% 
  semi_join(bad_fit, by = "country") %>% 
  ggplot(aes(year, lifeExp, colour = country)) +
  geom_line()

## two effects, tragedies of HIV/AIDS and Rwanda genocide.

##### Exercises 25.2.5 ###########################
gapminder %>%
  group_by(country, continent) %>%
  nest() %>%
  mutate(model = map(data, ~ lm(lifeExp ~ year, .))) %>%
  mutate(glance = map(model, broom::glance)) %>%
  unnest(glance) %>%
  unnest(data) %>%
  filter(r.squared < 0.25) %>%
  ggplot(aes(year, lifeExp)) +
  geom_line(aes(color = country))



#################################################
library("ggbeeswarm")
by_country %>%
  mutate(glance = map(model, broom::glance)) %>%
  unnest(glance, .drop = TRUE) %>%
  ggplot(aes(continent, r.squared)) +
  geom_beeswarm()


######## Section 25.3 List Columns #############

# list columns are implicit in the defintion of the data frame:
data frame is naemd lsit fo equal length vectors, a list is a vector, so 
# normal to use list as a column of data frame.

data.frame(x=list(1:3,3:5))

# can prevent this with I(), but the result doesn't print well

data.frame(
  x=I(list(1:3,3:5)),
  y=c("1,2","3,4,5")
  
)


## tibbles alleviate this problem:
tibble(
  
  x=I(list(1:3,3:5)),
  y=c("1,2","3,4,5")
  
)

## tribble makes this even easier !

tribble(
  ~y,~y,
  1:3,  "1, 2",
  3:5, "3,4,5"
)

##list columns are useful as intermediate data structure. Hard to work with directly,
#because most R functions work with atomic vectors or data frames.

## three parts of an effeective list-column pipeline:
# create the list column, using one of nest(), summarise() +list() or mutate + a map function 
# create other intemediate list columns by transforming existing list columns with map(), map2() or pmap()
# simplify the list-column back down to a data frame or atomic vector.


#### Section 25.4 Creating list-columns

##typically won't create list-columns with tibble(), create them from regular columns, using one of three methods
#With tidyr::nest() to convert a grouped data frame into a nested data frame where you have list-column of data frames.

With mutate() and vectorised functions that return a list.

With summarise() and summary functions that return multiple results.

# alternative could be create from a named list, tibble::enframe().

### Section 25.4.1 With Nesting

nest() creates nested data frame, which is a data frame with a list-column of data frames.
#In a nested data frame each row is a meta-observation: the other columns give variables that define the observation (like country and continent above), and the list-column of data frames gives the individual observations that make up the meta-observation.

There are two ways to use nest(). So far you've seen how to use it with a grouped data frame. When applied to a grouped data frame, nest() keeps the grouping columns as is, and bundles everything else into the list-column:
  
  gapminder%>%
  group_by(country,continent) %>%
  nest()

# can also use it on ungrouped data frame, specifying which columns to nest:

gapminder %>% 
  nest(year:gdpPercap)


## Section 25.4.2 From Vectorised Functions:
#Some useful functions take an atomic vector and return a list. For example, in strings you learned about stringr::str_split() which takes a character vector and returns a list of character vectors. If you use that inside mutate, you'll get a list-column:
  df <- tribble(
    ~x1,
    "a,b,c", 
    "d,e,f,g"
  ) 

df %>% 
  mutate(x2 = stringr::str_split(x1, ","))

#unnest() can handle these vectors
df%>% 
  mutate(x2=stringr::str_split(x1,","))  %>%
  unnest()

## Another example of this is using map(), map2() function from purr

## taken the final example from invoking different functions, and rewrite using mutate()

sim <- tribble(
  ~f,      ~params,
  "runif", list(min = -1, max = 1),
  "rnorm", list(sd = 5),
  "rpois", list(lambda = 10)
)

sim %>%
  mutate(sims=invoke_map(f,params,n=10))

## sim isnt homogenous, contains both double and integer.


##Section 25.4.3 From multivalued summaries

## restrictions to summarise(), only works with summary fucntions taht return a single value, like functions like quantile()

mtcars %>% 
  group_by(cyl) %>% 
  summarise(q = quantile(mpg))

## wrap the result in a list. since this makes each summary a list of length 1
mtcars %>% 
  group_by(cyl) %>% 
  summarise(q = list(quantile(mpg)))

## capture probabilies also.
probs <- c(0.01, 0.25, 0.5, 0.75, 0.99)
mtcars %>% 
  group_by(cyl) %>% 
  summarise(p = list(probs), q = list(quantile(mpg, probs))) %>% 
  unnest()


#Section 25.4.4  From a named list

## data frames with list columns provide a solution to common problem, what if we want to tierate over contents of a list and its elemnts.

# easy way to create a data frame from a list using tibble::enframe()

x <- list(
  a = 1:5,
  b = 3:4, 
  c = 5:6
) 

df <- enframe(x)
df

## will show character name that holds a list of integers
## this generalises to ahve useful names for cahracter vector.

#to iteratoe over names and values in parallel use map2()

df %>% 
  mutate(
    smry = map2_chr(name, value, ~ stringr::str_c(.x, ": ", .y[1]))
  )

##########################Exercsies 24.4.5 ###############################
#many functions in stringr package take a character vector and return a list :
str_split(sentences[1:3], " ")

str_match_all(c("abc", "aa", "aabaa", "abbbc"), "a+")

he map() function takes a vector and always returns a list.
map(1:3, runif)

#########################################################################




###############Section 25.5 Simplifying list-columns###########


## need to simplify the list-column back to a regular column, or set of columns, 
## if u want a singel value, use mutate() with map_lgl(), map_int(), map_dbl() and map_chr() to make a atomic vector
#  you want many values, use unnest() to convert list-columns back to regular columns, repeating the rows as many times as necessary.

## Section 25.5.1 List to Vector 
If you can reduce your list column to an atomic vector then it will be a regular column. For example, you can always summarise an object with its type and length, so this code will work regardless of what sort of list-column you have:
  
  
  df <- tribble(
    
    ~x,
    letters[1:5],
    1:3,
    runif(5)
  )

df%>% mutate(type=map_chr(x,typeof),
             length=map_int(x,length)
)

## same info from default tbl print, now can filter it if we have hetergeonous list.

## don't forget about the map_*() shjortcuts, like map_chr(x,"apple") to extract the string stored in apple for each element of x.
# use the .null argument to provide a value to use if element is missing (instead of returning NULL):
# useful for pulling apart nested lists into regular columns.
df <- tribble(
  ~x,
  list(a = 1, b = 2),
  list(a = 2, c = 4)
)
df %>% mutate(
  a = map_dbl(x, "a"),
  b = map_dbl(x, "b", .null = NA_real_)
)

##Section 25.5.2 Unnesting

## works by repeating the regular columns once for each element of list column.
# For example, int the following very simple example we repeat the first row 4 times (because there the first element of y has length four, second row once:)

tibble(x = 1:2, y = list(1:4, 1)) %>% unnest(y)

## cannot simulatenously unnest two columns that contain different number of elements:

df1 <- tribble(
  ~x, ~y,           ~z,
  1, c("a", "b"), 1:2,
  2, "c",           3
)
df1

df1 %>% unnest(y, z)
# doesnt work since y and z have differen # of elements.

df2 <- tribble(
  ~x, ~y,           ~z,
  1, "a",         1:2,  
  2, c("b", "c"),   3
)
df2

df2 %>% unnest(y, z)



##################### Exercsies 25.5.3 #############################



##########################################################

##Section 25.6 Making tidy data with broom

The broom package provides three general tools for turning models into tidy data frames:
  
  broom::glance(model) returns a row for each model. Each column gives a model summary: either a measure of model quality, or complexity, or a combination of the two.

broom::tidy(model) returns a row for each coefficient in the model. Each column gives information about the estimate or its variability.

broom::augment(model, data) returns a row for each row in data, adding extra values like residuals, and influence statistics.

map(1:3,runif)


range(mtcars$mpg)
#> [1] 10.4 33.9
fivenum(mtcars$mpg)
#> [1] 10.4 15.3 19.2 22.8 33.9
boxplot.stats(mtcars$mpg)

## whats missing in data frame?

mtcars%>%
  group_by(cyl) %>%
  summarise(q=list(quantile(mpg))) %>%
  unnest()

## missing values of quantiles 0%, 25%, 50%,75%, 100%

quantile(mtcars$mpg)

#because unnest function drops the names of vectors.

The lengths() function returns the lengths of each element in a list. It could be useful for testing whether all elements in a list-column are the same length. You could get the maximum length to determine how many atomic vector columns to create. It is also a replacement for something like map_int(x, length) or sapply(x, length).

##all types of vectors in data frames are atomic, lists are not atomic since they contain other lists and vectors !

####################