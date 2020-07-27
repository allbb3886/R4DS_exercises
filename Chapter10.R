########################## Section 10 Tibbles #############################

# using tibbles instead of R's data.frame

library(tidyverse)
## Creating tibbles, a core of tidyverse
## typically R packages use data frames, so to coerce a data frame to a tibble, we use the as_tibble() command

as_tibble(iris)

## can also create new tibbles forom individual vectors !
# using tibble()

tibble(x=1:5,y=1,z=x^2 + y)
### if familiar with data.frame(), note that tibble() does much less!. It never changes type of inputs 
# (never converts strings to factors), and never changes names or never creates row names)
## possible ofr a tibble to have column names that are not valid in R, nonsyntactic names.
# these names might not start with a letter, or they contain unsual characters. 
# to address these variables one ust surrond with backtikes, `
`

tb <- tibble(
  `:)` = "smile", 
  ` ` = "space",
  `2000` = "number"
)
tb

## Another way to create a tibble is with tribble(), short for transposed tibble.
## customised for data entry in code: column headings are defined by forumlas, the start with ~ and entries are separated by commas.
# makes it possible ot lay out small amounts of data in easy to read form
## example:

tribble(~x,~y,~z,
        #----/--/---
        "a", 2,3.6, "b", 1, 8.5)



### Section 10.3 Tibbles vs data.frame ######################
## differences? printing and subsetting:

### Section 10.3.1  Printing ################
# Tibbles create a refined print method showing the first 10 rows and all columns that fit on the screen.

##

tibble ( a=lubridate::now() + runif(1e3) * 86400, 
         b= lubridate::today() + runif(1e3)*30,
         c=1:1e3, 
         d= runif(1e3),
         e=sample(letters,1e3,replace=TRUE)
)

## tibbles are designed that you don't accidentally overwhelm console, 
# if one wishes to see more output, several options avaiable.

## print() the data frame and control the number of rows (n) and the width of the display., if
# width = Inf display all columns

#nycflights13:;flights%>% print(n=10,width = inf), #display all columns, 10 rows.

##options(tibble.print_max = n, tibble.print_min = m): if more than n rows, print only m rows. Use options(tibble.print_min = Inf) to always show all rows.

#Use options(tibble.width = Inf) to always print all columns, regardless of the width of the screen.

# can also use Rstudio's built in data viewer,

nycflights13::flights%>% 
  view()

############### Section 10.3.2 Subsetting ############################

## suppose we want to pull out a single variable, using $ and [[. This can extract by name or position; # only extracts by name

df <-  tibble(x=runif(5)
              , 
              y=rnorm(5))

## then to extract by name :
df$x
## or use
df[["x"]]
## if we want to extract by position
df[[2]]

## if we want to use a pipe, need placeholder, .

df %>% .$x

# or 
df%>% .[["x"]]



########### Section 10.4 Interacting with older code ###################
#older functions dont work with tibbles. Use as.data.frame() to turn a tibble back to a data.frame
class(as.data.frame(tb))

## 

############ 10.5 Exercises ###############

##let us print mtcars

mtcars
## recall that using tibbles shows first ten obserations
as_tibble(mtcars)

## can use is_tibble() function to check if tibble or not.
is_tibble(mtcars)

# but diamonds and flights are
is_tibble(ggplot::flights)
is_tibble(nycflights13::flights)
is_tibble(as_tibble(mtcars))

## use class() function to determine class of an object.
# typically, tibbles have classes("tbl_df","tbl","data.frame"), old data frames
# have only class data.frame

## example 
class(mtcars)

class(ggplot2::diamonds)


## Exercise 10.2 ####################
df <- data.frame(abc = 1, xyz = "a")
df$xy
df[, "xyz"]
df[, c("abc", "xyz")]
class(df)

## compute as a tibble
tbl <- as_tibble(df)
tbl$x

tbl[,"xyz"]

tbl[,c("abc","xyz")]

## $ operator will match any colum name that starts with the name following it.
# with data.frames, with [],. the type of object that is returned differes on the number of columns.
# if it is one column, it wont return a data frame, but will return a vector.


## Excercise 10.3 ########
#can use double brackets from a tibble, since using df$var, will lead to finding a column named var.
#so if you want to extract a variable from a tibble, use df[[var]]

## Exercise 10.4 #########
## create a data set called annoying with two columns
annoying <- tibble(`1`= 1:10,`2`=`1` *2+rnorm(length(`1`)))
## extract variable named 1:
annoying[["1"]]
## or
annoying$`1`

## scattor plot of 1 vs 2:
ggplot(annoying,aes(`1`,`2`)) + geom_point()

## add a new column 3,

mutate(annoying,`3` =`2` / `1`)

## or 
annoying[["3"]] <- annoying$`2` / annoying$`1`

# let us rename columns to one, two and three

annoying <-  rename(annoying,one=`1`, two=`2`, three=`3`)
annoying


## exercise 10.5 ############
## what does tibble::enframe() do?
## converts named vectors to a data frame with names and values

enframe(c(a=1,b=2,c=3))

##### Exercise 10.6 ###########
The help page for the print() method of tibble objects is discussed in ?print.tbl. The n_extra argument determines the number of extra columns to print information for.

