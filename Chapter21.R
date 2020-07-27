##Section 21_Iteration #########
library(tidyverse)
library("stringr")
library("microbenchmark")
### Section 21.2 For Loops

## consdier this tibble:
df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

# want to compute the median of each column.
## can do this copy-paste:

median(df$a)
#> [1] -0.246
median(df$b)
#> [1] -0.287
median(df$c)
#> [1] -0.0567
median(df$d)
#> [1] 0.144

# but never copy paste more than twice##

## use a for loop instead !

output <- vector("double", ncol(df)) # output
for (i in seq_along(df)) { # sequence
  output[[i]] <-  median(df[[i]]) # body
}
output

## components of every loop.

The output: output <- vector("double", length(x)). Before you start the loop, you must always allocate sufficient space for the output. This is very important for efficiency: if you grow the for loop at each iteration using c() (for example), your for loop will be very slow.

A general way of creating an empty vector of given length is the vector() function. It has two arguments: the type of the vector ("logical", "integer", "double", "character", etc) and the length of the vector.

# 2: The sequence: i in seq_along(df). This determines what to loop over: each run of the for loop will assign i to a different value from seq_along(df). It's useful to think of i as a pronoun, like "it".

You might not have seen seq_along() before. It's a safe version of the familiar 1:length(l), with an important difference: if you have a zero-length vector, seq_along() does the right thing:
  
  # 3: The body: output[[i]] <- median(df[[i]]). This is the code that does the work. It's run repeatedly, each time with a different value for i. The first iteration will run output[[1]] <- median(df[[1]]), the second will run output[[2]] <- median(df[[2]]), and so on.

That's all there is to the for loop! Now is a good time to practice creating some basic (and not so basic) for loops using the exercises below. Then we'll move on some variations of the for loop that help you solve other problems that will crop up in practice.


## Exercises 21.2.1
## compute meanof verry column in mtcars

output <- vector("double",ncol(mtcars))
names(output) <- names(mtcars)
for (i in names(mtcars)) {
  output[i] <- mean(mtcars[[i]])
}
output
mtcars

## determine the typ e of each column in nycflights13

output <- vector("list",ncol(nycflights13::flights))
names(output) <- names(nycflights13::flights)
for(i in names(nycflights13::flights)) {
  output[[i]] <- class(nycflights13::flights[[i]])
  
}
output


## Compute number of unique values in each column of iris dataset:
data("iris")
iris_uniq <- vector("double",ncol(iris))
names(iris_uniq) <- names(iris)
for ( i in names(iris)) {
  iris_uniq[i] <- length(unique(iris[[i]]))
}
iris_uniq


## generate 10 rabndom normals for each of u

n <- 10
mu <- c(-10,0,10,100)
normals <- vector("list",length(mu))
for (i in seq_along(normals)){
  normals[[i]] <- rnorm(n,mean=mu[i])
}
normals

matrix(rnorm(n * length(mu), mean = mu), ncol = n)
}


##Exercises: Remove the for loop if necessary.

x <- sample(100)
sd. <- 0
for (i in seq_along(x)) {
  sd. <- sd. + (x[i] - mean(x))^2
}
sd. <- sqrt(sd. / (length(x) - 1))
sd.


## lets use sd function 
sd(x)

# or just use the existing mean and sum functions
sqrt(sum((x - mean(x))^2) / (length(x) - 1))


## this counts the cumulative sum
x <- runif(100)
out <- vector("numeric", length(x))
out[1] <- x[1]
for (i in 2:length(x)) {
  out[i] <- out[i - 1] + x[i]
}
out


## exercise 21.2.3

## iterate from five to no jumps:
humps <- c("five","four","three","two","one","no")

for (i in humps){
  
  cat(str_c("Alice the camel has ", rep(i,3), "hump".,
            collapse="\n"
  ),"\n")
  if (i=="no") {
    cat("Now Alice is a horse.\n")
  } else {
    cat("So go, Alice, go.\n")
  }
  cat("\n")
}

## lyrics for Ten in the Bed:
numbers <- c(
  "ten", "nine", "eight", "seven", "six", "five",
  "four", "three", "two", "one"
)

for (i in numbers) {
  cat(str_c("There were ", i, " in the bed\n"))
  cat(" and the little one said\n")
  if (i=="one"){
    cat ("I'm lonely..")
  } else {
    cat("Roll over, roll over\n")
    cat("So they all rolled over and one fell out.\n")
  }
  cat("\n")
}

bottles <- function(n){
  if(n>1){
    str_c(n,"bottles")
  } else if (n==1) {
    "1 bottle"
  } else{
    "no more bottles"
  }
}

beer_bottles <-  function(total_bottles){
  ## print each lyric
  for (current_bottles in seq(total_bottles,0)){
    # first line
    cat(str_to_sentence(str_c(bottles(current_bottles)," of beer on the wall,", bottles(current_bottles," of beer. \n")))
        ## second line
        if (current_bottles>0) {
          cat(str_c(
            "Take one down and apss if around, ", bottles(current_bottles-1),
            " of beer on the wall\n"
          ))
        } else{
          cat(str_c("Go to the store and buy some more, ", bottles(total_bottles), " of beer on the wall \n".
          ))
        }
        cat("\n")
  }
}
beer_bottles(3)

### Ignore for now ##

## Exercise 21.2.4

## common to see for loops that don't preallocate the output

output <- vector("integer", 0)
for (i in seq_along(x)) {
  output <- c(output, lengths(x[[i]]))
}
output

## affects performance

## section 21.3 For Loops Variations
## four variations on the theme for loop:
Modifying an existing object, instead of creating a new object.
Looping over names or values, instead of indices.
Handling outputs of unknown length.
Handling sequences of unknown length.
## Section 21.3.1 Modifying an existing object:

## using a loop to modify an object, recall challenge from functions:
df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)
rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}

df$a <- rescale01(df$a)
df$b <- rescale01(df$b)
df$c <- rescale01(df$c)
df$d <- rescale01(df$d)

## Whats output, sequence, body?
# output is same as input,
#sequence, think about a data framse as a list of columns, so iterate over each column with seq_along(df)
#body? apply rescale01()
# 
for (i in seq_along(df)) {
  df[[i]] <- rescale01(df[[i]])
}


## Section 21.3.2 Looping Patterns:

# Three ways to loop over vector, so far we have seen the most general, looping over numeric indices with for (i in seq_along(xs))
# two other forms:
# loop over the elements, for(x in xs), if we want side effects this is it.

## loop over the names, for(nm in names(xs)). This gives you name, which can use to access the value with x[[nm]]. 
# useful if we want to use name in plot title.


results <- vector("list", length(x))
names(results) <- names(x)

for (i in seq_along(x)) {
  name <- names(x)[[i]]
  value <- x[[i]]
}


## Section 21.3.3 Unknown output length

# tempted to find random vectors of random length.

means <- c(0, 1, 2)

output <- double()
for (i in seq_along(means)) {
  n <- sample(100, 1)
  output <- c(output, rnorm(n, means[[i]]))
}
str(output)

## better solution to save results in a list, then combine into single vector after loop is done:

out <- vector("list", length(means))
for (i in seq_along(means)) {
  n <- sample(100, 1)
  out[[i]] <- rnorm(n, means[[i]])
}
str(out)


#You might be generating a long string. Instead of paste()ing together each iteration with the previous, save the output in a character vector and then combine that vector into a single string with paste(output, collapse = "").

#You might be generating a big data frame. Instead of sequentially rbind()ing in each iteration, save the output in a list, then use dplyr::bind_rows(output) to combine the output into a single data frame.

#Section 21.3.4 Unknown sequence length
# can also use while loop, if unsure about input sequence should run for.
# while loop is simpler than for loop, only two components, a condition and body:

while(condition) {
  # body
}

# while loop is more general than for loop, cause one can rewrite any for loop as a while loop.

for (i in seq_along(x)) {
  # body
}

# Equivalent to
i <- 1
while (i <= length(x)) {
  # body
  i <- i + 1 
}


## while loop to find how many tries to get three heads in a row

flip <- function() sample(c("T","H"),1)

flips <- 0
nheads <- 0
while(nheads<3){
  if(flip() == "H") {
    nheads <- nheads+1
  } else {
    nheads <- 0
  }
  flips <- flips + 1
}
flips

}
}
}
}


## Exercise 21.3.1
files <- dir("data/", pattern = "\\.csv$", full.names = TRUE)

## number of files is known
df_list <- vector("list",length(files))
## need to read each file into a data frame, assign it to an element in that list.
#result is a list of frames.

for (i in seq_along(files)) {
  df_list[[i]] <-  read_csv(files[[i]])
}

# can use bind_rows to combine list of data frames into single

df <- binds_rows(df_list)

print(df)

# An alternative could be to pre-allocate a list with the names of the files:

df2_list <- vector("list", length(files))
names(df2_list) <- files
for (fname in files) {
  df2_list[[fname]] <- read_csv(fname)
}


## Exercise 21.3.2

#What happens if you use for (nm in names(x)) and x has no names? What if only some of the elements are named? What if the names are not unique??

x <- c(11, 12, 13)
print(names(x))
#> NULL
for (nm in names(x)) {
  print(nm)
  print(x[[nm]])
}
# runs 0 iterations if no names for the vector.

#length of null is 0.
# if some names, an error happens for trying to access an element without a name
x <- c(a=11, 12, c=13)
names(x)

for (nm in names(x)) {
  print(nm)
  print(x[[nm]])
}

# if vector contains duplicates, then x[[nm]] returns the first element with hat name.
}
}

### Exercise 21.3.3
## 
#Write a function that prints the mean of each numeric column in a data frame, along with its name.
#For example, show_mean(iris) would print:

show_mean(iris)
# > Sepal.Length: 5.84
# > Sepal.Width:  3.06
# > Petal.Length: 3.76
# > Petal.Width:  1.20


show_mean <- function(df, digits = 2) {
  # Get max length of all variable names in the dataset
  maxstr <- max(str_length(names(df)))
  for (nm in names(df)) {
    if (is.numeric(df[[nm]])) {
      cat(
        str_c(str_pad(str_c(nm, ":"), maxstr + 1L, side = "right"),
              format(mean(df[[nm]]), digits = digits, nsmall = digits),
              sep = " "
        ),
        "\n"
      )
    }
  }
}
show_mean(iris)


## Exercise 21.3.4
## waht does this do?
trans <- list( 
  disp = function(x) x * 0.0163871,
  am = function(x) {
    factor(x, labels = c("auto", "manual"))
  }
)
for (var in names(trans)) {
  mtcars[[var]] <- trans[[var]](mtcars[[var]])
}


## mutates the disp and am columns:
#disp is multipled by 0.0163871.
# am is replaced by factor variable.

## loops over named list of functions, it calls the named function in the list on the column of mtcars.
## a function: trans[["disp"]]
# this applies the function to the column of mtcars
mtcars[[var]] <- trans[[var]](mtcars[[var]])


## section 21.4 For loops vs Functionals:
# For loops not that important in r as other languages, since R is a functional programming.
#possible to wrap up for loops in a function, call that function instead of using the for loop directly.

## consider this example:

df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

# recall we wanted to compute mean of every column

output <- vector("double",length(df))
for (i in seq_along(df)) {
  output[[i]] <- mean(df[[i]])
}
output

## lets extract into a function instead:

col_mean <- function(df) {
  output <- vector("double",length(df))
  for (i in seq_along(df)) {
    output[i] <-  mean(df[[i]])
  }
  output
}

col_median <- function(df) {
  output <- vector("double",length(df))
  for (i in seq_along(df)) {
    output[i] <-  median(df[[i]])
  }
}
col_sd <- function(df) {
  output <- vector("double", length(df))
  for (i in seq_along(df)) {
    output[i] <- sd(df[[i]])
  }
  output
}
}


## we copied pasted twice not good!

f1 <- function(x) abs(x - mean(x)) ^ 1
f2 <- function(x) abs(x - mean(x)) ^ 2
f3 <- function(x) abs(x - mean(x)) ^ 3
## notice lots of duplication!!

f <- function(x,i) abs(x-mean(x)) ^ i

# now we reduced chance of bungs, and easy to gernalise to new situations

## recall the col_summary and mean functions:
col_summary <-  function(df,fun) {
  out <- vector("double",length(df))
  for(i in seq_along(df)) {
    out[i] <- fun(df[[i]])
  }
  out
}

col_summary(df, median)
#> [1] -0.5185  0.0278  0.1730 -0.6116
col_summary(df, mean)
#> [1] -0.326  0.136  0.429 -0.250

# idea of a function to another function is powerful.
## purr packaages provides functions that eliminate the need for many for loops.
#How can you solve the problem for a single element of the list? Once you've solved that problem, purrr takes care of generalising your solution to every element in the list.

If you're solving a complex problem, how can you break it down into bite-sized pieces that allow you to advance one small step towards a solution? With purrr, you get lots of small pieces that you can compose together with the pipe.


### Section 21.5 Map functions:
## coomon of looping over a vector, doing something to each element and saving results,
# so purr package provides family of functions to do it.

map() makes a list.
map_lgl() makes a logical vector.
map_int() makes an integer vector.
map_dbl() makes a double vector.
map_chr() makes a character vector.

## fucntions take a vector as an input, applies a function to each piece,
# then returns a new vector thats the same length (same names) as the input.
# functions allow for easier code to write and read.

## we can even use these functions for the same computations as the last for loop.
map_dbl(df, mean)
map_dbl(df, median)
map_dbl(df, sd)
# focus is on the operation being performed.

df%>% map_dbl(mean)

df %>% map_dbl(median)

df %>% map_dbl(sd)

## differences between map_*() and col_summary():
All purrr functions are implemented in C. This makes them a little faster at the expense of readability.

The second argument, .f, the function to apply, can be a formula, a character vector, or an integer vector. You'll learn about those handy shortcuts in the next section.

map_*() uses . ([dot dot dot]) to pass along additional arguments to .f each time it's called:

  map_dbl(df, mean, trim = 0.5)

# map function also preserves names:

z <- list(x=1:3, y=4:5)
map_int(z,length)

######### Section 21.5.1 Shortcuts.

  
## can use .f in order to save little typing, suppose we want to fit a linear model for each group

models <- mtcars%>%
  split(.$cyl) %>%
  map(function(df) lm(mpg~wt,data=df))

# here . preresents as a pronoun, it referes to the current list element (same way i is referred to current index in for loop)

models <- mtcars %>% 
  split(.$cyl) %>% 
  map(~lm(mpg ~ wt, data = .))

## want to extract summary stats like R2.
models%>%
  map(summary) %>%
  map_dbl(~.$r.squared)

# an even easier way to extract named componenets is the following:

models%>%
  map(summary) %>%
  map_dbl("r.squared")
# can also use an integer to select elements by position.

x <- list(list(1, 2, 3), list(4, 5, 6), list(7, 8, 9))
x %>% map_dbl(2)

## Section 21.5.2 Base R

## noticed similarities with purrr functions:
lapply() is identical to map(), except map() is consistent with all other functions in purr, and shortcuts for .__C__data.frame
Base sapply() is a wrapper around lapply() that automatically simplifies the output. This is useful for interactive work but is problematic in a function because you never know what sort of output you'll get:
  
  x1 <- list(
    c(0.27, 0.37, 0.57, 0.91, 0.20),
    c(0.90, 0.94, 0.66, 0.63, 0.06), 
    c(0.21, 0.18, 0.69, 0.38, 0.77)
  )
x2 <- list(
  c(0.50, 0.72, 0.99, 0.38, 0.78), 
  c(0.93, 0.21, 0.65, 0.13, 0.27), 
  c(0.39, 0.01, 0.38, 0.87, 0.34)
)

threshold <- function(x, cutoff = 0.8) x[x > cutoff]
x1 %>% sapply(threshold) %>% str()
#> List of 3
#>  $ : num 0.91
#>  $ : num [1:2] 0.9 0.94
#>  $ : num(0)
x2 %>% sapply(threshold) %>% str()

## vapply() safe alternative to sapply(), as one needs to apply additional argument that defines type.

#vapply() is that it's a lot of typing: vapply(df, is.numeric, logical(1)) is equivalent to map_lgl(df, is.numeric)


##########################Section 21.5.3 Exercises ###################################

Write code that uses one of the map functions to:
  
  Compute the mean of every column in mtcars.
Determine the type of each column in nycflights13::flights.
Compute the number of unique values in each column of iris.
Generate 10 random normals for each of  mu = -10,0, 10, and 100


## calculate mean of every column in mtcars, apply fucntion mean() to each column and use map_dbl

map_dbl(mtcars,mean)

map_chr(nycflights13::flights,typeof)

There is no function that directly calculates the number of unique values in a vector. For a single column, the number of unique values of a vector can be calculated like so,

length(unique(iris$species))
To apply this to all columns, we can provide the map an anonymous function. We can write anonymous function using the standard R syntax.
map_int(iris,function(x) length(unique(x)))
# or the compact, one sided formula.
map_int(iris,~length(unique(.)))

map(c(-10,0,10,100),~rnorm(n=10,mean=.))
# a singel call of rnorm() returns a numeric vector with a length greater than one, cannot use map_dbl

##21.5.2
How can you create a single vector that for each column in a data frame indicates whether or not it's a factor?
  is.factor(diamonds$color)

# to check all columns, need a map_*() function.
map_lgl(diamonds,is.factor)

#exercise 21.5.3

What happens when you use the map functions on vectors that aren't lists? What does map(1:5, runif) do? Why?
  
  #map functions work with any vectors, not just lists.
  # map function will apply the function to each element of the vector.
  
  map(c(TRUE, FALSE, TRUE), ~ !.)


map(c("Hello", "World"), str_to_upper)


map(1:5, ~ rnorm(.))

map(c(-0.5, 0, 1), ~ rnorm(1, mean = .))

## output to any map() function is a list. even if inpuits are vectors


map(1:5, runif) is equivalent to:
  
  list(
    runif(1),
    runif(2),
    runif(3),
    runif(4),
    runif(5)
  )

The map() function loops through the numbers 1 to 5. For each value, it calls the runif() with that number as the first argument, which is the number of sample to draw. The result is a length five list with numeric vectors of sizes one through five, each with random samples from a uniform distribution. Note that although input to map() was an integer vector, the return value was a list.

####################Exercise 21.5.4##
What does map(-2:2, rnorm, n = 5) do? Why?
  
  What does map_dbl(-2:2, rnorm, n = 5) do? Why?
  
  map(-2:2, rnorm, n = 5)
# this will take samples of size 5 from normal distirbutions with means -2 to 2, but sd = 1.
# returns a list of each element a numeric vector of length 5.

map_dbl(-2:2, rnorm, n = 5)
# this produces an erorr, because map_dbl requires function it applies to each element to return a numeric vector of length one.
# if function returns a non-numeric or numeric vector with length >1, error.
#The reason for this strictness is that map_dbl() guarantees that it will return a numeric vector of the same length as its input vector.
#This concept applies to the other map_*() functions. The function map_chr() requires that the function always return a character vector of length one; map_int() requires that the function always return an integer vector of length one; map_lgl() requires that the function always return an logical vector of length one. Use the map() function if the function will return values of varying types or lengths.
#To return a double vector, we could use map() followed by flatten_dbl(),


## Exercise 21.5.5

Rewrite map(x, function(df) lm(mpg ~ wt, data = df)) to eliminate the anonymous function.

x <- split(mtcars, mtcars$cyl)
map(x, function(df) lm(mpg ~ wt, data = df))

# eliminate use of annoymous fucntion using ~ shortcut
map(x, ~ lm(mpg ~ wt, data = .))

# can also create a named one:
run_reg <- function(df) {
  lm(mpg ~ wt, data = df)
}
map(x, run_reg)


## Section 21.9 Other patterns of for loops
#
###### Exercise 21.9.1 ###########

Implement your own version of every() using a for loop. Compare it with purrr::every(). What does purrr's version do that your version doesn't?
  
  # Use ... to pass arguments to the function
  every2 <- function(.x, .p, ...) {
    for (i in .x) {
      if (!.p(i, ...)) {
        # If any is FALSE we know not all of then were TRUE
        return(FALSE)
      }
    }
    # if nothing was FALSE, then it is TRUE
    TRUE
  }

every2(1:3, function(x) {
  x > 1
})
#> [1] FALSE
every2(1:3, function(x) {
  x > 0
})
#> [1] TRUE

############# Exercise 21.9.2 #########
Create an enhanced col_summary() that applies a summary function to every numeric column in a data frame.

col_sum2 <- function(df,f,...) {
  
  map(keep(df,is.numeric),f,...)
}

col_sum2(iris, mean)




### Exercise 21..9.3 ########

col_sum3 <- function(df, f) {
  is_num <- sapply(df, is.numeric)
  df_num <- df[, is_num]
  sapply(df_num, f)
}

df <- tibble(
  x = 1:3,
  y = 3:1,
  z = c("a", "b", "c")
)
# OK
col_sum3(df, mean)
# Has problems: don't always return numeric vector
col_sum3(df[1:2], mean)
col_sum3(df[1], mean)
col_sum3(df[0], mean)

## what happens these bugs? #beahaviour of sapply().
# sapply() does not guarantee the type of vector is returns, and will return differen types of vectors depending on inputs.
#If no columns are selected, instead of returning an empty numeric vector, it returns an empty list. This causes an error since we can't use a list with [.

sapply(df[0], is.numeric)
sapply(df[1], is.numeric)
sapply(df[1:2], is.numeric)
###################Section 21.6 Dealing with Failure##################

#common to use map functions to repeat oeprations, we get error messge with no output.
# how to deal with this situation using safely().
# it takes a function( a verb) and returns a modifed version, which will not throw an error, but
# return a list with two  elements:
#1 is the result, the original result, if there was an error this is NULL.
# 2 : error is an error object, if operation was successful, this will be NULL.

safe_log <- safely(log)
str(safe_log(10))

## if function succeeds, result element contains the result, error element is null.
#when function fails, result element is null and the error element contains an error object.

x <- list(1, 10, "a")
y <- x %>% map(safely(log))
str(y)

# or look two lists, one of all the errors, and one of all the output.
# transpose()
y <- y %>% transpose()
str(y)

is_ok <- y$error %>% map_lgl(is_null)
x[!is_ok]
#> [[1]]
#> [1] "a"
y$result[is_ok] %>% flatten_dbl()
#> [1] 0.0 2.3

#Purr provides two useful adverbs:
# safely(), possibly() always suceeds. # we give it a default value to return when there is an error.

x <- list(1, 10, "a")
x %>% map_dbl(possibly(log, NA_real_))
#> [1] 0.0 2.3  NA

#quietly() performs similar to safely(), but instead of capturing errors, it captures printed output, messges and warnings.

x <- list(1, -1)
x %>% map(quietly(log)) %>% str()



## Section 21.7 Mapping over multiple arguments

## map2() and pmap() functions.
# suppose we want to simualte some random normals with different means. You know how to do that with map():

mu <- list(5, 10, -3)
mu %>% 
  map(rnorm, n = 5) %>% 
  str()
## what if we want to vary the standard deviation? # iterate over the indices and index into vectors of means and sds:

sigma <- list(1, 5, 10)
seq_along(mu) %>% 
  map(~rnorm(5, mu[[.]], sigma[[.]])) %>% 
  str()
## instead of this use the map2() function

map2(mu,sigma,rnorm,n=5) %>% str()


## map2() generates the following series of function calls:

mu( being a list of 5, 10 ,3), and sigman (1,5,10)
map2( mu,sigma,rnorm,n=5) # what this looks like is rnorm(5,1,n=5)
# then rnorm(10,5,n=5)
# then rnorm(-3,10,n=5)

map2 <- function(x, y, f, ...) {
  out <- vector("list", length(x))
  for (i in seq_along(x)) {
    out[[i]] <- f(x[[i]], y[[i]], ...)
  }
  out
}

## purrr provides a pmap() which takes a list of arguments. Can use this if wanted to vary the mean, standard deviation, and number of samples.
n <- list(1, 3, 5)
args1 <- list(n, mu, sigma)
args1 %>%
  pmap(rnorm) %>% 
  str()

# thi looks like args1, 3x3 data frame, where first column is (1,3,5), secon is (5,10,-3) and third is (1,5,10). 
# pmap(args1) looks like this, a 1x3 frame, where first entry is rnorm(1,5,1), second is rnorm(3,10,5), and rnorm(5,-3,10)

# if one does not name the list's elements, pmap() will use positional amtching when calling the function.
#thats a little fragile, and makes code harder to read, so name arguments!!

args2 <- list(mean = mu, sd = sigma, n = n)
args2 %>% 
  pmap(rnorm) %>% 
  str()

## longer, but safer calls: it generates a 3x3 matrix, first column is list of mu, 2nd column is list of sigma, 3rd is list of n.

pmap(args2) would be  three results: rnorm(mean=5,sigma = 1, n =1)
#rnorm(mean=10,sigma = 5, n =3)
rnorm(mean=-3,sigma = 10, n =5)

# single arguments are all same length, makes sense to store them in a data frame:

params <- tribble(
  ~mean, ~sd, ~n,
  5,     1,  1,
  10,     5,  3,
  -3,    10,  5
)
params %>% 
  pmap(rnorm)
#As soon as your code gets complicated, I think a data frame is a good approach because it ensures that each column has a name and is the same length as all the other columns.


## Section 21.7.1 Invoking Different Functions:

## cana lso vary the function itself:

f <- c("runif", "rnorm", "rpois")
param <- list(
  list(min = -1, max = 1), 
  list(sd = 5), 
  list(lambda = 10)
)

# to handle this use invoke_map():

invoke_map(f,param,n=5) %>% str()


# what this looks like is f is a data frame with three potential functions, "runif", "rnorm" and rpois"
# params icontains three lists, (min -1, max 1), 2nd is standard deviation of 5, 3rd list is lambda =10.

invoke_map(f,params,n=5) will run the following and place into a structure for n=5.
runif(min=1,max=1,n=5)
rnorm(sd=5,n=5)
rpois(lambda=10,n=5)

# first arugment is a list of functions or character vector or function names, second argument is a list of lists giving the arguments
.

And again, you can use tribble() to make creating these matching pairs a little easier:
  
  sim <- tribble(
    ~f,      ~params,
    "runif", list(min = -1, max = 1),
    "rnorm", list(sd = 5),
    "rpois", list(lambda = 10)
  )
sim %>% 
  mutate(sim = invoke_map(f, params, n = 10))


#Section 21.8 Walk

#Walk is an alternative to map when one wants to call a function for side efects rather than return a value.
# we use this if we want to render output to screen or save files to disk.

x <- list(1, "a", 3)

x %>% 
  walk(print)

#walk() not that useful compared to walk2() or pwalk().

# can use pwalk() to save each file to coressponding location on disk:

library(ggplot2)
plots <- mtcars %>% 
  split(.$cyl) %>% 
  map(~ggplot(., aes(mpg, wt)) + geom_point())
paths <- stringr::str_c(names(plots), ".pdf")

pwalk(list(paths, plots), ggsave, path = tempdir())


## Section 21.9 Other patterns of for loops:
#use less frequent than map  functions, but sueful!

# section 21.9.1 Predicate functions:

# work with predicate functinos that reutnr either a single TRUE or FALSE.
#keep() and discard() keep elements of the input where predicate is TRUE or FALSE respectively:

iris %>% 
  keep(is.factor) %>% 
  str()

iris %>% 
  discard(is.factor) %>% 
  str()

#detect() finds the first element, where predicate is true:, detect_index() returns its position.
x <- sample(10)
x

x %>% 
  detect(~ . > 5)

x %>% 
  detect_index(~ . > 5)

#head_while() and tail_while() take elements from the start or end of a vector while a predicate is true:

x %>% 
  head_while(~ . > 5) # from the start, elements >5
#> [1] 10  6

x %>% 
  tail_while(~ . > 5) # from the end where elements > 5


#Section 21.9.2 Reduce and Accumlate :

# sometimes want to reduce a complex list by aplpying a function that reduces a pair to a singleton.
## consider a list of data fram,es, one would want to reduce to a single data frame by joining the elements together:

dfs <- list(
  age = tibble(name = "John", age = 30),
  sex = tibble(name = c("John", "Mary"), sex = c("M", "F")),
  trt = tibble(name = "Mary", treatment = "A")
)

dfs%>%reduce(full_join)

# or maybe you have a list of vectors, and want to find intersection
vs <- list(
  c(1, 3, 5, 6, 10),
  c(1, 2, 3, 7, 8, 10),
  c(1, 2, 3, 4, 8, 9, 10)
)

vs %>% reduce(intersect)

# reduce function takes a binary function (function with two primary inputs) and applies it repeated to a list until ther eis only a single element left.
# accumulate is similar but it keeps all the interim results, could use it to implement a cumulative sum:

x <- sample(10)
x
#>  [1]  7  5 10  9  8  3  1  4  2  6
x %>% accumulate(`+`)
