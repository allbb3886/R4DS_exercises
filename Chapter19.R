####Section 19, Functions: ########
## Writing functions allows to automate common tasks
# can give a function an evocative names to make it easier to understand
# as requirements changes, only need to update code in one place
# eliminate the chance of making incidental mistakes
library(tidyverse)
library(Rmisc)
# should consider writing a function whenever we copied and apsted a block of code
# more than twice.
## Take a look at below
df <- tibble::tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

df$a <- (df$a - min(df$a, na.rm = TRUE)) / 
  (max(df$a, na.rm = TRUE) - min(df$a, na.rm = TRUE))
df$b <- (df$b - min(df$b, na.rm = TRUE)) / 
  (max(df$b, na.rm = TRUE) - min(df$a, na.rm = TRUE))
df$c <- (df$c - min(df$c, na.rm = TRUE)) / 
  (max(df$c, na.rm = TRUE) - min(df$c, na.rm = TRUE))
df$d <- (df$d - min(df$d, na.rm = TRUE)) / 
  (max(df$d, na.rm = TRUE) - min(df$d, na.rm = TRUE))

# to write a fucntion, analyse the code, and see how many inputs does it have?

(df$a - min(df$a, na.rm = TRUE)) /
  (max(df$a, na.rm = TRUE) - min(df$a, na.rm = TRUE))

# the above only has one input, df$a, 
# make inpurts more clear, good idea to rewrite the code using temporary variables with general names:

x <- df$a
(x-min(x,na.rm=TRUE)) / (max(x,na.rm=TRUE) -min(x,na.rm=TRUE))

# the code also has duplication, computing teh range of the data three times, so lets do it in one step

rng_ <- range(x,na.rm=TRUE)
(x-rng[1])/(rng[2] - rng[1])

## pulling out intermediate calculations into named variables is agood practice because it makes more clear what the code is doing:

rescale01 <- function(x){
  rng <- range(x,na.rm=TRUE)
  (x-rng[1]/(rng[2] - rng[1])
  )
  rescale01(c(0,5,10))
  
  
  #### Three steps to creating a new function:
  #### pick a name, here rescale01 was used to picka vector between 0 and 1
  ### list the inputs or arguments to the function inside function(), here it was jsut x, but it could be function(x,y,z)
  # place the code inside the body, {}
  
  ## easier to make a functiona fter you figure out how to work the input
  
  df$a <- rescale01(df$a)
  df$b <- rescale01(df$b)
  df$c <- rescale01(df$c)
  df$d <- rescale01(df$d)
  
  ## if changes are needed, only need to change one location:
  
  ## suppose previous code, variables include infinte values, so rescale01() fails.
  x <- c(1:10, Inf)
  rescale01(x)
  
  ## since we have a function only need to make one adjustment
  rescale01 <- function(x) {
    rng <- range(x, na.rm = TRUE, finite = TRUE)
    (x - rng[1]) / (rng[2] - rng[1])
  }
  rescale01(x)
  
  ##This is an important part of the "do not repeat yourself" (or DRY) principle. The more repetition you have in your code, the more places you need to remember to update when things change (and they always do!), and the more likely you are to create bugs over time.
  
  ###Exercises 19.2.1
  ## why si TRUE not a parameter to rescale01()? What would happen if x contained single missing value, na.rm was false?
  
  rescale01 <- function(x) {
    rng <- range(x, na.rm = TRUE, finite = TRUE)
    (x - rng[1]) / (rng[2] - rng[1])
  }
  
  # if x contains a missing value and na.rm=FALSE, then this function still returns a non-missing value.
  
  rescale01_alt <- function(x, na.rm = FALSE) {
    rng <- range(x, na.rm = na.rm, finite = TRUE)
    (x - rng[1]) / (rng[2] - rng[1])
  }
  rescale01_alt(c(NA, 1:5), na.rm = FALSE)
  #> [1]   NA 0.00 0.25 0.50 0.75 1.00
  rescale01_alt(c(NA, 1:5), na.rm = TRUE)
  #> [1]   NA 0.00 0.25 0.50 0.75 1.00
  
  ## option finite=TRUE to range() will drop all non-finite elements, and NA is non finite
  
  # if both finite= FALSE and na.rm= fALSE, then function will return a vector of NA values
  
  rescale01_alt2 <- function(x, na.rm = FALSE, finite = FALSE) {
    rng <- range(x, na.rm = na.rm, finite = finite)
    (x - rng[1]) / (rng[2] - rng[1])
  }
  rescale01_alt2(c(NA, 1:5), na.rm = FALSE, finite = FALSE)
  
  ##Exercise 19.2.2
  
  ##map functions to 0 and inf to 1
  
  rescale_inf <- function(x) {
    rng <- range(x,na.rm=TRUE, finite = TRUE)
    y <- (x-rng[1])/ (rng[2] - rng[1])
    y[y== -Inf] <- 0
    y[y==Inf] <-  1
    y
  }
  rescale_inf(c(Inf, -Inf, 0:5, NA))
  
}


##Exercise 19.2.3 #######

mean(is.na(x))

x / sum(x, na.rm = TRUE)

sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)

## what does the code do?

## portion of NA vlaues in a vector.

prop_na <-  function(x)
{
  mean(is.na(x))
}

## this takes a single argument x, and returns a value between 0 and 1)

prop_na(c(0,1,2,NA, 4,NA))

## code standaizes a vector so that it sums to one.
x/ sum(x,na.rm=TRUE)

# wrtie a function of single argument, x, the vector to standardize, optional arugment, na.rm makes function more expressive.

sum_to_one <- function(x,na.rm=FALSE) {
  x / sum(x,na.rm=na.rm)
}

sum_to_one(1:5)

## if any missing, return all missing:
sum_to_one(c(1:5,NA))
#drop missing values when standardizing
sum_to_one(c(1:5,NA), na.rm=TRUE)

## next line of code calculates coefficient of variations, the standard deviations divided by the mean

sd(x, na.rm=TRUE) / mean(x,na.rm=TRUE)

## lets write a function :
coef_variation <-  function(x,na.rm=FALSE) {
  sd(x,na.rm=na.rm) / mean (x, na.rm = na.rm)
}

t=c(1:5,NA)
coef_variation(1:5)
coef_variation(t)## creating a vector and putting its input into 
coef_variation(t,na.rm=TRUE)


## Exercise 19.2.4
##Write a function for sample variance
variance <- function (x,na.rm=TRUE) {
  n <- length(x)
  m <- mean(x,na.rm=TRUE)
  sq_error <- (x-m)^2
  sum(sq_error)/(n-1)
}

variance(c(1:10))

## function for skewness :

skewness <- function(x,na.rm=FALSE) {
  n <- length(x)
  m <- mean(x,na.rm=na.rm)
  v <- var(x,na.rm=na.rm)
  (sum((x-m)^3))/(n-2)/v^(3/2)
}
skewness(c(1,2,5,100))


## Exercise 19.2.5

# write both_na(), a function that takes two vectors of teh same length and retunrs the number of positions that have an NA in both vectors.

both_na <- function(x,y) {
  sum(is.na(x) & is.na(y))
}

both_na(c(NA,NA,1,2),
        c(NA,1,NA,2)
)

both_na(
  c(NA, NA, 1, 2, NA, NA, 1),
  c(NA, 1, NA, 2, NA, NA, 1)
)
#> [1] 3

##Exercise 19.2.6 
## what do the following functions do?
is_directory <- function(x) file.info(x)$isdir
is_readable <- function(x) file.access(x, 4) == 0

The function is_directory() checks whether the path in x is a directory. The function is_readable() checks whether the path in x is readable, meaning that the file exists and the user has permission to open it. These functions are useful even though they are short because their names make it much clearer what the code is doing.

## Lets complete the lyrics to Little Bunny FOo Foo, 


threat <- function(chances) {
  give_chances(
    from = Good_Fairy,
    to = foo_foo,
    number = chances,
    condition = "Don't behave",
    consequence = turn_into_goon
  )
}

lyric <- function() {
  foo_foo %>%
    hop(through = forest) %>%
    scoop(up = field_mouse) %>%
    bop(on = head)
  
  down_came(Good_Fairy)
  said(
    Good_Fairy,
    c(
      "Little bunny Foo Foo",
      "I don't want to see you",
      "Scooping up the field mice",
      "And bopping them on the head."
    )
  )
}

lyric()
threat(3)
lyric()
threat(2)
lyric()
threat(1)
lyric()
turn_into_goon(Good_Fairy, foo_foo)


### Section 19.3 Functions are for humans and computers ###

##make good functions names.

# Good
input_select()
input_checkbox()
input_text()

# Not so good
select_input()
checkbox_input()
text_input()

# Don't do this!
T <- FALSE
c <- 10
mean <- function(x) sum(x)
# Load data --------------------------------------

# Plot data --------------------------------------


## Exercises 19.3.1
f1 <- function(string, prefix) {
  substr(string, 1, nchar(prefix)) == prefix
}

f2 <- function(x) {
  if (length(x) <= 1) {
    return(NULL)
  }
  x[-length(x)]
}

f3 <- function(x, y) {
  rep(y, length.out = length(x))
}

# f1 test whether each element of the character vector, nchar starts with the string prefix

f1(c("abc","abcde","ad"),"ab")
# better name is has_prefix()

## what about function 2?
# drop the last element of a vector

## function 3 repeats the length of the vector of y
f3(1:3,5)
f3(1:5, 7)

## Exercises 19.3.3

#
Compare and contrast rnorm() and MASS::mvrnorm(). How could you make them more consistent?
  
  #rnorm() samples from the univariate normal distribution, while MASS::mvrnorm samples from the multivariate normal distribution.
  #The main arguments in rnorm() are n, mean, sd. The main arguments is MASS::mvrnorm are n, mu, Sigma. To be consistent they should have the same names. However, this is difficult.
  #In general, it is better to be consistent with more widely used functions, e.g. rmvnorm() should follow the conventions of rnorm(). However, while mean is correct in the multivariate case, sd does not make sense in the multivariate case. However, both functions are internally consistent. It would not be good practice to have mu and sd as arguments or mean and Sigma as arguments.
  
  
  ##Exercise 19.3.4 
  
  ##Make a case for why norm_r(), norm_d() etc would be better than rnorm(), dnorm(). Make a case for the opposite.
  
  If named norm_r() and norm_d(), the naming convention groups functions by their distribution.

If named rnorm(), and dnorm(), the naming convention groups functions by the action they perform.

r* functions always sample from distributions: for example, rnorm(), rbinom(), runif(), and rexp().

d* functions calculate the probability density or mass of a distribution: For example, dnorm(), dbinom(), dunif(), and dexp().

R distributions use this latter naming convention.

#Section 19.4 Conditional Execution ##

## if conditions allow us to conditionally execute code.

if (condition) {
  # code executed when condition is TRUE
} else {
  # code executed when condition is FALSE
}


has_name <- function(x) {}
nms <- names(x)
if (is.null(nms)) {
  rep(FALSE, length(x))
} else{
  !is.na(nms) & nms != ""
}
}
#return a logical vector describing whether or not each element of a vector is named.


## Section 19.4.1 Conditions:
# The condition must evaluate to either TRUE or FALSE. IF its a vector, you'll get a warning message; if tis an NA, you'll get an error.

if (c(TRUE, FALSE)) {}
#> Warning in if (c(TRUE, FALSE)) {: the condition has length > 1 and only the
#> first element will be used
#> NULL

if (NA) {}
#> Error in if (NA) {: missing value where TRUE/FALSE needed

## can use || (or) and && (and) to combine multiple expressions.
#as soon as || sees the first TRUE, it returns true without computing anything else.

#As soon as && seees first false, it reutnrs false.

#Careful when testing equality. == is vectorised, which means its easy to get more than one output.
#etiher check teh length is already 1, collapse with all() or any(), or use the non-vectorised identitcal().
#identical() is very strict, always returns either a single TRUE or single FALSE.
# need to be careful when comparing integers and doubles:

identical(0L,0)

x <- sqrt(2) ^ 2
x
#> [1] 2
x == 2
#> [1] FALSE
x - 2
#> [1] 4.44e-16

## better to use dplyr::near() for comparisons.


### Section 19.4.2 Multiple Conditions :
# chaining multiple if statements:

if (this) {
  # do that
} else if (that) {
  # do something else
} else {
  # 
}

# can have long series of chained if statements, consider rewriting. Or use
# switch() function, it allows you to evaluate selected code based on position or name.

#> function(x, y, op) {
#>   switch(op,
#>     plus = x + y,
#>     minus = x - y,
#>     times = x * y,
#>     divide = x / y,
#>     stop("Unknown op!")
#>   )
#> }

#another function to remove long chains of if statements is cut().

## code style

# Good
if (y < 0 && debug) {
  message("Y is negative")
}

if (y == 0) {
  log(x)
} else {
  y ^ x
}

# Bad
if (y < 0 && debug)
  message("Y is negative")

if (y == 0) {
  log(x)
} 
else {
  y ^ x
}
Both if and function should (almost) always be followed by squiggly brackets ({}), and the contents should be indented by two spaces. This makes it easier to see the hierarchy in your code by skimming the left-hand margin.

An opening curly brace should never go on its own line and should always be followed by a new line. A closing curly brace should always go on its own line, unless it's followed by else. Always indent the code inside curly braces.



## write a greeting function depending on the day:
greet <- function(time=lubridate::now()){
  hr <- lubridate::hour(time)
  if (hr<12) {
    print("good morning")
  } else if (hr<17) {
    print("Good afternoon")
  } else {
    print("good evening")
  }
}
greet()


## Exercise 19.4.3

## implement a fizzbuzz(), it takes a single number as input, if number is divisible by three, return "fizz"

## use modulo operator %% to check divsiionbility.

1:10%% 3 == 0

## four cases:
# if x is divisible by 3, return fizz
#if x is divisible by 5, return buzz
# if x is divisible by 3 and 5, fizzbuzz
# oterhwise, if x is nto divisble by 3 or 5 return x

## need to check if input is valid to begin
#where x%% y returns 0 if y divides x


## now we need a function:
fizzbuzz <- function(x){
  ## need valid input
  stopifnot(length(x)==1)
  stopifnot(is.numeric(x))

if(!(x%%3) && !(x %% 5)) {
  "fizzbuzz"
} else if(!(x%%3)){ # if value is divisible by 3
  "fizz"
} else if(!(x%%5)) { # value is divsible by 5
  "buzz" 
} else{
  # going to return a character vector
  as.character(x)
}

}

fizzbuzz(3)

## instead of only accetping one number, could workon a vector.
#case_when() function vectorizes multiple if-else conditions, so this is perfect.

fizzbuzz_vec <- function(x){
  case_when(
    !(x %% 3) & !(x %% 5) ~ "fizzbuzz",
    !(x %% 3) ~ "fizz",
    !(x %% 5) ~ "buzz",
    TRUE ~ as.character(x)
  )
}
fizzbuzz_vec(c(0,1,3,122,10,9,15))
  


## another example of vectorized fizzbuss that only use bracket assignment.

fizzbuzz_vec2 <- function(x) {
  y <- as.character(x)
  # put the individual cases first - any elements divisible by both 3 and 5
  # will be overwritten with fizzbuzz later
  y[!(x %% 3)] <- "fizz"
  y[!(x %% 3)] <- "buzz"
  y[!(x %% 3) & !(x %% 5)] <- "fizzbuzz"
  y
}
fizzbuzz_vec2(c(0, 1, 2, 3, 5, 9, 10, 12, 15))

## common programming questions.


##Exercise 19.4.4
if (temp <= 0) {
  "freezing"
} else if (temp <= 10) {
  "cold"
} else if (temp <= 20) {
  "cool"
} else if (temp <= 30) {
  "warm"
} else {
  "hot"
}

# how to use cut() here to simplify nested if else statements.

##consider 
temp <- seq(-10,50, by=5)
cut(temp,c(-Inf,0,10,20,30,Inf), right = TRUE,
    labels=c("freezing", "cold", "cool", "warm", "hot")
)

## to have intervals open on the left (using <), change right argument to false.

temp <- seq(-10, 50, by = 5)
cut(temp, c(-Inf, 0, 10, 20, 30, Inf),
    right = FALSE,
    labels = c("freezing", "cold", "cool", "warm", "hot")
)

Two advantages of using cut is that it works on vectors, whereas if only works on a single value (I already demonstrated this above), and that to change comparisons I only needed to change the argument to right, but I would have had to change four operators in the if expression.


## Exercsie 19.4.5

#In switch(n,...) if n is numeric, it will return the nth argument from ....

# switch(1,"apple","banana","cantaloupe")

#this would return apple.
## if we did swtich(2,..), it would be banana

# if using a non integer part, it will ignore the non integer. for example:
switch(1.2, "...") this would ignore 1.2, and put it at 1.

## note that swtich() truncates the numeric value

## exercise 19.4.6

#what does the following switch() do?

x <- "e"
switch(x,
       a = ,
       b = "ab",
       c = ,
       d = "cd"
)

## lets fwrite a switcheroo() function

switcheroo <- function(x) {
  switch(x,
         a = ,
         b = "ab",
         c = ,
         d = "cd"
  )
}
switcheroo("a")
Switch function returns the first non-missing argument value for first name it matches
# since a=, is missing, it will return the value of the enxt argument without a missing value,
b="ab"
#if object in the last (unamed argument) is present of NULL, sicne "e" is not one of the arugments it returns null

## same as :
switch(x,
       a = "ab",
       b = "ab",
       c = "cd",
       d = "cd",
       NULL # value to return if x not matched
)

##Section 19.5 Function Arguments:

# arguments toa  function fall into two sets:
# one set supplies the data to compute on, other supplies arguments that control the details of computation.

##In log(), the data  is x, detail is the base of the logarithm.
# in mean(), the data is x, the details are how much data from the ends (trim), and how to handle missing values(na.rm)

# in t.test(), the data re x and y, details of the test are alternative, mu, paired, and var.equal and conf.level

# generally data arguments come first, detail arguments on the end and usual default values.

## compute confidence interval around mean using normal approxiamtion

mean_ci <- function(x,conf=0.95){
  se <- sd(x) / sqrt(length(x))
  alpha <-  1- conf
  mean(x) + se* qnorm(c(alpha/2, 1- alpha/2))
}

x <- runif(100)
mean_ci(x)


##default value should almost always be most common value. ## it amkes sesne for na.rm to default to FALSE, since missing values are important.


When you call a function, you typically omit the names of the data arguments, because they are used so commonly. If you override the default value of a detail argument, you should use the full name:
  
  # Good
mean(1:10, na.rm = TRUE)

# Bad
mean(x = 1:10, , FALSE)
mean(, TRUE, x = c(1:10, NA))

### Section 19.5.1 Choosing names ##

x, y, z: vectors.
w: a vector of weights.
df: a data frame.
i, j: numeric indices (typically rows and columns).
n: length, or number of rows.
p: number of columns.


## Checking values section 19.5.2 
# make constraints explicit

wt_mean <- function(x, w) {
  sum(x * w) / sum(w)
}
wt_var <- function(x, w) {
  mu <- wt_mean(x, w)
  sum(w * (x - mu) ^ 2) / sum(w)
}
wt_sd <- function(x, w) {
  sqrt(wt_var(x, w))
}

#what happens if x adn w are nmot the same length?
wt_mean(1:6,1:3)
# R just recylces, so no error, just important to check preconditions

wt_mean <- function(x, w) {
  if (length(x) != length(w)) {
    stop("`x` and `w` must be the same length", call. = FALSE)
  }
  sum(w * x) / sum(w)
}
wt_mean(1:3,2:4)


## a lot of extra work for little gain !
wt_mean <- function(x, w, na.rm = FALSE) {
  if (!is.logical(na.rm)) {
    stop("`na.rm` must be logical")
  }
  if (length(na.rm) != 1) {
    stop("`na.rm` must be length 1")
  }
  if (length(x) != length(w)) {
    stop("`x` and `w` must be the same length", call. = FALSE)
  }
  
  if (na.rm) {
    miss <- is.na(x) | is.na(w)
    x <- x[!miss]
    w <- w[!miss]
  }
  sum(w * x) / sum(w)
}

##useful comprosmise is the built in stopif() :
# it checks taht each argument is TRUE, and produces a generic error emssage


wt_mean <- function(x, w, na.rm = FALSE) {
  stopifnot(is.logical(na.rm), length(na.rm) == 1)
  stopifnot(length(x) == length(w))
  
  if (na.rm) {
    miss <- is.na(x) | is.na(w)
    x <- x[!miss]
    w <- w[!miss]
  }
  sum(w * x) / sum(w)
}
wt_mean(1:6, 6:1, na.rm = "foo")
#> Error in wt_mean(1:6, 6:1, na.rm = "foo"): is.logical(na.rm) is not TRUE

# note that when using stopif(), you assert what should be true, rather than cehcking for what might be wrong.

## section 19.5.3 Dot-dot-dot (..)
# most functions rely on a special argument ..., this argument captures any number of arguments that aren't otherwise matched.


commas <- function(...) stringr::str_c(..., collapse = ", ")
commas(letters[1:10])
#> [1] "a, b, c, d, e, f, g, h, i, j"

rule <- function(..., pad = "-") {
  title <- paste0(...)
  width <- getOption("width") - nchar(title) - 5
  cat(title, " ", stringr::str_dup(pad, width), "\n", sep = "")
}
rule("Important output")
#> Important output -----------------------------------------------------------

## the ... lets me forward on any arguments that i dont want to dela with to str_c()
# downfall is that typos can go easily unoticed.


x <- c(1, 2)
sum(x, na.mr = TRUE)
#> [1] 4

##
Arguments in R are lazily evaluated: they're not computed until they're needed. That means if they're never used, they're never called. This is an important property of R as a programming language, but is generally not important when you're writing your own functions for data analysis. You can read more about lazy evaluation at http://adv-r.had.co.nz/Functions.html#lazy-evaluation.


## Section 19.5.5 Exercises
##what happens commas(letters,collapse="-")?
commas <- function(...) {
  str_c(..., collapse = ", ")
}
#This is because argument collapse is given to commas(), passed to str_c() as part of ...
# previous code equiavelent to str_c(letters,collapse="-", coll=se",")



### Section 19.6 Return Values

## Two things to consider for returning a value:
# does reutning early make function easier to read?
# can it make function pipeable?

## Explicit Return statements, 
# can choose to return early by using return().
# common to use this if inputs are empty.

complicated_function <- function(x, y, z) {
  if (length(x) == 0 || length(y) == 0) {
    return(0)
  }
  
  # Complicated code here
}

# if we have a if statement with one complex block and one simple block.

f <- function() {
  if (x) {
    # Do 
    # something
    # that
    # takes
    # many
    # lines
    # to
    # express
  } else {
    # return something short
  }
}

# long conditions, use an early return for simple case.

f <- function() {
  if (!x) {
    return(something_short)
  }
  
  # Do 
  # something
  # that
  # takes
  # many
  # lines
  # to
  # express
}

### Section 19.6.2 Writing Pipeable functions
#For dplyr and tidyr, object type is the data frame.
# two types of pipeable functions: transformations and side-effects.

# with Transformations, object is passed to functions first argument and modifed object is return.
# With side-effects, passed object is nto transformed, the function performs an action on the object, 
# like drawing a plt or saving a file. Side effect functions should "invisbly" return
# the first argument, so that while they're not pritned, they can be used in pipeline.
# example function that prints number of missing values.

show_missings <- function(df) {
  n <- sum(is.na(df))
  cat("Missing values: ", n, "\n", sep = "")
  
  invisible(df)
}

#invisible() means that the input, df, doesn't get printed out:

show_missings(mtcars)

#
# but its still there by default:
x <- show_missings(mtcars) 
#> Missing values: 0
class(x)
#> [1] "data.frame"
dim(x)
#> [1] 32 11
## 


# and still use in a pipe:
mtcars%>%
  show_missings() %>%
  mutate(mpg=ifelse(mpg<20,NA,mpg)) %>%
  show_missings()


## Section 19.7 Environment.
f <- function(x) {
  x + y
} 
, this would be an error, in many languages, because y is not definied insde the function
# in R, this is different, since R uses lexical scoping to find the value associated.
# R will look in the environment where the function was deinfed.
y <- 100
f(10)
#> [1] 110

y <- 1000
f(10)
#> [1] 1010

The advantage of this behaviour is that from a language standpoint it allows R to be very consistent. Every name is looked up using the same set of rules. For f() that includes the behaviour of two things that you might not expect: { and +. This allows you to do devious things like:
    