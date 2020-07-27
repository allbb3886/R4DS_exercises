### Section 20, Vectors
library(tidyverse)
# Atomic vectors - logical,itneger,double,character,complex and raw.
#lists which are called recuvrise vectors becauses lists can contain other lists.

# every vector has two props:
# its type, and its length

x <- list("a","b",1:10)
length(x)

# Augmentend vector: factors are bult on top of integer vectors
# dates and date-times are built on top of numeric vectors.
#data frames and tibbles are built on top of lists.

##Section 20.3 Types of Vectors
# Raw and complex not used in data analysis, not discussed.

## Section 20.3.1 Logical.
## they take FALSE, TRUE, and NA. # used with comparison operators, can create them by hand with c():

1:10 %% 3 == 0

c(TRUE,TRUE,FALSE,NA)


# Section 20.3.2 Numeric
## Integer and double vectors are numeric vectors.
# By default R sets doubles, to make an integer place L.


typeof(1)

typeof(1L)

## Note : Doubles are approximations can not be precisely represented by fixed memory.

# example:

x <- sqrt(2)^2
x
x-2

## instead of comparing floating point numbers using ==, should use dplyr::near()

## Integers have one special value, NA,
# doubles have four: NA, NaN, Inf and -Inf.
c(-1, 0, 1) / 0

## avoid using == to check, instead use helper functions like:
is.finite(), is.infinite(). and is.nan()


## Section 20.3.3 Character
x <- "This is a reasonably long string."
pryr::object_size(x)

### Section 20.3.4 Missing values:

## each type has its own missing value:

NA            # logical
#> [1] NA
NA_integer_   # integer
#> [1] NA
NA_real_      # double
#> [1] NA
NA_character_ # character
#> [1] NA

y <- rep(x,1000)
pryr::object_size(y)

##Section 20.4.1 Coercion

## four goals :How to convert from one type to another, and when that happens automatically.

How to tell if an object is a specific type of vector.

What happens when you work with vectors of different lengths.

How to name the elements of a vector.

How to pull out elements of interest.

#Explicit coericion happens when you call a function like as.logical(), as.integer(). as double(). or as.character()

#Implicit coericion when you use vector in specific context taht expects a certain type of vector
# For example, when you use a logical vector with a numeric summary function, or when you use a double vector where an integer vector is expected.

#Explicit not used as much as implicit.

x <- sample(20, 100, replace = TRUE)
y <- x > 10
sum(y)  # how many are greater than 10?
#> [1] 38
mean(y) # what proportion are greater than 10? #proportions of TRUE
#> [1] 0.3if (length(x)) {
# do something 
}

# in this case, 0 is converted to false.

typeof(c(TRUE, 1L))
#> [1] "integer"
typeof(c(1L, 1.5))
#> [1] "double"
typeof(c(1.5, "a"))
#> [1] "character"
# a list with multiple types with c(): most complex type awlays wins

# An atomic vector can not have a mix of different types because the type is a property of the complete vector, not the individual elements. If you need to mix multiple types in the same vector, you should use a list, which you'll learn about shortly.


#section 20.4.2 Test Functions

## types of test 
is_logical()	x				
is_integer()		x			
is_double()			x		
is_numeric()		x	x		
is_character()				x	
is_atomic()		
is_list()					
is_vector()

## often safer to use is_( functions provided by purrr.)
##no scalars, most built in functions are vectorised, meaning that they will operate on a vector of numbers.

sample(10) + 100
#>  [1] 107 104 103 109 102 101 106 110 105 108
runif(10) > 0.5
#>  [1] FALSE  TRUE FALSE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE

runif(10)>0.5

## vectors of different length?
1:10 + 1:2

## R will expand the shortest vector to the same length as the longest
1:10 + 1:3
##

tibble(x=1:4, y=1:2)
## need compataible sizes.
tibble(x=1:4,y=rep(1:2,2))

tibble(x = 1:4, y = rep(1:2, each = 2))

#20.4.4 Naming vectors

## name them during creation with c():

c(x=1,y=2,z=4)

# or after using purr::set_names()
set_names(1:3,c("a","b","c"))


## Section 20.4.5 Subsetting

#filter() only works with tibble:
, []. is the subsetting function, so x[a], there are four types of things taht subset a vector:
  
  # numeric vector only itnegers, integers must be all positive, all engative or zero.
  
  ## subsetting with positive
  
  x <- c("one","two","three","four","five")
x[c(3,2,5)]

# repeating a position, one can actually make a longer output than input:
x[c(1, 1, 5, 5, 5, 2)]

# negative values drop the elements at that position:
x[c(-1, -3, -5)]

## cannot mix negative and positives.

## susbetting with zero returns no values.


### Subsetting with a logical vector keeps all values coressponding to a TRUE value, useful in conjunction with comparison functions:

x <- c(10, 3, NA, 5, 8, 1, NA)
# All non-missing values of x
x[!is.na(x)]
#> [1] 10  3  5  8  1

# All even (or missing!) values of x
x[x %% 2 == 0]
#> [1] 10 NA  8 NA

## if a named vector, can subset with character vector.
x <- c(abc = 1, def = 2, xyz = 5)
x[c("xyz", "def")]

## subsetting with nothing, x[], returns entire x.
# help with high dimensional matrices:
# ix x is 2d, x[1,] will show first row all columsn
# x[,-1] selcts all rows and columns except the first.

### haven't done exercises yet


##section 20.5 Recursive Vectors (lists)

#lists are a step up in complexity from atomic vectors, lists can contain other lists.
## they create a hierarchial or tree like strcuture

x <- list(1,2,3)

## useful to use str(), 
str(x)
# tells us about structure and not the contents.
x_named <- list(a = 1, b = 2, c = 3)

## unlike vectors, lists can contain a mix of objects.

y <- list("a", 1L, 1.5, TRUE)
str(y)

#lists can contain other lists !

z <- list(list(1,2),list(3,4))
str(z)


## visualising lists section 20.5.1

x1 <- list(c(1, 2), c(3, 4))
x2 <- list(list(1, 2), list(3, 4))
x3 <- list(1, list(2, list(3)))

## lists have rounded corners, atomic vectors have square
#children are drawn insdie their parent, 

Section 20.5.2 subsetting a list

## 
a <- list(a = 1:3, b = "a string", c = pi, d = list(-1, -5))

## extract using [], which results in a list
str(a[1:2])
str(a[4])

## using double [[]], will extract a single componenet and removes hierarchy.

str(a[[1]])
str(a[[4]])

## recall $ isa short hand for extracting named elements of a list, simiarly to [[]], but dont need to use quotes

a$a
a[["a"]]

## distinction between [] and [[]], [[]] drills down into the list, where a[], simply returns a smaller list.

## Section 20.5.3 Lists of Conditions:

## consider a pepper shaker as the list, then x[1] is a pepper shaker with a single packet in it.
# then x[2] would be the same, but the second packer. and x[1:2] would be a pepper shaker with 2 packets
# then x[[1]] would be the one packet itself, if we want the contents of the pepper packet,
# we would use x[[1]][[1]], the pepper grains.


## Exercises not done


## Section 20.6 Attributes
# any vector can contain arbitrary additional metadta through its attributes.
# attributes can be thought of as named list of vectors that can be attached to any object.

## can get and set individual attribute values with attr(), or see them all at once with attributes().
x <- 1:10
attr(x,"greeting")
attr(x,"greeting") <- "hi!"
attr(x,"farewell") <- "Bye!"

attributes(x)

# Three attributes used to implment fundamental parts of R:
# names are used to name elements of vectors.
#dimensions(dims) make a vector behave like a matrix or arry
# class is used to implement the S3 object oriented system.

# tyical gneeric function looks like:
as.Date
#> function (x, ...) 
#> UseMethod("as.Date")
#> <bytecode: 0x2fa0fa0>
#> <environment: namespace:base>

## call to "UseMethod" means that this is a gneeric function, and it will call a specific method.

methods("as.Date")

# for example if x is acharacter vector, as.Date() will call as.Date.character(), if its a factor.


getS3method(): 
  getS3method("as.Date", "default")


## Section 20.7 Augmented Vectors
Atomic vectors and lists are the building blocks for other important vector types like factors and dates. I call these augmented vectors, because they are vectors with additional attributes, including class. Because augmented vectors have a class, they behave differently to the atomic vector on which they are built. In this book, we make use of four important augmented vectors:
  
  Factors
Dates
Date-times
Tibbles
These are described below.

## section 20.7.1 Factors

## Factors designed to represent categorical data that can take fixed set of values.
#built on top of integers, and have a levels attribute:

x <- factor(c("ab", "cd", "ab"), levels = c("ab", "cd", "ef"))
typeof(x)
#> [1] "integer"
attributes(x)
#> $levels
#> [1] "ab" "cd" "ef"
#> 
#> $class
#> [1] "factor"



## section 20.7.2 Dates an date times
x <- as.Date("1971-01-01")
unclass(x)
#> [1] 365

typeof(x)
#> [1] "double"
attributes(x)
#> $class
#> [1] "Date"


## Section 20.7.3 Tibbles

#Tibbles are augmeneted lists: the have class "tbl_df" + "tbl" + "data.frame", and names (column) and row.names attributes:

tb <- tibble::tibble(x=1:5,y=5:1)
typeof(tb)
attributes(tb)

## difference between a tibble and a list is that all elements of a data frame must be vectors with the same length.
# All functions that work with tibbles enforce this constraint.

## traditional data.frames ahve a very simiarly structure:
df <- data.frame(x = 1:5, y = 5:1)
typeof(df)
#> [1] "list"
attributes(df)
#> $names
#> [1] "x" "y"
#> 
#> $class
#> [1] "data.frame"
#> 
#> $row.names
#> [1] 1 2 3 4 5

## biggest difference is the class, the class of tibbles includes "data.frame' which means tibbles inherit the regualr data frame behaviour by default.
