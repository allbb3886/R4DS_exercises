## Pipes Section 18 ###################
#pipe comes from magrittr package, but also in tidyverse
library(magrittr)

## Section 18.2 Piping Alternatives
## pipe helps read statements easier!
## example of a story of a bunny named Foo FOo

Little bunny Foo Foo
Went hopping through the forest
Scooping up the field mice
And bopping them on the head

# defining an object to represent bunny
foo_foo <- little_bunny()

## use a function for each key verb: hop(), scoop(), and bop().

# 4 ways to tell this story.
#Save each intermediate step as a new object.
Overwrite the original object many times.
Compose functions.
Use the pipe.


#### Section 18.2 Intermediate Steps:
## simpelst approach is to save each step as a new object:

foo_foo_1 <- hop(foo_foo,through=forest)
foo_foo_2 <- scoop(foo_foo_1,up=field_mice)
foo_foo_3 <- bop(foo_foo_2, on = head)
# need to name each intermediate element.# It is helpful if there are natural names
# but in this case there are none, and we need numeric suffixes to make names unique.
# two problems arise: cluttered code and increment suffix on each line.
## can also write code like this, but more errors are likely to occur.

diamonds <- ggplot2::diamonds
diamonds2 <- diamonds%>%
  dplyr:mutate(price_per_carat = price/carat)

pryr::objectsize(diamonds)

## pryr() gives the memory occupied by all of its arguments

## size of each data frame is unchanged, but collective size increases:

diamonds$carat[1] <-  NA
pryr::object_size(diamonds)
pryr::object_size(diamonds2)
pryr::object_size(diamonds,diamonds2)
#object.size() only takes a single object 
#so it can't compute how data is shared across multiple objects



###Section 18.2.2 Overwrite the original

## instead of creating intermediate objects, we could overwrite the original object:

foo_foo <- hop(foo_foo, through = forest)
foo_foo <- scoop(foo_foo, up = field_mice)
foo_foo <- bop(foo_foo,on=head)

## less errors since no intermediates, but if you make a mistake, one has to rerun the compelte pipeline from the beginning

# repetition of object being transformed (written foo_foo six times) obscures whats changing on each line.

########## Section 18.2.3 Function Composition

## we can adandon assignemtn and just sring function calls together:
bop(
  scoop(
    hop(foo_foo,through=forest),up=field_mice),
  on=head
)
## disadvantage her is need to read from inside out, from right to left, and arguments end up spread far apart (evocatively called dagwood sandwhich problem)
# hard for a "human" to consume.

### 18.2.4 Use the pipe!

foo_foo %>%
  hop(through=forest) %>%
  scoop(up=field_mice) %>%
  bop(on = head)
## focuse more on "verbs" than nouns, can read as foo_foo hops, then scoops then bops.
## downside is one needs to be familiar with pipe, but it is simple enough to learn quickly.

# pipe works by perofrming a lexical transofmriation behind the scenes.
## this is how magrittr performs the pipe: 
my_pipe <- function(.) {
  . <- hop(., through = forest)
  . <- scoop(., up = field_mice)
  bop(., on = head)
}
my_pipe(foo_foo)

#SO pipe wont work for two classes of fucntions:
#functions that use current environment, example: assign() will create a new variable with the given name in the current environment
assign("x", 10)
x
#> [1] 10

"x" %>% assign(100)
x
#> [1] 10

## The use of assign with the pipe does not work because it assigns it to a temp. environment using %>%, if you want to use assign with pipe, need to be explicit.
env <- environment()
"x" %>% assign(100, envir = env)
x
#> [1] 100

### Functions that use lazy evaluation. in R, function arguments are only computed when fuinction uses them, not prior to calling the function.
tryCatch(stop("!"), error = function(e) "An error")
#> [1] "An error"

stop("!") %>% 
  tryCatch(error = function(e) "An error")
#> Error in eval(lhs, parent, parent): !

##Section 18.3 When not to use the pipe !

##The pipe is a powerful tool, but it's not the only tool at your disposal, and it doesn't solve every problem! Pipes are most useful for rewriting a fairly short linear sequence of operations. I think you should reach for another tool when:

Your pipes are longer than (say) ten steps. In that case, create intermediate objects with meaningful names. That will make debugging easier, because you can more easily check the intermediate results, and it makes it easier to understand your code, because the variable names can help communicate intent.

You have multiple inputs or outputs. If there isn't one primary object being transformed, but two or more objects being combined together, don't use the pipe.

You are starting to think about a directed graph with a complex dependency structure. Pipes are fundamentally linear and expressing complex relationships with them will typically yield confusing code.
#
#
#
##


### Section 18.4 Other tools from magrittr

# all packages in tidyverse automatically makes %>% available.
# however, useful tools inside magrittr to try out:

# When working with more complex pipes, sometimes useful to call a function for its side-effects.
# maybe you want to print out the current object, or plot it, or save it to disk.
# such functions don't return anything, effectively terminanting the pipe.

## can use the "tee" pipe. %T>% works like %>% except that it returns the left-hand side isntead of the right hand side.
# its called "tee" because its like aliteral T shaped pipe.

rnorm(100) %>%
  matrix(ncol = 2) %>%
  plot() %>%
  str()
#>  NULL

rnorm(100) %>%
  matrix(ncol = 2) %T>%
  plot() %>%
  str()

## also doesn't work for functions that don't have a data frame based API.
(passed them individual vectors, not a data frame, and expressions to be evaluated in the context of that data frame),
# might find %$% useful. It "explodes" out the variables in a data frame so you can refer to them explicitly.

mtcarts%$%
  cor(disp,mpg)

## for assignment amgrittr provides the %<>% operator which allows you to replace code like:
mt <- mtcars %>% transform(cyl=cyl * 2)
## with something like this:

mtcars %<>%transform(cyl=cyl*2)## not that helpful since assignments should be clear.
