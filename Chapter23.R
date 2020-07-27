#Section 22_23 MODELLING
library(tidyverse)

library(modelr)
options(na.action = na.warn)

## Section 23.2
ggplot(sim1,aes(x,y)) + geom_point()

# lets use a model

models <- tibble(a1=runif(250,-20,40),
                 a2=runif(250,-5,5)
)

ggplot(sim1, aes(x, y)) + 
  geom_abline(aes(intercept = a1, slope = a2), data = models, alpha = 1/4) +
  geom_point() 

## lots of models, but a lot are really bad. need to find a good model close to the data.

# distance is just difference between given by model (predication) and actual y value (response)

model1 <- function(a, data) {
  a[1] + data$x * a[2]
}
model1(c(7, 1.5), sim1)

## use root-mean squared deviation

measure_distance <- function(mod,data) {
  diff <- data$y - model1(mod,data)
  sqrt(mean(diff^2))
}
measure_distance(c(7,1.5),sim1)

# use purr to compute distance for all models.
sim1_dist <- function(a1, a2) {
  measure_distance(c(a1, a2), sim1)
}
#We need a helper function because our distance function expects the model as a numeric vector of length 2.

models <- models %>% 
  mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist))


# Next, let's overlay the 10 best models on to the data. I've coloured the models by -dist: this is an easy way to make sure that the best models (i.e. the ones with the smallest distance) get the brighest colours.

ggplot(sim1, aes(x, y)) + 
  geom_point(size = 2, colour = "grey30") + 
  geom_abline(
    aes(intercept = a1, slope = a2, colour = -dist), 
    data = filter(models, rank(dist) <= 10)
  )
models


## plotting the 10 best models, visualising with a scatterplot of a1 vs a2
ggplot(models, aes(a1, a2)) +
  geom_point(data = filter(models, rank(dist) <= 10), size = 4, colour = "red") +
  geom_point(aes(colour = -dist))

# can do systematic approach, a grid search.

grid <- expand.grid(
  a1 = seq(-5, 20, length = 25),
  a2 = seq(1, 3, length = 25)
) %>% 
  mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist))

#Thus overalying the 10 best models on original data, all look good!
grid %>% 
  ggplot(aes(a1, a2)) +
  geom_point(data = filter(grid, rank(dist) <= 10), size = 4, colour = "red") +
  geom_point(aes(colour = -dist)) 

ggplot(sim1, aes(x, y)) + 
  geom_point(size = 2, colour = "grey30") + 
  geom_abline(
    aes(intercept = a1, slope = a2, colour = -dist), 
    data = filter(grid, rank(dist) <= 10)
  )


## can iteratively making the grid finer and finer called Newton-Raphson search, by picking a starting point and looking around for the steepest slope.
#the optim() function does this:
best <- optim(c(0, 0), measure_distance, data = sim1)
best$par

ggplot(sim1, aes(x, y)) + 
  geom_point(size = 2, colour = "grey30") + 
  geom_abline(intercept = best$par[1], slope = best$par[2])


### If you have a function that defines the distance between a model and a dataset, 
#an algorithm that can minimise that distance by modifying the parameters of the model, 
#you can find the best model. The neat thing about this approach is that it will work for any family of models that you can write an equation for.
# linear model, using lm

sim1_mod <- lm(y~x,data=sim1)
coef(sim1_mod)




################## Exercises 23.2.1 #########################





#### Section 23.3 Visualising Models ############

### 23.3.1 Predictions

## lets create an evenly spaced grid

grid <- sim1%>%
  data_grid(x)
grid

## adding predictions, modelr::add_predictions()

grid <- grid%>%
  add_predictions(sim1_mod)
grid

## plot the predictions, works for any model in R.

ggplot(sim1,aes(x)) + geom_point(aes(y=y)) + 
  geom_line(aes(y=pred), data=grid, colour="red", size =1)


## Section 23.3.2 Residuals ##########

# the whtie noises / the pattern that model missed. just the distances betrween observed and predicted.

## lets add the residuals with add_residuals()

# using original data set, since residuals need actual y values:

sim1 <- sim1 %>% 
  add_residuals(sim1_mod)
sim1

## draw a geom_freqpoly to understand the spread of residuals.

ggplot(sim1, aes(resid)) + 
  geom_freqpoly(binwidth = 0.5)

# how far away are the predictions from observed values? # average of the residual will always be 0.
# its good to recreate plots using residuals instead of original predictor.




ggplot(sim1, aes(x, resid)) + 
  geom_ref_line(h = 0) +
  geom_point() 

########################## Exercises 23.3.3##################################
sim1a <- tibble(
  x = rep(1:10, each = 3),
  y = x * 1.5 + 6 + rt(length(x), df = 2)
)

ggplot(sim1a, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)


## more systematically, generating several simulations and plotting the line

simt <- function(i){
  tibble(
    x=rep(1:10,each=3),
    y=x*1.5+6 + rt(length(x), df=2),
    .id=i
  )
}

sims <- map_df(1:12,simt)

ggplot(sims, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "red") +
  facet_wrap(~.id, ncol = 4)

# what about normal distributions?

sim_norm <- function(i) {
  tibble(
    x = rep(1:10, each = 3),
    y = x * 1.5 + 6 + rnorm(length(x)),
    .id = i
  )
}

simdf_norm <- map_df(1:12, sim_norm)

ggplot(simdf_norm, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "red") +
  facet_wrap(~.id, ncol = 4)

# t distribution vs normal,. a t distribution has fatter tails than normal. T distribution assigns larger probbility to values further

tibble(
  x = seq(-5, 5, length.out = 100),
  normal = dnorm(x),
  student_t = dt(x, df = 2)
) %>%
  gather(distribution, density, -x) %>%
  ggplot(aes(x = x, y = density, colour = distribution)) +
  geom_line()

For a normal distribution with mean zero and standard deviation one, the probability of being greater than 2 is,

pnorm(2, lower.tail = FALSE)

For a Student's  
t
distribution with degrees of freedom = 2, it is more than 3 times higher,

pt(2, df = 2, lower.tail = FALSE)
#> [1] 0.0918
#> [1] 0.0228




### Exercise 23.2.2


### robust?

#use different distance measure, instead of root-mean-suqared, use mean-absolute distance.

make_prediction <- function(mod,data) {
  mod[1] + mod[2] * data$x
  
}
measure_distance <- function(mod,data) {
  diff <- data$y - make_prediction(mod,data)
  mean(abs(diff))
}

# need to define function, make prediction(), that takes numeric vector of length 2 and returns predictions


best <- optim(c(0, 0), measure_distance, data = sim1a)
best$par

## sim1a data, best parameters of least absolute deviation are:

best <- optim(c(0, 0), measure_distance, data = sim1a)
best$par

## using the sim1a data, parameters minimize least squares objective function:
measure_distance_ls <- function(mod, data) {
  diff <- data$y - (mod[1] + mod[2] * data$x)
  sqrt(mean(diff^2))
}

best <- optim(c(0, 0), measure_distance_ls, data = sim1a)
best$par


### Exercise 23.2.3 
model3 <- function(a, data) {
  a[1] + data$x * a[2] + a[3]
}

## linear combinations, so any a[1] =a1 and a[3] = a3, will have same fit where a[1] + a[3] == (a1 +a3)

measure_distance_3 <- function(a, data) {
  diff <- data$y - model3(a, data)
  sqrt(mean(diff^2))
}

Depending on our starting points, we can find different optimal values:
  
  
  best3a <- optim(c(0, 0, 0), measure_distance_3, data = sim1)
best3a$par
#> [1] 3.367 2.052 0.853
best3b <- optim(c(0, 0, 1), measure_distance_3, data = sim1)
best3b$par
#> [1] -3.47  2.05  7.69
best3c <- optim(c(0, 0, 5), measure_distance_3, data = sim1)
best3c$par
#> [1] -1.12  2.05  5.35
In fact there are an infinite number of optimal values for this model.






########################################################################


## Section 23.4 Forumlas and model families.

## recalled y~x, translate to y=a_1+a_2*x. 
#model_matrix() function takes a data frame and formula and returns a tibble that defines model equation.
#each column in the output is assocaited with one coefficient in the model.

df <- tribble(~y,~x1,~x2,
              4,2,5,
              5,1,6
)
model_matrix(df,y~x1)


# R addas the inercept to the model by having a column full of ones, if we want to drop it we use -1.

model_matrix(df,y~x1-1)

model_matrix(df,y~x1+x2)


### Section 23.4.1 Categorical Variables ###############
#problems arise when predictor is cateogrical, where y~sex, doesn't make sense, since sex isnt a number.
# R converts this to sex_male is one if sex is male, and zero otherwise.

df <- tribble(
  ~ sex, ~ response,
  "male", 1,
  "female", 2,
  "male", 1
)

model_matrix(df, response ~ sex)

##

ggplot(sim2) + 
  geom_point(aes(x, y))

## lets fit a model

mod2 <- lm(y~x,data=sim2)

grid <- sim2 %>% 
  data_grid(x) %>% 
  add_predictions(mod2)
grid

## categorical will predict the mean value for each cateogry.

ggplot(sim2, aes(x)) + 
  geom_point(aes(y = y)) +
  geom_point(data = grid, aes(y = pred), colour = "red", size = 4)


# cant make predictions about levels that we don't observe.
tibble(x = "e") %>% 
  add_predictions(mod2)



###############23.4.2 Interactions (continuous and categorical)####

ggplot(sim3, aes(x1, y)) + 
  geom_point(aes(colour = x2))


## Two possible models to fit to this data.

mod1 <- lm(y ~ x1 + x2, data = sim3)
mod2 <- lm(y ~ x1 * x2, data = sim3)

## two predictors, need to give data_grid() both variables, it finds all unique values of x1 and x2, then generates all combinations.
## need to also generate predictions from both models simulatneously, 
# we can use gather_predictions(), which adds each prediction as a row. #Cmoplement of gather_predictions() is spread_predictions() which adds each prediction toa new column
grid <- sim3 %>% 
  data_grid(x1, x2) %>% 
  gather_predictions(mod1, mod2)
grid

ggplot(sim3, aes(x1, y, colour = x2)) + 
  geom_point() + 
  geom_line(data = grid, aes(y = pred)) + 
  facet_wrap(~ model)

#Note that the model that uses + has the same slope for each line, but different intercepts. The model that uses * has a different slope and intercept for each line.

#Which model is better for this data? We can take look at the residuals. Here I've facetted by both model and x2 because it makes it easier to see the pattern within each group.


sim3 <- sim3 %>% 
  gather_residuals(mod1, mod2)

ggplot(sim3, aes(x1, resid, colour = x2)) + 
  geom_point() + 
  facet_grid(model ~ x2)


##Section 23.4.3 Interactions (two continuous)

mod1 <- lm(y ~ x1 + x2, data = sim4)
mod2 <- lm(y ~ x1 * x2, data = sim4)

grid <- sim4 %>% 
  data_grid(
    x1 = seq_range(x1, 5), 
    x2 = seq_range(x2, 5) 
  ) %>% 
  gather_predictions(mod1, mod2)
grid

# use of seq_range() inside data_grid(), isntead of using every unique value of x, going to use reguarly spaced grid of 5 values between the minimum and maximum numbers.

# two useful arguments to seq_range():
# pretty=TRUE, will generate a "pretty" sequence.
#trim = 0.1 will trim off 10% of tail values to focus more on values near the center.

x1 <- rcauchy(100)
seq_range(x1, n = 5)
#> [1] -115.9  -83.5  -51.2  -18.8   13.5
seq_range(x1, n = 5, trim = 0.10)
#> [1] -13.84  -8.71  -3.58   1.55   6.68
seq_range(x1, n = 5, trim = 0.25)
#> [1] -2.1735 -1.0594  0.0547  1.1687  2.2828
seq_range(x1, n = 5, trim = 0.50)

# expand = 0.1 is in some sense opposite of trim(), expands range by 10%


# next lets try and visualise the model. We have two continous predictors, imagine the model like 3d surface.
ggplot(grid, aes(x1, x2)) + 
  geom_tile(aes(fill = pred)) + 
  facet_wrap(~ model)


ggplot(grid, aes(x1, pred, colour = x2, group = x2)) + 
  geom_line() +
  facet_wrap(~ model)
ggplot(grid, aes(x2, pred, colour = x1, group = x1)) + 
  geom_line() +
  facet_wrap(~ model)

## the two models below show the interaction between two continous variables, need to consider both x1 and x2.


### Section 23.4.4 Transformations

##if transformations involve +, *, ^, or -, need to wrap it in I(), so R doesn't treat it like part of the model specification.
#For example, y ~ x + I(x ^ 2) is translated to y = a_1 + a_2 * x + a_3 * x^2. If you forget the I() and specify y ~ x ^ 2 + x, R will compute y ~ x * x + x. x * x means the interaction of x with itself
df <- tribble(
  ~y, ~x,
  1,  1,
  2,  2, 
  3,  3
)
model_matrix(df, y ~ x^2 + x)

model_matrix(df,y~I(x^2) + x)


## useful to use transformations for non-linear functions using poly()
# and fit equiations like y = a_1 + a_2 * x + a_3 * x^2 + a_4 * x ^ 3

model_matrix(df,y~poly(x,2))

## one major problem using poly(), outside the range of data, polynomials shoot to positive or engative infinity.

# one safer alternative is to use antural spline,
#splines::ns()

library(splines)
model_matrix(df, y ~ ns(x, 2))

## trying to approximate a non linear function

## fitting 5 models to this data.


mod1 <- lm(y ~ ns(x, 1), data = sim5)
mod2 <- lm(y ~ ns(x, 2), data = sim5)
mod3 <- lm(y ~ ns(x, 3), data = sim5)
mod4 <- lm(y ~ ns(x, 4), data = sim5)
mod5 <- lm(y ~ ns(x, 5), data = sim5)

grid <- sim5 %>% 
  data_grid(x = seq_range(x, n = 50, expand = 0.1)) %>% 
  gather_predictions(mod1, mod2, mod3, mod4, mod5, .pred = "y")

ggplot(sim5, aes(x, y)) + 
  geom_point() +
  geom_line(data = grid, colour = "red") +
  facet_wrap(~ model)


### Exercise 23.3.1
## Instead of using lm() to fit a straight line, you can use loess() to fit a smooth curve. Repeat the process of model fitting, grid generation, predictions, and visualization on sim1 using loess() instead of lm(). How does the result compare to geom_smooth()?


#### Exercise 23.4.2
For x1 * x2 when x2 is a categorical variable produces indicator variables x2b, x2c, x2d and variables x1:x2b, x1:x2c, and x1:x2d which are the products of x1 and x2* variables:
  
  
  
  x3 <- model_matrix(y ~ x1 * x2, data = sim3)
x3


all(x3[["x1:x2b"]] == (x3[["x1"]] * x3[["x2b"]]))

all(x3[["x1:x2c"]] == (x3[["x1"]] * x3[["x2c"]]))
#> [1] TRUE
all(x3[["x1:x2d"]] == (x3[["x1"]] * x3[["x2d"]]))
#> [1] TRUE

#> [1] TRUE
For x1 * x2 where both x1 and x2 are continuous variables, model_matrix() creates variables x1, x2, and x1:x2:
  
  x4 <- model_matrix(y ~ x1 * x2, data = sim4)
x4

Confirm that x1:x2 is the product of the x1 and x2,


all(x4[["x1"]] * x4[["x2"]] == x4[["x1:x2"]])
#> [1] TRUE
The asterisk * is good shorthand for an interaction since an interaction between x1 and x2 includes terms for x1, x2, and the product of x1 and x2.



####################### Section 23.4.5 Exercises ###########################
Using the basic principles, convert the formulas in the following two models into functions. (Hint: start by converting the categorical variable into 0-1 variables.)


mod1 <- lm(y ~ x1 + x2, data = sim3)
mod2 <- lm(y ~ x1 * x2, data = sim3)


model_matrix_mod1 <- function(.data) {
  mutate(.data,
         x2b = as.numeric(x2 == "b"),
         x2c = as.numeric(x2 == "c"),
         x2d = as.numeric(x2 == "d"),
         `(Intercept)` = 1
  ) %>%
    select(`(Intercept)`, x1, x2b, x2c, x2d)
}
model_matrix_mod1(sim3)


## Exercise 23.4.4
mod1 <- lm(y ~ x1 + x2, data = sim4)
mod2 <- lm(y ~ x1 * x2, data = sim4)

## add residuals
sim4_mods <- gather_residuals(sim4,mod1,mod2)

## frequency plots
ggplot(sim4_mods, aes(x = resid, colour = model)) +
  geom_freqpoly(binwidth = 0.5) +
  geom_rug()

ggplot(sim4_mods, aes(x = abs(resid), colour = model)) +
  geom_freqpoly(binwidth = 0.5) +
  geom_rug()
## mod2 has less residuals in tails of distributions


sim4_mods %>%
  group_by(model) %>%
  summarise(resid = sd(resid))




####################################################################


##############Section 23.5 Missing values ###################

Missing values obviously can not convey any information about the relationship between the variables, so modelling functions will drop any rows that contain missing values. R's default behaviour is to silently drop them, but options(na.action = na.warn) (run in the prerequisites), makes sure you get a warning.
df <- tribble(
  ~x, ~y,
  1, 2.2,
  2, NA,
  3, 3.5,
  4, 8.3,
  NA, 10
)

mod <- lm(y ~ x, data = df)

## to suppress the warning, set na.action = na.exclude:
mod <- lm(y ~ x, data = df, na.action = na.exclude)
