### Scetion 24 Model Building #################
library(tidyverse)
library(modelr)
options(na.action = na.warn)

library(nycflights13)
library(lubridate)

##Section 24.2 Low quality diamonds?

ggplot(diamonds, aes(cut, price)) + geom_boxplot()
ggplot(diamonds, aes(color, price)) + geom_boxplot()
ggplot(diamonds, aes(clarity, price)) + geom_boxplot()

## diamond weight is important factor for price

ggplot(diamonds, aes(carat,price)) + geom_hex(bins=50
)

# lets focus on smaller diamonds, and log transform the carat and price variables.

diamonds2 <- diamonds%>%
  filter(carat<=2.5) %>%
  mutate(lprice=log2(price, lcarat=log2(carat)))

mod_diamond <- lm(lprice~lcarat,data=diamonds2)

grid <- diamonds2 %>% 
  data_grid(carat = seq_range(carat, 20)) %>% 
  mutate(lcarat = log2(carat)) %>% 
  add_predictions(mod_diamond, "lprice") %>% 
  mutate(price = 2 ^ lprice)

ggplot(diamonds2, aes(carat, price)) + 
  geom_hex(bins = 50) + 
  geom_line(data = grid, colour = "red", size = 1)

## examine the residuals
diamonds2%>% diamonds2 %>%
  add_residuals(mod_diamond,"lresid")

ggplot(diamonds2, aes(cut, lresid)) + geom_boxplot()
ggplot(diamonds2, aes(color, lresid)) + geom_boxplot()
ggplot(diamonds2, aes(clarity, lresid)) + geom_boxplot()


To interpret the y axis, we need to think about what the residuals are telling us, and what scale they are on. A residual of -1 indicates that lprice was 1 unit lower than a prediction based solely on its weight.  
2
???
  1
is 1/2, points with a value of -1 are half the expected price, and residuals with value 1 are twice the predicted price.


## Section 24.2.2 A more complicated model

mod_diamond2 <- lm(lprice~lcarat+color+cut+clarity,data=diamonds2)

# lets get  grid

grid <- diamonds2%>%
  data_grid(cut,.model=model=mod_diamond2) %>%
  add_predictions(mod_diamond2)
grid

ggplot(grid, aes(cut, pred)) + 
  geom_point()


diamonds2 <- diamonds2 %>% 
  add_residuals(mod_diamond2, "lresid2")

ggplot(diamonds2, aes(lcarat, lresid2)) + 
  geom_hex(bins = 50)

## notice some diamonds have large residuals

diamonds2 %>% 
  filter(abs(lresid2) > 1) %>% 
  add_predictions(mod_diamond2) %>% 
  mutate(pred = round(2 ^ pred)) %>% 
  select(price, pred, carat:table, x:z) %>% 
  arrange(price)

######### Exercises 24.2.3####################################
##plot lcarat vs lprice


###Exercise 24.2.2
If log(price) = a_0 + a_1 * log(carat), what does that say about the relationship between price and carat?
  mod_log <- lm(log2(price) ~ log2(carat), data=diamonds)
mod_log

## the estimated relationship?

tibble(carat=seq(0.25,5, by = 0.25)) %>%
  add_predictions(mod_log) %>% ## adding predictions
  ggplot(aes(x=carat,y=2^pred)) + geom_line()
+ labs(x="carat", y="price")


## shows a non linear increase.

2^coef(mod_log)[2]

## lets check values of carat variable by increasing carat from 1 to 2
2^(predict(mod_log, newdata = tibble(carat = 2)) -
     predict(mod_log, newdata = tibble(carat = 1)))

####### Exercise 24.2.3#################
diamonds2 %>%
  filter(abs(lresid2) > 1) %>%
  add_predictions(mod_diamond2) %>%
  mutate(pred = round(2^pred)) %>%
  select(price, pred, carat:table, x:z) %>%
  arrange(price)


##Exercise 24.3.3

Create a new variable that splits the wday variable into terms, but only for Saturdays, i.e., it should have Thurs, Fri, but Sat-summer, Sat-spring, Sat-fall How does this model compare with the model with every combination of wday and term?
  
  daily <- daily%>%
  mutate(
    wday2=
      case_when(
        wday=="Sat" & term == "summer" ~ "Sat-summer",
        wday== "Sat" & term == "fall" ~ "Sat-fall",
        wday== "Sat" & term == "spring" ~ "Sat-spring",
        TRUE~as.character(wday)
      )
  )

mod3 <- lm(n ~ wday2, data = daily)

daily %>%
  gather_residuals(sat_term = mod3, all_interact = mod2) %>%
  ggplot(aes(date, resid, colour = model)) +
  geom_line(alpha = 0.75)



)
)



##

It's a little frustrating that Sunday and Saturday are on separate ends of the plot. Write a small function to set the levels of the factor so that the week starts on Monday.

See the chapter Factors for the function fct_relevel(). Use fct_relevel() to put all levels in-front of the first level ("Sunday").

monday_first <- function(x) {
  fct_relevel(x, levels(x)[-1])
}
Now Monday is the first day of the week.

daily <- daily %>%
  mutate(wday = wday(date, label = TRUE))
ggplot(daily, aes(monday_first(wday), n)) +
  geom_boxplot() +
  labs(x = "Day of Week", y = "Number of flights")




###############



## Section 24.3 What affects the number of daily flights?###########

daily <-  flights %>%
  mutate(date=make_date(year,month,day)) %>%
  group_by(date) %>%
  summarise (n=n())
daily


ggplot(daily, aes(date, n)) + 
  geom_line()

##plotting number of daily flights over the year



### Setion 24.3.1 Day of Week.

daily <- daily %>% 
  mutate(wday = wday(date, label = TRUE))
ggplot(daily, aes(wday, n)) + 
  geom_boxplot()

## examining the day of week effect by examining flight numbers by day of week.



## how to account for pattern that supports fewer flights on weekends?

mod <- lm(n ~ wday, data = daily)

grid <- daily %>% 
  data_grid(wday) %>% 
  add_predictions(mod, "n")

ggplot(daily, aes(wday, n)) + 
  geom_boxplot() +
  geom_point(data = grid, colour = "red", size = 4)

##compute visuals on residuals

daily <- daily %>%
  add_residuals(mod)
daily%>%
  ggplot(aes(date,resid)) + geom_ref_line(h=0) + 
  geom_line()


## the change in y axis, we see the deviation from the expected number of flights.

ggplot(daily, aes(date, resid, colour = wday)) + 
  geom_ref_line(h = 0) + 
  geom_line()

# cannot predict the number of flights on staurday on Staurday.
## notice some days have far fewer flights than expected.

daily %>%
  filter(resid<-100)


## long term smoothing?

daily %>% 
  ggplot(aes(date, resid)) + 
  geom_ref_line(h = 0) + 
  geom_line(colour = "grey50") + 
  geom_smooth(se = FALSE, span = 0.20)



###### Section 24.3.2 Seasonal Staurday Effect

daily%>%
  filter(wday=="Sat") %>%
  ggplot(aes(date,n)) +
  geom_point() +
  geom_line() +
  scale_x_date(NULL, date_breaks = "1 month", date_labels = "%b")

# many people go on holiday in the summer and people don't mind travelling on saturdays for vacations.
term <- function(date) {
  cut(date, 
      breaks = ymd(20130101, 20130605, 20130825, 20140101),
      labels = c("spring", "summer", "fall") 
  )
}

daily <- daily %>% 
  mutate(term = term(date)) 

daily %>% 
  filter(wday == "Sat") %>% 
  ggplot(aes(date, n, colour = term)) +
  geom_point(alpha = 1/3) + 
  geom_line() +
  scale_x_date(NULL, date_breaks = "1 month", date_labels = "%b")

### how does new variable affect other days of week?
daily %>% 
  ggplot(aes(wday, n, colour = term)) +
  geom_boxplot()
## significant variation across the terms, so fitting a seperate day of week effect for each term is reasonable
mod1 <- lm(n ~ wday, data = daily)
mod2 <- lm(n ~ wday * term, data = daily)

daily %>% 
  gather_residuals(without_term = mod1, with_term = mod2) %>% 
  ggplot(aes(date, resid, colour = model)) +
  geom_line(alpha = 0.75)

grid <- daily %>% 
  data_grid(wday, term) %>% 
  add_predictions(mod2, "n")

ggplot(daily, aes(wday, n)) +
  geom_boxplot() + 
  geom_point(data = grid, colour = "red") + 
  facet_wrap(~ term)

mod3 <- MASS::rlm(n ~ wday * term, data = daily)

daily %>% 
  add_residuals(mod3, "resid") %>% 
  ggplot(aes(date, resid)) + 
  geom_hline(yintercept = 0, size = 2, colour = "white") + 
  geom_line()



### Section 24.3.3 Computed variables #############
#usually a good idea to bundle creation of variables up into a function, so there's no chance of accidentally applying a different transformation in different places.

compute_vars <- function(data) {
  data %>% 
    mutate(
      term = term(date), 
      wday = wday(date, label = TRUE)
    )
}

wday2 <- function(x) wday(x, label = TRUE)
mod3 <- lm(n ~ wday2(date) * term(date), data = daily)
############