##section 15 Factors
##In R, factors are used to work with categorical variables, variables that have a fixed and known set of possible values. They are also useful when you want to display character vectors in a non-alphabetical order.

Historically, factors were much easier to work with than characters. As a result, many of the functions in base R automatically convert characters to factors. This means that factors often crop up in places where they're not actually helpful. Fortunately, you don't need to worry about that in the tidyverse, and can focus on situations where factors are genuinely useful.
library(tidyverse)

## use forcats package
# useful to deal with categorical variables
#Creating factors 

#we have a variable that records month:
x1 <- c("Dec","Apr","Jan", "Mar")

# using a string to record this variable has two problems:
# only twevel possible months, nothing saving you from typos

# it also doesnt sort well.

sort(x1)

## let just create a factor, by creating a list of valid levels

month_levels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug"
                  , "Sep", "Oct", "Nov", "Dec")

## now create a factor :
y1 <-  factor(x1,levels=month_levels)
y1
sort(y1)

# any values not in set will be converted to NA:

# if you want a warning can do :readr:parse_factor():
y2 <- parse_factor(x2, levels = month_levels)

# if we omit levels, they will take from data in alphabetical order
factor(x1)
##sometimes its better to order of the levels match the order of the first appearance in the data.
# can do this when creating factor by setting levels to unique(x), or after the fact, with fct_inorder():

f1 <- factor(x1,levels=unique(x1))

f2 <- x1%>% factor() %>% fct_inorder()

# if ever need to access the set of valid levels directly, do so with levels()

levels(f2)


## Section 15.3 General social Survey


gss_cat

## hard to see variables  levels in tibble, use count()
gss_cat %>% count(race)

# can also use barchart
ggplot(gss_cat,aes(race)) + geom_bar()


## ggplot2 will drop levels that have no values.
# can force display them with

ggplot(gss_cat, aes(race)) +
  geom_bar() +
  scale_x_discrete(drop = FALSE)

## levels represent valid values that simply did not occur in this dataset.

## common when working with factors to change the order and change the value of levels

#### Section 15.3.1 Exercises
#lets try geom_bar()

rincome_plot <- gss_cat%>%ggplot(aes(x=rincome))+ 
  geom_bar()
rincome_plot

## overplapping lables

rincome_plot+theme(axis.text.x=element_text(angle=90,hjust=1))
## this oen looks better


rincome_plot+coord_flip()

# could improve plot by doing : removing the "Not applicable" responses,
renaming "Lt $1000" to "Less than $1000",
using color to distinguish non-response categories ("Refused", "Don't know", and "No answer") from income levels ("Lt $1000", .),
adding meaningful y- and x-axis titles, and
formatting the counts axis labels to use commas.

gss_cat%>%
  filter(!rincome %in% c("Not applicable")) %>%
  mutate(rincome=fct_recode(rincome,
                            "Less than $1000" = "Lt $1000")) %>% 
  mutate(rincome_na=rincome %in% c("Refused", "Don't know", "No answer")) %>%
  ggplot(aes(x=rincome,fill=rincome_na)) +
  geom_bar() +
  coord_flip() +
  scale_y_continuous("Number of Respondents", labels = scales::comma) +
  scale_x_discrete("Respondent's Income") +
  scale_fill_manual(values = c("FALSE" = "black", "TRUE" = "gray")) +
  theme(legend.position = "None")

### Exercise 15.3.2

gss_cat%>% count(relig) %>%
  arrange(desc(n)) %>%
  head(10)

gss_cat %>% count(partyid) %>%
  arrange(desc(n)) %>% head(1)

levels(gss_cat$denom)

gss_cat %>%
  filter(!denom %in% c(
    "No answer", "Other", "Don't know", "Not applicable",
    "No denomination"
  )) %>%
  count(relig)


## Section 15.4 Modifying Factor Order

## often useful to change the order of the factor levels in a visualisation,
# explore the average number of hours spent watching TV per day across religions

relig_summary <- gss_cat%>%
  group_by(relig)%>%
  summarise(
    age= mean(age,na.rm=TRUE),
    tvhours=mean(tvhours,na.rm=TRUE),
    n=n()
  )


ggplot(relig_summary, aes(tvhours, relig)) + geom_point()

## re order the  plot by levels
#It is difficult to interpret this plot because there's no overall pattern. We can improve it by reordering the levels of relig using fct_reorder(). fct_reorder() takes three arguments:

#f, the factor whose levels you want to modify.
#x, a numeric vector that you want to use to reorder the levels.
#Optionally, fun, a function that's used if there are multiple values of x for each value of f. The default value is median.
#


ggplot(relig_summary,aes(tvhours,fct_reorder(relig,tvhours))) + geom_point()

## lets rewrite this

relig_summary %>%
  mutate(relig=fct_reorder(relig,tvhours)) %>%
  ggplot(aes(tvhours,relig)) +
  geom_point()

## lets loko at average age and how it varies across reported income level?

rincome_summary <- gss_cat %>%
  group_by(rincome)%>%
  summarise(age=mean(age,na.rm=TRUE),
            tvhours=mean(tvhours,na.rm=TRUE),
            n=n()
  )
ggplot(rincome_summary, aes(age, fct_reorder(rincome, age))) + geom_point()


### Another type of reordering is useful when you are colouring the lines on a plot. fct_reorder2() reorders the factor by the y values associated with the largest x values. This makes the plot easier to read because the line colours line up with the legend.
by_age <- gss_cat %>%
  filter(!is.na(age)) %>%
  count(age,marital) %>%
  group_by(age) %>%
  mutate(prop=n/sum(n))

ggplot(by_age, aes(age,prop,color=martial)) + 
  geom_line(na.rm=TRUE)
ggplot(by_age, aes(age, prop, colour = fct_reorder2(marital, age, prop))) +
  geom_line() +
  labs(colour = "marital")

gss_cat %>%
  mutate(marital = marital %>% fct_infreq() %>% fct_rev()) %>%
  ggplot(aes(marital)) +
  geom_bar()



### Section 15.5 Modifying factor levels #####

## changing values is powerful, fct_recode(), allows to recode, or change the value of each level.
gss_cat$partyid
gss_cat%>% count(partyid)

## levels are terse and inconsistent. Lets tweak them to be longer and use a aprallel construction.
gss_cat%>%
  mutate(partyid=fct_recode(partyid,    "Republican, strong"    = "Strong republican",
                            "Republican, weak"      = "Not str republican",
                            "Independent, near rep" = "Ind,near rep",
                            "Independent, near dem" = "Ind,near dem",
                            "Democrat, weak"        = "Not str democrat",
                            "Democrat, strong"      = "Strong democrat")) %>%
  count(partyid)

fct_recode will leave levels that arent mentioned as is. and if you accidentally refer to a level that doesnt exists(
  ## to combine groups, can assign multiple old levels to the same new level.
  
  gss_cat %>%
    mutate(partyid = fct_recode(partyid,
                                "Republican, strong"    = "Strong republican",
                                "Republican, weak"      = "Not str republican",
                                "Independent, near rep" = "Ind,near rep",
                                "Independent, near dem" = "Ind,near dem",
                                "Democrat, weak"        = "Not str democrat",
                                "Democrat, strong"      = "Strong democrat",
                                "Other"                 = "No answer",
                                "Other"                 = "Don't know",
                                "Other"                 = "Other party"
    )) %>%
    count(partyid)
  
  ## careful with this technique, can group together categories that are truly differnt could end up with misleading results
  
  ## to collapse a lot of levels, fct_collapse() is a useful variant of fct_recode().
  
  gss_cat %>%
    mutate(partyid = fct_collapse(partyid,
                                  other = c("No answer", "Don't know", "Other party"),
                                  rep = c("Strong republican", "Not str republican"),
                                  ind = c("Ind,near rep", "Independent", "Ind,near dem"),
                                  dem = c("Not str democrat", "Strong democrat")
    )) %>%
    count(partyid)
  
  gss_cat %>%
    mutate(relig = fct_lump(relig)) %>%
    count(relig)
  ## also lump together all the small groups to make a plot or table simpler.
  
  # default behaviour is to proressively lump together the smallest groups, ensuring the aggregate is still the smallest group,
  # ensuring the aggregate is still the smallest group, and possible to overcollapse.
  
  gss_cat %>%
    mutate(relig = fct_lump(relig, n = 10)) %>%
    count(relig, sort = TRUE) %>%
    print(n = Inf)
  
  #setting the n parameter to specify how many groups  we want to keep
  
  
  ### Exercise 15.5.1
  ## how have proportions changed over time?
  
  levels(gss_cat$partyid)
  
  gss_cat%>%
    mutate(partyid=
             fct_collapse(partyid,
                          other=c("No answer", "Don't know", "Other party"),
                          rep=c("Strong republican", "Not str republican"),
                          ind=c("Ind, near rep", "Independent", "Ind, near dem"),
                          dem=c("Not str democrat", "Strong democrat")
             )
    ) %>%
    count(year,partyid) %>%
    group_by(year) %>%
    mutate(p=n/sum(n)) %>%
    ggplot(aes(x=year, y=p, colour =fct_reorder2(partyid,year,p))) +
    geom_point()+geom_line()+
    labs(colour= "Party id")
  
  
  ## Exercise 15.5.2
  ## how to colapse rincome into smallest set of categories
  ## group all non-responses into one category, group other categories into a smaller number.
  ## there is clear ordering, dont use fct_lump
  
  levels(gss_cat$rincome)
  
  library("stringr")
  gss_cat %>%
    mutate(
      rincome =
        fct_collapse(
          rincome,
          `Unknown` = c("No answer", "Don't know", "Refused", "Not applicable"),
          `Lt $5000` = c("Lt $1000", str_c(
            "$", c("1000", "3000", "4000"),
            " to ", c("2999", "3999", "4999")
          )),
          `$5000 to 10000` = str_c(
            "$", c("5000", "6000", "7000", "8000"),
            " to ", c("5999", "6999", "7999", "9999")
          )
        )
    ) %>%
    ggplot(aes(x = rincome)) +
    geom_bar() +
    coord_flip()
  