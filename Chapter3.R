############## Section 3.7 Statistical Transformation ##############
library(tidyverse)
ggplot(data=diamonds)+geom_bar(mapping=aes(x=cut))

## X axis displays cut
## displays the cut, a variable from diamonds, y axis is the count.
## bar graphs on the y axis bin the data and plot the bin bounts.
#every geom has a default stat, where every stat has a default geom

ggplot(data = diamonds) + 
  stat_count(mapping = aes(x = cut))

demo <- tribble(
  ~cut,         ~freq,
  "Fair",       1610,
  "Good",       4906,
  "Very Good",  12082,
  "Premium",    13791,
  "Ideal",      21551
)

ggplot(data = demo) +
  geom_bar(mapping = aes(x = cut, y = freq), stat = "identity")

## want to voerride the default mapping from transofmred variables to aesthetics, so display a bar chart of proportion rather than count:
ggplot(data=diamonds)+geom_bar(mapping=aes(x=cut,y=stat(prop),group=1))

## now lets compute the summary stats for each.
ggplot(data=diamonds)+stat_summary(mapping=aes(x=cut,y=depth),fun.ymin=min,fun.ymax=max,fun.y=median)

## to see list of stats, ?stat_bin

## 3.71 exercises
#geom_bar makes height of bar propotional to number of cases in each group, if you want heights of bars to represent true values in data, geom_col is better.
##  WHAT DOES STAT_smooth() do? -> helps identify the patterns in overplotting

## notice the code below, if group is not set to 1, all graphs are all proportioned to 1.
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, y = ..prop..))

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = color, y = ..prop..))

### Position adjustments  section 3.8##

## can also colour bar charts, using colour aesthetic, or fill:

ggplot(data=diamonds)+geom_bar(mapping=aes(x=cut,colour=cut))
ggplot(data=diamonds)+geom_bar(mapping=aes(x=cut,fill=cut))

#using command like fill aesthetic to another variable like clairty, the bars are automatically stacked, and each color represents a combo of cut and clarity.

# example
ggplot(data=diamonds)+geom_bar(mapping=aes(x=cut,fill=clarity))

## if dont wanta  stacked bar chart, can use three other options, "identity", "dodge", or "fill"
#### Position = "identity" will place each object exactly where it falls in the context of the graph, not useful for bars since of the overlap.

ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity)) + 
  geom_bar(alpha = 1/5, position = "identity")
ggplot(data = diamonds, mapping = aes(x = cut, colour = clarity)) + 
  geom_bar(fill = NA, position = "identity")

## identity position adjustment more useful for 2d geoms, like points, where it is the default.
## can also use position ="fill" works like stacking, but makes each set of stacked bars the same height, making it easier to compare propotions across groups.
ggplot(data=diamonds) + geom_bar(mapping=aes(x=cut,fill=clarity),position="fill")

## position "dodge" places overlapping objects beside one another -> to compare individual values
ggplot(data=diamonds)+geom_bar(mapping=aes(x=cut,fill=clarity),position="dodge")

## recall before the issue of overplotting, where the arrangements make it difficult to see where the mass of the data is.
## on a scatterplot, can see where this overplotting occurs
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), position = "jitter")

## 3.8.1 exercises:
#whats wrong with the following plot?
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
  geom_point()

## doesnt show the mass of the plots ##look at the below

ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
  geom_point(position="jitter")

##geom_count counts the number of observations at each location and then maps the count to point area, helpful for discrete data and overplotting
##jitter geom is convenit shortcut for geom_point (position="jitter"), adds a small amount of random variation (or white noise) to location and is helpful for handling overplotting caused by discreteness in smaller datasets.
##notice default position adjustment for geom_boxplot() is "dodge2

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot()


########### COORDINATE SYSTEMS 3.9 ####################

## coord_flip() swaps the x and y axes, so if we wanted horizontal boxplots, or to adjust overlapping labeels on x axis.

ggplot(data=mpg,mapping=aes(x=class,y=hwy))+geom_boxplot()

## compared to a coordinate flip

ggplot(data=mpg,mapping=aes(x=class,y=hwy))+geom_boxplot()+coord_flip()

##coord_quickmap() sets aspect ratio correctly for maps, imporatnt for plotting saptial data with ggplot2

nz <- map_data("nz")

ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black")

ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black") +
  coord_quickmap()


## coord_polar() uses polar coordinates. Polar coordinates reveal interesting connection between a bar chart and coxcomb chart.
bar <- ggplot(data = diamonds) + 
  geom_bar(
    mapping = aes(x = cut, fill = cut), 
    show.legend = FALSE,
    width = 1
  ) + 
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)

bar + coord_flip()
bar + coord_polar()


########### SECTION3.10 LAYERD GRAMMAR OF GRAPHICS####################


## can make any type of plot with ggplot, now lets added position adjustments, stats, coordinate sysstems, faceting etc..

#ggplot(data = <DATA>) + 
#  <GEOM_FUNCTION>(
#    mapping = aes(<MAPPINGS>),
#    stat = <STAT>, 
#    position = <POSITION>
# ) +
## <FACET_FUNCTION>

#this template takes seven parameters, usually ggplot2 will provide useful defaults for everything, except data mappings and geom function.