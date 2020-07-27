################### Section 11, Data import #####################
##  data from plaintext rectangular files into R
library(tidyverse)
library(hms)
## turn flat files in data frames:

## using the rear package:

## most readr' functions are concerned with turning flat files into data frames:

#read_csv() reads comma delimited files, read_csv2() reads semicolon separated files,
# read_tsv() reads tab delimited files, and read_delim() reads in files with any delimiters.
#read_fwf() reads fixed width files. You can specify fields either by their widths with fwf_widths() or their position with fwf_positions(). read_table() reads a common variation of fixed width files where columns are separated by white space.

#read_log() reads Apache style log files. (But also check out webreadr which is built on top of read_log() and provides many more helpful tools.)

###focus on read_csv(), can apply to other methods.

##first argument to read-csv(), the path to the file

heights <- read_csv("data/heights.csv")

##parased with column specification:

read_csv("a,b,c
1,2,3
4,5,6")

## read_csv(), use the first line of the data for column names#

# to tweak this behaviour, can use skip() to skip the first n lines or use comment="#" to drop all lines that start with #
read_csv("The first line of metadeta
         The second line of emtadeta
         x,y,z
         1,2,3",skip=2)
read_csv("# A comment I want to skip
  x,y,z
  1,2,3", comment = "#")

## sometimes data might not have column names. Can sue col_names=FALSE to 
#read_csv() not to treat the first row as headings and label them sequentially from x1 to xn:

read_csv("1,2,3\n4,5,6",col_names=FALSE)

## TO REPRESENT MISSING VALUES::
read_csv("a,b,c\n1,2,.", na=".")


############## 11.2.1 Compared to Base R ##############
#why use read_csv() over read.csv()?
#read_csv() is faster, produces tibbles, they don't convert character vectors to factors, use row names, or munge the column names.
# more reproducible.

### Excerise 11.2.1 #########
#what function would you use to read a file where fields were separated with "|"?
## use read_delim(file,delim = "|")

#What are the most important arguments to read_fwf()?
#  
# The most important argument to read_fwf() which reads "fixed-width formats", is col_positions which tells the function where data columns begin and end.


## Exercise 11.2.4 ########
## sometimes strings in CSV file contain commands.
## what arguments do we need to sread the following text into a data frame
## using read_delim()

"x,y\n1,'a,b'", #specify delimtier
x <- "x,y\n1,'a,b'"
read_delim(x,",",quote="'")

## read_csv now supports quote argument,
read_csv(x,quote="'")





## whats wrong with the inline csv files?

read_csv("a,b \n1,2,3\n4,5,6")
## only two coloumns, a,b but we ahve 3 columns in each row

## add another column in 
read_csv("a,b,c\n1,2\n1,2,3,4")


## number of columns in the data do not match the number of columns in the ehader (three)

read_csv("a,b\n\"1")

read_csv("a,b\n1,2\na,b")

#both a and b are treated,
read_csv("a;b\n1;3
        ")

#use read csv2() instead to read the character vectors separated by ;
read_csv2("a;b\n1;3")



################# 11.3 Parsing a vector ####################
#parse_*()

## theses fucntions take a character vector and return a more speciaslised vector like a logical,intger or date:

str(parse_logical(c("TRUE","FALSE","NA")))

str(parse_integer(c("1","2","3")))

str(parse_date(c("2010-01-01","1979-12-14")))

## Important for readr.
## prase_*() functions are uniform, the first argument is a character vector to parse, and the na argument specifies which strings should be treated as missing:

parse_integer(c("1","231",".","456"),na=".")

# if a parsing fails, we get a warning
x <- parse_integer(c("123", "345", "abc", "123.45"))

## failures will be missing in the output.

# to fix parsing failures, use problems()
# which returns a tibble, to maniupulate with dplyr
#problems()
problems(x)

## eight types of parsers #
parse_logical() and parse_integer() parse logicals and integers respectively. There's basically nothing that can go wrong with these parsers so I won't describe them here further.

parse_double() is a strict numeric parser, and parse_number() is a flexible numeric parser. These are more complicated than you might expect because different parts of the world write numbers in different ways.

parse_character() seems so simple that it shouldn't be necessary. But one complication makes it quite important: character encodings.

parse_factor() create factors, the data structure that R uses to represent categorical variables with fixed and known values.

parse_datetime(), parse_date(), and parse_time() allow you to parse various date & time specifications. These are the most complicated because there are so many different ways of writing dates.



########## Section 11.3.1 Numbers ###############
#readr has notion of a "locale", an object that specifies parsing options that differe come place to place
# when parsing numbers,  imporant to account for decimal mark.
#can override the default value of . by creating a new locale and setting deciaml_mark argument:

## lets parse some numbers

parse_double("1.23")
parse_double("1.23",locale=locale(decimal_market=","))

#parse_number() address non numeric characters before and after the numer, $ and %.

parse_number("$100")
parse_number("20%")
parse_number("it cost $123.45")

# final problem is addressed by cominbation of parse_number() and the locale as parse_number() will ignore the grouping mark

# used in amerca,
#parse_number("$123,456,789")

## however, in europe:
parse_number("123.456.789",locale=locale(grouping_mark="."))

## used in switzerland
parse_number("123'456'789", locale = locale(grouping_mark = "'"))


#### Section 11.3.2 Srings ######################

#parse_character() should be simple, it just returns its input:

# underlying representation ofa  string using charToRaw():

charToRaw("Hadley")


#3 this will represent hexadecial number, ASCII.

parse_character(x1, locale = locale(encoding = "Latin1"))

parse_character(x2, locale = locale(encoding = "Shift-JIS"))

## can also guess encoding:

guess_encdoing(charToRaw(x1))

guess_encoding(charToRaw(x2))



########### Section 11.3.3 Factors #################

## factors to reprsent categorical variables that have a known set of possible values.
## give parse_factor() a vector of known levels to generate a warning whenever an unexpected value is present

fruit <- c("apple","banana")
x <- parse_factor(c("apple", "banana", "bananana"), levels=fruit)

problems(x)


## Section 11.3.4 Dates, dates-times, and times ############

## pick between three parsers depending on whether we wanta  date (numer of days since 1970-01-01, a date-time (number of seconds since midnight 1970-01-01), or a time (number of seconds wsince midnight).

parse_datetime("2010-10-01T2010")
parse_datetime("20101010")

#parse_date() expects a four digit year, a - or /, the month, a - or /, thjen the day:
parse_date("2010-10-01")

#parse_time() : expect the hour, :, minutes, optionally : and seconds, and an optinal am/pm specifier
library(hms)
parse_time("01:10 am")
parse_time("20:10:01")



  
### Exercises :
## Generate the correct format string to parse each of the following dates and times:
d1 <- "January 1, 2010"
d2 <- "2015-Mar-07"
d3 <- "06-Jun-2017"
d4 <- c("August 19 (2015)", "July 1 (2015)")
d5 <- "12/30/14" 
t1 <- "1705"
t2 <- "11:15:10.12 PM"
parse_date(d1,"%B %d, %Y")
parse_date(d2,"%Y-%b-%d")
parse_date(d3,"%d-%b-%Y")
parse_date(d4,"%B %d (%Y)")
parse_date(d5,"%m/%d/%y")
parse_time(t1,"%H%M")
parse_time(t2,"%H:%M:%OS %p")



####### SEction 11.4 parsing a file ####################
# now that we parse an indivdual vector, returning to beginning to see how readr parses a file.
##
## how readr automatically guesses the type of each column
## how to overide the default specification

#readr uses a heuristic to figure out the type of each column: it reads the first 1000 rows and uses some (moderately conservative) heuristics
#to figure out the type of each column. You can emulate this process with a character vector using guess_parser(),
#which returns readr's best guess, and parse_guess() which uses that guess to parse the column
#
#guess_parser() which returns readr's best guess and parse_guess() 
#which uses that guess to parse the column:

guess_parser("2010-10-01")
guess_parser("15:01")
guess_parser(c("TRUE","FALSE"))
guess_parser(c("1","5","9"))
guess_parser(c("12,352,561"))
str(parse_guess("2010-10-10"))

## Heuristic tries each of the type until it finds a match: 
ogical: contains only "F", "T", "FALSE", or "TRUE".
integer: contains only numeric characters (and -).
double: contains only valid doubles (including numbers like 4.5e-5).
number: contains valid doubles with the grouping mark inside.
time: matches the default time_format.
date: matches the default date_format.
date-time: any ISO8601 date.

## section 11.4.2 Problems ###############
## two problems: thousand of rows might be a special case, readr guesses a type that is not sufficiently general, column of doubles that only contains integers in first 
# 1000 rows.
## problem two is that coluimn could contain lots of missing values, if only first 1000 contain only NAs

## consider a challening csv that illustrates both problems

challenge <- read_csv(readr_example("challenge.csv"))

## readr_example finds the path to one of the files incluided
# two printed oputputs: column specification generated by looking at the first 1000 rows
# first five parsving failures, problems() is helpful here
problems(challenge)
## y column parsing problems

tail(challenge)
## use date parser instead

## to fix the call, copy and apste the column specification into the original call

challenge <- read_csv(
  readr_example("challenge.csv"), 
  col_types = cols(
    x = col_double(),
    y = col_logical()
  )
)

challenge <- read_csv(
  readr_example("challenge.csv"), 
  col_types = cols(
    x = col_double(),
    y = col_date()
  )
)
tail(challenge)

## every parse_xyz() function has acoressponding col_Xyz() function..
# you use parse_xyz(), when the data is in a character vector in R already.
# use col_xyz (), when u want to tell readr how to load data.

##highly recommend always supplying col_types, building yp from the print out provided by readr.
## this ensures that we have consistent and reproduciable data
#If you rely on the default guesses and your data changes, readr will continue to read it in. If you want to be really strict, use stop_for_problems(): that will throw an error and stop your script if there are any parsing problems.


######## Other Strategies section 11.4.3 ##########
## in previous eaxmple we got unlucky, ifw e look at more than one row than the default, we can correctly prase in one shot

##
challenge2 <- read_csv(readr_example("challenge.csv"),guess_max=1001)

challenge2
## consider reading in all columns as character vectors:

challenge2 <- read_csv(readr_example("challenge.csv"),col_types=cols(.default=col_character()))

# useful in conjuction with type_convert(), applie the parsing heuristics to character columns in a data frame
df <- tribble(~x, ~y,
              "1" , "1.21",
              "2" , "2.32",
              "3" , "4.56"
              )

## notice the coilumn types:

type_convert(df)

## if reading larger vile, set n_max to small number like 10,000 or 100,000.
# will accelerate iterations while you eliminate commmon problems.

#If you're having major parsing problems, sometimes it's easier to just read into a character vector of lines with read_lines(),
#or even a character vector of length 1 with read_file(). Then you can use the string parsing skills you'll learn later to parse more exotic formats.


###### Section 11.5 Writing to a file #################
##readr also comes with two useful functions, to write data back to disck:
# write_csv() and write_tsv()

## always encoding strings in UTF-8
#saving dates and date-times in ISO8601 format
# to export a csvfile to excel, use write_excel_csv().
#this writes a special character (a byte order mark) at the start of file.
##most importmant arguments are x, (data frame to save), and path(location to save)
# one can also specify how missing values are written with na, and if you want to append to an existing file.

write_csv(challenge,"challenge.csv")
challenge
write_csv(challenge,"challenge-2.csv")
read_csv("challenge-2.csv")

## makes CSVs a little unreliable for caching iterim results, need to recreate the column specification every time.

## two alternatives:
#one : write_rds() and read_rds() are uniform wrappers around base functions readRDS() and saveRDS().
# these store data in R's custom binary format called RDS:
write_rds(challenge,"challenge.rds")
read_rds("challenge.rds") ## saves the column parses


## option two is feather package, implements a fast binary file format to be shared across programming langauges:
library(feather)
write_feather(challenge,"challenge.feather")
read_feather("challenge.feather")

## faster thjan RDS and is usuable outside R. RDS supports list columns, feather does not


#### Other types of data: section 11.6
# toget other types of data in R, using tidyverse packages listed below:
#Rectangular data:
haven reads SPSS,Stata, and SAS
# readxl reads excel files (both xls and .xlsx)
# DBI, along with database specific backend, (RmySQL, RSQLite, RpostgreSQL), allows to run SQL queries against a database and return a dataframe.
#
For hierarchical data: use jsonlite (by Jeroen Ooms) for json, and xml2 for XML. Jenny Bryan has some excellent worked examples at https://jennybc.github.io/purrr-tutorial/.

For other file types, try the R data import/export manual and the rio package.
)