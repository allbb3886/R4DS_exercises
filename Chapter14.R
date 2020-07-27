############# Section 14 Strings #############
## string maniupulation
# regular expressions are useful because strings usually contain unstructured or semi-structured data.

library(tidyverse)
library(htmlwidgets)

## create strings with single or double quotes.
# no difference, typically  use ", unless you create a string that contains multiple "

string1 <-  "this is a string"
string2 <- 'If i want to include a "quote" inside a string, use single quotes'

## if we forget to include quotes we get errors

## to include a literal single or double quote in a string, use \ to "escape" it

double_qot <- "\"" # or '"'
single_quote <- '\'' # or "'"

# to include a literal backslash, need to double up, "\\"

# beware that printed represation of a string is not the same as string itself, 
printed represation shows the escapes. TO see raw contents of the string, use writeLines()

x <- c("\"", "\\")
x
writeLines(x)

## The most common are "\n", newline, and "\t", tab
# but one cans ee the complete list by requesting help on ":
# or ?"'"
## multiple strings are often stored in a character vector, create with c():

c("one","two","three")

#section 14.2.1 String Length ##########

## use functions stringr, these have more intutitive names, all start with str_.
# example str_length() tells you number of characters in a string.

str_length(c("a", "R for data science", NA))


############ Section 14.2.2 Combining Strings

# use str_c() to combine two or more strings

str_c("x","y")
str_c("x","y","z")
## use the sep arugment to control how to separte
str_c("x","y", sep=",")

# to print NA use str_replace_na()

x <- c("abc",NA)
str_c("|-",x,"-|")
str_c("-",str_replace_na(x),"-|")

# note that str_c() is vectorsied and automatically recylces shorter vectors to the same length as the longest.

str_c("prefix-",c("a","b","c"),"-suffix")

#3 objects of 0 length are silently dropped, useful in conjuction with if:
name <- "Hadley"
time_of_day <- "morning"
birthday <- FALSE
str_c("Good", time_of_day, "" , name,
      if (birthday) " and Happy Birhday", "."
)

## collapsing a vector of strings into a single string, use collapse.
str_c(c("x", "y", "z"), collapse = ", ")



### SEction 14.2.3 Subseting strings 
#extract parts of a string using str_sub()
#str_sub() takes start and end arugments which give the inclusive position of the substring:

x <- c("Apple","banana", "pear")
str_sub(x,1,3)
## start subsetting string at beginning then end at 3rd character
## negative numbers count backfroms from end
str_sub(x,-3,-1)

#str_sub() wont fail if string is too short, it will return as much as possible:
str_sub("a",1,5)

## can sue assignment form of str_sub() to modify strings:
str_sub(x,1,1) <- str_to_lower(str_sub(x,1,1))


########### Section 14.2.4 Locales ######
##str_to_lower() changes the text to lower case, str_to_upper() or str_to_title() can also change cases.
## different languages have differen rules.

# Turkish has two i's: with and without a dot, and it
# has a different rule for capitalising them:
str_to_upper(c("i", "i"))
#> [1] "I" "I"
str_to_upper(c("i", "i"), locale = "tr")
#> [1] "I" "I"

x <- c("apple", "eggplant", "banana")

str_sort(x, locale = "en")  # English
#> [1] "apple"    "banana"   "eggplant"

str_sort(x, locale = "haw") # Hawaiian
#> [1] "apple"    "eggplant" "banana"


########## Exercises 14.2.1
The function paste() separates strings by spaces by default, while paste0() does not separate strings with spaces by default.

paste("foo", "bar")
#> [1] "foo bar"
paste0("foo", "bar")
#> [1] "foobar"

# since str_c() does not separate strings with spaces by default, it is closer in behavior to paste0().
str_c("foo","bar")
str_c("foo", NA)
#> [1] NA
paste("foo", NA)
#> [1] "foo NA"
paste0("foo", NA)
#> [1] "fooNA"

#### Exercise 14.2.2

## difference between sep and collapse arguments to str_c()
# sep argument is the string inserted between arguments to str_c(), while collapse is the string used to separeate any elements of the character vector
# into a character vector of length one.

## Exercise 14.2.3#####
## select floor or ceiling for middle character of an even string.
x <- c("a", "abc", "abcd", "abcde", "abcdef")
L <- str_length(x)
L
m <- ceiling(L / 2)
m
str_sub(x, m, m)

### Exercise 14.2.4
The function str_wrap() wraps text so that it fits within a certain width. This is useful for wrapping long strings of text to be typeset.

##Exercise 14.2.5
The function str_trim() trims the whitespace from a string.
str_trim(" abc ")
#> [1] "abc"
str_trim(" abc ", side = "left")
#> [1] "abc "
str_trim(" abc ", side = "right")
#> [1] " abc"

str_pad("abc", 5, side = "both")
#> [1] " abc "
str_pad("abc", 4, side = "right")
#> [1] "abc "
str_pad("abc", 4, side = "left")
#> [1] " abc"


## Exercise 14.2.6
## requires functions, not there yet

############## Section 14.3 Matching Patterns with regular expressions 

#Regexps are very terse language to describe patterns in strings
# to learn regular expressions, we'll use str_view() and str_view_all().
# these functions take a character vector and a regular expression, and show how they match.

x <- c("apple","banana","pear")
str_view(x,"an")
## using str_view to find the string that matches an in x
## next step up is ., which matches any character (except a newline):
str_view(x,".a.") 
# this matches any character around a.
str_view(x,".an")

## if . matches any character, how do we actually match "."? Need to use
# an escape to tell the regular expression we want an exact match.
# backslash, \, helps us. so to match a ., we need regexp\.

#create the regular expression, we need \\
dot <- "\\."
#but expression only contains one:
writeLines(dot)

## telling r to look for an explicit .
str_view(c("abc","a.c","bef"),"a\\.c")

# simiarly, how do we match a liteal \?
## create regular expression \\, need to use a string to escape \. need to write "\\\\"

x <- "a\\b"
writeLines(x)

str_view(x,"\\\\")


# Exercises 14.3.1.1
\": This will escape the next character in the R string.
"\\": This will resolve to \ in the regular expression, which will escape the next character in the regular expression.
"\\\": The first two backslashes will resolve to a literal backslash in the regular expression, the third will escape the next character. So in the regular expression, this will escape some escaped character.

## How to match "'\?
str_view("\"'\\", "\"'\\\\", match = TRUE)


## Exercise 14.3.1.3
str_view(c(".a.b.c", ".a.b", "....."), c("\\..\\..\\.."), match = TRUE)


## section 14.3.2 Anchors

#regular expression will match any part of a string. Often useful to anchor the regular expresssion so that it matches from start or end.
## ^ to match start of string
# $ to match end of string.

x <- c("apple", "banana", "pear")
str_view(x,"^a")

str_view(x"a$")

## to force a regular expression to only match a complete string, anchor it with both % and $
x <- c("apple pie", "apple", "apple cake")
str_view(x, "apple")

str_vew(x"^apple$")

## can match boundary between words with \b, not that helpful though.


### Exercises 14.3.2.1
#How to match "$^$"?
str_view(c("$^$", "ab$^$sfas"),"^\\$^\\$$", match = TRUE)

str_view(strinr::wrods,"^y", match=TRUE)
## words that end with x
str_view(stringr::words,"x$", match =TRUE)

## exactly three letters long
str_view(stringr::words,"^...$", match =TRUE)

## seven letters or more
str_view(stringr::words,".......", match = TRUE)



### SEction 14.3.3 Character classes and alternatives
# special patterns that match more than one character.
# . matches any character.
# \d : any digit
# \s: matches any whitespace (e.g. space,tab,newline)
[abc] : matches a,b, or c
#[^abc}: matches anything but a,b or c

## To create  regular expression containg \d or \s, need escape \ for the string, so we will type "\\d" or "\\s"

A character class containing a single character is a nice alternative to backslash escapes when you want to include a single metacharacter in a regex. Many people find this more readable.

## look for a literal character that normally has speical meaning in regex

str_view(c("abc", "a.c", "a*c", "a c"), "a[.]c")
## looks for a.c


str_view(c("abc", "a.c", "a*c", "a c"), ".[*]c")
## looks for .*c, where . is anything

str_view(c("abc", "a.c", "a*c", "a c"), "a[ ]")
looks for a, white space

###
##This works for most (but not all) regex metacharacters: $ . | ? * + ( ) [ {. Unfortunately, a few characters have special meaning even inside a character class and must be handled with backslash escapes: ] \ ^ and -.

#You can use alternation to pick between one or more 
#alternative patterns. For example, abc|d..f will match either '"abc"', or "deaf". Note that the precedence for | is low, so that abc|xyz matches abc or xyz not abcyz or abxyz. Like with mathematical expressions, if precedence ever gets confusing, use parentheses to make it clear what you want:

## so for example abc|d..f will match either "abc" or "deaf", | has low precedence, so abc|xyz matches abc or xyz not abcyz or abxyz.

str_view(c("grey","gray"), "gr(e|a)y")


### Exercise 14.3.3.1
# or str_subset
str_subset(stringr::words,"^[aeiou]")
str_subset(stringr::words, "^[aeiou]")

## only consonants
str_subset(stringr::words, "^[^aeiou]+$")

str_subset(stringr::words,"[^e]ed$")
## words ending with ed but not eed


#section 14.3.4 Reptition ###

?: 0 or 1
+ : 1 or more
*: 0 or more.
x <- "1888 is the longest year in roman numerals: MDCCLXXXVIII"
str_view(x,"CC?")

str_view(x, "CC+")


# can match number of matches precisely:
{n}: exactly n
{n,}: n or more
{,m}: at most m
{n,m}: between n and m

str_view(x,"c{2}")

str_subset(x,"C{2,}")



############### Section 14.3.5 Grouping abd Backreferences

#parenthessis create a numbered capturing group (number 1, 2 etc). A capturing group stores the 
#part of the string matched by the part of the regular expression inside the aprentheses
# can refer to the same text as previously amtched by a capturing group with backreferences like \1, \2.
# expression finds all fruits that have a repeated pair of letters

str_view(fruit, "(..)\\1", match = TRUE) ##
#finds all fruit with repeated pair of letters


#### Section 14.4 Tools###############
## how to aplpy them in real problems :

Determine which strings match a pattern.
Find the positions of matches.
Extract the content of matches.
Replace matches with new values.
Split a string based on a match.

##
(.)\1\1: The same character appearing three times in a row. E.g. "aaa"
"(.)(.)\\2\\1": A pair of characters followed by the same pair of characters in reversed order. E.g. "abba".
(..)\1: Any two characters repeated. E.g. "a1a1".
"(.).\\1.\\1": A character followed by any character, the original character, any other character, the original character again. E.g. "abaca", "b8b.b".
"(.)(.)(.).*\\3\\2\\1" Three characters followed by zero or more characters of any kind followed by the same three characters but in reverse order. E.g. "abcsgasgddsadgsdgcba" or "abccba" or "abc1cba".


### Section 14.4.1 Detect matches ##

## Determine if a character vector matches a pattern, use str_detect(). It returns
# a logical vector the same length as the input.
x <- c("apple","banana","pear")
str_detect(x,"e")

## when you use a logical vector in a numeric context, FALSE becomes 0, TRUE becomes 1
# This makes sum() and mean() useful if we want to answer questions about matches

## how many common words start with t?
sum(str_detect(words,"^t"))

## proportion of common words that end with a vowel?
mean(str_detect(words,"[aeiou}$"))


## complex logical conditions (match a or b but no c unless d)
# often easier to combin multiple str_detect() calls with logical operators, rather than trying to create a single regular expression

## two ways to find all words with no vowels.
no_vowels_1 <- !str_detect(words,"[aeiou]")

# or 
no_vowels_2 <- str_detect(words,"^[aeiou]_$")

identical(no_vowels_1, no_vowels_2)

## first approach is easier to understand

words[str_detect(words,"x$")]
str_subset(words,"x$")

## however, strings will be one column of a data frame, so use a filter isntead:

df <- tibble(word=words,
             i=seq_along(word)
)
df%>% filter(str_detect(word,"x$"))

# a variation on str_detect() is str_count(): rather than a simple yes or no, it tells how many matches there are in a string.
x <- c("apple","banana","pear")
str_count(x,"a")

# On average, how many vowels per word?
mean(str_count(words, "[aeiou]"))

## natural to use str_count() into a mutate():

df %>% 
  mutate(
    vowels = str_count(word, "[aeiou]"),
    consonants = str_count(word, "[^aeiou]")
  )



## in abababa how many times will patern "aba" match?

str_count("abababa", "aba"),
#mathces never overlap


## Exercsies 14.4.1.1

#find all words end with or start with x
str_subset(words, "^x|x$")
#
start_with_x <- str_detect(words, "^x")
end_with_x <- str_detect(words, "x$")
words[start_with_x | end_with_x]

### words starting with voewel and ending with consonant
str_subset(words, "^[aeiou].*[^aeiou}$") %>% head()

start_with_vowel <- str_detect(words, "^[aeiou]")
end_with_consonant <- str_detect(words, "[^aeiou]$") 
words[start_with_vowel & end_with_consonant] %>% head()



### Exercuse 14,4,1,2

## what word has msot voewels?
vowels <- str_count(words, "[aeiou]")
words[which(vowels == max(vowels))]

prop_vowels <- str_count(words, "[aeiou]") / str_length(words)
words[which(prop_vowels == max(prop_vowels))]



#### Extract Matches section 14.4.2

# to extract actual text of a match, use str_extract()
# 
length(stringr::sentences)
head(sentences)
## find all sentences that contain a colour. Create a vector of colour names, 
# then turn into a single regular expression:

colours <- c("red","orange","yellow","green","blue","purple")
colour_match <- str_c(colours,collapse="|")
colour_match

#now we can select setences that contain a colur, and then extarct the colour to figoure out which one it is.

has_colour <- str_subset(sentences,colour_match)
matches <- str_extract(has_colour,colour_match)
head(matches)
##str_extract() only extracts the first match.

more <- sentences[str_count(sentences, colour_match) > 1]
str_view_all(more, colour_match)

