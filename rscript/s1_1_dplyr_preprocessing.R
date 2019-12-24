library(dplyr)
library(data.table)

# the stats::filter function and not the dplyr one. To make sure you get the right one, use the notation dplyr::filter.
# chain operator : ctrl + shift + m 

# load data ---------------------------------------------------------------

raw <- read.csv('./data/titanic.csv')
str(raw)
data.table(raw)

# dplyr -------------------------------------------------------------------

## select
raw %>% select(age, survived) %>% head()

## filter
raw %>% dplyr::filter(pclass == "1st") %>% head()
raw %>% dplyr::filter(age > 10) %>% head()

## mutate
raw %>% mutate(age_per10 = round(age/10)) %>% head(20)

## group_by : categorical variable
raw %>% str()
raw %>% summary()
raw %>% group_by(sex)

## summarise 
##
raw %>% group_by(sex) %>% summarise(n = n())
raw %>% group_by(sex) %>% summarise(sur_sum = sum(survived))
raw %>% group_by(sex) %>% summarise(sur_sum = sum(survived), n = n(), sur_ratio = sur_sum/n)
raw %>% group_by(sex) %>% summarise(sur_ratio = mean(survived))
raw %>% group_by(sex) %>% summarise(sur_sd = sd(survived))

raw %>% group_by(pclass) %>% summarise(sur_ratio = mean(survived))
raw %>% group_by(sex, pclass) %>% summarise(sur_ratio = mean(survived))
raw %>% group_by(pclass, sex) %>% summarise(sur_ratio = mean(survived))

## arrange
raw %>% select(age) %>% head(20)
raw %>% select(age) %>% arrange(age) %>% head(20)
raw %>% select(age) %>% arrange(desc(age)) %>% head(20)
raw %>% select(age) %>% arrange(-age) %>% head(20)

## join
x <- data.frame(name = c("John", "Paul", "George", "Ringo", "Stuart", "Pete"), 
                instrument = c("guitar", "bass", "guitar", "drums", "bass", "drums"))

y <- data.frame(name = c("John", "Paul", "George", "Ringo", "Brian"), band = c("TRUE", 
                                                                               "TRUE", "TRUE", "TRUE", "FALSE"))

x;y


inner_join(x, y)
left_join(x,y) 
anti_join(x,y)

# end of document ---------------------------------------------------------

