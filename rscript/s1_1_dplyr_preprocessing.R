library(dplyr)
# the stats::filter function and not the dplyr one. To make sure you get the right one, use the notation dplyr::filter.
# library(DT) # data -> table form
# chain operator : ctrl + shift + m 

# load data ---------------------------------------------------------------

raw <- read.csv('./data/titanic.csv')
str(raw)
datatable(raw)
summary(raw)
raw %>% head()

# dplyr -------------------------------------------------------------------

## select
raw %>% select(age, survived) %>% head()

## filter
raw %>% dplyr::filter(pclass == "1st") %>% head()
raw %>% dplyr::filter(age > 10) %>% head()

## mutate
raw %>% mutate(age_per10 = round(age/10)) %>% head(20)
raw # datatable 쪽 참고 

## group_by : categorical variable
raw %>% str()
raw %>% summary()
raw %>% group_by(sex)

## summarise 
### 정해진 함수 말고 사용자 함수도 적용이 가능함
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
semi_join(x,y) # key가 공통으로 존재하는 것을 왼쪽 테이블에서 뽑아옴, 데이터 사이즈를 줄이는 역할함
anti_join(x,y)

# end of document ---------------------------------------------------------

