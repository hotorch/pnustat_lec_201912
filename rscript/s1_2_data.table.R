# Comparison Velocity  ------------------------------------------------------------------

LETTERS
DF <- data.frame(x = runif(26000000), 
                 y = rep(LETTERS, each = 1000000))
DF %>% dim()
DF %>% head()  
system.time(DF[DF$y == "A",])

DT <- as.data.table(DF)
setkey(DT, y)
system.time(DT[J("A")])

rm(list = ls());gc()

# Load Data ---------------------------------------------------------------

raw <- read.csv("./data/titanic.csv")
# raw <- fread("./data/titanic.csv")
class(raw)

# data.table --------------------------------------------------------------
library(data.table)
library(dplyr)

titanic.dt <- data.table(raw)
class(titanic.dt) 

titanic.dt %>% str()
titanic.dt %>% head()

# colSums(is.na(titanic.dt))
# names(titanic.dt)[colSums(is.na(titanic.dt))>0]

# select var -----------------------------------------------------------------

titanic.dt[,1]
titanic.dt[,1] %>% class() 
titanic.dt[,pclass]
titanic.dt[,pclass] %>% class()

## list!
titanic.dt[,list(pclass)]
titanic.dt[,.(pclass)]

titanic.dt[,c(1:3)]
titanic.dt[,list(pclass, survived, name)]
titanic.dt[,.(pclass, survived, name)]


# select row ----------------------------------------------------------

## filter
titanic.dt[pclass == "1st",]
titanic.dt[pclass == "1st"]

## use key -> fast! 
### uni-key
#### setkey(data, var)
setkey(titanic.dt, pclass)
tables()

## 여러개의 key를 지정하는 경우 : 'sex' & 'pclass'
setkeyv(titanic.dt, c('sex', 'pclass'))
tables()

## key -> J expression 
setkey(titanic.dt, pclass)
titanic.dt["1st"]
titanic.dt[J("1st")]

setkeyv(titanic.dt, c('sex', 'pclass'))
titanic.dt[J("female","2nd")]
titanic.dt[J("female")]

## summarise using key 
setkey(titanic.dt, pclass)
titanic.dt[pclass == "1st", mean(survived)]
titanic.dt[J("1st"), mean(survived)]

## initialize key
setkey(titanic.dt, NULL)
tables()

# group - calculate -------------------------------------------------------------
setkey(titanic.dt, pclass)

## 
titanic.dt[, mean(survived), by = "pclass"]
# titanic.dt[, sur_ratio_class := mean(survived), by = "pclass"]
# titanic.dt[, sur_ratio_class := NULL]

## 
titanic.dt[, lapply(.SD, mean), by = sex, .SDcols = "survived"]

## 
titanic.dt[, lapply(.SD, mean), by = sex, .SDcols = c("survived", "age")]
titanic.dt[, lapply(.SD, mean, na.rm = T), by = sex, .SDcols = c("survived", "age")]

## 
titanic.dt[J("1st"), mean(survived), by = "sex"]
titanic.dt[J("3rd"), mean(survived), by = "sex"]

## 
titanic.dt[, mean(survived), by = c("pclass", "sex")]

## 
titanic.dt[, sum(pclass == "1st")]

## 
titanic.dt[pclass == "1st", .N] # 거의 이런 형태를 자주 씁니다

## Counting
## 
titanic.dt[pclass == "1st", .N, by = "sex"]

##
titanic.dt[, .N, by = "sex"]
titanic.dt[, .N, by = "pclass"]


## 
titanic.dt[J("1st"), length(which(age > 20))/.N, by = "sex"]

## 
titanic.dt[J("1st"), length(which(age > 20))/.N, by = "sex"]
titanic.dt[J("1st"), sum(age>20, na.rm = T)/.N, by = "sex"]
titanic.dt[J("1st"), sum(age>20, na.rm = T)/nrow(.SD), by = "sex"] 



# data merge --------------------------------------------------------------

DT <- data.table(x = runif(26000000), 
                 y = rep(LETTERS, each = 1000000))
dt <- data.table(y = c("A", "B", "C"),
                 z = c("a", "b", "c"))

DT %>% head()
DT %>% dim()
dt %>% head()

merge(DT, dt) # y라는 변수가 공통으로 있어서 알아서 key로 생각하고 병합
merge(DT, dt, all = T) # all = T 옵션에 의해, 적어도 한 쪽에 있는 결과를 모두 출력


## 참고
# X[Y, nomatch = NA] -- all rows in Y -- right outer join (default)
# X[Y, nomatch = 0] -- only rows with matches in both X and Y -- inner join
# merge(X, Y, all = TRUE) -- all rows from both X and Y -- full outer join
# merge(X, Y, all.x = TRUE) -- all rows in X -- left outer join
setkey(DT, y)
setkey(dt, y)
DT[dt] # dt기준으로 left 조인 
dt[DT]



# Data 수정 및 삭제  -----------------------------------------------------------

# DT[i, 새로운 변수명 := “값”]
# DT[i, “:=” (새로운 변수명 = “값”)]

# mutate
titanic.dt[ , isminor := "adult"]        #같은 표현식
titanic.dt[ , `:=`(isminor = "adult")]   #같은 표현식



# update
titanic.dt[age < 15, ":="(isminor = "child")]

titanic.dt[ , .N, by = "isminor"] 


# 
titanic.dt[ , ":="(fare.won = fare*1000)]


# update multi-variable  ----------------------------------
titanic.dt[ , ":="(isminor = "child",
                   fare.won = fare*1000)]
titanic.dt

rm(list = ls()) ; gc()

# end of document ---------------------------------------------------------
