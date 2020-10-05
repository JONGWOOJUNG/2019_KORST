## 상위 20개 경로구간 분석
print_top_course(main_df, 20) # 이동경로 상위 20개

newdf <- main_df %>%
  mutate(check = ifelse(ARR_ZONE_CODE == 207 & DEP_ZONE_CODE ==207
                       | ARR_ZONE_CODE == 502 & DEP_ZONE_CODE ==502
                       | ARR_ZONE_CODE == 152 & DEP_ZONE_CODE ==152
                       | ARR_ZONE_CODE == 2219 & DEP_ZONE_CODE ==2219
                       | ARR_ZONE_CODE == 186 & DEP_ZONE_CODE ==186
                       | ARR_ZONE_CODE == 113 & DEP_ZONE_CODE ==183
                       | ARR_ZONE_CODE == 2102 & DEP_ZONE_CODE ==2102
                       | ARR_ZONE_CODE == 3515 & DEP_ZONE_CODE ==3515
                       | ARR_ZONE_CODE == 565 & DEP_ZONE_CODE ==565
                       | ARR_ZONE_CODE == 511 & DEP_ZONE_CODE ==511
                       | ARR_ZONE_CODE == 2002 & DEP_ZONE_CODE ==2002
                       | ARR_ZONE_CODE == 592 & DEP_ZONE_CODE ==590
                       | ARR_ZONE_CODE == 907 & DEP_ZONE_CODE ==907
                       | ARR_ZONE_CODE == 272 & DEP_ZONE_CODE ==272
                       | ARR_ZONE_CODE == 2050 & DEP_ZONE_CODE ==2013
                       | ARR_ZONE_CODE == 502 & DEP_ZONE_CODE ==548
                       | ARR_ZONE_CODE == 583 & DEP_ZONE_CODE ==583
                       | ARR_ZONE_CODE == 2025 & DEP_ZONE_CODE ==2025
                       | ARR_ZONE_CODE == 590 & DEP_ZONE_CODE ==592, 1, 0))

df <- newdf %>%
  filter(newdf$check == 1)

head(df)
dim(df)
##############################################################################################
# weekday:0, weekend:1 / Round:0, Oneway:1
d01 <- df %>%
  mutate(day = ifelse(format(df$DEP_TIME, "%w") == 0 | format(df$DEP_TIME, "%w") == 1, 1, 0)) %>%
  mutate(trip = ifelse(df$DEP_ZONE_CODE == df$ARR_ZONE_CODE, 0, 1))

# dummy
d01$day <- as.factor(d01$day)
d01$trip <- as.factor(d01$trip)
summary(d01)

# calculate peak time / peak:1 nonpeak:0
d02 <- d01 %>%
  mutate(peak = ifelse(d01$day==0 & format(d01$DEP_TIME, "%H") == 7
                       | d01$day==0 & format(d01$DEP_TIME, "%H") == 8
                       | format(d01$DEP_TIME, "%H") == 17
                       | format(d01$DEP_TIME, "%H") == 18, 1, 0))

d02$peak <- as.factor(d02$peak)
summary(d02)

# merge subway data
colnames(zones_subway_1ndist) <- c("DEP_ZONE_CODE","SUB_CODE","DISTANCE")
d03 <- merge(d02,zones_subway_1ndist,BY=DEP_ZONE_CODE, all.x=T)
table(is.na(d03))

# LR DATA SET
# 종속변수 통행 유형(ROUND:0,ONEWAY:1)
# 독립변수 이용거리(연속형),주중여부(주중:0주말:1),첨두시여부(첨두:1,비첨두:0),지하철까지거리(연속형)
names(d03)
d04 <- d03[c(10, 13, 14, 15, 17)]
head(d04)
summary(d04)

# sampling data
library(mlbench)
library(ROSE)
library(tidyverse)
library(caret)
library(DMwR)

set.seed(seed = 190729)
index <- createDataPartition(y = d04$trip, p = 0.7, list = FALSE)
trainSet <- d04[index, ]
testSet <- d04[-index, ]
head(trainSet)

# check table
table(trainSet$trip)
table(testSet$trip)

# check classes distribution
prop.table(table(trainSet$trip))
prop.table(table(testSet$trip))

redd <- ovun.sample(trip ~., data = trainSet, method = "both", p=0.5, N=5000, seed = 1)$data
table(redd$trip)

# Logistic Regression
fitdd <- glm(trip ~., data = redd, family = binomial(link = 'logit'))
summary(fitdd)
exp(coef(fitdd))

## Predict the Values
predict <- predict(fitdd, testSet, type = 'response')

## Create Confusion Matrix
table(testSet$trip, predict > 0.5)

#ROCR Curve
library(ROCR)
ROCRpred <- prediction(predict, testSet$trip)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))




##########################################
yhat <- ifelse(predict>0.5,1,0)
class.tab = table(testSet$trip,yhat,dnn=c("Actual","predict"))
class.tab
performance(ROCRpred,'auc')@y.values #면적 계산

# 출처: https://kmrho1103.tistory.com/tag/ROCR [데이터마이너를 꿈꾸며]