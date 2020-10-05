getwd()

# main_df DEP&ARR_TIME STR Change
main_df$DEP_TIME <- as.POSIXct(main_df$DEP_TIME)
main_df$ARR_TIME <- as.POSIXct((main_df$ARR_TIME))


# weekday:0, weekend:1 / Round:0, Oneway:1
LR_DAT <- main_df %>%
  mutate(day = ifelse(format(main_df$DEP_TIME, "%w") == 0 | format(main_df$DEP_TIME, "%w") == 1, 1, 0)) %>%
  mutate(trip = ifelse(main_df$DEP_ZONE_CODE == main_df$ARR_ZONE_CODE, 0, 1))

# dummy
LR_DAT$day <- as.factor(LR_DAT$day)
LR_DAT$trip <- as.factor(LR_DAT$trip)
summary(LR_DAT)

# calculate peak time / peak:1 nonpeak:0
LR_DAT01 <- LR_DAT %>%
  mutate(peak = ifelse(format(main_df$DEP_TIME, "%H") == 8
                       | format(main_df$DEP_TIME, "%H") == 18, 1, 0))
summary(LR_DAT01)

# weekend data
LR_DAT01$peak <- ifelse(LR_DAT01$day==1 & (format(main_df$DEP_TIME, "%H") == 17 | format(main_df$DEP_TIME, "%H") == 18), 1, 0) 
LR_DAT01$peak <- as.factor(LR_DAT01$peak)

# merge subway data
colnames(zones_subway_1ndist) <- c("DEP_ZONE_CODE","SUB_CODE","DISTANCE")
LR_DAT03 <- merge(LR_DAT01,zones_subway_1ndist,BY=DEP_ZONE_CODE, all.x=T)
table(is.na(LR_DAT03))

LR_DAT04 <- na.omit(LR_DAT03)

# LR DATA SET
# 종속변수 통행 유형(ROUND:0,ONEWAY:1)
# 독립변수 이용거리(연속형),주중여부(주중:0주말:1),첨두시여부(첨두:1,비첨두:0),지하철까지거리(연속형)

LR_DATA_F <- LR_DAT04[c(10, 11, 12, 13, 14, 16)]
colnames(LR_DATA_F) <- c("USE_MINUTES", "USE_METERS", "TYPE_OF_WEEK", "TYPE_OF_TRIP", "PEAK", "NEARBY_SUBWAY")
head(LR_DATA_F)
str(LR_DATA_F)

# TYPE_OF_TRIP RATE
LR_DATA_F$TYPE_OF_TRIP %>% table() %>% prop.table()
LR_DATA_F$TYPE_OF_TRIP %>% table()

# Imbalanced Data SOLVE
# install.packages('ROSE')
# install.packages("DMwR")
# install.packages("mlbench")
library(mlbench)
library(ROSE)
library(tidyverse)
library(caret)
library(DMwR)

set.seed(seed = 190728)
index <- createDataPartition(y = LR_DATA_F$TYPE_OF_TRIP, p = 0.7, list = FALSE)
trainSet <- LR_DATA_F[index, ]
testSet <- LR_DATA_F[-index, ]
head(trainSet)

# check table
table(trainSet$TYPE_OF_TRIP)
table(testSet$TYPE_OF_TRIP)

# check classes distribution
prop.table(table(trainSet$TYPE_OF_TRIP))
prop.table(table(testSet$TYPE_OF_TRIP))

##########################################Solve Imbalanced sampling
# RE01 <- ovun.sample(TYPE_OF_TRIP ~., data = trainSet, method = "both", p=0.5, N=1500, seed = 1)$data
# table(RE01$TYPE_OF_TRIP)
# RE02 <- ROSE(TYPE_OF_TRIP ~., data = trainSet, seed = 1)$data
# table(RE02$TYPE_OF_TRIP)
# 
set.seed(12)
down_train <- downSample(x = trainSet[,-ncol(trainSet)], y= trainSet$TYPE_OF_TRIP)
table(down_train$TYPE_OF_TRIP)
colnames(down_train) <- c("USE_MINUTES","USE_METERS","TYPE_OF_WEEK","PEAK","NEARBY_SUBWAY","TYPE_OF_TRIP")
head(down_train)
table(is.na(down_train))

## solving imbalanced data <- so use this data_f
# x <- upSample(subset(LR_DATA_F, select=-TYPE_OF_TRIP),LR_DATA_F$TYPE_OF_TRIP)
# table(LR_DATA_F$TYPE_OF_TRIP)
# data_f <- data.frame(x)
# str(data_f)
# colnames(data_f) <- c("USE_METERS","TYPE_OF_WEEK","PEAK","NEARBY_SUBWAY","TYPE_OF_TRIP")
# 
y <- downSample(subset(dd, select=-TYPE_OF_TRIP),dd$TYPE_OF_TRIP)
table(dd$TYPE_OF_TRIP)
data_f <- data.frame(y)
str(data_f)
colnames(data_f) <- c("USE_METERS","TYPE_OF_WEEK","PEAK","NEARBY_SUBWAY","TYPE_OF_TRIP")


############################

###Logistic Regression
fit01 <- glm(TYPE_OF_TRIP ~., data = down_train, family = binomial(link = 'logit'))
summary(fit01)

fit01_2 <- glm(TYPE_OF_TRIP ~., data = RE02, family = binomial(link = 'logit'))
summary(fit01_2)

fit02 <- glm(TYPE_OF_TRIP ~., data = trainSet, family = binomial(link = 'logit'))
summary(fit02)

fit03 <- glm(TYPE_OF_TRIP ~., data = trainSet, family = binomial(link = 'logit'))
summary(fit02)
dfre02 <- data.frame(RE02)
summary(dfre02)
summary(LR_DATA_F)

fit_f <- glm(TYPE_OF_TRIP ~., data = data_f, family = binomial(link = 'logit'))
summary(fit_f)

head(LR_DATA_F)
summary(LR_DATA_F)
fit05 <- glm(TYPE_OF_TRIP ~., data = LR_DATA_F, family = binomial(link = 'logit'))
summary(fit05)

# NEW DATA
dd <- LR_DATA_F[c(1, 2, 3, 4, 6)]
head(dd)
summary(dd)


########################################################graph
## Predict the Values
predict <- predict(fit_f, testSet, type = 'response')

## Create Confusion Matrix
table(testSet$TYPE_OF_TRIP, predict > 0.5)

#ROCR Curve
library(ROCR)
ROCRpred <- prediction(predict, testSet$TYPE_OF_TRIP)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))
