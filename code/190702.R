
### 190707

getwd()
setwd("C:/Users/jongu/OneDrive/바탕 화면/JJW/06. 논문및학술발표자료/01. 학술발표/00. Smart_Bike_Sharing/01. Workplace")

### library 정리

library(readr)
library(dplyr)
library(VIM) # Missing values assesment used by VIM::aggr()
library(ggplot2) # Used in almost visualization
library(RColorBrewer) # plot의 color 설정
library(scales) # plot setting - x축, y축 설정

### CSV

read.csv.any <- function(text, sep = "", ...) {
  encoding <- as.character(guess_encoding(text)[1,1])
  setting <- as.character(tools::file_ext(text))
  if(sep != "" | !(setting  %in% c("csv", "txt")) ) setting <- "custom"
  separate <- list(csv = ",", txt = "\n", custom = sep)
  result <- read.table(text, sep = separate[[setting]], fileEncoding = encoding, ...)
  return(result)
}

### DATA

bike1809 <- read.csv.any("서울특별시 공공자전거 시간대별 대여정보_201809.csv", header = TRUE)
bike1810 <- read.csv.any("서울특별시 공공자전거 시간대별 대여정보_201810.csv", header = TRUE)
bikestation <- read.csv.any("서울특별시 공공자전거 대여소 정보_20181129.csv", header = TRUE)

# bike1811 <- read.csv.any("서울특별시 공공자전거 시간대별 대여정보_201811.csv", header = TRUE)
# biketotal <- dplyr::bind_rows(bike1809, bike1810, bike1811)

### DATA 확인

head(bike1809, 10)
head(bike1810, 10)

summary(bike1809)
summary(bike1810)

### Missing value

sum(is.na(bike1809))
sum(is.na(bike1810))
sum(is.na(bikestation))
colSums(is.na(bike1809))
