## Requires
library(dplyr)
library(sp)
library(ggplot2)
library(leaflet)
library(leaflet.extras)
# install.packages('rgdal')
library(rgdal)
library(htmlwidgets)
# install.packages('webshot')
library(webshot)


# Extract zones from main data
extract_zones <- function(df) {
  if (class(df$DEP_ZONE_CODE) != "character")
    df$DEP_ZONE_CODE <- as.character(df$DEP_ZONE_CODE)
  dzones <- df[match(unique(df$DEP_ZONE_CODE), df$DEP_ZONE_CODE),c('DEP_ZONE_CODE', 'DEP_ZONE_NAME')]
  azones <- df[match(unique(df$ARR_ZONE_CODE), df$ARR_ZONE_CODE),c('ARR_ZONE_CODE', 'ARR_ZONE_NAME')]
  colnames(dzones) <- c("CODE", "NAME")
  colnames(azones) <- c("CODE", "NAME")
  dzones <- rbind(dzones, azones)
  dzones <- dzones[match(unique(dzones$CODE), dzones$CODE),]
  return(dzones)
}


# Catch some wrong zone
catch_wrong_zone <- function(df) {
  z <- extract_zones(df)
  right <- c()
  for (row in 1:nrow(z)) {
    nz <- NA
    tryCatch({nz <- as.numeric(z[row, 'CODE'])}, error = function(e){}, warning = function(w){}) 
    if (!is.na(nz) & nz < 9000) { right <- c(right, z[row, 'CODE']) }
  }
  return(z[!z$CODE %in% right,])
}

# Refine some bad data by zone code
refine_df <- function(df) {
  wrong <- catch_wrong_zone(df)
  df <- df[!df$DEP_ZONE_CODE %in% wrong$CODE,]
  df <- df[!df$ARR_ZONE_CODE %in% wrong$CODE,]
  return(df)
}



# Subset by
subset_dep <- function(df, z) {
  return(subset(df, df$DEP_ZONE_CODE == z))
}
subset_arr <- function(df, z) {
  return(subset(df, df$ARR_ZONE_CODE == z))
}
subset_dep_arr <- function(df, d, a) {
  return(subset(df, df$DEP_ZONE_CODE == d & df$ARR_ZONE_CODE == a))
}
subset_dep_s <- function(df, zv) {
  r <- subset(df, df$DEP_ZONE_CODE == zv[1])
  for (i in 2:length(zv)) {
    r <- rbind(r, subset(df, df$DEP_ZONE_CODE == zv[i]))
  }
  return(r)
}
subset_arr_s <- function(df, zv) {
  r <- subset(df, df$ARR_ZONE_CODE == zv[1])
  for (i in 2:length(zv)) {
    r <- rbind(r, subset(df, df$ARR_ZONE_CODE == zv[i]))
  }
  return(r)
}

# Count by course(key: DEP_ZONE_CODE + ARR_ZONE_CODE)
# library(dplyr)
use_course <- function(df) {
  g <- df[c('DEP_ZONE_CODE', 'ARR_ZONE_CODE', 'USE_MINUTES', 'USE_METERS')] %>%
    group_by(DEP_ZONE_CODE, ARR_ZONE_CODE) %>%
    summarize(COUNT = n(), MEAN_USE_MINUTES = mean(USE_MINUTES), MEAN_USE_METERS = mean(USE_METERS))
  g <- data.frame(g)
  g <- merge(g, zones_df[c("CODE", "NAME")], by.x = "DEP_ZONE_CODE", by.y = "CODE", all.x = TRUE)
  colnames(g)[which(colnames(g) == 'NAME')] <- "DEP_ZONE_NAME"
  g <- merge(g, zones_df[c("CODE", "NAME")], by.x = "ARR_ZONE_CODE", by.y = "CODE", all.x = TRUE)
  colnames(g)[which(colnames(g) == 'NAME')] <- "ARR_ZONE_NAME"
  return(g)
}

# Count use by departure zone
# library(dplyr)
count_dep <- function(df) {
  z <- df[c('DEP_ZONE_CODE', 'USE_MINUTES', 'USE_METERS')] %>%
    group_by(DEP_ZONE_CODE) %>%
    summarize(DEP_COUNT = n(), MEAN_USE_MINUTES = mean(USE_MINUTES), MEAN_USE_METERS = mean(USE_METERS))
  return(data.frame(z))
}

# Count use by arrival zone
# library(dplyr)
count_arr <- function(df) {
  z <- df[c('ARR_ZONE_CODE', 'USE_MINUTES', 'USE_METERS')] %>%
    group_by(ARR_ZONE_CODE) %>%
    summarize(ARR_COUNT = n(), MEAN_USE_MINUTES = mean(USE_MINUTES), MEAN_USE_METERS = mean(USE_METERS))
  return(data.frame(z))
}

# Summary Amount of use by zone
# library(dplyr)
summary_byZone <- function(df) {
  z <- extract_zones(df)
  dep <- count_dep(df)
  colnames(dep) <- c("CODE", "DEP_COUNT", "MEAN_USE_MINUTES_DEP", "MEAN_USE_METERS_DEP")
  arr <- count_arr(df)
  colnames(arr) <- c("CODE", "ARR_COUNT", "MEAN_USE_MINUTES_ARR", "MEAN_USE_METERS_ARR")
  first_dep <- df[match(unique(df$DEP_ZONE_CODE), df$DEP_ZONE_CODE), c("DEP_ZONE_CODE", "DEP_TIME")]
  first_dep$DEP_TIME <- as.Date(first_dep$DEP_TIME)
  colnames(first_dep) <- c("CODE", "FIRST_DEP_DATE")
  first_dep['SINCE_DAYS'] <- as.numeric(as.Date(df[nrow(df), 'DEP_TIME']) - first_dep$FIRST_DEP_DATE)
  first_dep['SINCE_WEEKS'] <- round(first_dep$SINCE_DAYS / 7)
  z <- left_join(z, dep, by = 'CODE')
  z <- left_join(z, arr, by = 'CODE')
  z <- left_join(z, first_dep, by = 'CODE')
  z["SUM_COUNT"] <- z["DEP_COUNT"] + z["ARR_COUNT"]
  
  return(z)
}

# Amount of departure by hour("H")
use_hours_arr <- function(df) {
  uses_timecode <- table(factor(format(df$ARR_TIME, "%H")))
  uses_timecode <- data.frame(uses_timecode)
  colnames(uses_timecode) <- c("HOUR", "COUNT")
  rownames(uses_timecode) <- uses_timecode[,1]
  return(uses_timecode)
}

# Amount of arrival by hour("H")
use_hours <- function(df) {
  uses_timecode <- table(factor(format(df$DEP_TIME, "%H")))
  uses_timecode <- data.frame(uses_timecode)
  colnames(uses_timecode) <- c("HOUR", "COUNT")
  rownames(uses_timecode) <- uses_timecode[,1]
  return(uses_timecode)
}

# Amount of departure by weekdays("w")
use_weekdays <- function(df) {
  uses_timecode <- table(factor(format(df$DEP_TIME, "%w")))
  uses_timecode <- data.frame(uses_timecode)
  colnames(uses_timecode) <- c("WEEKDAYS", "COUNT")
  rownames(uses_timecode) <- uses_timecode[,1]
  return(uses_timecode)
}

# Amount of departure by timecode(%w%H)
use_timecode <- function(df) {
  uses_timecode <- table(factor(format(df$DEP_TIME, "%w%H")))
  uses_timecode <- data.frame(uses_timecode)
  colnames(uses_timecode) <- c("TIMECODE", "COUNT")
  rownames(uses_timecode) <- uses_timecode[,1]
  return(uses_timecode)
}

# Amount of departure by timecode(%w%H)
use_timecode2 <- function(df) {
  uses_timecode <- table(factor(format(df$DEP_TIME, "%w%H")))
  uses_timecode <- data.frame(uses_timecode)
  colnames(uses_timecode) <- c("TIMECODE", "COUNT")
  tc <- timecode_gen()
  tc <- as.data.frame(tc)
  colnames(tc) <- c("TIMECODE")
  uses_timecode <- left_join(tc, uses_timecode, by = "TIMECODE")
  uses_timecode[is.na(uses_timecode$COUNT),'COUNT'] <- 0
  return(uses_timecode)
}

# Amount of use(dep, arr) by timecode
zone_use_timecode <- function(df, z) {
  dep <- subset_dep(df, z)
  arr <- subset_arr(df, z)
  
  dep <- dep['DEP_TIME']
  arr <- arr['ARR_TIME']
  colnames(arr) <- c("DEP_TIME")
  dep <- use_timecode2(dep)
  colnames(dep)[which(colnames(dep) == 'COUNT')] <- "DEP_COUNT"
  arr <- use_timecode2(arr)
  colnames(arr)[which(colnames(arr) == 'COUNT')] <- "ARR_COUNT"
  a <- merge(dep, arr, by = "TIMECODE",  all = TRUE)
  a[is.na(a)] <- 0
  return(a)
}

# Timecode generator
timecode_gen <- function() {
  t <- c()
  for (w in 0:6) {
    for (h in 0:23) {
      if (h < 10) {
        t <- c(t, paste(w, "0", h, sep = ""))
      }
      else {
        t <- c(t, paste(w, h, sep = ""))
      }
    }
  }
  return(t)
}

# convert TIMECODE(%w%H) to sequential number(0:167)
timecode_s<- function(wh) {
  tc <- timecode_gen()
  return(which(tc == wh) - 1)
}


## Plots

# plot weekday$time - Amount of use Graph
#library(dplyr)
#library(ggplot2)
plot_timecode_use <- function(df) {
  df_use_timecode <- use_timecode(df)
  df_use_timecode$S_TIMECODE <- sapply(df_use_timecode$TIMECODE, timecode_s, USE.NAMES = FALSE) # make timecode sequential
  df_use_timecode_s <- data.frame(c(0:167))
  colnames(df_use_timecode_s) <- c("S_TIMECODE")
  df_use_timecode_s <- left_join(df_use_timecode_s, df_use_timecode, by = "S_TIMECODE")
  
  # df_use_weekdays <- use_weekdays(df)
  # df_use_weekdays['S_TIMECODE'] <- seq(12, 168, 24)
  
  rects <- data.frame(xstart = seq(0,144,24), xend = seq(24,168,24), col = c("steelblue1", "grey45", "grey55", "grey65", "grey75", "grey85", "indianred1"))
  ggplot() + 
    geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf), fill = rects$col, color = "grey", alpha = 0.4) +
    geom_line(data = df_use_timecode_s, aes(S_TIMECODE, COUNT)) +
    scale_x_continuous(name = "Times(hour)", breaks = seq(from = 0, to = 168, by = 12), labels = c("", "Sun", "", "Mon", "", "Tue", "", "Wed", "", "Tus", "", "Fri", "", "Sat", "")) +
    scale_y_continuous(name = "Amout of Use (Count)")
}

# plot inout graph of specific zone(Warning: Only for 6 month data)
plot_inout <- function (df, code) {
  a <- zone_use_timecode(df, code)
  
  a['INOUT'] <- (a$ARR_COUNT - a$DEP_COUNT) / 26
  a['TIMECODE_S'] <- c(0:167)
  
  rects <- data.frame(xstart = seq(0,144,24), xend = seq(24,168,24), col = c("steelblue1", "grey45", "grey55", "grey65", "grey75", "grey85", "indianred1"))
  ggplot() +
    geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf), fill = rects$col, color = "grey", alpha = 0.4) +
    geom_line(data = a, aes(TIMECODE_S, INOUT)) +
    scale_x_continuous(name = "Times(hour)", breaks = seq(from = 0, to = 168, by = 12), labels = c("", "Sun", "", "Mon", "", "Tue", "", "Wed", "", "Tus", "", "Fri", "", "Sat", "")) +
    scale_y_continuous(name = "In(+) / Out(-): (counted by hour)") +
    geom_hline(yintercept=0, col = "red")
}


## Print Some Resutls

# print summary about passage
print_passage_summary <- function(df) {
  mean_meter <- mean(df$USE_METERS)
  mean_minute <- mean(df$USE_MINUTES)
  mean_speed <- mean_meter / 1000 * 60 / mean_minute
  n_zones <- nrow(extract_zones(df))
  print(paste("Total Use:", nrow(df), "/ Zones:", n_zones))
  print(paste("First Use:", df[1, 'DEP_TIME'], " / Last Use:", df[nrow(df), 'DEP_TIME']))
  print(paste("Mean Distance:", mean_meter, "meter / Mean Use Time:", mean_minute, "minutes / Mean Speed:", mean_speed))
}

# print summary about usage by zones
library(readr)
print_byZone_summary <- function(df) {
  s <- summary_byZone(df)
  s <- s[which(CODE > 8999),]
  z <- s[which.max(s$SUM_COUNT / s$SINCE_DAYS),]
  print(paste("Max use zone: ", z$NAME, "(Departure by day: ", round(z$DEP_COUNT / z$SINCE_DAYS, digit = 1), ", Arrival by day: ", round(z$ARR_COUNT / z$SINCE_DAYS, digit = 1), ", DEP+ARR by day: ", round(z$SUM_COUNT / z$SINCE_DAYS, digit = 1), ")", sep = ""))
  z <- s[which.min(s$SUM_COUNT / s$SINCE_DAYS),]
  print(paste("Min use zone: ", z$NAME, "(Departure by day: ", round(z$DEP_COUNT / z$SINCE_DAYS, digit = 1), ", Arrival by day: ", round(z$ARR_COUNT / z$SINCE_DAYS, digit = 1), ", DEP+ARR by day: ", round(z$SUM_COUNT / z$SINCE_DAYS, digit = 1), ")", sep = ""))
}

# print sum count top n zone
print_sum_top_zones <- function(df, n){
  s <- summary_byZone(df)
  return(s[order(-s$SUM_COUNT),][1:n,])
}

# print top n course
print_top_course <- function(df, n){
  s <- use_course(df)
  print(s[order(-s$COUNT),][c(1:n),])
}
