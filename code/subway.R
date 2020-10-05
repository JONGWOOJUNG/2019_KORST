getwd()

## Subway

zones_nearsub <- function(d) {
  return(zones_subway_1ndist[which(zones_subway_1ndist$DISTANCE <= d),'CODE'])
}
zones_outsub <- function(d) {
  return(zones_subway_1ndist[which(zones_subway_1ndist$DISTANCE > d),'CODE'])
}

plot_1ndist_subway <- function(d1, d2) {
  z <- zones_nearsub(d1)
  df1 <- subset_dep_s(weekday_df, z)
  dep <- use_hours(df1)
  df1 <- subset_arr_s(weekday_df, z)
  arr <- use_hours_arr(df1)
  t <- as.data.frame(as.factor(c(c("00", "01", "02", "03", "04", "05", "06", "07", "08", "09"), as.character(c(10:23)))))
  colnames(t) <- c("HOUR")
  df1 <- left_join(t, dep, by = "HOUR")
  colnames(df1)[2] <- "DEP_COUNT"
  is.na(df1$DEP_COUNT) <- 0
  df1 <- left_join(df1, arr, by = "HOUR")
  colnames(df1)[3] <- "ARR_COUNT"
  
  z <- zones_outsub(d2)
  df2 <- subset_dep_s(weekday_df, z)
  dep <- use_hours(df2)
  df2 <- subset_arr_s(weekday_df, z)
  arr <- use_hours_arr(df2)
  t <- as.data.frame(as.factor(c(c("00", "01", "02", "03", "04", "05", "06", "07", "08", "09"), as.character(c(10:23)))))
  colnames(t) <- c("HOUR")
  df2 <- left_join(t, dep, by = "HOUR")
  colnames(df2)[2] <- "DEP_COUNT"
  is.na(df2$DEP_COUNT) <- 0
  df2 <- left_join(df2, arr, by = "HOUR")
  colnames(df2)[3] <- "ARR_COUNT"
  
  ggplot(group = 1) +
    geom_line(data = df1, aes(HOUR, DEP_COUNT / 26, group = 1, colour = "departure", linetype = "inner")) +
    geom_line(data = df1, aes(HOUR, ARR_COUNT / 26, group = 1, colour = "arrival", linetype = "inner")) +
    geom_line(data = df2, aes(HOUR, DEP_COUNT / 26, group = 1, colour = "departure", linetype = "outter")) +
    geom_line(data = df2, aes(HOUR, ARR_COUNT /26, group = 1, col = "arrival", linetype = "outter")) +
    scale_colour_manual(name="Line Color", values=c(departure="red", arrival="blue")) +
    scale_linetype_manual(name="Line Type", values=c(inner="solid", outter="longdash")) +
    xlab("Hours") + ylab("Mean usage per hour (counts)")
}

print_lm_subway <- function(df) {
  s <- summary_byZone(df)
  zones_subway_1ndist$CODE <- as.character(zones_subway_1ndist$CODE)
  s <- left_join(s, zones_subway_1ndist, by = 'CODE')
  l <- lm(data = s, formula = SUM_COUNT~DISTANCE)
  plot(y =s$SUM_COUNT, x =s$DISTANCE, xlab = "distance(meter)", ylab = "Total amount of use(count)")
  abline(l, col = "red")
  return(l)
}
