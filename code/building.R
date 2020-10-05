## building

print_lm_bd <- function(df) {
  s <- summary_byZone(df)
  zones_sq_bd$CODE <- as.character(zones_sq_bd$CODE)
  s <- left_join(s, zones_sq_bd, by = 'CODE')
  l <- lm(data = s, formula = SUM_COUNT~MEAN_FLOOR)
  plot(y =s$SUM_COUNT, x =s$MEAN_FLOOR, xlab = "mean floor of bilding near zone 350m", ylab = "Total amount of use(count)")
  abline(l, col = "red")
  return(l)
}
