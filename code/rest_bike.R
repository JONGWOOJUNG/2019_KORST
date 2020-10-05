## Rest Bike data

# no-bike times (* 10 minutes)
nobike_times <- function(df) {
  nobike <- subset(df, df$BIKE == 0)
  nobike <- data.frame(table(factor(nobike$CODE)))
  colnames(nobike) <- c("CODE", "NOBIKE_TIMES")
  return(nobike)
}

# print zone most n freuently lack of bike
print_max_nobike_times <- function(df, n) {
  nobike <- nobike_times(df)
  nobike <- nobike[nobike$CODE %in% zones_df$CODE, ]
  nobike$CODE <- as.character(nobike$CODE)
  nobike <- left_join(nobike, zones_df[c('CODE', 'NAME')], by = 'CODE')
  print(nobike[order(-nobike$NOBIKE_TIMES),][c(1:n),])
}

# plot map with rest-bike of specific hour(format: "%w%H")
rest_bike_map <- function (d, h) {
  tr <- subset(rest_bikes_q, as.numeric(format(rest_bikes_q[,1], "%w")) == d & as.numeric(format(rest_bikes_q[,1], "%H")) == h)
  tr['COL'] <- "blue"
  tr[tr$BIKE == 0,]$COL <- "red"
  m <- leaflet(tr) %>% addProviderTiles(providers$Stamen.Toner) %>%
    addCircleMarkers(
      lat = ~X,
      lng = ~Y,
      radius = ~sqrt(tr$BIKE) * 2 + 2,
      stroke = FALSE, color = ~COL, fillOpacity = 0.5
    )
  setView(m, 126.922646, 37.525792, 14)
  return(m)
}

# plot map with rest-bike of specific minutes(format: "%w%H%M")
rest_bike_mapDHM <- function (d, h, m) {
  tr <- subset(rest_bikes_q, as.numeric(format(rest_bikes_q[,1], "%w")) == d & as.numeric(format(rest_bikes_q[,1], "%H")) == h)
  tr <- subset(tr, as.numeric(format(tr[,1], "%M")) == m)
  tr['COL'] <- "blue"
  tr[tr$BIKE == 0,]$COL <- "red"
  m <- leaflet(tr) %>% addProviderTiles(providers$Stamen.Toner) %>%
    addCircleMarkers(
      lat = ~X,
      lng = ~Y,
      radius = ~sqrt(tr$BIKE) + 2,
      stroke = FALSE, color = ~COL, fillOpacity = 0.5
    )
  return(m)
  setView(m, 126.922646, 37.525792, 14)
}

# plot map with only empty zone
empty_bike_map <- function (d, h) {
  tr <- subset(rest_bikes_q, as.numeric(format(rest_bikes_q[,1], "%w")) == d & as.numeric(format(rest_bikes_q[,1], "%H")) == h)
  empty <- subset(tr, tr$BIKE == 0)
  leaflet(empty) %>% addProviderTiles(providers$Stamen.Toner) %>%
    addCircleMarkers(
      lat = ~X,
      lng = ~Y,
      stroke = FALSE, color = "red", fillOpacity = 0.3
    )
}

# automatically plot a map of rest bike and save webshot of map with interval 1 hour
map_gen <- function() {
  for (ww in 0:6) {
    for (hh in 0:23) {
      saveWidget(rest_bike_map(ww, hh), "temp.html", selfcontained = FALSE)
      webshot("temp.html", file = paste("/map/by1h/w", as.character(ww), "h", as.character(hh), ".png", sep = ""),
              cliprect = "viewport")
    }
  }
}


# automatically plot a map of rest bike and save webshot of map with interval 10 minutes
map_gen_AtoB <- function(w, h1, h2) {
  for (hh in h1:(h2 - 1)) {
    for (mm in seq(0, 50, by = 10)) {
      saveWidget(rest_bike_mapDHM(w, hh, mm), "temp.html", selfcontained = FALSE)
      webshot("temp.html", file = paste("/map/by10m/yy/w", as.character(w), "h", as.character(hh), "m", as.character(mm), ".png", sep = ""),
              cliprect = "viewport")
    }
  }
}
