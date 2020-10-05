## zone locations

# get zone location by code vector
get_zone_loc <- function(cv) {
  cv <- data.frame(as.character(cv))
  colnames(cv) <- c("CODE")
  cv <- left_join(cv, zones_df, by = "CODE")
  return(cv[c('X', 'Y')])
}

# Show map of Total use(dep + arr)
total_use_map <- function (df, circle_size, set_color = "blue") {
  s <- summary_byZone(df)
  s <- cbind(s, get_zone_loc(s$CODE))
  
  m <- leaflet(s) %>% addTiles %>% 
    addPolygons(data = seoul_shp, weight = 1, color = "black", fillColor = "black") %>%
    addCircleMarkers(
      lat = ~Y,
      lng = ~X,
      radius = ~sqrt(s$SUM_COUNT / s$SINCE_DAYS * circle_size),
      stroke = FALSE, color = set_color, fillOpacity = 0.5
    )
  return(m)
}

# generate heatmap of zone (zl should have X, Y)
zone_heatmap <- function(zl, k_size) {
  leaflet() %>% addTiles() %>% 
    addMapPane("back", zIndex = 300) %>%
    addPolygons(data = seoul_shp, weight = 1, color = "black", fillColor = "black", options = pathOptions(pane = "back")) %>%
    addHeatmap(data = zl, lat = ~Y, lng = ~X, radius = k_size)
}

# basic zone map
plot_zone_basicmap <- function() {
  plot(seoul_shp)
  plot(zones_loc, add = T, pch = 16, cex = .4, col = "forestgreen")
}