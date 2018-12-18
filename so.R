library(ggmap)
library(ggplot2)
library(readr)

lat = c(38.4123, 38.4139, 38.3997, 38.3941, 38.3949, 38.3950)
lon = c(-110.7835, -110.7819, -110.7916, -110.7896, -110.7869, -110.7852)
data = data.frame(lon=lon, lat=lat)

lat = c(34.4123, 38.4139, 38.3997, 38.3941, 38.3949, 38.3950)
lon = c(-110.7835, -110.7819, -110.7916, -110.7896, -110.7869, -110.7852)
data = data.frame(lon=lon, lat=lat)

data$size <- c(1,1,1,1,1,1)
data$color <- c("hotpink1", "hotpink1", "steelblue4", "#009900", "#009900", "#009900")

# Case 1
df <- read_csv(file = "coordinatesEVA3.csv")
df <- as.data.frame(df)
data <- data[,1:ncol(data)]

bbox <- ggmap::make_bbox(lon=lon, lat=lat, data, f = 0.05)
mapLoc <- get_map(location = bbox, zoom = 5, maptype = "satellite")

p <- ggmap(mapLoc, extent = "panel", legend = "bottomright") + geom_line(aes(x = lon, y = lat), data=data) + geom_point(aes(x = lon, y = lat, size = size, color = color), data = data) + scale_color_identity() + theme(legend.position="none") + xlab("Longitude") + ylab("Latitude") + scale_size_identity()

