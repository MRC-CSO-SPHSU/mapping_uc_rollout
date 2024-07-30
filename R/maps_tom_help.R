# set up for maps
library(gganimate)
library(rgdal)
library(parlitools)
library(ggplot2)
library(viridis)
library(sp)
library(maptools)
library(dplyr)
library(ggthemes)
library(broom)
library(reshape2)

# Load UK countries' shapefile into R
# shapefile <- readOGR(dsn="/Users/Sophie/Downloads/Local_Authority_Districts_December_2017_Super_Generalised_Clipped_Boundaries_in_United_Kingdom_WGS84 (1)", layer="Local_Authority_Districts_December_2017_Super_Generalised_Clipped_Boundaries_in_United_Kingdom_WGS84")
#Reshape for ggplot2 using the Broom package
# mapdata <- tidy(shapefile, region="lad17cd")
# Loading UC data
data <- read.csv("uc_test.csv")
datm <- melt(data, 'Month.Year', 
             variable.name = 'id',
             value.name = 'UC'
)
# maps <- merge(mapdata, datm,
#               by.x="id",
#               by.y="id",
#               all=T)
# remove month year NAs
# maps <- maps %>% filter(!is.na(Month.Year))

datm <- datm %>% filter(!is.na(Month.Year))

# load in datm from saved file
datm <- read.csv("UC_data.csv", stringsAsFactors = FALSE, header = T)

# create phase variable (one per month)
datm$phase <-as.factor(datm$Month.Year)

# Load the hexagonal maps
hex_dat <- parlitools::local_hex_map

# make it into a SpatialPolygonsDataFrame object
hex_poly <- as(hex_dat, "Spatial")
hex_poly <- as(hex_poly, "SpatialPolygonsDataFrame")

# fortify to map using ggplot
hex_map <- ggplot2::fortify(hex_poly, region ="la_code")
# Join to fortified dataset
hex_map <- left_join(hex_map, datm, by = c("id" = "id"))


# Make the map
p <- ggplot(data = hex_map, aes(x=long, y=lat, group=group)) +
  geom_polygon(aes(fill=UC), colour="black", alpha = 0.9) +
  scale_fill_viridis(limit = c(0,100), breaks = seq(0,100,20), direction = -1) +
  labs(title = 'Year and month {closest_state}', fill = "Percentage of people on UC") +
  theme_void() +
  coord_map() +
  transition_states(phase, transition_length = 1, state_length = 1) 

animate(p, fps = 4, nframes = 136)
anim_save("uc_hexagons.gif")






write.csv(hex_map, file = "UC_hex.csv")
write.csv(datm, file = "UC_data.csv")

######################################################################################
p <- ggplot(maps, aes(long, lat, group = group, fill=UC)) +
  geom_polygon() +
  geom_path(colour="white") +
  theme_map()+
  transition_manual(Month.Year)

animate(
  plot = p,
  nframes = length(unique(maps$Month.Year)),
  fps = 1)


test1 <- animate(p)
anim_save("test.gif")


