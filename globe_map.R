library(sf)

map <- st_read('UIA_World_Countries_Boundaries')

map <- st_transform(map, crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=-90 +lat_0=50")

#ggplot() + 
#  geom_sf(data=tracts) + 
#  geom_point(data=dc_schools, aes(x=LONGITUDE, y=LATITUDE, color=CHARTER), alpha=0.5) + 
#  scale_color_manual(values=c("#1696d2", "#fdbf11"))

USnow <- subset(USall,USall$year == 2017)
USnow <- ddply(USnow, .(cty_name), summarize, imports = sum(imports), exports = sum(exports))
USnow$Country <- USnow$cty_name

map <- left_join(map, USnow, by = 'Country')
map$exports <- ifelse(is.na(map$exports), 0, map$exports)
map$imports <- ifelse(is.na(map$imports), 0.0001, map$imports)

submap <- subset(map, map$exports == 0)
cent <- st_centroid(submap)

logo <- ggplot(data = map, aes(fill = log(imports))) +
  geom_sf(size = .05) +
  scale_fill_staples(discrete = F, palette = 'new') +
  theme(
    axis.ticks = element_line(color = NA),
    axis.line = element_line(color = NA),
    axis.text = element_text(color = NA),
    legend.position="none")

ggsave('logo.jpg', width=.5, height=.5)
