# This is to map the EL percentages by county

library(tigris)
library(mapview)
library(tmap)
library(RColorBrewer)
library(viridis)
library(scales)

options(tigris_use_cache = TRUE)


counties <- counties("CA")


map.counties <- left_join(counties, summation, by = c("NAME" = "CountyName")) %>%
    mutate(perc.EL = round2(perc.EL*100, 1) ,
           perc.EEL = round2(perc.EEL*100, 1) )



##

ggplot(map.counties) +
    geom_sf(aes(fill = perc.EL))


##

pal <- seq_gradient_pal("white", "red")(seq(0, 1, length.out = 50))  # from Scales


mapview(map.counties, zcol = "perc.EL", legend = FALSE, col.regions = pal)

map.el <- mapview(map.counties, zcol = "perc.EL", legend = FALSE, col.regions = pal)

mapshot(map.el, url = "PercentEL.html")

##

tmap_mode("plot")

tm<- tm_shape(map.counties ) +
    tm_fill("perc.EL", alpha = 1) +
    tm_borders()# +
   # tm_text("NAME", auto.placement = TRUE)


tmap_save(tm, "map-el-percent.png")
