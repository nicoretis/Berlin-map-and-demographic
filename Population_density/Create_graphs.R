pacman::p_load(ggplot2, dplyr, tibble,
               readxl, conflicted, sf,
               sfdep, tidyr,wrappedtools,
               lubridate
)



rawdata <- readRDS("~/Desktop/R/Berlin-map-and-demographic/Population_density/longausper.rds")

#Finally we have 10 year, and 48 frames for single year for a total of 480 frames for the final video.
#The following loop will create a new tibble out of every row and will save the plot in my folder.

for (fr in seq(1, 480)) {
  frametibble <- rawdata |>
    filter(frame == fr)
  ggplot(frametibble, aes(fill = frametibble$Perc_change)) +
    geom_sf(data = frametibble$geometry, size = 1.5, color = "black") +
    scale_fill_gradientn( colors= c("#093454", "#e7470b","#f307c1"), limits = c(0, 380)) +
    labs(fill = "% increment")+ #very easy way of changing legend names
 ggtitle(paste0("Change (%) of Foreigns in Berlin Districts, year 2013-",
                 as.numeric(format(frametibble$Stichtag[1], "%Y")))) + 
  theme_minimal() + # Use a minimal theme instead of theme_void()
  theme(plot.background = element_rect(fill = "white"))
  ggsave(filename = paste0("~/Desktop/R/Berlin-map-and-demographic/Population_density/video/plot", fr, ".png"), width = 10, height = 8, dpi = 300 , device = "png")
}



  
  


