pacman::p_load(ggplot2, dplyr, tibble,
               readxl, conflicted, sf,
               sfdep, tidyr,wrappedtools,
               lubridate
)



#Finally we have 10 year, and 48 frames for single year for a total of 480 frames for the final video.
#The following loop will create a new tibble out of every row and will save the plot in my folder.

#Foreign
foreign <- readRDS("~/Desktop/R/Berlin-map-and-demographic/Population_density/big_files/foreign.rds")

for (fr in seq(1, 100)) {
  frametibble <- foreign |>
    filter(frame == fr)
  ggplot(frametibble, aes(fill = frametibble$Perc_change)) +
    geom_sf(data = frametibble$geometry, size = 1.5, color = "black") +
    scale_fill_gradientn( colors= c("#3f7ed4","#3fd450","#ef4747","#c53fc7","#833fc7"), limits = c(0, 400)) +
    labs(fill = "% increment")+ #very easy way of changing legend names
 ggtitle(paste0("Change (%) of Foreigns in Berlin Districts, year 2013-",
                 as.numeric(format(frametibble$Stichtag[1], "%Y")))) + 
  theme_minimal() + # Use a minimal theme instead of theme_void()
  theme(plot.background = element_rect(fill = "white"))
  ggsave(filename = paste0("~/Desktop/R/Berlin-map-and-demographic/Population_density/big_files/foreignvideo/plot",fr , ".png"), width = 10, height = 8, dpi = 300 , device = "png")
}

#German
german <- readRDS("~/Desktop/R/Berlin-map-and-demographic/Population_density/big_files/german.rds")

for (fr in seq(1, 475)) {
  frametibble <- german |>
    filter(frame == fr)
  ggplot(frametibble, aes(fill = frametibble$Perc_change)) +
    geom_sf(data = frametibble$geometry, size = 1.5, color = "black") +
    scale_fill_gradientn( colors= c("#11b163", "#bad22d","#d2842d"), limits = c(-5, 5)) +
    labs(fill = "% change")+ #very easy way of changing legend names
    ggtitle(paste0("Change (%) of German in Berlin Districts, year 2013-",
                   as.numeric(format(frametibble$Stichtag[1], "%Y")))) + 
    theme_minimal() + # Use a minimal theme instead of theme_void()
    theme(plot.background = element_rect(fill = "white"))
  ggsave(filename = paste0("~/Desktop/R/Berlin-map-and-demographic/Population_density/big_files/germanvideo/plot",fr , ".png"), width = 10, height = 8, dpi = 300 , device = "png")}


  
  


