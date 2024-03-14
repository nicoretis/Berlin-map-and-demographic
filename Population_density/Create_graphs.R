pacman::p_load(ggplot2, dplyr, tibble,
               readxl, conflicted, sf,
               sfdep, tidyr,wrappedtools,
               lubridate
)



#Finally we have 10 year, and 48 frames for single year for a total of 480 frames for the final video.
#The following loop will create a new tibble out of every row and will save the plot in my folder.

######Foreign######
foreign <- readRDS("~/Desktop/R/Berlin-map-and-demographic/big_files/foreign.rds")

for (fr in seq(100, 480)) {
  frametibble <- foreign |>
    filter(frame == fr)
  ggplot(frametibble, aes(fill = frametibble$Perc_change)) +
    geom_sf(data = frametibble$geometry, size = 1.5, color = "black") +
    scale_fill_gradientn( colors= c("#3f7ed4","#3fd450","#ef4747","#c53fc7","#833fc7"), limits = c(0, 400)) +
    labs(fill = "% increment")+ #very easy way of changing legend names
 ggtitle(paste0("Change (%) of Foreigns in Berlin Districts, years 2013-",
                 as.numeric(format(frametibble$Stichtag[1], "%Y")))) + 
  theme_minimal() + # Use a minimal theme instead of theme_void()
  theme(plot.background = element_rect(fill = "white"))
  ggsave(filename = paste0("~/Desktop/R/Berlin-map-and-demographic/big_files/foreignvideo/plot",fr , ".png"), width = 10, height = 8, dpi = 300 , device = "png")
}

######German######
german <- readRDS("~/Desktop/R/Berlin-map-and-demographic/big_files/german.rds")

for (fr in seq(460, 475)) {
  frametibble <- german |>
    filter(frame == fr)
  ggplot(frametibble, aes(fill = frametibble$Perc_change)) +
    geom_sf(data = frametibble$geometry, size = 1.5, color = "black") +
    scale_fill_gradientn( colors= c("#11b163", "#bad22d","#d2842d"), limits = c(-5, 5)) +
    labs(fill = "% change")+ #very easy way of changing legend names
    ggtitle(paste0("Change (%) of German in Berlin Districts, years 2013-",
                   as.numeric(format(frametibble$Stichtag[1], "%Y")))) + 
    theme_minimal() + # Use a minimal theme instead of theme_void()
    theme(plot.background = element_rect(fill = "white"))
  ggsave(filename = paste0("~/Desktop/R/Berlin-map-and-demographic/big_files/germanvideo/plot",fr , ".png"), width = 10, height = 8, dpi = 300 , device = "png")}


######Total######
total <- readRDS("~/Desktop/R/Berlin-map-and-demographic/big_files/total.rds")

for (fr in seq(1, 480)) {
  frametibble <- total |>
    filter(frame == fr)
  ggplot(frametibble, aes(fill = frametibble$Perc_change)) +
    geom_sf(data = frametibble$geometry, size = 1.5, color = "black") +
    scale_fill_gradientn( colors= c("#54cf44", "#c3cf44","#cf5444"), limits = c(0, 20)) +
    labs(fill = "% change")+ #very easy way of changing legend names
    ggtitle(paste0("Change (%) of Total Population in Berlin Districts, years 2013-",
                   as.numeric(format(frametibble$Stichtag[1], "%Y")))) + 
    theme_minimal() + # Use a minimal theme instead of theme_void()
    theme(plot.background = element_rect(fill = "white"))
  ggsave(filename = paste0("~/Desktop/R/Berlin-map-and-demographic/big_files/totalvideo/plot",fr , ".png"), width = 10, height = 8, dpi = 300 , device = "png")}

#####Share#####
share<- readRDS("~/Desktop/R/Berlin-map-and-demographic/big_files/share.rds")

for (fr in seq(1, 480)) {
  frametibble <- share |>
    filter(frame == fr)
  ggplot(frametibble, aes(fill = frametibble$Perc_change)) +
    geom_sf(data = frametibble$geometry, size = 1.5, color = "black") +
    scale_fill_gradientn(colors= c("#3f7ed4","#3fd450","#ef4747","#c53fc7","#833fc7")
                         , limits = c(0, 50)) +
    labs(fill = "Share of Foreign %")+ #very easy way of changing legend names
    ggtitle(paste0("Change of share of Foreign in the total Population in Berlin Districts, years 2013-",  as.numeric(format(frametibble$Stichtag[1], "%Y")))) + 
    theme_minimal() + # Use a minimal theme instead of theme_void()
    theme(plot.background = element_rect(fill = "white"))
  ggsave(filename = paste0("~/Desktop/R/Berlin-map-and-demographic/big_files/sharevideo/plot",fr , ".png"), width = 10, height = 8, dpi = 300 , device = "png") }

  
  


