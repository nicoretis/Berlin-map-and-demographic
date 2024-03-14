pacman::p_load(ggplot2, dplyr, tibble,
               readxl, conflicted, sf,
               sfdep, tidyr,wrappedtools,
               lubridate
)

conflicts_prefer(dplyr::filter)

#Preprocess the Excell file
rawdata <- read_xlsx("~/Desktop/R/Berlin-map-and-demographic/Berlindemographics.xlsx",sheet = "T11") |>
  as_tibble() |>
  select(-1) |>
  head(168)

for (i in 1:ncol(rawdata)) {
  if (i == 2 | i == 11) {
    col_name <- rawdata[2, i]
  } else {
    col_name <- rawdata[3, i]
  }
  colnames(rawdata)[i] <- col_name
}

names(rawdata)[1] <- "Bezirk"
names(rawdata)[2] <- "Insgesamt"
colnames(rawdata) <-lapply(colnames(rawdata), function(x) gsub("\r\n", " ", x))
rawdata<- rawdata |> filter(!is.na(`Darunter weiblich`),Bezirk!="Zusammen", Bezirk!="Insgesamt") |> mutate_at(vars(Insgesamt:`Darunter weiblich`), as.numeric)

rawdata <- rawdata[-1, ]

orts_sh <- st_read("~/Desktop/R/Berlin-map-and-demographic/Shapefiles/Ortsteilen_shapes/berlin_ortsteile.shp") |> st_make_valid()

districts_sh <- st_read("~/Desktop/R/Berlin-map-and-demographic/Shapefiles/Districts_shapes/bezirksgrenzen.shp") |> st_make_valid()

########

#There is no point in filtering since no neighborhood as extremely low values. 
rawdata |>filter(`65 und mehr`<150)
rawdata |>filter(`unter 6`<150)

#To be accurate the aging index is expressed as the ratio of aged 65+ and younger than 14. Here tho my group age is up to 15 years old.
rawdata$ageind <- rawdata$`65 und mehr`/(rawdata$`unter 6`+ rawdata$`6 - 15`)
              
rawdata<- merge(rawdata, orts_sh,
                  by.x = "Bezirk",
                  by.y = "name", all = FALSE)

#Plot
ggplot(rawdata, aes(fill = rawdata$ageind)) +
  geom_sf(data = rawdata$geometry, size = 1.5, color = "black") +
  scale_fill_gradientn( colors= c("#1107d9","#f01313"), limits = c(0.3, 3)) +
  labs(fill = "older than 65 /\r\n younger than 15")+ #very easy way of changing legend names
  ggtitle("Aging index for each Neighborhood of Berlin in 2023")+
  theme_minimal() + # Use a minimal theme instead of theme_void()
  theme(plot.background = element_rect(fill = "white"))
ggsave(filename = "~/Desktop/R/Berlin-map-and-demographic/Aging_index/aging_index", width = 10, height = 8, dpi = 300 , device = "png")


#Animation part
rawdata <-rawdata|> arrange(ageind)
bbox <- st_bbox(rawdata$geometry)
s<-0
for (frames in seq(0.38,3,0.015)){
  frame= rawdata|>filter(ageind<=frames)
  ggplot(frame, aes(fill = frame$ageind)) +
    geom_sf(data = frame$geometry, size = 1.5, color = "black") +
    scale_fill_gradientn( colors= c("#1107d9","#f01313"), limits = c(0.3, 3)) +
    labs(fill = "older than 65 /\r\n younger than 15")+ #very easy way of changing legend names
    ggtitle("Aging index for each Neighborhood of Berlin in 2023")+
    theme_minimal() + # Use a minimal theme instead of theme_void()
    theme(plot.background = element_rect(fill = "white"))+
    coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]), ylim = c(bbox["ymin"], bbox["ymax"]))
  s<- s+1
  ggsave(filename = paste0("~/Desktop/R/Berlin-map-and-demographic/big_files/aging_animation/aging_index",s,".png"), width = 10, height = 8, dpi = 300 , device = "png")+
    geom_text(data = frame, aes(label = frame$Bezirk), size = 2, color = "black", check_overlap = FALSE)
  
  }

#How do I print only the district names on the side or on the bottom of the graph?







