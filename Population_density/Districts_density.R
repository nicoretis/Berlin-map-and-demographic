#On the following script I'll create images of population density in Berlin for each district and their changes from 2013 to 2023

pacman::p_load(ggplot2, dplyr, tibble,
               readxl, conflicted, sf,
               sfdep, tidyr,wrappedtools,
               lubridate
)
conflicts_prefer(dplyr::filter)

rawdata <- read_xlsx("~/Desktop/R/Berlin-map-and-demographic/Berlindemographics.xlsx",
                      sheet = "T3 T4") |>
                     as_tibble()

#Second row contains the name of the columns
colnames(rawdata) <- as.character(rawdata[2, ])

#Inconsistent break lines are removed
colnames(rawdata) <-lapply(colnames(rawdata), function(x) gsub("-\r\n", "", x))

#Dash lines are reinserted before in the joined districts such as Tempelhof-Schöneberg"
colnames(rawdata) <-lapply(colnames(rawdata), function(x)  gsub("([a-z])([A-Z])", "\\1-\\2", x))

#Creating Herkunft column
herkunft_content <-rawdata[c(4, 17, 30), "Berlin"]
rawdata[5:15, "Herkunft"] <- herkunft_content[1,]
rawdata[18:28, "Herkunft"] <- herkunft_content[2,]
rawdata[31:41, "Herkunft"] <- herkunft_content[3,]

#Select only first table, change the formate of Stichstag and remove NAs lines
rawdata <-rawdata |>
  slice(5:41) |>
  mutate(Stichtag = as.Date(as.numeric(Stichtag), origin = "1899-12-30")) |>
  mutate_at(vars(Berlin:Reinickendorf), as.numeric)|>
  filter(!is.na(Stichtag)) 


districts_sh <- st_read("~/Desktop/R/Berlin-map-and-demographic/Districts_shapes/bezirksgrenzen.shp") |>
  select(Gemeinde_n, geometry) |>
  mutate(area= as.numeric(st_area(geometry))/10**6)



# #Creation of one single row of a specific year of the total population for each districts
# data <- rawdata |> filter( Stichtag=='2013-12-31' ,Herkunft=='insgesamt') |> select (-Berlin)
# 
# #Pivot longer to prepare the data to the join with the shapefile
# data <- data |>
#   pivot_longer(cols = -c(Stichtag, Herkunft), names_to = "District", values_to = "Population") 
# 
# data<- merge(data, districts_sh,
#                   by.x = "District",
#                   by.y = "Gemeinde_n", all = FALSE) 
# 
# 
# population<- data$Population
# 
# ggplot(data, aes(fill=population/area)) + 
#   geom_sf(data = districts_sh, size = 1.5, color = "black") +
#   scale_fill_continuous()+
#   ggtitle("Berlin Districts Population 2013") + 
#   theme_void()


aus<- rawdata |> filter(Herkunft=='Ausländer')
numvar <- ColSeeker(varclass="numeric")

#A function that calculates the percentage difference among two number, probably it already exists but I couldn't find it
calc_perc <- function(reference, value) {
  round((value / reference - 1) * 100, digits = 1)
}

#Creation of a new tibble with the percentage increment of population for each districts thru the years. Reference year= 2013
make_percentage <- function(oldtibble){
newtibble<- tibble()
reference <- oldtibble[1,numvar$names]
for (i in seq_along(oldtibble[[1]])){
  percentage <- calc_perc(reference=reference, oldtibble[i,numvar$names])
  newtibble <- bind_rows(newtibble, percentage)
}
newtibble$Stichtag <- oldtibble$Stichtag
return(newtibble)
}

#ausper <- Ausländer percentage
ausper<- make_percentage(aus)





#Now it's time to create 24 fps for 2 seconds for each year  If I want to do a real smooth change: Idea, work with ranges (seq) on the the "aus" (Ausländer) tibble.
longaus<- tibble()
for (el in seq_along(aus$Berlin)){
  if (el < length(aus$Berlin)){
    start_values <- aus[el, numvar$names]
    end_values <- aus[el+1, numvar$names]
    sequence <- mapply(seq, from = start_values, to = end_values, length.out = 49) |>
      tail(-1) |> as_tibble() # to avoid first element repeated
    longaus <-bind_rows(longaus, sequence)
    }}

#Adding the elongated stichtag column
longaus$Stichtag<- rep(aus$Stichtag[-1], each = 48)



longausper<- make_percentage(longaus)
longausper$frame<- seq(1,480)



#Pivot longer to prepare the data to the join with the shapefile
longausper <- longausper |> 
  select(-Berlin) |>
  pivot_longer(cols = -c(Stichtag, frame), names_to='District', values_to = 'Perc_change') |>
  left_join(districts_sh, by = c('District' = 'Gemeinde_n'))

#Saving the file for the data visualization
saveRDS(longausper, file = "~/Desktop/R/Berlin-map-and-demographic/Population_density/longausper.rds")







