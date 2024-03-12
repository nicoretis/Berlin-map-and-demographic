#On the following script I'll create images of population density in Berlin for each district and their changes from 2013 to 2023

pacman::p_load(ggplot2, dplyr, tibble,
               readxl, conflicted, sf,
               sfdep, tidyr,wrappedtools,
               lubridate
)
conflicts_prefer(dplyr::filter)

#Preprocess the Excell file
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
rawdata <- rawdata |>
  mutate(Herkunft = ifelse(row_number() %in% 5:15, "Deutsche",
                    ifelse(row_number() %in% 18:28, "Ausländer",
                    ifelse(row_number() %in% 31:41, "insgesamt",''))))


#Selected only first table, changed the format of Stichstag and removed NAs lines
rawdata <-rawdata |>
  slice(5:41) |>
  mutate(Stichtag = as.Date(as.numeric(Stichtag), origin = "1899-12-30")) |>
  mutate_at(vars(Berlin:Reinickendorf), as.numeric)|>
  filter(!is.na(Stichtag)) 

#Read the shape file of each districts and add the area
districts_sh <- st_read("~/Desktop/R/Berlin-map-and-demographic/Districts_shapes/bezirksgrenzen.shp") |>
  select(Gemeinde_n, geometry) |>
  mutate(area= as.numeric(st_area(geometry))/10**6)


#Creation of a new tibble with the percentage increment of population for each districts through the years. Reference year= 2013
############################
#########FUNCTIONS##########
############################
create_gradient<- function(oldtibble,newtibble){
  newtibble <- tibble()
  for (el in seq_along(oldtibble$Berlin)){
    if (el < length(oldtibble$Berlin)){
      start_values <- oldtibble[el, numvar$names]
      end_values <- oldtibble[el+1, numvar$names]
      sequence <- mapply(seq, from = start_values, to = end_values, length.out = 49) |>
        tail(-1) |> as_tibble() # to avoid first element repeated
      newtibble <-bind_rows(newtibble, sequence)}}
  newtibble$Stichtag<- rep(oldtibble$Stichtag[-1], each = 48)
  return (newtibble)
}

calc_perc <- function(reference, value) {
  round((value / reference - 1) * 100, digits = 1)
}

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

save_pivot_longer<- function(tibble,folder){
  tibble$frame<- seq(1,480)
  newtibble <- tibble |>
    select(-Berlin) |>
    pivot_longer(cols = -c(Stichtag, frame), names_to='District', values_to = 'Perc_change') |>
    left_join(districts_sh, by = c('District' = 'Gemeinde_n'))
  saveRDS(newtibble, file = paste0("~/Desktop/R/Berlin-map-and-demographic/Population_density/big_files/",folder, ".rds"))
}

#Numeric values
numvar <- ColSeeker(varclass="numeric")

#For the Ausländer (foreign)
aus<- rawdata |> filter(Herkunft=='Ausländer')
longaus<- create_gradient(aus,longaus)
longausper<- make_percentage(longaus)
save_pivot_longer(longausper, "foreign")

#Then the Germans
ger<- rawdata |> filter(Herkunft=='Deutsche')
longger<- create_gradient(ger,longer)
longgerper<- make_percentage(longger)
save_pivot_longer(longgerper, "german")

#And for the total population
#This need to be fixed
tot<- rawdata |> filter(Herkunft=='Insgesamt')
longtot<- create_gradient(tot,longtot)
longtotper<- make_percentage(longtot)
save_pivot_longer(longtotper, "total")






