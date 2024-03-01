#Loading the total population living in each postal code area.
#It is more specific than using the district or neighborhood in determining the population-adjusted-centroid of Berlin.

pacman::p_load(ggplot2, dplyr, tibble,
               readxl, conflicted
)

conflicts_prefer(dplyr::filter)

#Importing the excel file
rawdata <- read_excel("~/Desktop/R/berlincentroidpop/popberlin.xlsx",         sheet = "T14")|>
        as_tibble()

#Assigning the right column name
for (i in 1:ncol(rawdata)) {
  if (i <= 3 | i == 12) {
    col_name <- rawdata[2, i]
  } else {
    col_name <- rawdata[3, i]
  }
  colnames(rawdata)[i] <- col_name
}

#Removing the first four rows containing columns names
rawdata<- slice(rawdata, -(1:4))
#Removing last row as well since it contains calculations
rawdata <-head(rawdata, -1)

#Renaming
rawdata<- rawdata |> rename(Total=`Ins-\r\ngesamt`,
                            `65 +`=`65 und\r\nmehr`,
                            `Weiblich`=`Darunter\r\nweiblich`)

#Assigning the right numeric type

rawdata<- rawdata |>
  mutate(across(Total:Weiblich, as.integer))

#Now we have a total of 237 postal codes. On the shapefile we will construct the map we have only 194. What are the postal codes missing? Can I join the smaller area covered by a postal code?

#Looking what postal code are missing on the maps
rawdatapc <- rawdata$Postleitzahl|> unique() #a set of unique postal code for the rawdata tibble
shapefilepc <- posten_sh$plz |> unique() # same but for the shapefile postal codes

#It seems there were duplicates on both postal codes. Three postal codes are still missing on the rawdata postal codes.
#What code are them?
missing_postal_codes<-  setdiff(shapefilepc,rawdatapc)

#Showing the postal codes that are missing in the map

missing_posten_sh <- filter(
  posten_sh, plz %in% missing_postal_codes)

#Since the area covered by those postal codes is very small I can safely ignore them.
ggplot() + 
  geom_sf(data = missing_posten_sh, size = 1.5, color = "black", fill = "cyan1") + 
  ggtitle("Missing Population Data For These Postal Codes") +
  coord_sf()

#Joining duplicates in the rawdata
rawdata|> filter(duplicated(rawdata$Postleitzahl))
rawdata|> filter(rawdata$Postleitzahl=="10178")

#Those are not mistakes, not all the postal codes are 100% inside a specific district sometimes they may spread in two districts, that's the reason of the duplication.
#For simplicity reasons I'm going to join the duplicates using the row with the greatest population.

#As the result of this operation I will loose Bezirk column
joined_without_bezirk<- rawdata |>
  group_by(Postleitzahl) |>
  summarize(across(where(is.numeric), sum))

#I add back the bezirk column
bezirk <- rawdata |>
  group_by(Postleitzahl) |>
  arrange(desc(Total)) |> 
  slice(1)|>
  ungroup()|>
  select(Bezirk)

#The result is a tibble with no duplicated postal codes, the remaining row contains the sum of all the numeric columns of the two neighborhood that shares the same postal code and the name of the neighborhood with the highest population for that postal code.
data <- bind_cols(bezirk, joined_without_bezirk)

#Now we should be done with pre-processing
#//////////////////////////////////
#//////////////////////////////////

centroids <- st_centroid(posten_sh)

# Extract X and Y coordinates of centroids
centroid_coords <- st_coordinates(centroids)

# Sum X and Y coordinates
total_x <- sum(centroid_coords[, "X"])
total_y <- sum(centroid_coords[, "Y"])

# Calculate the average X and Y coordinates
num_centroids <- nrow(centroid_coords)
average_x <- total_x / num_centroids
average_y <- total_y / num_centroids

#Convoluted way but I have struggled with CRS problems

# First I have to create a data frame with the average centroid coordinates
average_centroid_df <- data.frame(x = average_x, y = average_y)

# Then I convert the data frame to an sf object
average_centroid_sf <- st_as_sf(average_centroid_df, coords = c("x", "y"))

# Now I assign the missing CRS to match the CRS of posten_sh
st_crs(average_centroid_sf) <- st_crs(posten_sh)

#And finally I can plot it!
ggplot() + 
  geom_sf(data = posten_sh, size = 1.5, color = "black", fill = "cyan1") +
  geom_sf(data = average_centroid_sf, color = "red", size = 3) +
  ggtitle("Berlin Neighbors Outline with Centroids") + 
  coord_sf()


#Let's compute the population-weighted centroid
mergedata<- merge(centroids, data,
                  by.x = "plz",
                  by.y = "Postleitzahl", all = FALSE)
#idea1 creating a very big tibble with all 3.8 millions rows, one for every person living in Berlin.
bigdata<- mergedata |>
  slice(rep(row_number(), mergedata$Total)) |>
  select(geometry)|>
  ungroup()


centroid_coords <- st_coordinates(bigdata)

# Sum X and Y coordinates
total_x <- sum(centroid_coords[, "X"])
total_y <- sum(centroid_coords[, "Y"])

# Calculate the average X and Y coordinates
num_centroids <- nrow(centroid_coords)
average_x <- total_x / num_centroids
average_y <- total_y / num_centroids


average_centroid_df <- data.frame(x = average_x, y = average_y)

# Then I convert the data frame to an sf object
average_centroid_sf <- st_as_sf(average_centroid_df, coords = c("x", "y"))

# Now I assign the missing CRS to match the CRS of posten_sh
st_crs(average_centroid_sf) <- st_crs(posten_sh)

#And finally I can plot it!
ggplot() + 
  geom_sf(data = posten_sh, size = 1.5, color = "black", fill = "cyan1") +
  geom_sf(data = average_centroid_sf, color = "red", size = 3) +
  ggtitle("Berlin Neighbors Outline with Centroids Weighted by Population") + 
  coord_sf()












