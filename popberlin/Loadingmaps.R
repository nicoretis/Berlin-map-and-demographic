pacman::p_load(sf, ggplot2)
#LOADING THE SHAPEFILES FOR THE BERLIN NEIGHBOREHOODS


# Read the shapefiles and clean geometries
ortsteilen_sh <- st_read("~/Desktop/R/berlincentroidpop/shapefiles/berlin_ortsteile.shp") |> st_make_valid()

posten_sh <- st_read("~/Desktop/R/berlincentroidpop/postenshapefiles/plz.shp") |> st_make_valid()

# Calculate centroids
# centroids_ort <- st_centroid(ortsteilen_sh)
# centroids_plz <- st_centroid(posten_sh)

# Plot the neighborhoods and centroids
ggplot() + 
  geom_sf(data = ortsteilen_sh, size = 1.5, color = "black", fill = "cyan1") + 
  ggtitle("Berlin Neighborhood Outline") + 
  coord_sf()

# Plot the postenzahlen and centroids
ggplot() + 
  geom_sf(data = posten_sh, size = 1.5, color = "black", fill = "cyan1") + 
  ggtitle("Berlin Postenzahlen Outline") + 
  coord_sf()

#Cool, now let's import the data from the population per postal code


