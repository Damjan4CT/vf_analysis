library(dplyr)
library(ggplot2)
library(cwi) 
if (require(sf)) {
  sf::sf_use_s2(FALSE)
}
library(RSQLite)
library(DBI)
library(leaflet)
library(leaflet.extras2)
theme_set(theme_void())
library(htmlwidgets)
library(tidygeocoder)

####################################################################################
# Sources:
# https://ct-data-haven.github.io/cwi/articles/shapefiles.html
# https://github.com/CT-Data-Haven/town_profile_data22/blob/main/scripts/05_upload_shapes_release.sh
# https://github.com/CT-Data-Collaborative/ct-zip-to-town/blob/master/data/2020zip-to-town.csv
# https://ctdatahaven.org/data-dashboard/2024-connecticut-state-legislative-district-profiles/
# https://geodata.ct.gov/datasets/CTDOT::ctdot-municipalities/about 
# ct data haven data dashboard: https://www.ctdatahaven.org/data-dashboard/connecticut-town-data-viewer

# https://catalog.data.gov/dataset/tiger-line-shapefile-current-state-connecticut-state-legislative-district-sld-lower-chamber
# https://www.census.gov/cgi-bin/geo/shapefiles/index.php?year=2020&layergroup=ZIP%20Code%20Tabulation%20Areas

# Git Hosting without Pages: https://raw.githack.com/
## https://rawcdn.githack.com/Damjan4CT/vf_analysis/19067009fdbabfb463a1eac1cc9bd1bc0351e134/ct03_likely_signers.html
## file:///C:/Users/Ruairi/Documents/Coding%20Projects%202025/ct04_testing/ct03_likely_signers.html

####################################################################################

# Initialize voterfile from Sqlite -> R df
# Downloaded voters.db from "https://drive.usercontent.google.com/download?id=1_rvPowWrDK2tecWRwAi6KTx9zyrjOAKV&export=download&authuser=0"
setwd("C:/Users/Ruairi/Documents/Coding Projects 2025/ct04_testing")
conn <- dbConnect(RSQLite::SQLite(), "C:/Users/Ruairi/Downloads/voters.db")
df_voters <- dbGetQuery(conn, "SELECT * FROM voters")

# Aggregate the voterfile by town
agg_voters <- df_voters %>% 
  group_by(town_name) %>% 
  summarise(vf_records = n(),
            dems = sum(party_code == 'D'),
            unaff = sum(party_code == 'U'),
            active = sum(status == 'A'),
            address_available = sum(address_number != ''),
            phone_available = sum(phone != '')) %>%
  mutate(p_dems = dems/vf_records,
         p_unaff = unaff/vf_records,
         p_active = active/vf_records,
         p_adds = address_available/vf_records,
         p_phones = phone_available/vf_records)
# Aggregation Notes: 
# Note: There are some folks with misspelled town names in the data, we drop below
# Note: There are folks with towns outside the district (though vf says they are CT03),
#       we remove them below

# Join shapefile geometries (from DataHaven's cwi package) in with the aggregated voter file
df_joined = left_join(agg_voters, town_sf, by = join_by("town_name" == "name"))

# Remove the misspelled towns and the folks who live in towns outside district, but vf says are in CT-03 
df_joined_reduced <- df_joined %>% 
  filter(town_name %in% town_sf$name) %>% # could also do an inner join
  filter(vf_records > 10) 
# Make the data an "sf" object
df_map <- st_as_sf(df_joined_reduced, sf_column_name = "geometry")

# A basic plot with ggplot
# ggplot(df_map) +
#   geom_sf(aes(fill = vf_records,  geometry = geometry), color = "white", linewidth = 0.5) +
#   geom_sf_text(aes(label = town_name), size = 3, color = "white") +
#   ggtitle("Connecticut towns")

###################################################################
## Cutting off the town boundaries at the congressional boundaries:

df_map <- st_transform(df_map, 3857) # Have to make this transformation before joining with the congressional map (needs to be planar or something)

# Source: https://catalog.data.gov/dataset/tiger-line-shapefile-2020-state-connecticut-ct-118th-congressional-district
# Read in CT Congressional Shapefile:
congressional_bound <- st_read("C:/Users/Ruairi/Documents/Coding Projects 2025/ct04_testing/tl_2020_09_cd118")
congressional_bound_003 <- congressional_bound[congressional_bound$CD118FP == '03',] # Grab CT-03 boundaries
congressional_bound_003 <- st_transform(congressional_bound_003, 3857) # Have to make this transformation before joining with town map for consistency

towns_clipped <- st_intersection(df_map, congressional_bound_003) # Makes town map (df_map) respect the congressional boundary

# Remove geographies from the places we clipped out
towns_clipped <- towns_clipped %>% 
  filter(!st_is_empty(geometry))

# Keep only polygon-like geometries
towns_clipped <- towns_clipped %>% 
  st_collection_extract("POLYGON")  # OR "MULTIPOLYGON"

######################################################################
# Use leaflet to make basic, interactive html maps with voterfile data

# leaflet prefers crs = 4326, so we use a function to convert 
df_map_wgs <- st_transform(towns_clipped, crs = 4326)  # 4326 = WGS84 lat/lon

# Create some color palettes for each variable of interest  
pal_total_records <- colorNumeric("Reds", df_map$vf_records, na.color = "transparent")
pal_p_dems <- colorNumeric("Blues", df_map$p_dems, na.color = "transparent")
pal_p_unaff <- colorNumeric("Purples", df_map$p_unaff, na.color = "transparent")
pal_p_active <- colorNumeric("Reds", df_map$p_active, na.color = "transparent")
pal_p_adds <- colorNumeric("Reds", df_map$p_adds, na.color = "transparent")
pal_p_phones <- colorNumeric("Reds", df_map$p_phones, na.color = "transparent")

town_centroids <- st_centroid(df_map_wgs)


# Build the base map
m <- leaflet(df_map_wgs) %>%
  addProviderTiles("CartoDB.Positron") %>%
  
  # First layer: Number of VF Records
  addPolygons(
    fillColor = ~pal_total_records(vf_records),
    color = "white", weight = 1, fillOpacity = 0.7,
    group = "Number of VF Records",
    popup = ~paste0("<b>", town_name, "</b><br>",
                    "n: ",vf_records)
  ) %>%
  
  # Second layer: Proportion Democrats
  addPolygons(
    fillColor = ~pal_p_dems(p_dems),
    color = "white", weight = 1, fillOpacity = 0.7,
    group = "Proportion Democrats",
    popup = ~paste0("<b>", town_name, "</b><br>",
                    "p Dems: ", round(100*p_dems,1), "%")
  ) %>%
  
  # Third layer: Proportion Unaffiliated
  addPolygons(
    fillColor = ~pal_p_unaff(p_unaff),
    color = "white", weight = 1, fillOpacity = 0.7,
    group = "Proportion Unaffiliated",
    popup = ~paste0("<b>", town_name, "</b><br>",
                    "p Unaff.: ", round(100*p_unaff,1), "%")
  ) %>%
  
  # Fourth layer: Proportion w/ Phone Number in VF
  addPolygons(
    fillColor = ~pal_p_phones(p_phones),
    color = "white", weight = 1, fillOpacity = 0.7,
    group = "Proportion w/ Phone Number in VF",
    popup = ~paste0("<b>", town_name, "</b><br>",
                    "p Valid Phone #: ", round(100*p_phones,1), "%")
  ) %>%
  
  # Fourth layer: Proportion Active
  addPolygons(
    fillColor = ~pal_p_active(p_active),
    color = "white", weight = 1, fillOpacity = 0.7,
    group = "Proportion Active Voters",
    popup = ~paste0("<b>", town_name, "</b><br>",
                    "p Active Voters: ", round(100*p_active,1), "%")
  ) %>%
  
  # Add layer toggle control
  addLayersControl(
    baseGroups = c("Number of Records", "Proportion Democrats", "Proportion Unaffiliated", "Proportion w/ Phone Number in VF", 'Proportion Active Voters' ),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%

  # Add legends for each layer
  addLegend(
    "bottomright", pal = pal_total_records, values = ~vf_records,
    title = "Number of VF Records", group = "Number of VF Records"
  ) %>%
  addLegend(
    "bottomright", pal = pal_p_dems, values = ~p_dems,
    title = "Proportion Democrats", group = "Proportion Democrats"
  ) %>%
  addLegend(
    "bottomright", pal = pal_p_unaff, values = ~p_unaff,
    title = "Proportion Unaffiliated", group = "Proportion Unaffiliated"
  ) #%>%
  # 
  # addLabelOnlyMarkers(
  #   data = town_centroids,
  #   label = ~town_name,
  #   labelOptions = labelOptions(
  #     noHide = TRUE,          # always show
  #     direction = 'center',   # center text
  #     textOnly = TRUE,        # no marker icon
  #     style = list(
  #       "color" = "darkblue",
  #       "font-size" = "12px",
  #       "font-weight" = "bold",
  #       "text-shadow" = "1px 1px 3px white"
  #     )
  #   )
  # )
 

# Save as self-contained HTML
saveWidget(m, "ct03_town_map_clipped.html", selfcontained = TRUE)
