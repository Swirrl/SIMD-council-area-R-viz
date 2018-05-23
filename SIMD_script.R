# Load required packages
library(SPARQL)
library(dplyr)
library(leaflet)
library(rgdal)

# SPARQL query to retrieve data zones and SIMD rank values
query <- 'PREFIX qb: <http://purl.org/linked-data/cube#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX sdmx: <http://purl.org/linked-data/sdmx/2009/concept#>
PREFIX data: <http://statistics.gov.scot/data/>
PREFIX sdmxd: <http://purl.org/linked-data/sdmx/2009/dimension#>
PREFIX mp: <http://statistics.gov.scot/def/measure-properties/>
PREFIX stat: <http://statistics.data.gov.uk/def/statistical-entity#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>

SELECT ?dataZone ?SIMDrank
WHERE {
    ?indicator qb:dataSet data:scottish-index-of-multiple-deprivation-2016;
              <http://statistics.gov.scot/def/dimension/simdDomain> <http://statistics.gov.scot/def/concept/simd-domain/simd>;
              mp:rank ?SIMDrank;
              sdmxd:refPeriod <http://reference.data.gov.uk/id/year/2016> ;
              sdmxd:refArea ?area.
    ?area rdfs:label ?dataZone.
}'


# SPARQL endpoint to retrive the data
endpoint <- "http://statistics.gov.scot/sparql"

# Assign output of SPARQL query to 'qddata'
qddata <- SPARQL(endpoint, query)

# Assign results of SPARQL query to data frame 'SIMDrank'
SIMDrank <- qddata$results

# SPARQL query to retrieve data zones, council areas and council area codes
query2 <- 'PREFIX qb: <http://purl.org/linked-data/cube#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX sdmx: <http://purl.org/linked-data/sdmx/2009/concept#>
PREFIX data: <http://statistics.gov.scot/data/>
PREFIX sdmxd: <http://purl.org/linked-data/sdmx/2009/dimension#>
PREFIX mp: <http://statistics.gov.scot/def/measure-properties/>
PREFIX stat: <http://statistics.data.gov.uk/def/statistical-entity#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>

SELECT ?dataZone ?councilArea ?councilAreaCode 
WHERE {
    ?dz <http://statistics.gov.scot/def/hierarchy/best-fit#council-area> ?ca.
    ?ca rdfs:label ?councilArea.
    ?ca <http://publishmydata.com/def/ontology/foi/code> ?councilAreaCode. 
    ?dz rdfs:label ?dataZone.
}'


# Assign output of SPARQL query to 'qddata2'
qddata2 <- SPARQL(endpoint, query2)

# Assign results of SPARQL query to data frame 'geo_lkp'
geo_lkp <- qddata2$results

# Join the 2 data frames to link SIMD to council areas
SIMD_dz_ca <- inner_join(SIMDrank, geo_lkp, by="dataZone")

# Calculate mean SIMD rank per council area
SIMD_ca_mean <- SIMD_dz_ca %>% 
  group_by(councilAreaCode, councilArea) %>% 
  summarise(meanSIMDrank=mean(SIMDrank))

# Download Local Authority District (LAD) shapefiles to working directory
download.file("https://opendata.arcgis.com/datasets/fab4feab211c4899b602ecfbfbc420a3_4.zip?outSR=%7B%22wkid%22%3A27700%2C%22latestWkid%22%3A27700%7D",
              destfile="LAD.zip")

# Unzip the downloaded shapefiles
unzip("LAD.zip")

# Load shapefile into R as spatial polygons data frame
boundary <- readOGR(dsn=getwd(), layer="Local_Authority_Districts_December_2017_Ultra_Generalised_Clipped_Boundaries_in_United_Kingdom_WGS84")

# Merge spatial polygons data frame with data frame containing mean SIMD rank
merged <- merge(boundary, SIMD_ca_mean, by.x = "lad17nm", 
                by.y = "councilArea", all.x = FALSE)

# Project data to WGS84 using spTransform
merged_ll <- spTransform(merged, 
                         CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

# Create bins and palette for mean SIMD rank
bins <- c(2000, 2500, 3000, 3500, 4000, 4500, 5000, Inf)
pal <- colorBin("YlOrRd", domain = merged_ll$meanSIMDrank, bins = bins)

# Plot mean SIMD rank for each council area
map <- leaflet(merged_ll) %>% 
  addProviderTiles("CartoDB.Positron", 
                   options= providerTileOptions(opacity = 0.99)) %>% 
  addPolygons(fillColor = ~pal(meanSIMDrank),
              weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7,
              highlight = highlightOptions(
                weight = 2,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE),
              label=~paste(merged_ll$lad17nm, 
                           round(merged_ll$meanSIMDrank)),
              labelOptions = labelOptions(textsize = "15px",
                                          direction = "auto"))

# Add a legend to the map
mapLegend <- map %>% 
  addLegend(pal = pal, 
            values = ~meanSIMDrank, 
            opacity = 0.7, 
            title = "Mean SIMD rank",
            position = "bottomright")

# View the map
mapLegend


