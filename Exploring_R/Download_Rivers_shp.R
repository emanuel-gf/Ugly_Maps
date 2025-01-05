## Downloading shapefile rivers

# 3. GET RIVERS DATA
#-------------------
print("Get Rivers")
# https://data.hydrosheds.org/file/HydroRIVERS/HydroRIVERS_v10_sa_shp.zip

get_rivers <- function() {
  url <- "https://data.hydrosheds.org/file/HydroRIVERS/HydroRIVERS_v10_na_shp.zip"
  file_name <- "na-rivers.zip"
  
  download.file(
    url = url,
    destfile = file_name,
    mode = "wb"
  )
  
  unzip(file_name)
}

get_rivers()
print("Getting Rivers")
list.files()


## Extract from Milos @github/milos-agathon