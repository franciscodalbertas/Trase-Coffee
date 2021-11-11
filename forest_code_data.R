#===============================================================================

# aggregates forest deficit per municipallity

#===============================================================================

#==== packages =================================================================

library(sf)
library(dplyr)
#library(gdalUtils)
library(foreign)
library(stringr)
#===============================================================================

# municipallities

mun <- st_read("mun_AF.shp")
st_geometry(mun) <- NULL



# great to open large datasets:
# https://jayrobwilliams.com/posts/2020/09/spatial-sql

# open first row only to check the data

spatial_car1 <- read_sf(file.path(getwd(),"done_fa_proc06_final_202108041632_shp"),
  query = "SELECT * FROM done_fa_proc06_final_202108041632_shp WHERE FID = 1")


shape_name <- "done_fa_proc06_final_202108041632_shp"

## build SQL query to open only target municipallities

query_str <- str_c('SELECT * FROM done_fa_proc06_final_202108041632_shp WHERE cd_mun =',
                   mun$code_mn[1],' OR ',
                   str_c('cd_mun =',mun$code_mn[2:854],collapse = " OR "))

start_time <- Sys.time()

# open shapefile restriced by the query (very slow)
spatial_car2 <- read_sf(file.path(getwd(),"done_fa_proc06_final_202108041632_shp"),
  query = query_str)


st_geometry(spatial_car2) <- NULL

end_time <- Sys.time()

time <- end_time - start_time

# saving data

# write.csv(spatial_car2,"car_targeted_mun.csv",row.names = F)

#===============================================================================

# alternatively subseting data on arcgis or qgis is faster 

#===============================================================================

# read the subset
# check if takes too long to read it!

spatial_car2 <- st_read(file.path("CAR_mun_coffee"))

# excluding spatial data

st_geometry(spatial_car2) <- NULL


spatial_car2 <- spatial_car2[spatial_car2$cd_mun %in% mun$code_mn,]

# saving table

write.csv(spatial_car2,"car_targeted_mun.csv",row.names = F)

# aggregating data by municipallity

car_agg <- spatial_car2%>%
  group_by(cd_mun)%>%
  summarise(app_deficit_t=sum(app_defici),lr_deficit=sum(lr_deficit),
            lr_deficit_1=sum(lr_defic_1))


length(unique(car_agg$cd_mun)) # tem 840 municipios


write.csv(x = car_agg,file = "legal_forest_debt.csv",row.names = F)


