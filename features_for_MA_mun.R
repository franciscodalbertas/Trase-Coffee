#===============================================================================

# creates a multipolygon shapefile with Brazilian municipalities whitin the
# AF

#===============================================================================

#==== pacotes ==================================================================

library(geobr)
library(dplyr)
library(readr)
library(sf)

#===============================================================================

# get code of target municipalities


ATF_cafe <- read_csv("ATF_cafe.csv", locale = locale(encoding = "WINDOWS-1252"))

codes <- as.integer(ATF_cafe$codigo_ibg)

# Read l municipalities at a given year

mun <- read_municipality(code_muni="all", year=2018) %>%
  filter(code_muni %in% codes)

# saving as shapefile

st_write(mun, "mun_AF.shp")

# OBS: use this shapefile to upload to GEE

