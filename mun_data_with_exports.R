#===============================================================================

# generating data frame with muncipalities within the AF that export or not
# coffee

#===============================================================================

#==== packages ================================================================

library(dplyr)
library(tidyr)

#===============================================================================

# forest cover data
veg <- read.csv("mun_forest_cover.csv")

veg <- veg[,1:8] # discard spatial info

# trase data
export <- read.csv("exports_codigo_ibg.csv")


# aggregating data per mun

exp_ag <- export %>% 
  group_by(codigo_ibg,YEAR,BIOME) %>%
  summarise(total_land=sum(land_use_ha),total_prod=sum(production)) %>%
  filter(BIOME=='MATA ATLANTICA')


# first change data format

names(veg)[3:5] <- paste0(seq(2015,2017,1))


veg_long <- veg %>% pivot_longer(cols =c(3:5),names_to = "YEAR",values_to = "fc_m" )


str(veg_long)

veg_long$YEAR <- as.integer(veg_long$YEAR)

# combining the data

exp_veg <- left_join(veg_long,exp_ag,by= c("code_mn"="codigo_ibg","YEAR"))


exp_veg$exporter[is.na(exp_veg$total_land)] <- "non-exporter"

exp_veg$exporter[!is.na(exp_veg$total_land)] <- "exporter"


# renaming columns and fixing biome info.

exp_veg$BIOME <- "Atlantic-Forest"

names(exp_veg)[9:10] <- paste(names(exp_veg)[9:10],"_",'trase')


# exporting data

write.csv(exp_veg,"fc_export.csv",row.names = F)
