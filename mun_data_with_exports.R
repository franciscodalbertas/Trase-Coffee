#===============================================================================

# generating data frame with muncipalities within the AF that export or not
# coffee

#===============================================================================

#==== packages ================================================================

library(dplyr)

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


# combining the data


exp_veg <- left_join(veg,exp_ag,by= c("code_mn"="codigo_ibg"))

exp_veg$exporter[is.na(exp_veg$total_land)] <- "non-exporter"

exp_veg$exporter[!is.na(exp_veg$total_land)] <- "exporter"


# renaming columns and fixing biome info.

exp_veg$BIOME <- "Atlantic-Forest"

names(exp_veg)[11:12] <- paste(names(exp_veg)[11:12],"_",'trase')


# exporting data

write.csv(exp_veg,"fc_export.csv",row.names = F)
