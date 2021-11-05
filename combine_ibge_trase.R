#===============================================================================

# combining ibge production data for coffee with trase data

#===============================================================================

#==== pacotes ==================================================================

library(dplyr)
library(readr)

#===============================================================================

# target mun 

df <-read_csv("fc_export.csv")

# # change names
# 
# names(df)[3:5] <- paste0(seq(2015,2017,1))
# 
# 
# df_long <- df %>% pivot_longer(cols =c(3:5),names_to = "year",values_to = "p_veg" )

# changing to long


# ibge data

prod <- read.csv("coffee_production_15_17.csv")

prod_ara <- prod[prod$cd_crop==31619,]
prod_can <- prod[prod$cd_crop==31620,]

ibge_trase <- left_join(prod,df,by=c("cd_mun"="code_mn","year"="YEAR"))

# cleaning df with columns of interest

nm <- names(ibge_trase)[c(1:10,12:19)]

ibge_trase <- ibge_trase %>% select(nm)

write.csv(ibge_trase,"exportation_combined_production_IBGE.csv",row.names = F)
