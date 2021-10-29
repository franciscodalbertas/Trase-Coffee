#===============================================================================

# acessing coffee production data

#===============================================================================

#==== packages =================================================================

library(sidrar)
library(dplyr)
library(stats)
require(tidyr)
library(reshape2)

#===============================================================================

# target mun

df <- read.csv("fc_export.csv")

# filters for ibge datasets

permanente <- 1613
periodo <- c('2015','2016','2017')
nivel_territorial <- "City"

# split data into small groups( sidra limits donwload size to 50.000 entries)


num_groups = 50

list_df <- df %>% 
  group_by((row_number()-1) %/% (n()/num_groups)) %>%
  nest %>% pull(data)

#listing mun code

mun_code <- lapply(list_df,function(x)as.character(unique(x$code_mn)))


f <- function(x)get_sidra(x = permanente,period = periodo,geo=nivel_territorial,
               geo.filter = list(x))

# looping get_sidr through the list

agri_data <- lapply(mun_code,f)


# combining the data again

agri <- as.data.frame(do.call(rbind,agri_data))

# eliminando duplicatas!
agri <- agri[!duplicated(agri),]

# saving full data


write.csv(agri,"permanent_agri_prod_IBGE_15_17.csv",row.names = F)


# filtering coffee info.

# 31619 - Café (em grão) Arábica
# 31620 - Café (em grão) Canephora
str(agri$`Produto das lavouras permanentes (Código)`)

names(agri)[12] <- "cd_crop"

coffee <-filter(agri,cd_crop == 31619|cd_crop==31620)


# converting to wide data

names(coffee)[c(6:7,9,11)] <- c("cd_mun","mun","year","variable")


coffee_wide <-  coffee%>% pivot_wider(id_cols =c(6,7,9,12,13) ,
                          names_from =variable,values_from = Valor)



coffee_wide[, 6:10][is.na(coffee_wide[, 6:10])] <- 0


write.csv(coffee_wide,"coffee_production_15_17,csv",row.names = F)
