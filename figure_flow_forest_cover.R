#===============================================================================

# analyzing exporting pattern regarding the amount of forest cover 
# at the municipality

# here we consider a 0.2 threshold, which is similar to the biodiversity
#threshold and the legal requirement!

#===============================================================================


#==== pacotes ==================================================================

# Load package

library(dplyr)
library(ggalluvial)
library(ggpubr)
library(tidyr)
library(ggforce)

#===============================================================================


trase <- read.csv("exports_codigo_ibg.csv",row.names = 1)
names(trase)
# agregate by mu, year and exporter

trase_agg <- trase %>% group_by(MUNICIPALITY,YEAR,COUNTRY,codigo_ibg)%>%
  summarise(production_t=sum(production))

# adding fc data

fc <- read.csv("fc_export.csv")

fc <- unique(fc[,c(4,6,7)])

trase_fc <- left_join(trase_agg,fc,by=c("codigo_ibg"="code_mn",
                                               "YEAR"="YEAR"))

# clean nas from fc

trase_fc <- trase_fc%>% drop_na(fc_m)

# categorical forest cover

trase_fc$fc_cat <- cut(trase_fc$fc_m,breaks = c(-1,0.2,1.1))

summary(trase_fc$fc_cat)

# plot 2015 data

trase_2015 <- filter(trase_fc,YEAR==2015)

# aggregating data

summary(trase_2015$production_t)


trase_2015$COUNTRY_agg <- NA

trase_2015$COUNTRY_agg[trase_2015$production_t>=2*(mean(trase_2015$production_t))] <- 
    trase_2015$COUNTRY[trase_2015$production_t>=2*mean(trase_2015$production_t)]

trase_2015$COUNTRY_agg[trase_2015$production_t<2*mean(trase_2015$production_t)] <- 
  "OTHERS"

trase_2015$mun_agg <- NA
trase_2015$mun_agg[trase_2015$COUNTRY_agg=="OTHERS"] <- "OHTERS"
trase_2015$mun_agg[trase_2015$COUNTRY_agg!="OTHERS"] <- trase_2015$MUNICIPALITY[trase_2015$COUNTRY_agg!="OTHERS"]

levels(as.factor(trase_2015$COUNTRY_agg))

levels(as.factor(trase_2015$mun_agg))
 


# set data right format

trase_2015_rs <- gather_set_data(trase_2015, c(8,9))


trase_2015_rs$x <- factor(trase_2015_rs$x,levels = c("mun_agg","COUNTRY_agg"))


ggplot(trase_2015_rs, aes(x, id = id, split = y, value = production_t)) +
  geom_parallel_sets(aes(fill = fc_cat), alpha = 0.3, axis.width = 0.3)+
  #theme_void()
  geom_parallel_sets_axes(axis.width = 0.3)+
  geom_parallel_sets_labels(colour = 'darkgray',angle =0, size = 2)+
  theme_void()

