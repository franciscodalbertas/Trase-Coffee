#===============================================================================

# bubble ploot gdp, lr (or fc deficit ) and quantity exported


#===============================================================================

#https://www.r-project.org/nosvn/pandoc/WDI.html


#==== pacotes ==================================================================

# Load package

library(dplyr)
library(ggalluvial)
library(ggpubr)
library(tidyr)
library(ggforce)
library(WDI)
library(viridis)
library(hrbrthemes)
#===============================================================================


trase <- read.csv("exports_codigo_ibg.csv",row.names = 1)

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

# gdp data

countries <-unique( tolower(trase_fc$COUNTRY))

dat = WDI(indicator='NY.GDP.PCAP.KD', start=2015, end=2015)

dat$country_l <- tolower(dat$country)

commom <- (countries[countries %in% dat$country_l]) # 78. 11 missing

# aggregating gdp

trase_fc$COUNTRY_l <- tolower(trase_fc$COUNTRY)

trase_gdp <- left_join(trase_fc,dat,by=c("COUNTRY_l"="country_l"))

# check ones which did not work

nw <- unique(trase_gdp$COUNTRY_l[is.na(trase_gdp$NY.GDP.PCAP.KD)])

##### missing data #############################################################

# "china (hong kong)" = iso2c = HK

# south korea = KR Korea, Rep.

# syria = SY

# china (mainland) = CN

# "venezuela" =  VE

# taiwan = no

#egypt = EG

# slovakia = SK
 
# cape verde = CV

# iran = IR

# north korea  = no

################################################################################

# ading missing data by hand (some countries do not have gdp available)

dat_f <- dat[dat$iso2c %in% c("HK","KR","EG","SK","CV","IR"),]

dat_f$country_trase <- c("cape verde","egypt","china (hong kong)","iran",
                         "south korea","slovakia")
trase_gdp2 <- trase_gdp[!is.na(trase_gdp$NY.GDP.PCAP.KD),]

trase_gdp3 <- trase_gdp[is.na(trase_gdp$NY.GDP.PCAP.KD),]

trase_gdp3 <- left_join(trase_gdp3,dat_f,
          by=c("COUNTRY_l"= "country_trase"))   

trase_gdp3 <- trase_gdp3 %>% drop_na(country_l)

trase_gdp3 <- trase_gdp3[,-c(8:11,16)]

names(trase_gdp3)[8:11] <- names(trase_gdp2)[8:11]

trase_gdp <- rbind(trase_gdp2,trase_gdp3)

names(trase_gdp)[10] <- 'GDPpercapta' 

# legal data

lr <- read.csv("legal_forest_debt.csv")

trase_lr <- left_join(trase_gdp,lr[,c(1,6)],by=c("codigo_ibg"="cd_mun"))

#===============================================================================

# ploting

# 2015

trase_2015 <- trase_lr %>% filter(YEAR==2015)

# Most basic bubble plot
trase_2015 %>%
  arrange(desc(production_t)) %>%
  mutate(country = factor(country, country)) %>%
  ggplot(aes(x=GDPpercapta, y=fc_m, size=production_t, fill=country)) +
  geom_point(alpha=0.5, shape=21, color="black") +
  scale_size(range = c(.1, 15), name="volume imported") +
  scale_fill_viridis(discrete=TRUE, guide=FALSE, option="A") +
  theme_ipsum() +
  theme(legend.position="bottom") +
  ylab("Forest cover") +
  xlab("Gdp per Capita") +
  theme(legend.position = "none")


trase_2015 %>%
  arrange(desc(GDPpercapta)) %>%
  mutate(country = factor(country, country)) %>%
  ggplot(aes(x=production_t, y=fc_m, size=GDPpercapta, fill=country)) +
  geom_point(alpha=0.5, shape=21, color="black") +
  scale_size(range = c(.1, 10), name="Population (M)") +
  scale_fill_viridis(discrete=TRUE, guide=FALSE, option="A") +
  theme_ipsum() +
  theme(legend.position="bottom") +
  ylab("Forest cover") +
  xlab("volume imported") +
  theme(legend.position = "none")

trase_2015 %>%
  arrange(desc(production_t)) %>%
  mutate(country = factor(country, country)) %>%
  ggplot(aes(x=country, y=fc_m,fill=country)) +
  geom_point(alpha=0.5, shape=21, color="black",size=2) +
  scale_size(range = c(.1, 24), name="Population (M)") +
  scale_fill_viridis(discrete=TRUE, guide=FALSE, option="A") +
  theme_ipsum() +
  theme(legend.position="bottom") +
  ylab("Forest cover") +
  xlab("Gdp per Capita") +
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#reorder origin by ascending count
trase_2015$country <- reorder(trase_2015$country, -trase_2015$GDPpercapta)


#reorder origin by ascending count

total_import <- trase_2015 %>% group_by(COUNTRY)%>%
  summarise(total_import =sum(production_t))

total_import <- total_import%>% arrange(desc(total_import))

total_import$COUNTRY <-  reorder(total_import$COUNTRY, -total_import$total_import)

total_import$rank <- seq(1:78)


trase_2015 <- left_join(trase_2015,total_import)

trase_2015$country <- reorder(trase_2015$country, trase_2015$rank)

trase_2015 %>%
  #arrange(desc(production_t)) %>%
  #mutate(country = factor(country, country)) %>%
  ggplot(aes(x=country, y=fc_m, size=production_t, fill=country)) +
  geom_point(alpha=0.5, shape=21, color="black") +
  scale_size(range = c(.1, 15), name="volume imported") +
  scale_fill_viridis(discrete=TRUE, guide=FALSE, option="A") +
  theme_ipsum() +
  theme(legend.position="bottom") +
  ylab("Forest cover") +
  xlab(" Decreasing volume imported") +
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


trase_2015 %>%
  #arrange(desc(production_t)) %>%
  #mutate(country = factor(country, country)) %>%
  ggplot(aes(x=country, y=relative_deficit, size=production_t, fill=country)) +
  geom_point(alpha=0.5, shape=21, color="black") +
  scale_size(range = c(.1, 10), name="volume imported") +
  scale_fill_viridis(discrete=TRUE, guide=FALSE, option="A") +
  theme_ipsum() +
  theme(legend.position="bottom") +
  ylab("Legal reserve deficit") +
  xlab(" Decreasing volume imported") +
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



