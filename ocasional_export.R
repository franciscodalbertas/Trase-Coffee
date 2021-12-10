### Trase Export Cafe Paper ###
## Cria variável categorica (3) para Export Ocasional ##
# 10 Dezembro 2021 #

#########
#Data
#########
setwd("/Users/Patricia/Documents/parcerias/Trade_cafe_florestas")
ethi<-read.csv("exportation_combined_production_IBGE.csv")
head(ethi)

###################
#Previous edition
###################

###Transformando os NA (= muncp que nao exportaram) em 0 (zeros):
ethi$total_land._.trase[is.na(ethi$total_land._.trase)]<-0
ethi$total_prod._.trase [is.na(ethi$total_prod._.trase )]<-0
names(ethi)

##Separating data from IBGE to calculate total coffee produced, irrespectively of the coffee species:
ibge<-ethi[,c(1:10)]

require(doBy)
###Adding the values of each coffee species:
ibge.mean<-summaryBy(Area.colhida+Quantidade.produzida+Rendimento.medio.da.producao+Valor.da.producao~cd_mun+mun+year, FUN=sum, data=ibge)
head(ibge.mean); nrow(ibge); nrow(ibge.mean)

###Separating the data from TRASE to calculate mean value per municipality (as it was duplicated (for species):
trase<-ethi[,c(1,3,11:18)]
trase.mean<-summaryBy(total_land._.trase+total_prod._.trase~cd_mun+year+abbrv_s+cod_stt+BIOME+exporter+fc_m, FUN=mean, data=trase)
head(trase.mean); nrow(trase); nrow(trase.mean)

##Generating export dara for coffee production without coffee species information: 
ex<-merge(ibge.mean, trase.mean, by=c("cd_mun", "year"), all=T)
names(ex)

##Adding vegetation deficit information:
dvf<-read.csv("legal_forest_debt.csv", sep=",")
head(dvf)

##merging data sets
cafex<-merge(ex, dvf, by="cd_mun", all.x=T)

cafex$p_expo<-round(100*(cafex$total_prod._.trase.mean/cafex$Quantidade.produzida.sum),1)
summary(cafex$p_expo)

#Eliminating years for which coffee was not produced in any of the municipalities:
#cafex<-cafex[cafex$Quantidade.produzida.sum>0,];nrow(cafex) #2491 linhas #não vou usar este comando porque desbalancea a base

cafex$p_expo[is.na(cafex$p_expo)] <- 0

#Verify muni with exportation higher than prodcution
c1 <- cafex[cafex$p_expo > 100, ] ##NOTA: tres municipios exportaram mais do que 100% da producao 

#Exclude 3 municipalities with weird production/exportation ratios (Bom Sucesso/MG, Taparuba/MG and Timburi.SP)
cafex <- cafex[cafex$p_expo<=100,]
(length(unique(cafex$cd_mun))) # Total of 851 municipios

#############################
#Create variable 'freq_expo'
#############################

#Prepare for loop
muni <- unique(cafex$cd_mun) #get muni codes

cafex$freq_expo <- 0 #create column for freq_expo
coffex <- cafex[0, ] #create df w/ same structure

#Go loop
for(m in muni){
  df1 <- cafex[cafex$cd_mun == m, ]
  df1$freq_expo <- ifelse(sum(df1$p_expo)==0, "non-exporter",
                          ifelse(sum(df1$p_expo)!= 0 & any(df1$p_expo==0), "ocasional",
                                 ifelse(any(df1$p_expo!=0), "constant")))
  coffex <- rbind(coffex, df1)
}

#Define structure
coffex$freq_expo <- as.factor(coffex$freq_expo)
str(coffex)
coffex$freq_expo <- relevel(coffex$freq_expo, ref = "non-exporter")

##########
#Save
##########
write.csv(coffex, "coffex.csv")
save(coffex, file = "coffex.Rdata")

######### FIM #########
