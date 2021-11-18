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

# essa tabela ta errada! tem mtas combinacoes de estado com mun. precisa
# corrigir e gerar ela de novo

# precisa de um id unico excluindo estado acho! vou ignorar por enquanto
# e gerar figuras como se tivesse certo!

trase <- read.csv("exports_codigo_ibg.csv",row.names = 1)

teste <- trase[trase$MUNICIPALITY=="ABATIA",]

teste2 <- teste[duplicated(teste$production),]

# adding fc data

fc <- read.csv("fc_export.csv")

fc <- unique(fc[,c(4,6,7)])

trase_fc <- left_join(trase,fc,by=c("codigo_ibg"="code_mn",
                                               "YEAR"="YEAR"))

# excluir NA apenas enquanto o dado esta errado

trase_fc <- trase_fc%>% drop_na(fc_m)

# categorical forest cover

trase_fc$fc_cat <- cut(trase_fc$fc_m,breaks = c(-1,0.2,1.1))

# plot 2015 data

trase_2015 <- filter(trase_fc,YEAR==2015)

trase_2015 <- trase_2015[,c(1,12,16,19)]

# set data right format

trase_2015_rs <- gather_set_data(trase_2015, c(4,2))

str(trase_2015_rs$COUNTRY)

ggplot(trase_2015_rs, aes(x, id = id, split = y, value = production)) +
  geom_parallel_sets(aes(fill = fc_cat), alpha = 0.3, axis.width = 0.3)+
  #theme_void()
  geom_parallel_sets_axes(axis.width = 0.3)+
  geom_parallel_sets_labels(colour = 'darkgray',angle =0, size = 2)+
  theme_void()

