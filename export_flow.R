#===============================================================================

# exportation flows from Brazil to importer countries

#===============================================================================

#==== pacotes ==================================================================

# Load package

library(dplyr)
library(ggalluvial)
library(ggpubr)

#===============================================================================


trase <- read.csv("exports_codigo_ibg.csv")

# os nos tem q ser os source e target tb! nao tudo igual!
# source e target tem q ser inteiros!!

# e os valores precisam bater, acho q os valores tem q ser unicos, nao repetidos!

# aggregate data by State

trase_agg <- trase %>% group_by(STATE,COUNTRY,TYPE)%>%
  summarise(total_land=sum(land_use_ha),total_prod=sum(production))


#===============================================================================

# limitando ano

trase_agg <- trase %>% group_by(STATE,COUNTRY,TYPE,YEAR)%>%
  summarise(total_land=sum(land_use_ha),total_prod=sum(production))


trase_agg <- trase_agg[trase_agg$YEAR==2015,]  


# https://cran.r-project.org/web/packages/ggalluvial/vignettes/ggalluvial.html


#tipo de grafico 1

ggplot(trase_agg,
       aes(y = total_prod, axis1 = STATE, axis2 = COUNTRY)) +
  geom_alluvium(aes(fill = STATE), width = 1/10) +
  geom_stratum(width = 1/12, fill = "black", color = "grey") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
  #scale_x_discrete(limits = c("Gender", "Dept"), expand = c(.05, .05)) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  #ggtitle("UC Berkeley admissions and rejections, by sex and department")
  theme_void()


# ex 2

library(ggforce)

data <- reshape2::melt(Titanic)
data <- gather_set_data(data, 1:4)

ggplot(data, aes(x, id = id, split = y, value = value)) +
  geom_parallel_sets(aes(fill = Sex), alpha = 0.3, axis.width = 0.1) +
  geom_parallel_sets_axes(axis.width = 0.1) +
  geom_parallel_sets_labels(colour = 'white')


ggplot(trase_agg, aes(x, id = id, split = y, value = total_prod)) +
  geom_parallel_sets(aes(fill = STATE), alpha = 0.3, axis.width = 0.1)

# funcao  pra organizar dados!
trase_agg2 <- gather_set_data(trase_agg, c(1,2))

trase_agg2$x <- factor(trase_agg2$x,levels = c("STATE","COUNTRY"))


x <- ggplot(trase_agg2, aes(x, id = id, split = y, value = total_prod)) +
  geom_parallel_sets(aes(fill = STATE), alpha = 0.3, axis.width = 0.3)+
  #theme_void()
  geom_parallel_sets_axes(axis.width = 0.3)+
  geom_parallel_sets_labels(colour = 'darkgray',angle =0, size = 2)+
  theme_void()

ggsave(filename = "flow_chart.jpg",plot = x,width = 20,height = 30,units = "cm")
