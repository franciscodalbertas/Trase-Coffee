#===============================================================================

# exportation flows from Brazil to importer countries

#===============================================================================

#==== pacotes ==================================================================

# Load package
library(networkD3)
library(dplyr)

#===============================================================================

#==== Example plot: ============================================================

# Load energy projection data
URL <- "https://cdn.rawgit.com/christophergandrud/networkD3/master/JSONdata/energy.json"
Energy <- jsonlite::fromJSON(URL)


Energy_nodes <- Energy[[2]]
Energy_links <- Energy[[1]]

# n levels = numero de linhas dos links

head(Energy[[1]])
head(Energy[[2]])
nrow(Energy[[1]])
nrow(Energy[[2]])# o df de source, target eh maior!

summary(Energy$links$source)
summary(Energy$links$target)


Energy$links$source <- as.character(Energy$links$source)
Energy$links$target <- as.character(Energy$links$target)

# Now we have 2 data frames: a 'links' data frame with 3 columns 
# (from, to, value), and a 'nodes' data frame that gives the name of each node.

# Thus we can plot it
sankeyNetwork(Links = Energy$links, Nodes = Energy$nodes, Source = "source",
              Target = "target", Value = "value", NodeID = "name",
              units = "TWh", fontSize = 12, nodeWidth = 30)
?networkD3

str(Energy$links)

#==== testing with trase data ==================================================

trase <- read.csv("exports_codigo_ibg.csv")

# os nos tem q ser os source e target tb! nao tudo igual!
# source e target tem q ser inteiros!!

# e os valores precisam bater, acho q os valores tem q ser unicos, nao repetidos!




# aggregate data by State

trase_agg <- trase %>% group_by(STATE,COUNTRY,TYPE)%>%
  summarise(total_land=sum(land_use_ha),total_prod=sum(production))

# tem q ser 0 indexed, ou seja comecar do 0
trase_agg$source <- as.integer(as.factor(trase_agg$STATE))
trase_agg$source <- trase_agg$source-1
max(trase_agg$source)
# tem q ser de 9 pra cima
trase_agg$target <- as.integer(as.factor(trase_agg$COUNTRY))
min(trase_agg$target)# aqui tem q somar 9 pra dar 10
trase_agg$target <- trase_agg$target+9

nos1 <- data.frame(unique(trase_agg[,1]))
nos2 <- data.frame(unique(trase_agg[,2]))

names(nos1) <- "name"
names(nos2) <- "name"

nos <-rbind(nos1,nos2)

trase_list <- list(links=as.data.frame(trase_agg[,c(6:7,5)]),nodes=nos)

head(trase_list[[1]])
head(trase_list[[2]])
str(trase_list[[1]])
str(trase_list[[2]])



sankeyNetwork(Links = trase_list$links, Nodes =trase_list$nodes, Source = "source",
              Target = "target", Value = "total_prod", NodeID = "name",
               fontSize = 14)

#===============================================================================

trase_agg <- trase %>% group_by(STATE,COUNTRY,TYPE,YEAR)%>%
  summarise(total_land=sum(land_use_ha),total_prod=sum(production))

# limitando ano
trase_agg <- trase_agg[trase_agg$YEAR==2015,]  

# tem q ser 0 indexed, ou seja comecar do 0
trase_agg$source <- as.integer(as.factor(trase_agg$STATE))
trase_agg$source <- trase_agg$source-1
max(trase_agg$source)
# tem q ser de 8 pra cima
trase_agg$target <- as.integer(as.factor(trase_agg$COUNTRY))
min(trase_agg$target)# aqui tem q somar 9 pra dar 10
trase_agg$target <- trase_agg$target+8

nos1 <- data.frame(unique(trase_agg[,1]))
nos2 <- data.frame(unique(trase_agg[,2]))

names(nos1) <- "name"
names(nos2) <- "name"

nos <-rbind(nos1,nos2)

trase_list <- list(links=as.data.frame(trase_agg[,c(7,8,6)]),nodes=nos)

head(trase_list[[1]])
head(trase_list[[2]])
str(trase_list[[1]])
str(trase_list[[2]])



p <- sankeyNetwork(Links = trase_list$links, Nodes =trase_list$nodes, Source = "source",
              Target = "target", Value = "total_prod", NodeID = "name",
              fontSize = 14,nodeWidth = 30)

# more options of packages. Esse parece melhor e mais flexivel:

# https://chart-studio.plotly.com/~alishobeiri/1591/plotly-sankey-diagrams/#/


#==== tentativa com ggplot =====================================================

# https://cran.r-project.org/web/packages/ggalluvial/vignettes/ggalluvial.html

library(ggalluvial)
library(ggpubr)
library(tidyr)
library(forcats)

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

# funcao mto boa pra organizar dados!
trase_agg2 <- gather_set_data(trase_agg, c(1,2))

trase_agg2$x <- factor(trase_agg2$x,levels = c("STATE","COUNTRY"))


x <- ggplot(trase_agg2, aes(x, id = id, split = y, value = total_prod)) +
  geom_parallel_sets(aes(fill = STATE), alpha = 0.3, axis.width = 0.3)+
  #theme_void()
  geom_parallel_sets_axes(axis.width = 0.3)+
  geom_parallel_sets_labels(colour = 'darkgray',angle =0, size = 2)+
  theme_void()

 ggsave(filename = "flow_chart.jpg",plot = x,width = 20,height = 30,units = "cm")
