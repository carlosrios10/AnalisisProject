library(igraph)
library(plyr)
### Red Social de Amistad
redSocial<-read.csv("NYC-user-relations.csv",header = F,sep = ",")
str(redSocial)
### Codigo para unificar las edge y crear un grafo para el codigo java
grafoTotal<-graph.data.frame(redSocial, directed=T, vertices=NULL)
grafoTotal<- as.undirected(grafoTotal,mode ="collapse")
is.simple(grafoTotal)
grafoTotal <- simplify(grafoTotal,remove.multiple = TRUE, remove.loops = TRUE)
grafoTotal<- as.directed(grafoTotal)
is.directed(grafoTotal)
df<- as_data_frame(grafoTotal)
str(df)
write.table(df,file = 'NY-grafo.csv',sep = ',',row.names = FALSE, col.names = T,quote = FALSE)
#write.graph(grafoTotal,file="datasets/foursquare/datasets_csv/grafoTotal.graphml",format="graphml")

### Codigo para el Analisis
red_social <- read.csv('NY-grafo.csv', header = T,sep = ",")
grafoTotal<-graph.data.frame(red_social, directed=T, vertices=NULL)
grafoTotal<- as.undirected(grafoTotal,mode ="collapse")
is.simple(grafoTotal)

### Creo un grafo solo con aquellos usuarios que por los menos tienen un tips
tipsVenueUserAll <- read.csv("NYC-TipsVenueUserAll.csv",sep = '\t',header = T,colClasses = 'character')
tipsVenueUserAll$isUserNY<- grepl(pattern = "NY|NEW YORK|NYC",x = tipsVenueUserAll$homeCity)
str(tipsVenueUserAll)
length(unique(tipsVenueUserAll$user))
users_tips <- unique(tipsVenueUserAll$user)
nodes_names <- vertex_attr(grafoTotal)$name
length(nodes_names)
valid_names <- intersect(users_tips,nodes_names)

## grafo con usuarios con tips
grafoUserTips <- induced.subgraph(grafoTotal,vids = valid_names)
max(degree(grafoUserTips))
min(degree(grafoUserTips))
grafoUserTips<- as.directed(grafoUserTips)
is.directed(grafoUserTips)
is.simple(grafoUserTips)
grafoUserTips_df<- as_data_frame(grafoUserTips)
str(grafoUserTips_df)
write.table(grafoUserTips_df,file = 'grafoUserTips_df.csv',sep = ',',row.names = FALSE, col.names = T,quote = FALSE)
grafoTotal<-graph.data.frame(redSocial, directed=T, vertices=NULL)

grafoUserTips<-graph.data.frame(grafoUserTips_df, directed=T, vertices=NULL)
grafoUserTips<- as.undirected(grafoUserTips,mode ="collapse")
is.directed(grafoUserTips)
is.simple(grafoUserTips)
length(unique(grafoUserTips_df$to))

neighborhood.size(grafoUserTips,order = 1,nodes='1661',mode = 'all')
neighborhood.size(grafoUserTips,order = 2,nodes='1661',mode = 'all')




length(valid_names)
d<-distances(grafoUserTips,v = valid_names[1],to = valid_names[1])
dim(d)
d[1,1]
valid_names[1:10]
for(i in 1:length(valid_names)){
    
}


sink("grafo_distancias_user_user.csv")
for(i in 1:length(valid_names)){
    for(j in (i+1):length(valid_names)){
    d<-distances(grafoUserTips,v = valid_names[i],to = valid_names[j])
    usuarioDist<-paste(valid_names[i],valid_names[j],d[1,1],sep = "\t")
    cat(usuarioDist)
    cat("\n")
    }
}
sink()

tips_con_user_grafo <- tipsVenueUserAll[tipsVenueUserAll$user %in% valid_names,]
str(tips_con_user_grafo)
head(tips_con_user_grafo)
length(unique(tips_con_user_grafo$user))
sum(tipsVenueUserAll$user %in% valid_names)
table(tips_con_user_grafo$isUserNY)
count_tips_con_user_grafo<- count(tips_con_user_grafo,c('user','numVenueId'))
head(count_tips_con_user_grafo)
count_tips_con_user_grafo$rating <- 0
count_tips_con_user_grafo$rating[count_tips_con_user_grafo$freq==1]<-2
count_tips_con_user_grafo$rating[count_tips_con_user_grafo$freq==2]<-3
count_tips_con_user_grafo$rating[count_tips_con_user_grafo$freq==3]<-4
count_tips_con_user_grafo$rating[count_tips_con_user_grafo$freq>=4]<-5
write.table(x = count_tips_con_user_grafo[c("user","numVenueId","rating")],file ="count_tips_con_user_grafo.csv",col.names = F,quote = F,row.names = F,sep=",")




## user NY
tips_con_user_grafo_ny <- tips_con_user_grafo[tips_con_user_grafo$isUserNY,]
table(tips_con_user_grafo_ny$isUserNY)
count_tips_con_user_grafo_ny<- count(tips_con_user_grafo_ny,c('user','numVenueId'))
head(count_tips_con_user_grafo_ny)
count_tips_con_user_grafo_ny$rating <- 0
count_tips_con_user_grafo_ny$rating[count_tips_con_user_grafo_ny$freq==1]<-2
count_tips_con_user_grafo_ny$rating[count_tips_con_user_grafo_ny$freq==2]<-3
count_tips_con_user_grafo_ny$rating[count_tips_con_user_grafo_ny$freq==3]<-4
count_tips_con_user_grafo_ny$rating[count_tips_con_user_grafo_ny$freq>=4]<-5
write.table(x = count_tips_con_user_grafo_ny[c("user","numVenueId","rating")],file ="count_tips_con_user_grafo_ny.csv",col.names = F,quote = F,row.names = F,sep=",")



### grafo con usuarios ny y tips
users_tips_ny <- unique(tips_con_user_grafo_ny$user)
grafoUserTips_ny <- induced.subgraph(grafoUserTips,vids = users_tips_ny)
is.directed(grafoUserTips_ny)
grafoUserTips_ny_df<- as_data_frame(grafoUserTips_ny)
str(grafoUserTips_ny_df)
write.table(grafoUserTips_ny_df,file = 'grafoUserTips_ny_df.csv',sep = ',',row.names = FALSE, col.names = T,quote = FALSE)


##################################################
#################################################

################GRAFO TOTAL
max(degree(grafoTotal))
min(degree(grafoTotal))
plot(degree_distribution(grafoTotal))

neighborhood.size(grafoTotal,order = 1,nodes='1661',mode = 'all')
neighborhood.size(grafoTotal,order = 2,nodes='1661',mode = 'all')
neighborhood.size(grafoTotal,order = 3,nodes='1661',mode = 'all')
neighborhood.size(grafoTotal,order = 4,nodes='1661',mode = 'all')
neighborhood.size(grafoTotal,order = 5,nodes='1661',mode = 'all')

##############GRAFO CON USUARIO Y TIPS
grafoUserTips <- as.undirected(grafoUserTips)
is.directed(grafoUserTips)
is.conect(grafoUserTips)
max(degree(grafoUserTips))
min(degree(grafoUserTips))
plot(degree_distribution(grafoUserTips))

neighborhood.size(grafoUserTips,order = 1,nodes='1661',mode = 'all')
neighborhood.size(grafoUserTips,order = 2,nodes='1661',mode = 'all')
neighborhood.size(grafoUserTips,order = 3,nodes='1661',mode = 'all')
neighborhood.size(grafoUserTips,order = 4,nodes='1661',mode = 'all')
neighborhood.size(grafoUserTips,order = 5,nodes='2715580',mode = 'all')

count_tips_con_

which(degree(grafoUserTips)==1)
neighborhood.size(grafoUserTips,order = 4,nodes='5809320',mode = 'all')


###############GRAFO CON USUARIOS Y TIPS DE NY
grafoUserTips_ny <- as.undirected(grafoUserTips_ny)
is.directed(grafoUserTips_ny)
max(degree(grafoUserTips_ny))
min(degree(grafoUserTips_ny))
plot(degree_distribution(grafoUserTips_ny))

neighborhood.size(grafoUserTips_ny,order = 1,nodes='1661',mode = 'all')
neighborhood.size(grafoUserTips_ny,order = 2,nodes='1661',mode = 'all')
neighborhood.size(grafoUserTips_ny,order = 3,nodes='1661',mode = 'all')
neighborhood.size(grafoUserTips_ny,order = 4,nodes='1661',mode = 'all')
neighborhood.size(grafoUserTips_ny,order = 5,nodes='1661',mode = 'all')

which.min(degree(grafoUserTips_ny))
neighborhood.size(grafoUserTips_ny,order = 2,nodes='2961976',mode = 'all')

vecinos <- names(neighborhood(grafoUserTips_ny,order = 1,nodes = '1661',mode = 'all')[[1]])
plot.igraph(induced.subgraph(grafoUserTips_ny,vecinos))

## comunidades
comunidades <- cluster_walktrap(grafoTotal)

34979 - c(34784,34711,34528,34333,33986)
34979 - c(34775,34691,34496,34314,34048)
