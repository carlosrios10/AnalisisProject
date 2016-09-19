library(igraph)
library(reshape2)
usuarios_visitas <- read.csv(file ='../resultados/cantidad_visitas_comunes2.csv',sep = "\t",header = F, colClasses = c('character','character','character'))
usuarios_ny = read.csv(file ='users_tips_ny_grafo.csv',sep = ",",header = T)
names(usuarios_visitas) <- c('user_id1','user_id2','visitas_comunes')
head(usuarios_visitas)
tail(usuarios_visitas)
str(usuarios_visitas)
usuarios_visitas$visitas_comunes <- as.numeric(usuarios_visitas$visitas_comunes)
hist(usuarios_visitas$visitas_comunes)
head(usuarios_ny)
str(usuarios_ny)
### Codigo para unificar las edge y crear un grafo para el codigo java
grafo_usuarios_visitas<-graph.data.frame(usuarios_visitas[,c('user_id1','user_id2')], directed=F, vertices=NULL)
is.directed(grafo_usuarios_visitas)
is.simple(grafo_usuarios_visitas)
grafo_usuarios_visitas <- as.directed(grafo_usuarios_visitas)
df_grafo_usuarios_visitas<- as_data_frame(grafo_usuarios_visitas)
write.table(df_grafo_usuarios_visitas,file = 'grafo_usuario_relacion_visitas.csv',sep = ',',row.names = FALSE, col.names = T,quote = FALSE)

################### Grafo donde la relacion esta dada si hay concidencia en por lo menos dos lugares
grafo_usuarios_visitas_mas2<-graph.data.frame(usuarios_visitas[usuarios_visitas$visitas_comunes>1,c('user_id1','user_id2')], directed=F, vertices=NULL)
is.directed(grafo_usuarios_visitas_mas2)
is.simple(grafo_usuarios_visitas_mas2)
grafo_usuarios_visitas_mas2 <- as.directed(grafo_usuarios_visitas_mas2)
df_grafo_usuarios_visitas_mas2<- as_data_frame(grafo_usuarios_visitas_mas2)
write.table(df_grafo_usuarios_visitas_mas2,file = 'grafo_usuario_relacion_visitas_mas2.csv',sep = ',',row.names = FALSE, col.names = T,quote = FALSE)

grafo_vistas = read.csv(file='grafo_usuario_relacion_visitas_mas2.csv',header = T)
head(grafo_vistas)
str(grafo_vistas)
grafo_social = read.csv(file='grafoUserTips_df.csv',header = T)
str(grafo_social)
################################# Descripcion del grafo
E(grafo_usuarios_visitas)       # The edges of the "net" object
V(grafo_usuarios_visitas)       # The vertices of the "net" object
E(grafo_usuarios_visitas)$type  # Edge attribute "type"
E(grafo_usuarios_visitas)$media # Vertex attribute "media"
E(grafo_usuarios_visitas)$weight <- usuarios_visitas[,'visitas_comunes']

# You can also manipulate the network matrix directly:
grafo_usuarios_visitas[1,]
grafo_usuarios_visitas[5,7]

# Vecindarios de un usuario
neighborhood.size(grafo_usuarios_visitas_mas2,order = 5,nodes='2657')
vecinos_2657 <- names(neighborhood(grafo_usuarios_visitas,order = 1,nodes='2657')[[1]])
grafo_vecinos_2657 <- induced.subgraph(grafo_usuarios_visitas,vecinos_2657)



l <- layout.fruchterman.reingold(grafo_vecinos_2657, repulserad=vcount(grafo_vecinos_2657)^3, 
                                 area=vcount(grafo_vecinos_2657)^2.4)
par(mfrow=c(1,2),  mar=c(0,0,0,0)) # plot two figures - 1 row, 2 columns
plot(grafo_vecinos_2657, layout=layout.fruchterman.reingold,edge.width=E(grafo_vecinos_2657)$weight)
plot(grafo_vecinos_2657, layout=l,edge.width=E(grafo_vecinos_2657)$weight)
dev.off()

l <- layout.fruchterman.reingold(grafo_vecinos_2657)
l <- layout.norm(l, ymin=-1, ymax=1, xmin=-1, xmax=1)
plot(grafo_vecinos_2657,
     vertex.size =10 ,
     vertex.label.cex =1,
     edge.width=E(grafo_vecinos_2657)$weight,
     rescale=F, 
     layout=l)
layouts <- grep("^layout\\.", ls("package:igraph"), value=TRUE) 
layouts <- layouts[!grepl("bipartite|merge|norm|sugiyama", layouts)]
par(mfrow=c(3,3))
for (layout in layouts) {
    print(layout)
    l <- do.call(layout, list(grafo_vecinos_2657)) 
    plot(grafo_vecinos_2657, edge.arrow.mode=0, layout=l, main=layout) }


hist(E(grafo_vecinos_2657)$weight)
mean(E(grafo_vecinos_2657)$weight)
sd(E(grafo_vecinos_2657)$weight)
cut.off <- 2
grafo_vecinos_2657.sp <- delete.edges(grafo_vecinos_2657, E(grafo_vecinos_2657)[weight<cut.off])
l <- layout.fruchterman.reingold(grafo_vecinos_2657.sp)
plot(grafo_vecinos_2657.sp, 
     layout=l,
     edge.width=E(grafo_vecinos_2657.sp)$weight,
     vertex.size =10, vertex.label.cex =1
     )


col <- rep("grey40", vcount(grafo_vecinos_2657.sp))
col[names(V(grafo_vecinos_2657.sp))=='2657'] <- "#ff5100"

l <- layout.fruchterman.reingold(grafo_vecinos_2657.sp)
l <- layout.norm(l, ymin=-1, ymax=1, xmin=-1, xmax=1)

par(mfrow=c(2,2), mar=c(0,0,0,0))
plot(grafo_vecinos_2657.sp, rescale=F, layout=l*1.0,vertex.size =10, vertex.label.cex =1)
plot(grafo_vecinos_2657.sp, rescale=F, layout=l*1.1,vertex.size =10, vertex.label.cex =1,vertex.color=col,edge.width=E(grafo_vecinos_2657.sp)$weight)
plot(grafo_vecinos_2657.sp, rescale=F, layout=l*2.5,vertex.size =10, vertex.label.cex =1)

pdf('grafo-vistas.pdf')
plot(grafo_usuarios_visitas, rescale=F, layout=l*1.0)
dev.off()

netm <- get.adjacency(grafo_vecinos_2657, attr="weight", sparse=F)
colnames(netm) <- names(V(grafo_vecinos_2657))
rownames(netm) <- names(V(grafo_vecinos_2657))
palf <- colorRampPalette(c("gold", "dark orange")) 
heatmap(netm, Rowv = NA, Colv = NA, col = palf(100), 
        scale="none", margins=c(10,10) )

dd <- degree.distribution(grafo_vecinos_2657, cumulative=T, mode="all")
plot(dd, pch=19, cex=1, col="orange", xlab="Degree", ylab="Cumulative Frequency")
