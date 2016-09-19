
library(igraph)
library(reshape2)
# Funciones ---------------------------------------------------------------
generarScoringUULengthPathGrafo <- function(grafo,usuarios,csvResultado){
    vertices = names(V(grafo))
    for (user in usuarios) {
        if (as.character(user) %in% vertices){
            distancias = melt(distances(grafo,v = as.character(user)))
            write.table(x = distancias,file = csvResultado,quote = F,sep = ',',append = T,row.names = F,col.names = F)
            
        }else{
            print(as.character(user))
            
        }
    }
}

# --- ---------------------------------------------------------------------

usuarios_visitas <- read.csv(file ='../resultados/cantidad_visitas_comunes2.csv',sep = "\t",header = F, colClasses = c('character','character','character'))
usuarios_ny = read.csv(file ='users_tips_ny_grafo.csv',sep = ",",header = T)

names(usuarios_visitas) <- c('user_id1','user_id2','visitas_comunes')
head(usuarios_visitas)
tail(usuarios_visitas)
str(usuarios_visitas)
usuarios_visitas$visitas_comunes <- as.numeric(usuarios_visitas$visitas_comunes)
head(usuarios_ny)
str(usuarios_ny)

grafo_usuarios_visitas<-graph.data.frame(usuarios_visitas[,c('user_id1','user_id2')], directed=F, vertices=NULL)
usuarios = usuarios_ny$userId
file_csv= 'scoring_camino_usuario_usuario_grafo_visitas.csv'
generarScoringUULengthPathGrafo(grafo_usuarios_visitas,usuarios,file_csv)

scoring_path = read.csv(file = 'scoring_camino_usuario_usuario_grafo_visitas.csv',header = F)
