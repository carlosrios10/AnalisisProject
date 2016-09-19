library(reshape2)
library(ggplot2)
library(gridExtra)
##varibles<-c("Agregation","Nvecinos","Mae","Rms","Precision","Recall","F_measure")
path<-"C:/Users/Usuario/Desktop/carlos/Tesis/datasets/forsquare-2/resultados/"
varibles<-c("Agregation","Nvecinos","Precision","Recall","F_measure")
variablesTabla <- c("TVecinos","Nvecinos","Mae","Rms","Precision","Recall","F1Measure")
variablesNum <- c("Mae","Rms","Precision","Recall","F1Measure")

################ RESULTADOS SELECCION DE VECINOS - RED SOCIAL - NY - CONDADOS
variables_figura <- c("TVecinos","Nvecinos","Mae")
resultados_red_social <- read.csv(paste(path,'selVecinos/resultados_VecinosSocial_vecinos_cantidad.csv',sep = ""))
resultados_red_social[,variablesNum] <- round(resultados_red_social[variablesNum],digits = 3)

resultados_red_visitas <- read.csv(paste(path,'selVecinos/resultados_UserNY_Vecinos_Grafo_visitas_mas2.csv',sep = ""))
resultados_red_visitas[,variablesNum] <- round(resultados_red_visitas[variablesNum],digits = 3)


resultados_condado <- read.csv(paste(path,'selVecinos/resultados_UserNY_VecinosNY_Condados.csv',sep = ""))
resultados_condado[,variablesNum] <- round(resultados_condado[variablesNum],digits = 3)
summary(resultados_condado)
resultados_zona <- read.csv(paste(path,'selVecinos/resultados_UserNY_Vecinos_Zonas.csv',sep = ""))
resultados_zona[,variablesNum] <- round(resultados_zona[variablesNum],digits = 3)


resultado_base <-  resultados_red_social[resultados_red_social$TVecinos == 'K_NEIGHBORHOOD',]
#############
pdf(paste(path,'selVecinos/figuras/sel_vecinos_resultados_red_social_red_visitas.pdf',sep = ""))
multiplot(p1, p2,cols=1)
dev.off()
############ GRAFICO DE LINEAS solo redes
resultados_red_social[,variables_figura]
melt_resultados_red_social <- melt(resultados_red_social[,variables_figura], id.vars = c("TVecinos","Nvecinos"))
#setEPS()
#postscript(paste(path,"figuras/neighbor_weighting_overlapliked_visitas2.eps"))
pdf(paste(path,'selVecinos/figuras/sel_vecinos_resultados_red_social.pdf',sep = ""))
p1 = ggplot(melt_resultados_red_social, aes(x=Nvecinos, y=value,fill=TVecinos)) +
    geom_line()+ # Line type depends on cond
    geom_point(aes(shape=TVecinos),fill = "white",size = 4)   + # Shape depends on cond
    facet_grid(. ~ variable) + 
    scale_x_continuous(breaks=c(5,10,20,30,50))+ 
    scale_shape_discrete(name  ="",
                        breaks=c("K_NEIGHBORHOOD", 
                                  "K_FRIENDS_NIVEL_1",
                                  "K_FRIENDS_NIVEL_2",
                                  "K_FRIENDS_NIVEL_3",
                                  "K_FRIENDS_NIVEL_4",
                                  "K_FRIENDS_NIVEL_5"),
                        labels=c("Base", 
                                 "Red_Social_N_1",
                                 "Red_Social_N_2",
                                 "Red_Social_N_3",
                                 "Red_Social_N_4",
                                 "Red_Social_N_5"))+
    scale_linetype_discrete(name  ="",
                         breaks=c("K_NEIGHBORHOOD", 
                                  "K_FRIENDS_NIVEL_1",
                                  "K_FRIENDS_NIVEL_2",
                                  "K_FRIENDS_NIVEL_3",
                                  "K_FRIENDS_NIVEL_4",
                                  "K_FRIENDS_NIVEL_5"),
                         labels=c("Base", 
                                  "Red_Social_N_1",
                                  "Red_Social_N_2",
                                  "Red_Social_N_3",
                                  "Red_Social_N_4",
                                  "Red_Social_N_5")
                                    )+
    xlab("Nro. Vecinos") +  
    ylab("Valor") + 
    ggtitle("Selección de vecinos en red social")+
    theme(legend.position="right")+
    theme(legend.text = element_text(size = 8, face = "bold"))
dev.off()


############ GRAFICO DE LINEAS
resultados_red_social[,variables_figura]
melt_resultados_red_social <- melt(resultados_red_social[,variables_figura], id.vars = c("TVecinos","Nvecinos"))
#setEPS()
#postscript(paste(path,"figuras/neighbor_weighting_overlapliked_visitas2.eps"))
pdf(paste(path,'selVecinos/figuras/sel_vecinos_resultados_red_social.pdf',sep = ""))
ggplot(melt_resultados_red_social, aes(x=Nvecinos, y=value,fill=TVecinos)) +
    geom_line()+ # Line type depends on cond
    geom_point(aes(shape=TVecinos),fill = "white",size = 4)   + # Shape depends on cond
    facet_grid(. ~ variable) + 
    scale_x_continuous(breaks=c(5,10,20,30,50))+ 
    scale_shape_discrete(name  ="",
                         breaks=c("K_NEIGHBORHOOD", 
                                  "K_FRIENDS_NIVEL_1",
                                  "K_FRIENDS_NIVEL_2",
                                  "K_FRIENDS_NIVEL_3",
                                  "K_FRIENDS_NIVEL_4",
                                  "K_FRIENDS_NIVEL_5"),
                         labels=c("Base", 
                                  "K_FRIENDS_NIVEL_1",
                                  "K_FRIENDS_NIVEL_2",
                                  "K_FRIENDS_NIVEL_3",
                                  "K_FRIENDS_NIVEL_4",
                                  "K_FRIENDS_NIVEL_5"))+
    scale_linetype_discrete(name  ="",
                            breaks=c("K_NEIGHBORHOOD", 
                                     "K_FRIENDS_NIVEL_1",
                                     "K_FRIENDS_NIVEL_2",
                                     "K_FRIENDS_NIVEL_3",
                                     "K_FRIENDS_NIVEL_4",
                                     "K_FRIENDS_NIVEL_5"),
                            labels=c("Base", 
                                     "K_FRIENDS_NIVEL_1",
                                     "K_FRIENDS_NIVEL_2",
                                     "K_FRIENDS_NIVEL_3",
                                     "K_FRIENDS_NIVEL_4",
                                     "K_FRIENDS_NIVEL_5")
    )+
    xlab("Nro. Vecinos") +  
    ylab("Valor") + 
    ggtitle("Seleccion de vecinos en red social")+
    theme(legend.position="right")+
    theme(legend.text = element_text(size = 6, face = "bold"))
dev.off()
#################
resultados_condado[,variables_figura]
resultados_condado$TVecinos <- as.character(resultados_condado$TVecinos)
resultados_condado[resultados_condado$TVecinos == 'K_NEIGHBORHOOD',c('TVecinos')] <- 'Estado_NY'
resultado_geo <-  rbind(resultados_condado,resultado_base)
resultado_geo <-  rbind(resultado_geo,resultados_zona)

melt_resultados_condado <- melt(resultado_geo[,variables_figura], id.vars = c("TVecinos","Nvecinos"))
#setEPS()
#postscript(paste(path,"figuras/neighbor_weighting_overlapliked_visitas2.eps"))
pdf(paste(path,'selVecinos/figuras/sel_vecinos_resultados_geo.pdf',sep = ""))
ggplot(melt_resultados_condado, aes(x=Nvecinos, y=value,fill=TVecinos)) +
    geom_line()+ # Line type depends on cond
    geom_point(aes(shape=TVecinos),fill = "white",size = 4)   + # Shape depends on cond
    facet_grid(. ~ variable) + 
    scale_x_continuous(breaks=c(5,10,20,30,50))+ 
    scale_shape_discrete(name  ="",
                         breaks=c("K_NEIGHBORHOOD",
                                  "Estado_NY", 
                                  "K_NEIGHBORHOOD_GRUPO",
                                  "K_NEIGHBORHOOD_ZONA"
                                  ),
                         labels=c("Base",
                                  "Estado_NY", 
                                  "Condado",
                                  "Zona"
                                  ))+
    xlab("Nro. Vecinos") +  
    ylab("Valor") + 
    ggtitle("Seleccion de vecinos por geolocalizacion")+
    theme(legend.position="bottom")+
    theme(legend.text = element_text(size = 9, face = "bold"))
dev.off()
##############
resultados_red_visitas[,variables_figura]
resultados_red_visitas <- rbind(resultados_red_visitas,resultado_base)
melt_resultados_red_visitas <- melt(resultados_red_visitas[,variables_figura], id.vars = c("TVecinos","Nvecinos"))
#setEPS()
#postscript(paste(path,"figuras/neighbor_weighting_overlapliked_visitas2.eps"))
pdf(paste(path,'selVecinos/figuras/sel_vecinos_resultados_red_visitas.pdf',sep = ""))
p2 = ggplot(melt_resultados_red_visitas, aes(x=Nvecinos, y=value,fill=TVecinos)) +
    geom_line()+ # Line type depends on cond
    geom_point(aes(shape=TVecinos),fill = "white",size = 4)   + # Shape depends on cond
    facet_grid(. ~ variable) + 
    scale_x_continuous(breaks=c(5,10,20,30,50))+ 
    scale_shape_discrete(name  ="",
                         breaks=c("K_NEIGHBORHOOD", 
                                  "K_FRIENDS_NIVEL_1",
                                  "K_FRIENDS_NIVEL_2",
                                  "K_FRIENDS_NIVEL_3",
                                  "K_FRIENDS_NIVEL_4",
                                  "K_FRIENDS_NIVEL_5"),
                         labels=c("Base", 
                                  "Red_Visitas_N_1",
                                  "Red_Visitas_N_2",
                                  "Red_Visitas_N_3",
                                  "Red_Visitas_N_4",
                                  "Red_Visitas_N_5"))+
    scale_linetype_discrete(name  ="",
                            breaks=c("K_NEIGHBORHOOD", 
                                     "K_FRIENDS_NIVEL_1",
                                     "K_FRIENDS_NIVEL_2",
                                     "K_FRIENDS_NIVEL_3",
                                     "K_FRIENDS_NIVEL_4",
                                     "K_FRIENDS_NIVEL_5"),
                            labels=c("Base", 
                                     "Red_Visitas_N_1",
                                     "Red_Visitas_N_2",
                                     "Red_Visitas_N_3",
                                     "Red_Visitas_N_4",
                                     "Red_Visitas_N_5")
    )+
    xlab("Nro. Vecinos") +  
    ylab("Valor") + 
    ggtitle("Seleccion de vecinos en red visitas")+
    theme(legend.position="right")+
    theme(legend.text = element_text(size = 8, face = "bold"))
dev.off()


###################
resultado_geo
resultados_red_social <- resultados_red_social[resultados_red_social$TVecinos!='K_NEIGHBORHOOD',]
resultados_red_social$TVecinos <-  paste('Social_',resultados_red_social$TVecinos,sep="")
resultados_red_visitas <- resultados_red_visitas[resultados_red_visitas$TVecinos!='K_NEIGHBORHOOD',]
resultado_todos <- rbind(resultado_geo,resultados_red_social,resultados_red_visitas)
resultado_todos[,variables_figura]
melt_resultado_todos <- melt(resultado_todos[,variables_figura], id.vars = c("TVecinos","Nvecinos"))
str(melt_resultado_todos)
melt_resultado_todos$Nvecinos<-as.factor(as.character(melt_resultado_todos$Nvecinos))
levels(melt_resultado_todos$Nvecinos)

pdf(paste(path,'selVecinos/figuras/sel_vecinos_todos.pdf',sep = ""))
ggplot(melt_resultado_todos, aes((Nvecinos), TVecinos)) + 
    geom_tile(aes(fill = value),colour = "white") +
    facet_grid(. ~ variable) + 
    scale_fill_gradient(low = "white",high = "black")+
    scale_x_discrete(limits=c('5',
                              '10',
                              '20',
                              '30',
                              '50'
                              ))+
    scale_y_discrete(limits=c('K_NEIGHBORHOOD',
                              'Social_K_FRIENDS_NIVEL_1',
                              'Social_K_FRIENDS_NIVEL_2',
                              'Social_K_FRIENDS_NIVEL_3',
                              'Social_K_FRIENDS_NIVEL_4',
                              'Social_K_FRIENDS_NIVEL_5',
                              'K_FRIENDS_NIVEL_1',
                              'K_FRIENDS_NIVEL_2',
                              'K_FRIENDS_NIVEL_3',
                              'K_FRIENDS_NIVEL_4',
                              'K_FRIENDS_NIVEL_5',
                              'Estado_NY',
                              'K_NEIGHBORHOOD_GRUPO',
                              'K_NEIGHBORHOOD_ZONA'),
                     labels= c('Base',
                               'Red_Social_N_1',
                               'Red_Social_N_2',
                               'Red_Social_N_3',
                               'Red_Social_N_4',
                               'Red_Social_N_5',
                               'Red_Visitas_N_1',
                               'Red_Visitas_N_2',
                               'Red_Visitas_N_3',
                               'Red_Visitas_N_4',
                               'Red_Visitas_N_5',
                               'Estado_NY',
                               'Condado',
                               'Zona'))+
    xlab("Nro. Vecinos") +  
    ylab("Tipo de Seleccion") +
    ggtitle("MAE y RMS en la seleccion de vecinos")
dev.off()
########################################
var_cover = c("TVecinos","Nvecinos","numUserTest","userTestConVecinos","promedioVecinos","numPrefTest","noEstimado" )

resultados_covergage_base = read.csv(file = '../resultados/selVecinos/resultados_UserNY_Base_Red_Social_Zona.csv')
resultados_covergage_red = read.csv(file = '../resultados/selVecinos/resultados_UserNY_Red_Visitas_Condado.csv')
resultados_covergage_base$TVecinos <- as.character(resultados_covergage_base$TVecinos)
resultados_covergage_red$TVecinos <- as.character(resultados_covergage_red$TVecinos)


resultados_covergage_base[, var_cover]
resultados_covergage_red[, var_cover]


resultados_covergage_base[resultados_covergage_base$TVecinos == 'K_NEIGHBORHOOD',]$TVecinos <-  'BASE'
resultados_covergage_base[grep("K_FRIENDS_NIVEL",resultados_covergage_base$TVecinos),]$TVecinos <-  paste("Social_",resultados_covergage_base[grep("K_FRIENDS_NIVEL",resultados_covergage_base$TVecinos),]$TVecinos,sep = "")


resultados_covergage_red[resultados_covergage_red$TVecinos == 'K_NEIGHBORHOOD_GRUPO',]$TVecinos <-  'CONDADO'
resultados_covergage_red[resultados_covergage_red$TVecinos == 'K_NEIGHBORHOOD_GRUPO_NIVEL_2',]$TVecinos <-  'ESTADO'

base_col_noEstiamdo = c(34744,34671,34499,34297,33950,34741,34668,34459,34306,34045,34698
                        ,34652,34580,34552,34504,34752,34680,34536,34378,34098,34751,34689,34550
                        ,34402,34074,34759,34697,34556,34414,34087,34759,34699
                        ,34560,34423,34090)
red_col_noEstiamdo = c(34769,34646,34426,34257,33977,34769,34721,34586,34409,34084,33120,32386,31709,31336,30950,34578,34370,34014,33686,
                       33197,34720,34621,34396,34175,33739,34748,34661,34448,34246,33814,34752,34668,34461,34258,33825)

resultados_covergage_base$noEstimado = base_col_noEstiamdo
resultados_covergage_red$noEstimado = red_col_noEstiamdo
 
resultados_covergage_todos = rbind(resultados_covergage_base[, var_cover],resultados_covergage_red[, var_cover])

resultados_covergage_todos$numUserTest <- 13035
resultados_covergage_todos$numPrefTest <- 34979
resultados_covergage_todos[resultados_covergage_todos$TVecinos=='BASE',]$promedioVecinos <- 47240
resultados_covergage_todos$porcEstimados = ((resultados_covergage_todos$numPrefTest - resultados_covergage_todos$noEstimado)/34979)

canti_vecinos = resultados_covergage_todos[,c('TVecinos','promedioVecinos')]
canti_vecinos = canti_vecinos[canti_vecinos$promedioVecinos!=0,]
canti_vecinos = canti_vecinos[c(-1,-2,-3,-4),]

canti_vecinos$TVecinos = as.factor(canti_vecinos$TVecinos)
levels(canti_vecinos$TVecinos)
canti_vecinos$TVecinos <-factor(canti_vecinos$TVecinos, levels=canti_vecinos[order(canti_vecinos$promedioVecinos), "TVecinos"])
### solo redes
canti_vecinos_redes = canti_vecinos[canti_vecinos$TVecinos!= 'K_NEIGHBORHOOD_ZONA'&
                            canti_vecinos$TVecinos!= 'CONDADO' &
                            canti_vecinos$TVecinos!= 'ESTADO',]
str(canti_vecinos_redes)
table(canti_vecinos_redes$TVecinos)
table(droplevels(canti_vecinos_redes$TVecinos))
canti_vecinos_redes$TVecinos =  droplevels(canti_vecinos_redes$TVecinos)
###    
pdf(paste(path,'selVecinos/figuras/cantidad_vecinos_potenciales_redes.pdf',sep = ""))
ggplot(data=canti_vecinos_redes, aes(x=TVecinos, y=promedioVecinos)) +
    geom_bar(stat="identity",fill="grey50") +
    geom_text(aes(x=TVecinos, y=promedioVecinos, label = promedioVecinos)) +
    coord_flip() +
    scale_x_discrete(limits=levels(canti_vecinos_redes$TVecinos),
                     labels= c('Red_Visitas_N_1',
                               'Red_Social_N_1',
                               'Red_Visitas_N_2',
                               'Red_Visitas_N_3',
                               'Red_Visitas_N_4',
                               'Red_Social_N_2',
                               'Red_Visitas_N_5',
                               'Red_Social_N_3',
                               'Red_Social_N_4',
                               'Red_Social_N_5',
                               'BASE'))+
    ylab("N° potenciales vecinos") +  
    xlab("Tipo de Selección") +
    ggtitle("")
dev.off()
###    
pdf(paste(path,'selVecinos/figuras/cantidad_vecinos_potenciales.pdf',sep = ""))
ggplot(data=canti_vecinos, aes(x=TVecinos, y=promedioVecinos)) +
    geom_bar(stat="identity",fill="grey50") +
    geom_text(aes(x=TVecinos, y=promedioVecinos, label = promedioVecinos)) +
    coord_flip() +
    scale_x_discrete(limits=levels(canti_vecinos$TVecinos),
                     labels= c('Red_Visitas_N_1',
                               'Red_Social_N_1',
                               'Red_Visitas_N_2',
                               'Zona',
                               'Red_Visitas_N_3',
                               'Red_Visitas_N_4',
                               'Red_Social_N_2',
                               'Red_Visitas_N_5',
                               'Condado',
                               'Estado_NY',
                               'Red_Social_N_3',
                               'Red_Social_N_4',
                               'Red_Social_N_5',
                               'BASE'))+
    ylab("N° potenciales vecinos") +  
    xlab("Tipo de Seleccion") +
    ggtitle("Cantidad Promedio de Potenciales Vecinos")
dev.off()

### Tabla################
pdf(paste(path,'figuras/resultados_VecinosSocial_vecinos_cantidad.pdf',sep = ""),width=12,height=18,paper = "a4")
grid.table(resultados_red_social[,c(varibles)])
dev.off()

pdf(paste(path,'figuras/resultados_UserNY_VecinosNY_Condados.pdf',sep = ""),width=12,height=18,paper = "a4")
grid.table(resultados_condado[,c(varibles)])
dev.off()
##
variables_plot <- c("TVecinos","Nvecinos","Mae","Rms")
resultados_red_social[,variables_plot]
melt_resultados_red_social<-melt(resultados_red_social[,variables_plot], id.vars = c("TVecinos","Nvecinos"))
#setEPS()
#postscript(paste(path,"figuras/neighbor_weighting_overlapliked_visitas2.eps"))
pdf(paste(path,'figuras/neighbor_weighting_overlapliked_visitas.pdf',sep = ""))
ggplot(melt_resultados_red_social, aes(x=Nvecinos, y=value)) +
    geom_line(aes(linetype=TVecinos), # Line type depends on cond
              size = 1) +       # Thicker line
    geom_point(aes(shape=TVecinos),
               fill = "white", # Shape depends on cond
               size = 4)   +   
    facet_grid(. ~variable) + 
    #scale_x_continuous(breaks=c(5,10,20,30,50))+  
    xlab("Nro. Vecinos") +  
    ylab("Valor") +  
    theme(legend.title=element_blank()) + 
    theme(legend.position="bottom") + 
    scale_shape_discrete(name  ="name", breaks=c("BASE", "HARMONIC_MEAN","ONLY_SCORE"),labels=c("Base", "Harmonic Mean","Only Score"))+
    scale_linetype_manual(values=c("solid","dotdash", "dotted"),name  ="name",breaks=c("BASE", "HARMONIC_MEAN","ONLY_SCORE"),labels=c("Base", "Harmonic Mean","Only Score")) 
dev.off()

##Neighbor Weighting
resultadosLV<-read.csv(paste(path,'resultados_VecinosSocial_vecinos_cantidad.csv',sep = ""))

resultadosLHV<-read.csv(paste(path,'USER_NEIGHBOR_WEIGHTING_LandH_RATING_VISTAS_SCALA.csv',sep = ""))

resultadosLS<-read.csv(paste(path,'USER_NEIGHBOR_WEIGHTING_L_RATING_SENTI.csv',sep = ""))

resultadosLHS<-read.csv(paste(path,'USER_NEIGHBOR_WEIGHTING_LandH_RATING_SENTI.csv',sep = ""))

### Tabla
resultadosLV[variablesNum]<- round(resultadosLV[variablesNum],digits = 3)
pdf(paste(path,'figuras/resultados_VecinosSocial_NW_liked.pdf',sep = ""),pointsize = 8)
grid.table(resultadosLV[variablesTabla])
dev.off()

resultadosLHV[variablesNum]<- round(resultadosLHV[variablesNum],digits = 3)
pdf(paste(path,'figuras/neighbor_weighting_overlaplikedAndHated_visitas_tabla.pdf',sep = ""))
grid.table(resultadosLHV[variablesTabla])
dev.off()

resultadosLS[variablesNum]<- round(resultadosLS[variablesNum],digits = 3)
pdf(paste(path,'figuras/neighbor_weighting_overlapliked_Senti_tabla.pdf',sep = ""))
grid.table(resultadosLS[variablesTabla])
dev.off()

resultadosLHS[variablesNum]<- round(resultadosLHS[variablesNum],digits = 3)
pdf(paste(path,'figuras/neighbor_weighting_overlaplikedAndHated_Senti_tabla.pdf',sep = ""))
grid.table(resultadosLHS[variablesTabla])
dev.off()


# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    library(grid)
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
        # Make the panel
        # ncol: Number of columns of plots
        # nrow: Number of rows needed, calculated from # of cols
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                         ncol = cols, nrow = ceiling(numPlots/cols))
    }
    
    if (numPlots==1) {
        print(plots[[1]])
        
    } else {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
        
        # Make each plot, in the correct location
        for (i in 1:numPlots) {
            # Get the i,j matrix positions of the regions that contain this subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
            
            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))
        }
    }
}

