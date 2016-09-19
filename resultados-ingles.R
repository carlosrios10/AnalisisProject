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

# RED SOCIAL --------------------------------------------------------------
plot(1:10)
############ GRAFICO DE LINEAS solo redes
resultados_red_social[,variables_figura]
melt_resultados_red_social <- melt(resultados_red_social[,variables_figura], id.vars = c("TVecinos","Nvecinos"))
min(melt_resultados_red_social$value)
pdf(paste(path,'selVecinos/figuras/sel_vecinos_resultados_red_social_matriz_visitas_ingles.pdf',sep = ""),width=5)
ggplot(melt_resultados_red_social, aes(x=Nvecinos, y=value,fill=TVecinos)) +
    geom_line(aes(linetype=TVecinos))+ # Line type depends on cond
    geom_point(aes(shape=TVecinos),fill = "white",size = 4)   + # Shape depends on cond
    #facet_grid(. ~ variable) +
    expand_limits(y=c(0.17,0.41)) + 
    scale_x_continuous(breaks=c(5,10,20,30,50)) + 
    scale_shape_manual(name  ="",values=c(15,16,1,18,0,17),
                         breaks=c("K_NEIGHBORHOOD", 
                                  "K_FRIENDS_NIVEL_1",
                                  "K_FRIENDS_NIVEL_2",
                                  "K_FRIENDS_NIVEL_3",
                                  "K_FRIENDS_NIVEL_4",
                                  "K_FRIENDS_NIVEL_5"),
                         labels=c("Baseline", 
                                  "Social_Network_level_1",
                                  "Social_Network_level_2",
                                  "Social_Network_level_3",
                                  "Social_Network_level_4",
                                  "Social_Network_level_5"))+
    scale_linetype_manual(name  ="",
                            values=c("solid","solid","solid","solid","solid","dotdash"),
                            breaks=c("K_NEIGHBORHOOD", 
                                     "K_FRIENDS_NIVEL_1",
                                     "K_FRIENDS_NIVEL_2",
                                     "K_FRIENDS_NIVEL_3",
                                     "K_FRIENDS_NIVEL_4",
                                     "K_FRIENDS_NIVEL_5"),
                            labels=c("Baseline", 
                                     "Social_Network_level_1",
                                     "Social_Network_level_2",
                                     "Social_Network_level_3",
                                     "Social_Network_level_4",
                                     "Social_Network_level_5")
    )+
    xlab("Neighborhood size") +  
    ylab("MAE") + 
    ggtitle("")+
    #theme(legend.position="right")+
    theme(legend.justification=c(0,0), legend.position=c(0,0))+
    theme(legend.text = element_text(size = 14, face = "bold"))+
    theme(text = element_text(size=14))+
    theme(plot.margin = unit(c(-0.8,0,0,-0.5), "cm"))
dev.off()


# RED VISITAS -------------------------------------------------------------

resultados_red_visitas[,variables_figura]
resultados_red_visitas <- rbind(resultados_red_visitas,resultado_base)
melt_resultados_red_visitas <- melt(resultados_red_visitas[,variables_figura], id.vars = c("TVecinos","Nvecinos"))
min(melt_resultados_red_visitas$value)
#setEPS()
#postscript(paste(path,"figuras/neighbor_weighting_overlapliked_visitas2.eps"))
pdf(paste(path,'selVecinos/figuras/sel_vecinos_resultados_red_visitas_matriz_visitas_ingles.pdf',sep = ""),width = 5)
ggplot(melt_resultados_red_visitas, aes(x=Nvecinos, y=value,fill=TVecinos)) +
    geom_line(aes(linetype=TVecinos))+ # Line type depends on cond
    geom_point(aes(shape=TVecinos),fill = "white",size = 4)   + # Shape depends on cond
    #facet_grid(. ~ variable) +
    expand_limits(y=c(0.17,0.41)) + 
    scale_x_continuous(breaks=c(5,10,20,30,50))+ 
    scale_shape_manual(name  ="",values=c(15,16,1,18,0,17),
                         breaks=c("K_NEIGHBORHOOD", 
                                  "K_FRIENDS_NIVEL_1",
                                  "K_FRIENDS_NIVEL_2",
                                  "K_FRIENDS_NIVEL_3",
                                  "K_FRIENDS_NIVEL_4",
                                  "K_FRIENDS_NIVEL_5"),
                         labels=c("Baseline", 
                                  "Location_Network_level_1",
                                  "Location_Network_level_2",
                                  "Location_Network_level_3",
                                  "Location_Network_level_4",
                                  "Location_Network_level_5"))+
    scale_linetype_manual(name  ="",
                          values=c("solid","solid","solid","solid","solid","dotdash"),
                            breaks=c("K_NEIGHBORHOOD", 
                                     "K_FRIENDS_NIVEL_1",
                                     "K_FRIENDS_NIVEL_2",
                                     "K_FRIENDS_NIVEL_3",
                                     "K_FRIENDS_NIVEL_4",
                                     "K_FRIENDS_NIVEL_5"),
                            labels=c("Baseline", 
                                     "Location_Network_level_1",
                                     "Location_Network_level_2",
                                     "Location_Network_level_3",
                                     "Location_Network_level_4",
                                     "Location_Network_level_5")
    )+
    xlab("Neighborhood size") +  
    ylab("MAE") + 
    ggtitle("")+
    #theme(legend.position="right")+
    theme(legend.justification=c(1,1), legend.position=c(1,1))+
    theme(legend.text = element_text(size = 14, face = "bold"))+
    theme(text = element_text(size=14))+
    theme(plot.margin = unit(c(-0.8,0,0,-0.5), "cm"))
dev.off()


# GEO-LOC -----------------------------------------------------------------

resultados_condado[,variables_figura]
resultados_condado$TVecinos <- as.character(resultados_condado$TVecinos)
resultados_condado[resultados_condado$TVecinos == 'K_NEIGHBORHOOD',c('TVecinos')] <- 'Estado_NY'
resultado_geo <-  rbind(resultados_condado,resultado_base)
resultado_geo <-  rbind(resultado_geo,resultados_zona)

melt_resultados_condado <- melt(resultado_geo[,variables_figura], id.vars = c("TVecinos","Nvecinos"))
min(melt_resultados_condado$value)
#setEPS()
#postscript(paste(path,"figuras/neighbor_weighting_overlapliked_visitas2.eps"))
pdf(paste(path,'selVecinos/figuras/sel_vecinos_resultados_geo_matriz_visitas_ingles.pdf',sep = ""),width = 5)
ggplot(melt_resultados_condado, aes(x=Nvecinos, y=value,fill=TVecinos)) +
    geom_line(aes(linetype=TVecinos))+ # Line type depends on cond
    geom_point(aes(shape=TVecinos),fill = "white",size = 4)   + # Shape depends on cond
    #facet_grid(. ~ variable) + 
    expand_limits(y=c(0.17,0.41)) + 
    scale_x_continuous(breaks=c(5,10,20,30,50))+ 
    scale_shape_manual(name  ="",values=c(15,17,1,18),
                         breaks=c("K_NEIGHBORHOOD",
                                  "Estado_NY", 
                                  "K_NEIGHBORHOOD_GRUPO",
                                  "K_NEIGHBORHOOD_ZONA"
                         ),
                         labels=c("Baseline",
                                  "State_NY", 
                                  "County",
                                  "Visiting Area"
                         ))+
    scale_linetype_manual(name  ="",
                          values=c("solid","dotdash","solid","solid"),
                          breaks=c("K_NEIGHBORHOOD",
                                   "Estado_NY", 
                                   "K_NEIGHBORHOOD_GRUPO",
                                   "K_NEIGHBORHOOD_ZONA"
                          ),
                          labels=c("Baseline",
                                   "State_NY", 
                                   "County",
                                   "Visiting Area")
    )+
    xlab("Neighborhood size") +  
    ylab("MAE") + 
    ggtitle("")+
    #theme(legend.position="right")+
    theme(legend.justification=c(0,0), legend.position=c(0,0))+
    theme(legend.text = element_text(size = 14, face = "bold"))+
    theme(text = element_text(size=14))+
    theme(plot.margin = unit(c(-0.8,0,0,-0.5), "cm"))
dev.off()


# CANTIDAD VISITAS --------------------------------------------------------

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
                     labels= c('Location_Network_level_1',
                               'Social_Network_level_1',
                               'Location_Network_level_2',
                               'Location_Network_level_3',
                               'Location_Network_level_4',
                               'Social_Network_level_2',
                               'Location_Network_level_5',
                               'Social_Network_level_3',
                               'Social_Network_level_4',
                               'Social_Network_level_5',
                               'BASE'))+
    ylab("N° potenciales vecinos") +  
    xlab("Tipo de Selección") +
    ggtitle("")
dev.off()
###    
pdf(paste(path,'selVecinos/figuras/cantidad_vecinos_potenciales_ingles.pdf',sep = ""),height = 5)
ggplot(data=canti_vecinos, aes(x=TVecinos, y=promedioVecinos)) +
    geom_bar(stat="identity",fill="grey50") +
    geom_text(aes(x=TVecinos, y=promedioVecinos, label = promedioVecinos)) +
    coord_flip() +
    scale_x_discrete(limits=levels(canti_vecinos$TVecinos),
                     labels= c('Location_Network_level_1',
                               'Social_Network_level_1',
                               'Location_Network_level_2',
                               'Visiting Area',
                               'Location_Network_level_3',
                               'Location_Network_level_4',
                               'Social_Network_level_2',
                               'Location_Network_level_5',
                               'County',
                               'State_NY',
                               'Social_Network_level_3',
                               'Social_Network_level_4',
                               'Social_Network_level_5',
                               'Baseline'))+
    ylab("Number of users") +  
    xlab("") +
    ggtitle("")+
    theme(plot.margin = unit(c(-1,1,0,-0.5), "cm"))
dev.off()
