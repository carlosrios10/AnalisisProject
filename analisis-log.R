library(ggplot2)
path= "../../forsquare-2/resultados/selVecinos/"


# Funciones ---------------------------------------------------------------
library(stringr)
COSENOK_NEIGHBORHOOD="COSENO-K_NEIGHBORHOOD"
COSENOK_NEIGHBORHOOD_ZONA="COSENO-K_NEIGHBORHOOD_ZONA"
COSENOK_FRIENDS_NIVEL_1="COSENO-K_FRIENDS_NIVEL_1"
COSENOK_FRIENDS_NIVEL_2="COSENO-K_FRIENDS_NIVEL_2"
COSENOK_FRIENDS_NIVEL_3="COSENO-K_FRIENDS_NIVEL_3"
COSENOK_FRIENDS_NIVEL_4="COSENO-K_FRIENDS_NIVEL_4"
COSENOK_FRIENDS_NIVEL_5="COSENO-K_FRIENDS_NIVEL_5"

getstr <- function(mystring, initial.character, final.character) {
    the_pattern <- paste(initial.character,".*",final.character, sep="")
    snippet <- str_extract(mystring, the_pattern)
    snippet <- sub("^.", "", snippet)
    snippet <- sub(".$", "", snippet)
    return(snippet)
}

getCoverage =  function(logName, totalPref){
    lines <- readLines(paste(path,logName,sep=""))
    cantidades = c()
    estrategias = c()
    nVecinos = c()
    maes = c()
    for (i in c(1:length(lines))) {
        if(grepl('COSENO',lines[i])){
            mae = getstr(lines[i-1],": ","[0123456789]")
            cantidad  = getstr(lines[i-2],"n "," c")
            estrategia = getstr(lines[i],"- C","-BASE")
            nVecino = str_split(getstr(estrategia,"-[0123456789]","--1.0"),"-")[[1]][1]
            estrategia = (str_split(getstr(estrategia,"-K","-[0123456789]"),"-")[[1]][1])
            nVecinos = c(nVecinos,as.numeric(str_trim(nVecino)))
            cantidades = c(cantidades, as.numeric(str_trim(cantidad)))
            estrategias = c(estrategias, str_trim(estrategia))
            maes = c(maes,as.numeric(str_trim(mae)))
            
        }
        
    }
    df = data.frame(Estrategia=estrategias,
                    cantNoRecomienda=cantidades,
                    nVecinos=nVecinos,
                    mae=maes)
    df$SiRecomienda = totalPref - df$cantNoRecomienda 
    df$porcentaje = (df$SiRecomienda/totalPref)*100
    return (df)
    
}

# Matriz senti descreta 2 corrida1 0.8,0.2 ------------------------------------------------------------
total_pre = 34979
df = getCoverage('resultados_UserNY_Red_Visitas_Condado_matriz_visitas_V2_test1.log',total_pre)
df2 = getCoverage('resultados_UserNY_Red_Visitas_Condado_matrizSenti_discreta_2.log',total_pre)
df2$Estrategia = str_replace(df2$Estrategia,"FRIENDS", "VISITAS")
total =NULL
total = rbind(df,df2)
total$Estrategia = as.character(total$Estrategia)
total$Estrategia = str_replace(total$Estrategia,"\\bK_NEIGHBORHOOD\\b", "Baseline")
total$Estrategia = str_replace(total$Estrategia,"\\bK_NEIGHBORHOOD_ZONA\\b", "Visiting Area")
total$Estrategia = str_replace(total$Estrategia,"K_FRIENDS_NIVEL", "Social_Network_level")
total$Estrategia = str_replace(total$Estrategia,"\\bK_NEIGHBORHOOD_GRUPO\\b", "County")
total$Estrategia = str_replace(total$Estrategia,"\\bK_NEIGHBORHOOD_GRUPO_NIVEL_2\\b", "State_NY")
total$Estrategia = str_replace(total$Estrategia,"K_VISITAS_NIVEL", "Location_Network_level")
total$mayor100 = total$nVecinos>100


red_visitas_matriz_visitas = getCoverage('resultados_UserNY_Red_Visitas_Condado_matriz_visitas_V2_test1.log',total_pre)
red_visitas_matriz_sentimiento = getCoverage('resultados_UserNY_Red_Visitas_Condado_matriz_sentimiento_V2_test1.log',total_pre)

hist(red_visitas_matriz_sentimiento$mae)
hist(red_visitas_matriz_visitas$mae)


sum(red_visitas_matriz_sentimiento$mae==red_visitas_matriz_visitas$mae )


# Matriz senti descreta 2  corrida2 0.7,0.3---------------------------------------------------

total_pre = 51846
df_v2 = getCoverage('resultados_UserNY_Red_Visitas_Condado_matriz_visitas_V2_test1.log',total_pre)
df_v2_2 = getCoverage('resultados_UserNY_Red_Visitas_Condado_matriz_visitas_V2.log',total_pre) 
df_v2_2$Estrategia = str_replace(df_v2_2$Estrategia,"FRIENDS", "VISITAS")
total = rbind(df_v2,df_v2_2)
total$Estrategia = as.character(total$Estrategia)
total$Estrategia = str_replace(total$Estrategia,"\\bK_NEIGHBORHOOD\\b", "Baseline")
total$Estrategia = str_replace(total$Estrategia,"\\bK_NEIGHBORHOOD_ZONA\\b", "Visiting Area")
total$Estrategia = str_replace(total$Estrategia,"K_FRIENDS_NIVEL", "Social_Network_level")
total$Estrategia = str_replace(total$Estrategia,"\\bK_NEIGHBORHOOD_GRUPO\\b", "County")
total$Estrategia = str_replace(total$Estrategia,"\\bK_NEIGHBORHOOD_GRUPO_NIVEL_2\\b", "State_NY")
total$Estrategia = str_replace(total$Estrategia,"K_VISITAS_NIVEL", "Location_Network_level")
total$mayor100 = total$nVecinos>100

# Coverage ----------------------------------------------------------------

ggplot(total[total$nVecinos<350,], aes(x=nVecinos, y=porcentaje,fill=Estrategia)) +
    geom_line(aes(linetype=Estrategia))+ # Line type depends on cond
    geom_point(aes(shape=Estrategia),fill = "white",size = 2)   + # Shape depends on cond
    # expand_limits(y=c(0.5,0.9)) + 
    facet_grid(mayor100 ~ .) +
    scale_x_continuous(breaks=c(5,10,20,30,50,100,150,200,250,300))+ 
    scale_shape_manual(name  ="",values=c(18,1,2,3,4,5,6,7,8,9,10,11,12,13),
                       breaks=c("Baseline", 
                                "Visiting Area",
                                "Social_Network_level_1",
                                "Social_Network_level_2",
                                "Social_Network_level_3",
                                "Social_Network_level_4",
                                "Social_Network_level_5",
                                "County",
                                "State_NY",
                                "Location_Network_level_1",
                                "Location_Network_level_2",
                                "Location_Network_level_3",
                                "Location_Network_level_4",
                                "Location_Network_level_15"))+
    scale_linetype_manual(name  ="",values=c("dotdash","solid","solid","solid","solid","solid",
                                             "solid","solid","solid","solid","solid","solid","solid","solid"),
                          breaks=c("Baseline", 
                                   "Visiting Area",
                                   "Social_Network_level_1",
                                   "Social_Network_level_2",
                                   "Social_Network_level_3",
                                   "Social_Network_level_4",
                                   "Social_Network_level_5",
                                   "County",
                                   "State_NY",
                                   "Location_Network_level_1",
                                   "Location_Network_level_2",
                                   "Location_Network_level_3",
                                   "Location_Network_level_4",
                                   "Location_Network_level_15"))+
    xlab("Neighborhood size") +  
    ylab("Coverage") + 
    ggtitle("")+
    theme(legend.position="right")+
    theme(legend.text = element_text(size = 8, face = "bold"))

# MAE ---------------------------------------------------------------------

ggplot(total[total$nVecinos<300,], aes(x=nVecinos, y=mae,fill=Estrategia)) +
    geom_line(aes(linetype=Estrategia))+ # Line type depends on cond
    geom_point(aes(shape=Estrategia),fill = "white",size = 2)   + # Shape depends on cond
    # expand_limits(y=c(0.5,0.9)) + 
    scale_x_continuous(breaks=c(5,10,20,30,50,100,150,200,250,300))+ 
    scale_shape_manual(name  ="",values=c(18,1,2,3,4,5,6,7,8,9,10,11,12,13),
                       breaks=c("Baseline", 
                                "Visiting Area",
                                "Social_Network_level_1",
                                "Social_Network_level_2",
                                "Social_Network_level_3",
                                "Social_Network_level_4",
                                "Social_Network_level_5",
                                "County",
                                "State_NY",
                                "Location_Network_level_1",
                                "Location_Network_level_2",
                                "Location_Network_level_3",
                                "Location_Network_level_4",
                                "Location_Network_level_15"))+
    scale_linetype_manual(name  ="",values=c("dotdash","solid","solid","solid","solid","solid",
                                             "solid","solid","solid","solid","solid","solid","solid","solid"),
                          breaks=c("Baseline", 
                                   "Visiting Area",
                                   "Social_Network_level_1",
                                   "Social_Network_level_2",
                                   "Social_Network_level_3",
                                   "Social_Network_level_4",
                                   "Social_Network_level_5",
                                   "County",
                                   "State_NY",
                                   "Location_Network_level_1",
                                   "Location_Network_level_2",
                                   "Location_Network_level_3",
                                   "Location_Network_level_4",
                                   "Location_Network_level_15"))+
    xlab("Neighborhood size") +  
    ylab("MAE") + 
    ggtitle("")+
    theme(legend.position="right")+
    theme(legend.text = element_text(size = 8, face = "bold"))+
    facet_wrap(~ mayor100)

# barplot -----------------------------------------------------------------
vecinos_name <- c(
    `5` = "Cant. Vecinos 5",
    `10` = "Cant. Vecinos 10",
    `20` = "Cant. Vecinos 20",
    `30` = "Cant. Vecinos 30",
    `50` = "Cant. Vecinos 50",
    `100` = "Cant. Vecinos 100",
    `150` = "Cant. Vecinos 150",
    `200` = "Cant. Vecinos 200",
    `250` = "Cant. Vecinos 250",
    `300` = "Cant. Vecinos 300"
)

ggplot(total[total$nVecinos<250,], aes(x=factor(Estrategia), y=mae)) +
    geom_bar(width =0.3 ,stat="identity", position=position_dodge(),colour="black")+
    facet_wrap( ~ nVecinos,ncol=4,labeller = as_labeller(vecinos_name))+
    coord_flip()+
    xlab("Estrategias") +  
    ylab("MAE")

ggplot(total[total$nVecinos<250,], aes(x=factor(Estrategia), y=porcentaje)) +
    geom_bar(width =0.3,stat="identity", position=position_dodge(),colour="black")+
    facet_wrap(~ nVecinos,ncol=4,labeller = as_labeller(vecinos_name))+
    coord_flip()+
    xlab("Estrategias") +  
    ylab("Coverage")


# Separados en tres -------------------------------------------------------
red_social =c("Baseline", 
              "Social_Network_level_1",
              "Social_Network_level_2",
              "Social_Network_level_3",
              "Social_Network_level_4",
              "Social_Network_level_5")

ggplot(total[(total$nVecinos<200) & (total$Estrategia%in%red_social),], aes(x=nVecinos, y=mae,fill=Estrategia)) +
    geom_line(aes(linetype=Estrategia))+ # Line type depends on cond
    geom_point(aes(shape=Estrategia),fill = "white",size = 2)   + # Shape depends on cond
    #facet_grid(. ~ variable) +
    expand_limits(y=c(0.6,0.9)) + 
    scale_x_continuous(breaks=c(5,10,20,30,50,100,150,200,250,300)) + 
    scale_shape_manual(name  ="",values=c(15,16,1,18,0,17),
                       breaks=c("Baseline", 
                                "Social_Network_level_1",
                                "Social_Network_level_2",
                                "Social_Network_level_3",
                                "Social_Network_level_4",
                                "Social_Network_level_5"))+
    scale_linetype_manual(name  ="",
                          values=c("dotdash","solid","solid","solid","solid","solid"),
                          breaks=c("Baseline", 
                                   "Social_Network_level_1",
                                   "Social_Network_level_2",
                                   "Social_Network_level_3",
                                   "Social_Network_level_4",
                                   "Social_Network_level_5"))+
    xlab("Neighborhood size") +  
    ylab("MAE") + 
    ggtitle("")+
    theme(legend.position="right")+
    theme(legend.text = element_text(size = 8, face = "bold"))+
    theme(plot.margin = unit(c(-0.8,0,0,-0.5), "cm"))



## Red visitas
red_visitas =c("Baseline", 
               "Location_Network_level_1",
               "Location_Network_level_2",
               "Location_Network_level_3",
               "Location_Network_level_4",
               "Location_Network_level_5")

ggplot(total[(total$nVecinos<200) & (total$Estrategia%in%red_visitas),], aes(x=nVecinos, y=mae,fill=Estrategia)) +
    geom_line(aes(linetype=Estrategia))+ # Line type depends on cond
    geom_point(aes(shape=Estrategia),fill = "white",size = 2)   + # Shape depends on cond
    #facet_grid(. ~ variable) +
    expand_limits(y=c(0.6,0.9)) + 
    scale_x_continuous(breaks=c(5,10,20,30,50,100,150,200,250,300)) + 
    scale_shape_manual(name  ="",values=c(15,16,1,18,0,17),
                       breaks=c("Baseline", 
                                "Location_Network_level_1",
                                "Location_Network_level_2",
                                "Location_Network_level_3",
                                "Location_Network_level_4",
                                "Location_Network_level_5"))+
    scale_linetype_manual(name  ="", values=c("dotdash","solid","solid","solid","solid","solid"),
                          breaks=c("Baseline", 
                                   "Location_Network_level_1",
                                   "Location_Network_level_2",
                                   "Location_Network_level_3",
                                   "Location_Network_level_4",
                                   "Location_Network_level_5"))+
    xlab("Neighborhood size") +  
    ylab("MAE") + 
    ggtitle("")+
    theme(legend.position="right")+
    theme(legend.text = element_text(size = 8, face = "bold"))+
    theme(plot.margin = unit(c(-0.8,0,0,-0.5), "cm"))

### Geo-loc
red_geo =c("Baseline", 
               "Visiting Area",
               "County",
               "State_NY")

ggplot(total[(total$nVecinos<200) & (total$Estrategia%in%red_geo),], aes(x=nVecinos, y=mae,fill=Estrategia)) +
    geom_line(aes(linetype=Estrategia))+ # Line type depends on cond
    geom_point(aes(shape=Estrategia),fill = "white",size = 2)   + # Shape depends on cond
    #facet_grid(. ~ variable) +
    expand_limits(y=c(0.6,0.9)) + 
    scale_x_continuous(breaks=c(5,10,20,30,50,100,150,200,250,300)) + 
    scale_shape_manual(name  ="",values=c(15,16,1,18,0,17),
                       breaks=c("Baseline", 
                                "Visiting Area",
                                "County",
                                "State_NY"))+
    scale_linetype_manual(name  ="",
                          values=c("dotdash","solid","solid","solid","solid","solid"),
                          breaks=c("Baseline",
                                   "Visiting Area",
                                   "County",
                                   "State_NY"))+
    xlab("Neighborhood size") +  
    ylab("MAE") + 
    ggtitle("")+
    theme(legend.position="right")+
    theme(legend.text = element_text(size = 8, face = "bold"))+
    theme(plot.margin = unit(c(-0.8,0,0,-0.5), "cm"))