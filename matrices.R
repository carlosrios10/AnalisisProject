library(plyr)
matrizVisitasTodos = read.csv("C:/Users/Usuario/Desktop/carlos/Tesis/datasets/forsquare-2/AnalisisProject/count_tips_con_user_grafo_ny.csv",header = F)

matrizVisitasTodosNY = read.csv("C:/Users/Usuario/Desktop/carlos/Tesis/datasets/forsquare-2/AnalisisProject/count_tips_con_user_grafo_ny.csv",header = F)
hist(matrizVisitasTodos$V3)
hist(matrizVisitasTodosNY$V3)


matrizSentiTodos = read.csv("C:/Users/Usuario/Desktop/carlos/Tesis/datasets/forsquare-2/AnalisisProject/clean_dataset/count_tips_con_user_grafo_tips_senti_discret_2.csv",header = F)
matrizSentiTodosNY = read.csv("C:/Users/Usuario/Desktop/carlos/Tesis/datasets/forsquare-2/AnalisisProject//count_tips_con_user_grafo_ny_tips_senti_discret_2.csv",header = F)
                                                                                                                        
head(matrizSentiTodosNY)
hist(matrizSentiTodos$V3)
hist(matrizSentiTodosNY$V3)

user_count = count(matrizSentiTodosNY,c('V1'))
head(user_count)
str(user_count)
user_count = user_count[order(user_count$freq,decreasing=T),]

visitas_32 = matrizVisitasTodosNY[matrizVisitasTodosNY$V1==305584 | matrizVisitasTodosNY$V1==2756865,]
visitas_32 = matrizVisitasTodosNY[matrizVisitasTodosNY$V1==305584 | matrizVisitasTodosNY$V1==2756865,]

sentimiento_32 = matrizSentiTodosNY[matrizSentiTodosNY$V1==305584  | matrizSentiTodosNY$V1==2756865,]
sentimiento_32 = matrizSentiTodosNY[matrizSentiTodosNY$V1==305584  | matrizSentiTodosNY$V1==2756865,]



visitas_32 = visitas_32[order(visitas_32$V2),]
sentimiento_32 = sentimiento_32[order(sentimiento_32$V2),] 
head(sentimiento_32)
head(visitas_32)

write.table(visitas_32,file = 'count_tips_con_user_grafo_ny_32.csv',sep = ',',row.names = FALSE, col.names = F,quote = FALSE)
write.table(sentimiento_32,file = 'count_tips_con_user_grafo_ny_tips_senti_discret_2_32.csv',sep = ',',row.names = FALSE, col.names = F,quote = FALSE)



# Analizar Log de Vecinos --------------------------------------------------------
library(stringr)
log_visitas = "C:/Users/Usuario/Desktop/carlos/Tesis/workspaceJ/recomendacionfc/visitas.out"
log_sentimiento = "C:/Users/Usuario/Desktop/carlos/Tesis/workspaceJ/recomendacionfc/sentimiento.out"

crearDF <- function(log_name){
    lines <- readLines(log_name)
    user_ids = c()
    item_ids = c()
    estimations = c()
    vecinos = c()
    for (i in c(1:length(lines))) {
        if(grepl('DEBUG',lines[i])){
            debug = str_split(string = lines[i],pattern = "-")[[1]][4]
            datos = str_split(string = debug,pattern = ":")[[1]]
            user_ids = c(user_ids,datos[1])
            item_ids = c(item_ids,datos[2])
            estimations = c(estimations,datos[3])
            vecinos = c(vecinos,datos[4])

        }
    }
    df =  data.frame(user=user_ids,
                     item=item_ids,
                     estimation=estimations,
                     vecinos=vecinos,stringsAsFactors = F)
    
    return (df)
    
}    

procesarUserItemVecinos <-  function(userItemVecinos,dfOriginal){
    vecinos = str_split(userItemVecinos$vecinos,pattern = ",")
    item = userItemVecinos$item
    preferencias = c()
    for (vecino in vecinos) {
        pref = dfOriginal[dfOriginal$V1 == vecino & dfOriginal$V2 ==item,]$V3
        
        
    }
    
    
}


df_sentimiento = crearDF(log_sentimiento)
df_visitas = crearDF(log_visitas)

head(df_sentimiento,1)
head(df_visitas,1)

matrizSentiTodos[matrizSentiTodos$V1 == 32 & matrizSentiTodos$V2 ==120003,]
matrizVisitasTodos[matrizVisitasTodos$V1 == 32 & matrizVisitasTodos$V2 ==120003,]



