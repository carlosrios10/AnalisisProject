### venues ####
venuesLines<-readLines("../FourSquareDataset/Venues/NYC/NYC-Venues.txt")
venuesLines <- gsub("\"","",venuesLines)
listVenues<-strsplit(venuesLines,split = "\t")
maxSize<-max(sapply(listVenues, length)) 
min(sapply(listVenues, length))
listVenues[[3]]
sink("NYC-Venues.csv")
for(i in 1:length(listVenues)){
    infoVenue<-listVenues[[i]]
    cat(infoVenue,sep = "\t")
    cat("\n")
}
sink()
venues<-read.csv("NYC-Venues.csv",sep = "\t",header = F,fill=TRUE,colClasses = c(rep("character",maxSize)))
names(venues)<- c("venueId","name","lat","lon","address","city","state","numCheckin","numCheckedUser","numCurrentUser",
                  "numTodo","numTodoDos","numCategoria","cat1","cat2","cat3","cat4","cat5","cat6","cat7",
                  "cat8","cat9","cat10","cat11","cat12","cat13","cat14","cat15","cat16","cat17","cat18")
venues$numVenueId<- 1:nrow(venues)
write.table(x = venues,file ="NYC-Venues.csv",sep = "\t",row.names = FALSE)
## exploracion de venues
venues<-read.csv("NYC-Venues.csv",sep = "\t",header = T,colClasses = "character")
str(venues)
head(venues)
freq_venue_state <- count(venues,c('city','state'))
freq_venue_state <- arrange(freq_venue_state, desc(freq))
head(freq_venue_state,n=100)
table(venues$state)


##########  users
usuersLine<-readLines("../FourSquareDataset/Users/NYC/NYC-Users.txt")
usuersLine<-gsub("\"","",usuersLine)
head(usuersLine)
listUsers<-strsplit(usuersLine,split = "\t")
max(sapply(listUsers,length))
min(sapply(listUsers, length))
v<- sapply(listUsers, length)
which(v==min(v))

which.min(sapply(listUsers, length))
listUsers[654]

sink("NYC-Users.csv")
for(i in 1:length(listUsers)){
    user<- listUsers[[i]]
    cat(user,sep = "\t")
    cat("\n")
}
sink()
users<-read.csv("NYC-Users.csv",sep = "\t",header = F,fill = T, colClasses = "character")
names(users)<- c("userId","firstName","lastName","pic","gender","homeCity")
write.table(x = users,file ="NYC-Users.csv",sep = "\t",row.names = FALSE)
### Exploracion de usuarios
users<-read.csv("NYC-Users.csv",sep = "\t",header = T,fill = T, colClasses = "character")
freq_home_city <- count(users,c('homeCity'))
freq_home_city <- arrange(freq_home_city,desc(freq))
head(freq_home_city,n=50)
head(freq_home_city)



library(stringr)
users$cityhome<-str_trim(users$V6,side = "both")
users$cityhome<-toupper(users$cityhome)
users$isNJ<-grepl(pattern = "NJ|NEW JERSEY",users$cityhome)
head(users)
table(users$cityhome)

#### tips
tipsLine<-readLines("../FourSquareDataset/Tips/NYC/NYC-Tips.txt")
tipsLine<-gsub("\"","",tipsLine)
head(tipsLine)
listTips<-strsplit(tipsLine,split = "\t")
max(sapply(listTips, length))
min(sapply(listTips, length))
v<-sapply(listTips, length)
which(v == min(v))

sink("NYC-Tips.csv")
maxColumnSize<-0
for(i in 1:length(listTips)) {
    userHistory<-listTips[[i]]
    userId<-userHistory[1]
        j<-2
        while (j<length(userHistory)) {
            venueId<-userHistory[j]
            nulll<-userHistory[j+1]
            text<-userHistory[j+2]
            if(text=="0"){
            text<-userHistory[j+3]
            time<-userHistory[j+4]
            todo<-userHistory[j+5]
            done<-userHistory[j+6]
            numCategoria<-userHistory[j+7]
            numCategoria<-as.numeric(numCategoria)
            usuarioTips<-paste(userId,
                               venueId,
                               nulll,
                               text,
                               time,
                               todo,
                               done,
                               numCategoria,
                               sep = "\t")
            k<-1
            while (k <= numCategoria) {
                categoria<-userHistory[j+7+k]
                usuarioTips<-paste(usuarioTips,categoria,sep = "\t")
                k<-k+1
            }
            j<-j+7+numCategoria+1
            }else{
                time<-userHistory[j+3]
                todo<-userHistory[j+4]
                done<-userHistory[j+5]
                numCategoria<-userHistory[j+6]
                numCategoria<-as.numeric(numCategoria)
                
                usuarioTips<-paste(userId,
                                   venueId,
                                   nulll,
                                   text,
                                   time,
                                   todo,
                                   done,
                                   numCategoria,
                                   sep = "\t")
                k<-1
                while (k <= numCategoria) {
                    categoria<-userHistory[j+6+k]
                    usuarioTips<-paste(usuarioTips,categoria,sep = "\t")
                    k<-k+1
                }
                j<-j+6+numCategoria+1
                  
                }
 
            cat(usuarioTips)
            cat("\n")
           
            colSize<-8+numCategoria
            if(colSize>maxColumnSize){
            maxColumnSize<-colSize
            }
    }
        
}
sink()
tips<-read.csv("NYC-Tips.csv",sep = "\t",header = F,fill=TRUE,colClasses = c(rep("character",maxColumnSize)))
names(tips)<-c("user","venue","null","text","time","todo","done","numCategoria","cat1","cat2","cat3","cat4","cat5","cat6","cat7","cat8","cat9","cat10","cat11","cat12","cat13","cat14","cat15","cat16","cat17","cat18")
str(tips)
write.table(x = tips,file ="NYC-Tips.csv",sep = "\t",row.names = FALSE)

##### creo un archivo donde a los tips le agrego datos de los usuarios y datos de las venues
tips<-read.csv("NYC-Tips.csv",sep = "\t",header = T,colClasses = "character")
venues<-read.csv("NYC-Venues.csv",sep = "\t",header = T,colClasses = "character")
users<-read.csv("NYC-Users.csv",sep = "\t",header = T,fill = T, colClasses = "character")
length(unique(venues$venueId))
max(as.numeric(venues$numVenueId))


tips$user <- as.numeric(tips$user)
users$userId <- as.numeric(users$userId)

str(users)
str(venues)
str(tips)
tipsVenueUser <- merge(tips,venues,by.x = 2,by.y = 1,all.x = T)
tipsVenueUser <- merge(tipsVenueUser,users,by.x = 2,by.y = 1,all.x = T)
write.table(x = tipsVenueUser,file ="NYC-TipsVenueUserAll.csv",sep = "\t",row.names = FALSE)

## me quedo con los usuarios de NY
tipsVenueUser$isUserNY<- grepl(pattern = "NY|NEW YORK|NYC",x = tipsVenueUser$homeCity)
tipsVenueUser_userNY <- tipsVenueUser[tipsVenueUser$isUserNY,]
table(tipsVenueUser_userNY$homeCity)
length(unique(tipsVenueUser_userNY$homeCity))
str(tipsVenueUser_userNY)
head(tipsVenueUser_userNY)

freq_homeCity <- count(tipsVenueUser_userNY,c('homeCity'))
freq_homeCity <- arrange(freq_homeCity,desc(freq),homeCity)
head(freq_homeCity,n=100)


freq_usuario_venue <- count(tipsVenueUser_userNY,c('user','numVenueId'))
head(freq_usuario_venue)
freq_usuario_venue <- arrange(freq_usuario_venue,desc(freq))
head(freq_usuario_venue,n=50)
table(freq_usuario_venue$freq)
## Mapeo las visitas con una escala de 5
freq_usuario_venue$rating <- 0
freq_usuario_venue$rating[freq_usuario_venue$freq==1]<-2
freq_usuario_venue$rating[freq_usuario_venue$freq==2]<-3
freq_usuario_venue$rating[freq_usuario_venue$freq==3]<-4
freq_usuario_venue$rating[freq_usuario_venue$freq>=4]<-5

write.table(x = freq_usuario_venue[c("user","numVenueId","rating")],file ="tipsUserNYScalado.csv",col.names = F,quote = F,row.names = F,sep=",")


pdf('power_law_visitas.pdf')
ggplot(freq_usuario_venue, aes(rating))+
    geom_freqpoly(binwidth = 1)+
    scale_x_continuous(breaks=c(1,2,3,4,5))+  
    xlab("Freq. de visitas usuario-lugar") +  
    ylab("Cantidad") +  
    theme(legend.title=element_blank()) + 
    theme(legend.position="bottom") 
dev.off()
getwd()

count(freq_usuario_venue,c('freq'))
#### merge tips and venue
library(plyr)
library(stringr)
library("rjson")
library(jsonlite)
str(tips)
str(venues)
#### veo frecuencia de donde fueron realizados los tips
tips<-read.csv("NYC-Tips.csv",sep = "\t",header = T,colClasses = "character")
tipsVenue <- merge(tips,venues,by.x = 2,by.y = 1,all.x = T)
freq_tipsVenue_state <- count(tipsVenue,c('city','state'))
freq_tipsVenue_state <- arrange(freq_tipsVenue_state,desc(freq))
head(freq_tipsVenue_state,n=50)

head(tipsVenue)
tipsVenue$cityStateVenue<- tipsVenue$city
tipsVenue$cityStateVenue<- gsub(pattern = "[0-9]", "",x = tipsVenue$cityStateVenue)
tipsVenue$cityStateVenue<- toupper(str_trim(tipsVenue$cityStateVenue))
tipsVenue$isNY<- grepl(pattern = "^NY|NEW YORK",x = tipsVenue$cityStateVenue)
table(tipsVenue$isNY)
tipsVenueNY <- tipsVenue[tipsVenue$isNY,]

json_file <- "../FourSquareDataset/categories.txt"
json_data <- fromJSON(json_file)
cate<-json_data$response$categories[[3]]$categories
df<-data.frame(t(sapply(cate,c)))
var_cate <- c('cat1.x','cat2.x','cat3.x','cat4.x','cat5.x','cat6.x','cat7.x','cat8.x','cat9.x',
              'cat10.x','cat11.x','cat12.x','cat13.x','cat14.x','cat15.x','cat16.x','cat17.x','cat18.x')
tipsVenueNY$isFood <- FALSE

for(i in 1:nrow(tipsVenueNY)){
    if (sum(tipsVenueNY[var_cate][i,] %in% df$id) >0)
        tipsVenueNY[i,]$isFood <- TRUE
}

table(tipsVenueNY$isFood)
tipsVenueNYFood <- tipsVenueNY[tipsVenueNY$isFood,]
head(tipsVenueNYFood)
write.table(x = tipsVenueNYFood,file ="tipsVenueNYFood.csv",sep = ",",row.names = FALSE, col.names = T,quote = FALSE)
sum(is.na(tipsVenueNYFood$text))
tail(tipsVenueNYFood)
#### Agrupar tips con usuer
str(users)
users$userId <- as.character(users$userId)
str(tips)
tipsUsers <- merge(tips,users,by.x = 1,by.y = 1,all.x = T)
head(tipsUsers)
freqH_tips_users <- count(tipsUsers,c('homeCity'))
freqH_tips_users <- arrange(freqH_tips_users,desc(freq))
head(freqH_tips_users,n=50)

#### agrupar usuario venue y hacer una merge con tips
library(plyr)
usuarioTips<-count(tips,c('user', 'venue'))
str(usuarioTips)
usuarioTips<- merge(usuarioTips,venues,by.x = 2,by.y = 1,all.x = T)
usuarioTips<- merge(usuarioTips,users,by.x = 2,by.y = 1,all.x = T)
str(usuarioTips)
write.table(x = usuarioTips,file ="NYC-UsuarioTips.csv",sep = "\t",row.names = FALSE)

head(usuarioTips[,c("city","state")],n=500)
tail(usuarioTips[,c("city","state")],n=500)

library(stringr)
usuarioTips$cityStateVenue<- usuarioTips$city
usuarioTips$cityStateVenue<- gsub(pattern = "[0-9]", "",x = usuarioTips$cityStateVenue)
usuarioTips$cityStateVenue<- toupper(str_trim(usuarioTips$cityStateVenue))
usuarioTips$isNY<- grepl(pattern = "^NY|NEW YORK",x = usuarioTips$cityStateVenue)
table(usuarioTips$isNY)
usuarioTips$homeCityClean<- usuarioTips$homeCity
usuarioTips$homeCityClean<- gsub(pattern = "[0-9]","",x = usuarioTips$homeCityClean)
usuarioTips$homeCityClean<- toupper(str_trim(usuarioTips$homeCityClean))
usuarioTips$isNJ<- grepl(pattern = "^NJ|NEW JERSEY",x = usuarioTips$homeCityClean)
usuarioTips$isMayor2<- usuarioTips$freq>2
usuarioTipsNY<- usuarioTips[usuarioTips$isNY==T & usuarioTips$isMayor2==T,c("user","numVenueId","freq")]
write.table(x = usuarioTipsNY,file ="NYC-UsuarioTipsClean.csv",sep = ",",row.names = FALSE, col.names = FALSE,quote = FALSE)
usuarioTipsNY[usuarioTipsNY$user==32611,]
length(unique(usuarioTipsNY$numVenueId))

head(usuarioTips[usuarioTips$isNY==F,])
#### otra forma de buscar matching en con cadenas
codes <- c("NY", "NEW YORK")
D <- adist(usuarioTips[usuarioTips$isNY==F,]$cityStateVenue, codes)
colnames(D) <- codes
rownames(D) <- usuarioTips[usuarioTips$isNY==F,]$cityStateVenue
head(D,n=1000)
tail(D,n=1000)
length(unique(tips$user))
dim(D)
i <- apply(D, 1, which.min)
un<-do.call("rbind", i)
head(i)
un<- as.vector(i)
dd<-data.frame(coded = codes[un])
unlist(i[1])

cityLineas <- readLines("C:/Users/Usuarioç/Desktop/carlos/Tesis/datasets/worldcities/worldcities.csv")
cities<- read.csv(file = "C:/Users/Usuarioç/Desktop/carlos/Tesis/datasets/worldcities/worldcities.csv",header = T, sep = ",")
table(cities$ISO.3166.1.country.code)
head(cities[cities$ISO.3166.1.country.code=="US",])
head(cityLineas)
log2(1)
####### red Social en NY
redSocial<-read.csv("../FourSquareDataset/Friendship/NYC/NYC-user-relations.txt",header = F,sep = "")
head(redSocial)
sum(!complete.cases(redSocial))
write.table(x = redSocial,file ="NYC-user-relations.csv",sep = ",",row.names = FALSE, col.names = FALSE,quote = FALSE)
str(redSocial)
sum(is.na(redSocial$V1))
sum(is.na(redSocial$V2))
dim(redSocial[redSocial$V2==1661,])


############## en LA #################################
### venues ####
venuesLines<-readLines("../FourSquareDataset/Venues/LA/LA-Venues.txt")
venuesLines <- gsub("\"","",venuesLines)
listVenues<-strsplit(venuesLines,split = "\t")
maxSize<-max(sapply(listVenues, length)) 
min(sapply(listVenues, length))
listVenues[[3]]
sink("LA-Venues.csv")
for(i in 1:length(listVenues)){
    infoVenue<-listVenues[[i]]
    cat(infoVenue,sep = "\t")
    cat("\n")
}
sink()
venues<-read.csv("LA-Venues.csv",sep = "\t",header = F,fill=TRUE,colClasses = c(rep("character",maxSize)))
names(venues)<- c("venueId","name","lat","lon","address","city","state","numCheckin","numCheckedUser","numCurrentUser",
                  "numTodo","numTodoDos","numCategoria","cat1","cat2","cat3","cat4","cat5","cat6","cat7",
                  "cat8","cat9","cat10","cat11","cat12","cat13","cat14","cat15","cat16","cat17","cat18")
venues$numVenueId<- 1:nrow(venues)
write.table(x = venues,file ="LA-Venues.csv",sep = "\t",row.names = FALSE)
venues<-read.csv("LA-Venues.csv",sep = "\t",header = T,colClasses = "character")
str(venues)
table(venues$state)

##########  users
usuersLine<-readLines("../FourSquareDataset/Users/LA/LA-Users.txt")
usuersLine<-gsub("\"","",usuersLine)
head(usuersLine)
listUsers<-strsplit(usuersLine,split = "\t")
max(sapply(listUsers,length))
min(sapply(listUsers, length))
v<- sapply(listUsers, length)
which(v==min(v))

which.min(sapply(listUsers, length))
listUsers[654]

sink("LA-Users.csv")
for(i in 1:length(listUsers)){
    user<- listUsers[[i]]
    cat(user,sep = "\t")
    cat("\n")
}
sink()
users<-read.csv("LA-Users.csv",sep = "\t",header = F,fill = T, colClasses = "character")
names(users)<- c("userId","firstName","lastName","pic","gender","homeCity")
write.table(x = users,file ="LA-Users.csv",sep = "\t",row.names = FALSE)
str(users)
users$userId<- as.numeric(users$userId)


#### tips
tipsLine<-readLines("../FourSquareDataset/Tips/LA/tail.txt")
tipsLine<-gsub("\"","",tipsLine)
head(tipsLine)
listTips<-strsplit(tipsLine,split = "\t")
max(sapply(listTips, length))
min(sapply(listTips, length))
v<-sapply(listTips, length)
which(v == min(v))

sink("LA-Tips.csv")
maxColumnSize<-0
for(i in 1:length(listTips)) {
    if (i == 31192 | i == 31193 | i == 31194 | i == 31195 | i == 31196 )
        next
    
    userHistory<-listTips[[i]]
    userId<-userHistory[1]
    j<-2
    while (j<length(userHistory)) {
        venueId<-userHistory[j]
        nulll<-userHistory[j+1]
        text<-userHistory[j+2]
        if(text=="0"){
            text<-userHistory[j+3]
            time<-userHistory[j+4]
            todo<-userHistory[j+5]
            done<-userHistory[j+6]
            numCategoria<-userHistory[j+7]
            numCategoria<-as.numeric(numCategoria)
            usuarioTips<-paste(userId,
                               venueId,
                               nulll,
                               text,
                               time,
                               todo,
                               done,
                               numCategoria,
                               sep = "\t")
            k<-1
            while (k <= numCategoria) {
                categoria<-userHistory[j+7+k]
                usuarioTips<-paste(usuarioTips,categoria,sep = "\t")
                k<-k+1
            }
            j<-j+7+numCategoria+1
        }else{
            time<-userHistory[j+3]
            todo<-userHistory[j+4]
            done<-userHistory[j+5]
            numCategoria<-userHistory[j+6]
            numCategoria<-as.numeric(numCategoria)
            
            usuarioTips<-paste(userId,
                               venueId,
                               nulll,
                               text,
                               time,
                               todo,
                               done,
                               numCategoria,
                               sep = "\t")
            k<-1
            while (k <= numCategoria) {
                categoria<-userHistory[j+6+k]
                usuarioTips<-paste(usuarioTips,categoria,sep = "\t")
                k<-k+1
            }
            j<-j+6+numCategoria+1
            
        }
        
        cat(usuarioTips)
        cat("\n")
        
        colSize<-8+numCategoria
        if(colSize>maxColumnSize){
            maxColumnSize<-colSize
        }
    }
    
}
sink()
tips<-read.csv("LA-Tips.csv",sep = "\t",header = F,fill=TRUE,colClasses = c(rep("character",maxColumnSize)))
names(tips)<-c("user","venue","null","text","time","todo","done","numCategoria","cat1","cat2","cat3","cat4","cat5","cat6","cat7","cat8","cat9","cat10","cat11","cat12","cat13","cat14","cat15","cat16","cat17","cat18")
str(tips)
write.table(x = tips,file ="LA-Tips.csv",sep = "\t",row.names = FALSE)
tips<-read.csv("LA-Tips.csv",sep = "\t",header = T,colClasses = "character")
table(tips$null)



#### merge
library(plyr)
usuarioTips<-count(tips,c('user', 'venue'))
str(usuarioTips)
usuarioTips<- merge(usuarioTips,venues,by.x = 2,by.y = 1,all.x = T)
usuarioTips<- merge(usuarioTips,users,by.x = 2,by.y = 1,all.x = T)
str(usuarioTips)
write.table(x = usuarioTips,file ="LA-UsuarioTips.csv",sep = "\t",row.names = FALSE)
library(stringr)
usuarioTips$cityStateVenue<- usuarioTips$city
usuarioTips$cityStateVenue<- gsub(pattern = "[0-9]", "",x = usuarioTips$cityStateVenue)
usuarioTips$cityStateVenue<- toupper(str_trim(usuarioTips$cityStateVenue))
usuarioTips$isNY<- grepl(pattern = "^NY|NEW YORK",x = usuarioTips$cityStateVenue)
table(usuarioTips$isNY)
usuarioTips$homeCityClean<- usuarioTips$homeCity
usuarioTips$homeCityClean<- gsub(pattern = "[0-9]","",x = usuarioTips$homeCityClean)
usuarioTips$homeCityClean<- toupper(str_trim(usuarioTips$homeCityClean))
usuarioTips$isNJ<- grepl(pattern = "^NJ|NEW JERSEY",x = usuarioTips$homeCityClean)
usuarioTips$isMayor2<- usuarioTips$freq>2
usuarioTipsNY<- usuarioTips[usuarioTips$isNY==T & usuarioTips$isMayor2==T,c("user","numVenueId","freq")]
write.table(x = usuarioTipsNY,file ="NYC-UsuarioTipsClean.csv",sep = ",",row.names = FALSE, col.names = FALSE,quote = FALSE)
usuarioTipsNY[usuarioTipsNY$user==32611,]
length(unique(usuarioTipsNY$numVenueId))

sum(usuarioTips$cityStateVenue=="NULL")
head(usuarioTips[usuarioTips$cityStateVenue=="NULL",])

sum(usuarioTips$city=="null")
tf<-as.data.frame(table(usuarioTips$cityStateVenue)) 
tf<- tf[order(-tf$Freq),]
head(tf)
