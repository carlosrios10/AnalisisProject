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
venues<-read.csv("NYC-Venues.csv",sep = "\t",header = T,colClasses = "character")
str(venues)
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
str(users)
users$userId<- as.numeric(users$userId)

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
tips<-read.csv("NYC-Tips.csv",sep = "\t",header = T,colClasses = "character")
#### merge
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
