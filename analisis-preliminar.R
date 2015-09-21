### venues ####
venuesLines<-readLines("../FourSquareDataset/Venues/NYC/NYC-Venues.txt")
venuesLines <- gsub("\"","",venuesLines)
listVenues<-strsplit(venuesLines,split = "\t")
max(sapply(listVenues, length)) 
table(sapply(listVenues, length))
min(sapply(listVenues, length))
sink("NYC-Venues.csv")
for(i in 1:length(venuesLines)){
    linea<-strsplit(venuesLines[i],split = "\t")
    cat(linea[[1]][1:14],sep = "\t")
    cat("\n")
}
sink()
venues<-read.csv("NYC-Venues.csv",sep = "\t",header = F,colClasses = "character")

##########  users
usuersLine<-readLines("../FourSquareDataset/Users/NYC/NYC-Users.txt")
usuersLine<-gsub("\"","",usuersLine)
head(usuersLine)
listUsers<-strsplit(usuersLine,split = "\t")
max(sapply(usuersLine,length))
sink("NYC-Users.csv")
for(i in 1:length(usuersLine)){
    linea<-strsplit(usuersLine[i],split = "\t")
    cat(linea[[1]],sep = "\t")
    cat("\n")
}
sink()
users<-read.csv("NYC-Users.csv",sep = "\t",header = F,colClasses = "character")

#### tips
tipsLine<-readLines("../FourSquareDataset/Tips/NYC/NYC-Tips.txt")
tipsLine<-gsub("\"","",tipsLine)
head(tipsLine)
listTips<-strsplit(tipsLine,split = "\t")
max(sapply(listTips, length))
min(sapply(listTips, length))

sink("NYC-Tips.csv")
maxColumnSize<-0
for(i in 1:3) {
    userHistory<-listTips[[i]]
    userId<-userHistory[1]
        j<-2
        while (j<length(userHistory)) {
            venueId<-userHistory[j]
            nulll<-userHistory[j+1]
            text<-userHistory[j+2]
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
            cat(usuarioTips)
            cat("\n")
            j<-j+6+numCategoria+1
            colSize<-8+numCategoria
            if(colSize>maxColumnSize){
            maxColumnSize<-colSize
            }
    }
    
}
sink()
tips<-read.csv("NYC-Tips.csv",sep = "\t",header = F,fill=TRUE,colClasses = c(rep("character",maxColumnSize)))
names(tips)<-c("user","venue","null","text","time","todo","done","numCategoria","cat1","cat2","cat3","cat4","cat5","cat6","cat7","cat8","cat9","cat10","cat11","cat12","cat13","cat14","cat15","cat16","cat17","cat18")
names(tips)<-c("user","venue","null","text","time","todo","done","numCategoria","cat1","cat2","cat3","cat4","cat5")

write.table(x = tips,file ="NYC-Tips2.csv",sep = "\t")
tips2<-read.csv("NYC-Tips2.csv",sep = "\t",header =T)


