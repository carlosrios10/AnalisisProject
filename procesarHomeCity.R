library(stringr)
library (plyr)
#####
count_tips_con_user_grafo_ny <- read.csv("count_tips_con_user_grafo_ny.csv",sep = ',',header = F)
names(count_tips_con_user_grafo_ny) <- c('user','venue','rating')
user_Total <- read.csv("NYC-Users.csv",sep = '\t',header = T,colClasses = 'character')
head(user_Total)
length(unique(user_Total$userId))
user_Total$userId <- as.numeric(user_Total$userId)

user_ny <-user_Total[user_Total$userId %in% unique(count_tips_con_user_grafo_ny$user),]
head(user_ny)
table(user_ny$homeCity)
str(user_ny)
### Normalizar UserCity
user_ny$homeCity2<-user_ny$homeCity
user_ny$homeCity<-user_ny$homeCity2

user_ny$homeCity<- tolower(user_ny$homeCity)
user_ny$homeCity<- str_trim(user_ny$homeCity)
user_ny$homeCity<- gsub('[0-9]',"",user_ny$homeCity)
user_ny$homeCity<- gsub('[.#:!~*]',"",user_ny$homeCity)
#user_ny$homeCity<- gsub("[^[:alnum:]///' ]","",user_ny$homeCity)
countHomeCity<-count(user_ny,c('homeCity'))
countHomeCity<- arrange(countHomeCity,desc(freq)) 

user_ny$homeCity[user_ny$homeCity=='ny']<-'new york, ny'
user_ny$homeCity[user_ny$homeCity=='ny, ny']<-'new york, ny'
user_ny$homeCity[user_ny$homeCity=='new york,ny']<-'new york, ny'
user_ny$homeCity[user_ny$homeCity==', ny']<-'new york, ny'
user_ny$homeCity[user_ny$homeCity=='x, ny']<-'new york, ny'
user_ny$homeCity[user_ny$homeCity=='new york ny']<-'new york, ny'
user_ny$homeCity[user_ny$homeCity=='new york, ny, united states']<-'new york, ny'
user_ny$homeCity[user_ny$homeCity=='new york ny']<-'new york, ny'
user_ny$homeCity[user_ny$homeCity=='new york, ny, usa']<-'new york, ny'
user_ny$homeCity[user_ny$homeCity=='new york,  ny']<-'new york, ny'
user_ny$homeCity[user_ny$homeCity=='new york , ny']<-'new york, ny'
user_ny$homeCity[user_ny$homeCity=='new york']<-'new york, ny'
user_ny$homeCity[user_ny$homeCity=='new york/ miami/ boston']<-'new york, ny'
user_ny$homeCity[user_ny$homeCity=='new york, ny and san francisco, ca']<-'new york, ny'
user_ny$homeCity[user_ny$homeCity=='new york, ny/philadelphia, pa']<-'new york, ny'
user_ny$homeCity[user_ny$homeCity=='st petersburg,fl & new york, ny']<-'new york, ny'
user_ny$homeCity[user_ny$homeCity=='new york, ny, ma']<-'new york, ny'
user_ny$homeCity[user_ny$homeCity=='nd ave new york, ny']<-'new york, ny'

user_ny$homeCity[user_ny$homeCity=='brooklyn ny']<-'brooklyn, ny'
user_ny$homeCity[user_ny$homeCity=='brooklyn,ny']<-'brooklyn, ny'
user_ny$homeCity[user_ny$homeCity=='nyc & brooklyn']<-'brooklyn, ny'
user_ny$homeCity[user_ny$homeCity=='brooklyn, nyc']<-'brooklyn, ny'
user_ny$homeCity[user_ny$homeCity=='brooklyn , ny']<-'brooklyn, ny'
user_ny$homeCity[user_ny$homeCity=='brooklyn/nyc']<-'brooklyn, ny'
user_ny$homeCity[user_ny$homeCity=='waslevittown pa/isbrooklyn nyc']<-'brooklyn, ny'
user_ny$homeCity[user_ny$homeCity=='greenpoint, brooklyn, nyc']<-'brooklyn, ny'
user_ny$homeCity[user_ny$homeCity=='brooklyn, ny york']<-'brooklyn, ny'
user_ny$homeCity[user_ny$homeCity=='brooklyn heights, ny']<-'brooklyn, ny'
user_ny$homeCity[user_ny$homeCity=="people's republic of brooklyn, ny"]<-'brooklyn, ny'
user_ny$homeCity[user_ny$homeCity=="non-hipster brooklyn, ny"]<-'brooklyn, ny'
user_ny$homeCity[user_ny$homeCity=="park slope, brooklyn, ny"]<-'brooklyn, ny'

user_ny$homeCity[grepl('\\queens\\b',x = user_ny$homeCity)]<-'queens, ny'
user_ny$homeCity[user_ny$homeCity=="the bronx, nyc"]<-'bronx, ny'
user_ny$homeCity[user_ny$homeCity=="bronx ,ny"]<-'bronx, ny'
user_ny$homeCity[user_ny$homeCity=="north bronx, nyc"]<-'bronx, ny'
user_ny$homeCity[user_ny$homeCity=="bronx ny"]<-'bronx, ny'
user_ny$homeCity[user_ny$homeCity=="the bronx, ny"]<-'bronx, ny'
user_ny$homeCity[user_ny$homeCity=="bronx,ny"]<-'bronx, ny'
user_ny$homeCity[user_ny$homeCity=="city island, bronx, nyc"]<-'bronx, ny'
user_ny$homeCity[user_ny$homeCity=="astoria,ny"]<-'astoria, ny'
user_ny$homeCity[user_ny$homeCity=="astoria nyc"]<-'astoria, ny'
user_ny$homeCity[grepl('staten island',x = user_ny$homeCity)]<-'staten island, ny'
user_ny$homeCity[grepl('new york city',x = user_ny$homeCity)]<-'new york city, ny'
user_ny$homeCity[grepl('flushing',x = user_ny$homeCity)]<-'flushing, ny'
user_ny$homeCity[grepl('forest hills',x = user_ny$homeCity)]<-'forest hills, ny'
user_ny$homeCity[grepl('yonkers',x = user_ny$homeCity)]<-'yonkers, ny'
user_ny$homeCity[grepl('white plains',x = user_ny$homeCity)]<-'white plains, ny'
user_ny$homeCity[grepl('manhattan',x = user_ny$homeCity)]<-'manhattan, ny'
user_ny$homeCity[grepl('syracuse',x = user_ny$homeCity)]<-'syracuse, ny'
user_ny$homeCity[grepl('westbury',x = user_ny$homeCity)]<-'westbury, ny'
user_ny$homeCity[grepl('albany',x = user_ny$homeCity)]<-'albany, ny'
user_ny$homeCity[grepl('jamaica',x = user_ny$homeCity)]<-'jamaica, ny'
user_ny$homeCity[grepl('ithaca',x = user_ny$homeCity)]<-'ithaca, ny'
user_ny$homeCity[grepl('farmingdale',x = user_ny$homeCity)]<-'farmingdale, ny'
user_ny$homeCity[grepl('huntington',x = user_ny$homeCity)]<-'huntington, ny'
user_ny$homeCity[grepl('long island',x = user_ny$homeCity)]<-'long island, ny'
user_ny$homeCity[grepl('new hyde park',x = user_ny$homeCity)]<-'new hyde park, ny'
user_ny$homeCity[grepl('pelham',x = user_ny$homeCity)]<-'pelham, ny'
user_ny$homeCity[grepl('elmhurst',x = user_ny$homeCity)]<-'elmhurst, ny'
user_ny$homeCity[grepl('sunnyside',x = user_ny$homeCity)]<-'sunnyside, ny'
user_ny$homeCity[grepl('kew gardens',x = user_ny$homeCity)]<-'kew gardens, ny'
user_ny$homeCity[grepl('saratoga springs',x = user_ny$homeCity)]<-'saratoga springs, ny'
user_ny$homeCity[grepl('garden city',x = user_ny$homeCity)]<-'garden city, ny'
user_ny$homeCity[grepl('nyack',x = user_ny$homeCity)]<-'nyack, ny'
user_ny$homeCity[grepl('patchogue',x = user_ny$homeCity)]<-'patchogue, ny'
user_ny$homeCity[grepl('larchmont',x = user_ny$homeCity)]<-'larchmont, ny'
user_ny$homeCity[grepl('nyc',x = user_ny$homeCity)]<-'new york city, ny'
user_ny$homeCity[grepl('middle village',x = user_ny$homeCity)]<-'middle village, ny'
user_ny$homeCity[grepl('hempstead',x = user_ny$homeCity)]<-'hempstead, ny'
user_ny$homeCity[grepl('massapequa',x = user_ny$homeCity)]<-'massapequa, ny'
user_ny$homeCity[grepl('roslyn',x = user_ny$homeCity)]<-'roslyn, ny'
user_ny$homeCity[grepl('bethpage',x = user_ny$homeCity)]<-'bethpage, ny'
user_ny$homeCity[grepl('oceanside',x = user_ny$homeCity)]<-'oceanside, ny'
user_ny$homeCity[grepl('mastic',x = user_ny$homeCity)]<-'mastic, ny'
user_ny$homeCity[grepl('sag harbor',x = user_ny$homeCity)]<-'sag harbor, ny'
user_ny$homeCity[grepl('harlem',x = user_ny$homeCity)]<-'harlem, ny'
user_ny$homeCity[grepl('merrick',x = user_ny$homeCity)]<-'merrick, ny'
user_ny$homeCity[grepl('vestal',x = user_ny$homeCity)]<-'vestal, ny'
user_ny$homeCity[user_ny$homeCity=="li, ny"]<-'long island, ny'
user_ny$homeCity[user_ny$homeCity=="lic, ny"]<-'long island, ny'
user_ny$homeCity[grepl('babylon',x = user_ny$homeCity)]<-'babylon, ny'
user_ny$homeCity[grepl('bronxville',x = user_ny$homeCity)]<-'bronxville, ny'
user_ny$homeCity[grepl('islip',x = user_ny$homeCity)]<-'islip, ny'
user_ny$homeCity[grepl('little neck',x = user_ny$homeCity)]<-'little neck, ny'
user_ny$homeCity[grepl('mineola',x = user_ny$homeCity)]<-'mineola, ny'
user_ny$homeCity[grepl('sea cliff',x = user_ny$homeCity)]<-'sea cliff, ny'
user_ny$homeCity[grepl('suffolk',x = user_ny$homeCity)]<-'suffolk, ny'
user_ny$homeCity[grepl('ozone park',x = user_ny$homeCity)]<-'ozone park, ny'
user_ny$homeCity[grepl('northport',x = user_ny$homeCity)]<-'northport, ny'
user_ny$homeCity[grepl('nassau',x = user_ny$homeCity)]<-'nassau, ny'
user_ny$homeCity[grepl('ronkonkoma',x = user_ny$homeCity)]<-'ronkonkoma, ny'
user_ny$homeCity[grepl('rye',x = user_ny$homeCity)]<-'rye brook, ny'
user_ny$homeCity[grepl('hollis',x = user_ny$homeCity)]<-'hollis, ny'
user_ny$homeCity[grepl('richmond hill',x = user_ny$homeCity)]<-'richmond hill, ny'
user_ny$homeCity[grepl('rockland',x = user_ny$homeCity)]<-'rockland, ny'
user_ny$homeCity[grepl('yorktown',x = user_ny$homeCity)]<-'yorktown, ny'
user_ny$homeCity[grepl('new rochelle',x = user_ny$homeCity)]<-'new rochelle, ny'
user_ny$homeCity[grepl('bayside',x = user_ny$homeCity)]<-'bayside, ny'
user_ny$homeCity[grepl('hyde park',x = user_ny$homeCity)]<-'hyde park, ny'
user_ny$homeCity[grepl('malverne',x = user_ny$homeCity)]<-'malverne, ny'
user_ny$homeCity[grepl('amherst',x = user_ny$homeCity)]<-'amherst, ny'
user_ny$homeCity[grepl('new york, ny/usa',x = user_ny$homeCity)]<-'new york, ny'
user_ny$homeCity[grepl('springfield gardens',x = user_ny$homeCity)]<-'springfield gardens, ny'
user_ny$homeCity[grepl('jackson hts',x = user_ny$homeCity)]<-'jackson hts, ny'
user_ny$homeCity[grepl('fire island',x = user_ny$homeCity)]<-'fire island, ny'
user_ny$homeCity[grepl('east setauket',x = user_ny$homeCity)]<-'east setauket, ny'
user_ny$homeCity[grepl('nj',x = user_ny$homeCity)]<-'new york, ny'
user_ny$homeCity[grepl('laurelton',x = user_ny$homeCity)]<-'laurelton, ny'
user_ny$homeCity[grepl('highland',x = user_ny$homeCity)]<-'highland, ny'
user_ny$homeCity[grepl('uptown',x = user_ny$homeCity)]<-'uptown, ny'
user_ny$homeCity[grepl('ulster',x = user_ny$homeCity)]<-'ulster, ny'
user_ny$homeCity[grepl('stuyvesant',x = user_ny$homeCity)]<-'stuyvesant, ny'
user_ny$homeCity[grepl('bushwick',x = user_ny$homeCity)]<-'bushwick, ny'
user_ny$homeCity[grepl('uptown',x = user_ny$homeCity)]<-'uptown, ny'
user_ny$homeCity[grepl('shelter island',x = user_ny$homeCity)]<-'shelter island, ny'
user_ny$homeCity[user_ny$homeCity=="golden's bridge, ny"]<-'goldens bridge, ny'
user_ny$homeCity[user_ny$homeCity=="bridgehanpton, ny"]<-'bridgehampton, ny'
user_ny$homeCity[grepl('bedford',x = user_ny$homeCity)]<-'bedford, ny'
user_ny$homeCity[grepl('atlantic beach',x = user_ny$homeCity)]<-'atlantic beach, ny'
user_ny$homeCity[user_ny$homeCity=="si, ny"]<-'staten island, ny'
user_ny$homeCity[grepl('saratoga',x = user_ny$homeCity)]<-'saratoga, ny'
user_ny$homeCity[user_ny$homeCity=="brookly, ny"]<-'brooklyn, ny'
user_ny$homeCity[user_ny$homeCity=="roosevelt, ny"]<-'roosevelt island, ny'
user_ny$homeCity[grepl('port jefferson',x = user_ny$homeCity)]<-'port jefferson, ny'
user_ny$homeCity[grepl('woodmere',x = user_ny$homeCity)]<-'woodmere, ny'
user_ny$homeCity[grepl('amherst',x = user_ny$homeCity)]<-'amherst, ny'
user_ny$homeCity[user_ny$homeCity=="ny,ny"]<-'new york, ny'
user_ny$homeCity[user_ny$homeCity=="newyork,ny"]<-'new york, ny'
user_ny$homeCity[user_ny$homeCity=="new yokr, ny"]<-'new york, ny'
user_ny$homeCity[user_ny$homeCity=="la & ny"]<-'new york, ny'
user_ny$homeCity[user_ny$homeCity=="ny/la/seattle"]<-'new york, ny'
user_ny$homeCity[user_ny$homeCity=="ny/tx"]<-'new york, ny'
user_ny$homeCity[user_ny$homeCity=="ny, the united states of"]<-'new york, ny'


countHomeCity[grepl("ny",x = countHomeCity$homeCity),]
for(i in 1:nrow(countHomeCity)){
    var <- str_split_fixed(countHomeCity$homeCity[i],',',n=2)
    nr<-nrow(countHomeCity[grepl(var[1],x = countHomeCity$homeCity),])
    if(nr>1)
        print(paste(var[1],nr,sep = ':'))
}

#####busco condados
countHomeCity<-count(user_ny,c('homeCity'))
countHomeCity<- arrange(countHomeCity,desc(freq))  
length(unique(countHomeCity$homeCity))
locations <- ldply(countHomeCity$homeCity, function(x) geoCode(x))
locations <- procesarLocation(locations)
table(locations$county,useNA = 'always')
countHomeCity <- cbind(countHomeCity,locations)

countHomeCity[is.na(countHomeCity$county),]
countHomeCity$county[countHomeCity$homeCity=='new york, ny'] <- 'new york, ny'
countHomeCity$county[countHomeCity$homeCity=='new york city, ny'] <- 'new york city, ny'
countHomeCity$county[countHomeCity$homeCity=='long island, ny'] <- 'long island, ny'
countHomeCity$county[countHomeCity$homeCity=='ny metro area'] <- 'ny metro area'
countHomeCity$county[countHomeCity$homeCity=='boros,ny'] <- 'boros,ny'
countHomeCity$county[countHomeCity$homeCity=='yo, ny'] <- 'yo, ny'
countHomeCity$county[countHomeCity$homeCity=='west new york, ny'] <- 'west new york, ny'
countHomeCity$county[countHomeCity$homeCity=='gotham, ny'] <- 'Bronx County'
countHomeCity$county[countHomeCity$homeCity=='the village, new york, ny'] <- 'Bronx County'
countHomeCity$county[countHomeCity$homeCity=='yoektown stage, ny'] <- 'Westchester County'
length(unique(countHomeCity$homeCity))
length(unique(user_ny$homeCity))
uniqueCounty <- count(countHomeCity,c('county'))
uniqueCounty$idCounty <- 1:nrow(uniqueCounty)
uniqueCounty$freq <- NULL

countHomeCity <- merge(countHomeCity,uniqueCounty,by.x=c('county'),by.y=c('county'),all.x = T)
user_ny<- merge(user_ny,countHomeCity,by.x =c('homeCity') ,by.y = c('homeCity'),all.x = T)
table(user_ny$idCounty,useNA = 'always')


head(user_ny)
head(user_ny[,c('userId','V1','V2','idCounty')])
user_ny[user_ny$userId==81864,]

write.table(x = countHomeCity,file ="countHomeCity.csv",sep = "\t",row.names = FALSE, col.names = T,quote = FALSE)
write.table(x = locations,file ="countHomeCityCounty.csv",sep = "\t",row.names = FALSE, col.names = T,quote = FALSE)
write.table(x = uniqueCounty,file ="countyNY.csv",sep = "\t",row.names = FALSE, col.names = T,quote = FALSE)
write.table(x = user_ny[,c('userId','V1','V2','idCounty')],file ="users_tips_ny_grafo.csv",sep = ",",row.names = FALSE, col.names = T,quote = FALSE)
############### le agrego el id state
user_ny =  read.csv(file = "users_tips_ny_grafo.csv")
str(user_ny)
head(user_ny)
user_ny$idState = 1
write.table(x = user_ny,file ="users_tips_ny_grafo.csv",sep = ",",row.names = FALSE, col.names = T,quote = FALSE)

