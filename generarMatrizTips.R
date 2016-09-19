library(plyr)
library(ggplot2)
usuario_lugar <- read.csv(file = 'count_tips_con_user_grafo.csv',header = F)
names(usuario_lugar)<- c('usuario','lugar','rating')
str(usuario_lugar)
nyc_tips_all = read.csv( file ="NYC-TipsVenueUserAll.csv",sep = "\t")
head(nyc_tips_all)
str(nyc_tips_all)
nyc_tips_all$text = as.character(nyc_tips_all$text)

usuario_lugar_tips = ddply(nyc_tips_all, .(user,numVenueId), summarize, 
                           text_concatenated = paste(text, collapse = " "),
                           freq=length(user))
str(usuario_lugar_tips)
head(usuario_lugar_tips)
write.table(x = usuario_lugar_tips,file ="usuario_lugar_tips_concat.csv",sep = "\t",row.names = FALSE)
hist(usuario_lugar_tips$freq)
table(usuario_lugar_tips$freq)
table(usuario_lugar$rating)

tips_con_senti = read.csv(file='usuario_lugar_tips_concat_con_sent.csv',sep="\t")
tips_con_senti$text_concatenated = as.character(tips_con_senti$text_concatenated)
tips_con_senti$inlogit = invlogit(tips_con_senti$senti)
str(tips_con_senti)
summary(tips_con_senti[,c(-1,-2,-3)])
hist(tips_con_senti$senti)
hist(tips_con_senti$subje)
hist(tips_con_senti$freq)
hist(tips_con_senti$inlogit)
head(tips_con_senti)
plot(log(tips_con_senti[tips_con_senti$freq>1,]$freq),(tips_con_senti[tips_con_senti$freq>1,]$inlogit))
plot((tips_con_senti$subje),(tips_con_senti$senti))
model = lm(senti ~ subje ,data = tips_con_senti)
summary(model) 
boxplot(tips_con_senti$freq)
boxplot(tips_con_senti$senti ~ tips_con_senti$freq  )

ggplot(data = tips_con_senti,mapping = aes(x = senti)) + geom_histogram()
ggplot(data = tips_con_senti, aes(x = senti)) + geom_histogram(binwidth = 5,color="blue")

table(tips_con_senti$freq)
sort(tapply(tips_con_senti$senti, tips_con_senti$freq, mean))
log(22)
which(tips_con_senti$senti== -1)
tips_con_senti[393827,]
tips_con_senti[204512,]


#############
usuario_lugar = merge(usuario_lugar,tips_con_senti,by.x = c('usuario','lugar'),by.y = c('user','numVenueId'),all.x = T)
str(usuario_lugar)
boxplot(usuario_lugar$senti)
write.table(x = usuario_lugar[,c('usuario','lugar','senti')],
            file ="count_tips_con_user_grafo_tips_senti.csv",sep = ",",
            row.names = FALSE,
            quote = FALSE,
            col.names = F)
###########
usuario_lugar_ny <- read.csv(file = 'count_tips_con_user_grafo_ny.csv',header = F)
names(usuario_lugar_ny)<- c('usuario','lugar','rating')
str(usuario_lugar_ny)
usuario_lugar_ny = merge(usuario_lugar_ny,tips_con_senti,by.x = c('usuario','lugar'),by.y = c('user','numVenueId'),all.x = T)
write.table(x = usuario_lugar_ny[,c('usuario','lugar','senti')],
            file ="count_tips_con_user_grafo_ny_tips_senti.csv",sep = ",",
            row.names = FALSE,
            quote = FALSE,
            col.names = F)


# Discretizar en 5 valores ------------------------------------------------
usuario_lugar = read.csv(file='count_tips_con_user_grafo_tips_senti.csv',header = F)
names(usuario_lugar) = c('usuario','lugar','senti')
str(usuario_lugar)
ggplot(data = usuario_lugar,mapping = aes(x = senti)) + geom_histogram(bins = 5)

# Discretizacion1 ---------------------------------------------------------

# [-1,0) -> 1
# [0,0.25) -> 2
# [0.25,0.5) -> 3
# [0.5,0.75) -> 4
# [0.75,0.1] -> 5
prefer <- cut(usuario_lugar$senti, breaks = c(-1, 0, 0.25, 0.5, 0.75, 1), labels = c(1:5),right = F,include.lowest = T) 
table(prefer, useNA = 'always')
usuario_lugar$discretSenti = prefer
usuario_lugar$discretSenti =  as.numeric(as.character(usuario_lugar$discretSenti))
head(usuario_lugar)
str(usuario_lugar)

ggplot(data = usuario_lugar,mapping = aes(x = discretSenti)) + geom_histogram(bins = 5)


write.table(x = usuario_lugar[,c('usuario','lugar','discretSenti')],
            file ="count_tips_con_user_grafo_tips_senti_discret.csv",sep = ",",
            row.names = FALSE,
            quote = FALSE,
            col.names = F)


# Discretizacion2 ---------------------------------------------------------

# [-1,-0.6) -> 1
# [-0.6,-0.2) -> 2
# [-0.2,0.2) -> 3
# [0.2,0.6) -> 4
# [0.6,1] -> 5
prefer <- cut(usuario_lugar$senti, breaks = c(-1, -0.6, -0.2, 0.2, 0.6, 1), labels = c(1:5),right = F,include.lowest = T) 
table(prefer, useNA = 'always')
usuario_lugar$discretSenti = prefer
usuario_lugar$discretSenti =  as.numeric(as.character(usuario_lugar$discretSenti))
head(usuario_lugar)
tail(usuario_lugar)
str(usuario_lugar)
head(usuario_lugar[usuario_lugar$senti==0,])
sum(usuario_lugar$senti==0)
sum(usuario_lugar$discretSenti==3)

ggplot(data = usuario_lugar,mapping = aes(x = discretSenti)) + geom_histogram(bins = 5)


write.table(x = usuario_lugar[,c('usuario','lugar','discretSenti')],
            file ="count_tips_con_user_grafo_tips_senti_discret_2.csv",sep = ",",
            row.names = FALSE,
            quote = FALSE,
            col.names = F)
# Discretizacion NY -------------------------------------------------------


usuario_lugar_ny = read.csv(file='count_tips_con_user_grafo_ny_tips_senti.csv',header = F)
names(usuario_lugar_ny) = c('usuario','lugar','senti')
str(usuario_lugar_ny)
sum(usuario_lugar_ny$senti==0)
ggplot(data = usuario_lugar_ny,mapping = aes(x = senti)) + geom_histogram(bins = 5)

# Discretizacion1 ---------------------------------------------------------


# [-1,0) -> 1
# [0,0.25) -> 2
# [0.25,0.5) -> 3
# [0.5,0.75) -> 4
# [0.75,0.1] -> 5
prefer <- cut(usuario_lugar_ny$senti, breaks = c(-1, 0, 0.25, 0.5, 0.75, 1), labels = c(1:5),right = F,include.lowest = T) 
table(prefer, useNA = 'always')
usuario_lugar_ny$discretSenti = prefer
head(usuario_lugar_ny)


write.table(x = usuario_lugar_ny[,c('usuario','lugar','discretSenti')],
            file ="count_tips_con_user_grafo_ny_tips_senti_discret.csv",sep = ",",
            row.names = FALSE,
            quote = FALSE,
            col.names = F)

# Discretizacion2 ---------------------------------------------------------
# [-1,-0.6) -> 1
# [-0.6,-0.2) -> 2
# [-0.2,0.2) -> 3
# [0.2,0.6) -> 4
# [0.6,1] -> 5
prefer <- cut(usuario_lugar_ny$senti, breaks = c(-1, -0.6, -0.2, 0.2, 0.6, 1), labels = c(1:5),right = F,include.lowest = T) 
table(prefer, useNA = 'always')
usuario_lugar_ny$discretSenti = prefer
usuario_lugar_ny$discretSenti = as.numeric(as.character(usuario_lugar_ny$discretSenti))
str(usuario_lugar_ny)
table(usuario_lugar_ny$discretSenti, useNA = 'always')
head(usuario_lugar_ny)
ggplot(data = usuario_lugar_ny,mapping = aes(x = discretSenti)) + geom_histogram(bins = 5)

write.table(x = usuario_lugar_ny[,c('usuario','lugar','discretSenti')],
            file ="s",sep = ",",
            row.names = FALSE,
            quote = FALSE,
            col.names = F)

# ****************** ------------------------------------------------------
DFsenti_discret_2 = read.csv(file = 'count_tips_con_user_grafo_ny_tips_senti_discret_2.csv',header = F)
head(DFsenti_discret_2)
str(DFsenti_discret_2)
hist(DFsenti_discret_2$V3)
frq = count(DFsenti_discret_2,vars = c('V1'))
frq = frq[frq$freq>5,]
head(frq)
table(frq$freq)
hist(frq$freq)
138873- (138873*0.8)
27774.6- 26075
182038- (182038*0.8)
36407.6- 34000
df2= merge(DFsenti_discret_2,frq,by.x = c('V1'),by.y = c('V1'))
str(df2)

write.table(x = df2[,c('V1','V2','V3')],
            file ="count_tips_con_user_grafo_ny_tips_senti_discret_2_reducido.csv",sep = ",",
            row.names = FALSE,
            quote = FALSE,
            col.names = F)

