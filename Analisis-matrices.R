library(ggplot2)
library(plyr)
# Matriz visitas ----------------------------------------------------------
vistas_todos_usuarios = read.csv(file = "count_tips_con_user_grafo.csv" ,header = F)
colnames(vistas_todos_usuarios) = c("user","numVenueId","rating")
head(vistas_todos_usuarios)
str(vistas_todos_usuarios)

ggplot(vistas_todos_usuarios, aes(x=(rating))) + geom_histogram(bins=4)

ggplot(vistas_todos_usuarios, aes(x=rating)) +
    geom_histogram(bins=4) +
    scale_x_continuous(expand=c(0, 1)) +
    scale_y_continuous(expand=c(0.015,0)) +
    theme_bw()

ggplot(vistas_todos_usuarios, aes(x=rating)) + 
    geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                   binwidth=.5,
                   colour="black", fill="white") +
    geom_density(alpha=.2, fill="#FF6666")

ggplot(vistas_todos_usuarios, aes(x=log(rating))) + geom_density()

ggplot(vistas_todos_usuarios, aes(log(rating)))+
    geom_freqpoly(binwidth = 1)+
    scale_x_continuous(breaks=c(1,2,3,4,5))+  
    xlab("Freq. de visitas usuario-lugar") +  
    ylab("Cantidad") +  
    theme(legend.title=element_blank()) + 
    theme(legend.position="bottom")

ggplot(visitas, aes(x=1, y=rating)) + geom_boxplot()
