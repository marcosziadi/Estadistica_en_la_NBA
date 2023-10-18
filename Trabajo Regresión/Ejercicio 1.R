options(scipen=999)
options(warn=-1)
library(ggplot2)
library(GGally)
data1 <- nba_2010
data2 <- players
df <- merge(data1,data2, by.x="Player", by.y="Player")


# 1) ¿Qué características presentan los jugadores estudiados?


# 1.1 Realice un análisis descriptivo univariado de las variables relevadas. ¿Hay valores
# atípicos? ¿Hay valores perdidos?
# ----

df_height <- density(df$height)
plot(df_height,
     main="Distribución de la altura de los jugadores",
     xlab = "Altura (cm)",
     ylab = "Frecuencia Relativa")
bp_height = boxplot(df$height,
                    main="Distribución de la altura de los jugadores",
                    ylab="Altura",
                    col="green3")
bp_height$out
hist(df$height,
     main="Distribución de la altura de los jugadores", 
     xlab="Altura (cm)",
     ylab="Frecuencia",
     col="coral")
summary(df$height)
sd(df$height)


df_weight <- density(df$weight)
plot(df_weight,
     main="Distribución del peso de los jugadores",
     xlab = "Peso (kg)",
     ylab = "Frecuencia Relativa")
bp_weight = boxplot(df$weight,
                    main="Distribución del peso de los jugadores",
                    ylab="Peso (kg)",
                    col="green3")
bp_weight$out
hist(df$weight,
     main="Distribución del peso de los jugadores", 
     xlab="Peso (kg)",
     ylab="Frecuencia",
     breaks = 15,
     col="coral")
summary(df$weight)
sd(df$weight)


df_age <- density(df$Age)
plot(df_age,
     main="Distribución de la edad de los jugadores",
     xlab = "Edad",
     ylab = "Frecuencia Relativa")
bp_age = boxplot(df$Age,
                 main="Distribución de la edad de los jugadores",
                 ylab="Edad",
                 col="green3")
bp_age$out
hist(df$Age,
     main="Distribución de la edad de los jugadores", 
     xlab="Edad",
     ylab="Frecuencia",
     col="coral")
summary(df$Age)
sd(df$Age)


df_pts <- density(df$PTS)
plot(df_pts,
     main="Distribución de los puntos anotados de los jugadores",
     xlab = "Puntos",
     ylab = "Frecuencia Relativa")
bp_pts = boxplot(df$PTS,
                 main="Distribución de los puntos anotados de los jugadores",
                 ylab="Puntos",
                 col="green3")
bp_pts$out
hist(df$PTS,
     main="Distribución de los puntos anotados de los jugadores", 
     xlab="Puntos",
     ylab="Frecuencia",
     col="coral")
summary(df$PTS)
sd(df$PTS)


df_mp <- density(df$MP)
plot(df_mp,
     main="Distribución de los minutos jugados por los jugadores",
     xlab = "Minutos",
     ylab = "Frecuencia Relativa")
bp_mp = boxplot(df$MP,
                main="Distribución de los minutos jugados por los jugadores",
                ylab="Minutos",
                col="green3")
bp_mp$out
hist(df$MP,
     main="Distribución de los minutos jugados por los jugadores", 
     xlab="Minutos",
     ylab="Frecuencia",
     breaks = 15,
     col="coral")
summary(df$MP)
sd(df$MP)


df_g <- density(df$G)
plot(df_g,
     main="Distribución de los partidos jugados por los jugadores",
     xlab = "Partidos",
     ylab = "Frecuencia Relativa")
bp_g = boxplot(df$G,
               main="Distribución de los partidos jugados por los jugadores",
               ylab="Partidos",
               col="green3")
bp_g$out
hist(df$G,
     main="Distribución de los partidos jugados por los jugadores", 
     xlab="Partidos",
     ylab="Frecuencia",
     breaks = 15,
     col="coral")
summary(df$G)
sd(df$G)


ggplot(df, aes(x = Pos, fill = Pos)) + geom_bar() + labs(fill = "Posición")

p1 <- 0
p2 <- 0
p1n <- 0
p2n <- 0
datos <- df[!is.na(df$perf), ]
for(i in 1:nrow(datos)){
  if(datos[i,8]==1){
    p1 = p1 + datos[i,9]
    p1n = p1n + 1
  }
  else{
    p2 = p2 + datos[i,9]
    p2n = p2n + 1
  }
}
performances <-  data.frame(Performance = c("Muy Buena", "Buena/Regular"),
                            PromedioPuntos = c(round(p1/p1n),round(p2/p2n)))
barplot(performances[, 2], names.arg = performances[, 1], xlab = "Performance", ylab = "Promedio de puntos", main = "Promedio de puntos por Performance",
        col = "blue", ylim = c(0, max(posiciones[, 2]) * 1.2))
text(x = barplot(performances[,2], names.arg = performances[, 1], plot = FALSE), y = performances[, 2], labels = performances[, 2], pos = 3)




pg_count <- 0
pf_count <- 0
sg_count <- 0
pgn <- 0
pfn <- 0
sgn <- 0

# unique(df[,4])
for(i in 1:nrow(df)){
  if(df[i,4]=="PG"){
    pg_count = pg_count + df[i,9]
    pgn = pgn + 1
  }
  else{
    if(df[i,4]=="PF-C"){
      pf_count = pf_count + df[i,9]
      pfn = pfn + 1
    }
    else{
      sg_count = sg_count + df[i,9]
      sgn = sgn + 1
    }
  }
}

posiciones <-  data.frame(Posicion = c("PG", "PF-C", "SG-SF"),
                          PromedioPuntos = c(round(pg_count/pgn),round(pf_count/pfn),round(sg_count/sgn)))
barplot(posiciones[, 2], names.arg = posiciones[, 1], xlab = "Posición", ylab = "Promedio de puntos", main = "Promedio de puntos por posición",
        col = "blue", ylim = c(0, max(posiciones[, 2]) * 1.2))
text(x = barplot(posiciones[, 2], names.arg = posiciones[, 1], plot = FALSE), y = posiciones[, 2], labels = posiciones[, 2], pos = 3)


for (col in colnames(df)){
  print(paste("Valores perdidos encontrados en la columna", col, "=", sum(is.na(df[col]))))
}
# [1] "Valores perdidos encontrados en la columna Player = 0"
# [1] "Valores perdidos encontrados en la columna Year = 0"
# [1] "Valores perdidos encontrados en la columna Age = 0"
# [1] "Valores perdidos encontrados en la columna Pos = 0"
# [1] "Valores perdidos encontrados en la columna Tm = 0"
# [1] "Valores perdidos encontrados en la columna G = 0"
# [1] "Valores perdidos encontrados en la columna MP = 0"
# [1] "Valores perdidos encontrados en la columna perf = 5"
# [1] "Valores perdidos encontrados en la columna PTS = 0"
# [1] "Valores perdidos encontrados en la columna height = 0"
# [1] "Valores perdidos encontrados en la columna weight = 0"
# Hay 5 valores perdidos, y dichos valores se encuentran en la columna llamada "perf"
# ----


# 1.2 ¿Qué relación tienen las variables consideradas explicativas con los puntos
# totales anotadas? ¿Entre ellas presentan alguna relación?
# Variables consideradas explicativas: "Age","Pos","G","MP","perf","height","weight"
# ----

unique(df$Pos)
df_modified <- df
df_modified$Player <- NULL
df_modified$Tm <- NULL
df_modified$Year <- NULL
df_modified$d1 <- ifelse(df$Pos=="PG",1,0)
df_modified$d2 <- ifelse(df$Pos=="PF-C",1,0)
df_modified$Pos <- NULL
ggcorr(df_modified, label = TRUE) 
cor(df_modified)
# ----

  
  