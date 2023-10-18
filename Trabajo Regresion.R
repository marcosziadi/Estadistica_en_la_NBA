options(scipen=999)
options(warn=-1)
# Librerias  ----
install.packages("GGally")
install.packages("ggplot2")
install.packages("nortest")
install.packages("lmtest")
install.packages("car")
library(lmtest)
library(nortest)
library(GGally)
library(ggplot2)
library(car)
# Data ----
data1 <- nba_2010
data2 <- players
df <- merge(data1,data2, by.x="Player", by.y="Player")

# unique(df$Pos)
df_modified <- df
df_modified$Player <- NULL
df_modified$Tm <- NULL
df_modified$Year <- NULL
df_modified$d1 <- ifelse(df$Pos=="PG",1,0)
df_modified$d2 <- ifelse(df$Pos=="PF-C",1,0)
df_modified$Pos <- NULL

# -------------------------------------------------------------------------------
# Se realizaron las siguientes preguntas:
#   1- ¿A mayor altura del jugador, mayor cantidad de puntos anotados?
#   2- ¿La posición de juego influye en los puntos anotados?
#   3- ¿Cómo influye la edad en la cantidad de puntos anotados?
#   4- ¿Influye en los puntos anotados la cantidad de minutos que el jugador estuvo
# en el campo de juego?
#   5- ¿La performance histórica del jugador influye en el total de puntos anotados
# en esa temporada?


# 1- ¿A mayor altura del jugador, mayor cantidad de puntos anotados?
modelo_pregunta1 <- lm(PTS~height,data=df)
modelo_pregunta1

plot(data=df, 
     PTS~height, 
     main="Altura vs Puntos Anotados",
     xlab="Altura", 
     ylab="Puntos Anotados", 
     col="deepskyblue3",
     pch=19)
abline(modelo_pregunta1)
summary(modelo_pregunta1)
anova(modelo_pregunta1)
qf(0.95,1,385)
# Fobs<Fcrit -> No rechazo H0, no aporta significativamente  la altura

# 2- ¿La posición de juego influye en los puntos anotados?
unique(df$Pos)
df_modified$d1 <- ifelse(df$Pos=="PG",1,0)
df_modified$d2 <- ifelse(df$Pos=="PF-C",1,0)
modelo_pregunta2 <- lm(PTS~d1+d2,data=df_modified)
modelo_pregunta2

plot(data=df_modified, 
     PTS~d1+d2, 
     main="Relación altura del jugadors vs cantidad de puntos anotados",
     xlab="Altura", 
     ylab="Puntos anotados", 
     col="deepskyblue3",
     pch=19)
abline(modelo_pregunta1)
summary(modelo_pregunta1)
anova(modelo_pregunta2)
# PG    -> 1
# PF-C  -> 2
# SG-SF -> 3
pg_count <- 0
pf_count <- 0
sg_count <- 0
total_points_pos <- dplyr::count(df_modified, df_modified$Pos, sort = TRUE)
for (i in 1:length(df_modified)){
  if(df_modified[i,3]==1){pg_count = pg_count + df_modified[i,7]}
  else if(df_modified[i,3]==2){pf_count = pf_count + df_modified[i,7]}
  else {sg_count = sg_count + df_modified[i,7]}
}

# pg_count 
# pf_count 
# sg_count 
avg_points <- data.frame(matrix(nrow=3, ncol=2))
avg_points[1,2] <- pg_count/total_points_pos[3,2]
avg_points[2,2] <- pf_count/total_points_pos[2,2]
avg_points[3,2] <- sg_count/total_points_pos[1,2]
avg_points[1,1] <- "PG"
avg_points[2,1] <- "PF/C"
avg_points[3,1] <- "SG/SF"

avg_points$X1 <- factor(avg_points$X1)

ggplot(avg_points, aes(x = X1, y = X2, fill=X1)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(X2, 2)), vjust = -0.5, color = "black", size = 3) +
  xlab("Posición") +
  ylab("Promedio de Puntos") +
  ggtitle("Promedio de Puntos Anotados por Posición") +
  theme_minimal() +
  labs(fill = "Posiciones") +
  theme(axis.title = element_text(size = 14),
        plot.title = element_text(size = 16)) +
  guides(fill = FALSE)


# 
# pg_count_1 <- 0
# pf_count_1 <- 0
# sg_count_1 <- 0
# pg_count_0 <- 0
# pf_count_0 <- 0
# sg_count_0 <- 0
# pg1 <-0
# pf1 <-0
# sg1 <-0
# pg0 <-0
# pf0 <-0
# sg0 <-0
# 
# for (i in 1:length(df_modified)){
#   if(df_modified[i,3]==1 && df_modified[i,6]==1){
#     pg_count_1 = pg_count_1 + df_modified[i,7]
#     pg1 = pg1 + 1}
#   else if(df_modified[i,3]==1 && df_modified[i,6]==0){
#     pg_count_0 = pg_count_0 + df_modified
#     pg0 = pg0 + 1}
#   else if(df_modified[i,3]==2 && df_modified[i,6]==1){
#     pf_count_1 = pf_count_1 + df_modified[i,7]
#     pf1 = pf1 + 1}
#   else if(df_modified[i,3]==2 && df_modified[i,6]==0){
#     pf_count_0 = pf_count_0 + df_modified
#     pf0 = pf0 + 1}
#   else if(df_modified[i,3]==3 && df_modified[i,6]==1){
#     sg_count_1 = sg_count_1 + df_modified
#     sg1 = sg1 + 1}
#   else {sg_count_0 = sg_count_0 + df_modified[i,7]
#   sg0 = sg0 + 1}
# }
# 
# 
# avg_points1 <- data.frame(matrix(nrow=3, ncol=2))
# avg_points1[1,2] <- pg_count_1/pg1
# avg_points1[2,2] <- pf_count_1/pf1
# avg_points1[3,2] <- sg_count_1/sg1
# avg_points1[1,1] <- "PG 1" 
# avg_points1[2,1] <- "PF/C 1"
# avg_points1[3,1] <- "SG/SF 1"
# avg_points2 <- data.frame(matrix(nrow=3, ncol=2))
# avg_points2[1,2] <- pg_count_0/pg0
# avg_points2[2,2] <- pf_count_0/pf0
# avg_points2[3,2] <- sg_count_0/sg0
# avg_points2[1,1] <- "PG 0"
# avg_points2[2,1] <- "PF/C 0"
# avg_points2[3,1] <- "SG/SF 0"
# 
# ggplot(avg_points1, aes(x = X1, y = X2, fill=X1)) +
#   geom_bar(stat = "identity") +
#   geom_text(aes(label = round(X2, 2)), vjust = -0.5, color = "black", size = 3) +
#   xlab("Posición") +
#   ylab("Promedio de Puntos") +
#   ggtitle("Promedio de Puntos Anotados por Posición") +
#   theme_minimal() +
#   labs(fill = "Posiciones") +
#   theme(axis.title = element_text(size = 14),
#         plot.title = element_text(size = 16)) +
#   guides(fill = FALSE)
# 
# ggplot(avg_points2, aes(x = X1, y = X2, fill=X1)) +
#   geom_bar(stat = "identity") +
#   geom_text(aes(label = round(X2, 2)), vjust = -0.5, color = "black", size = 3) +
#   xlab("Posición") +
#   ylab("Promedio de Puntos") +
#   ggtitle("Promedio de Puntos Anotados por Posición") +
#   theme_minimal() +
#   labs(fill = "Posiciones") +
#   theme(axis.title = element_text(size = 14),
#         plot.title = element_text(size = 16)) +
#   guides(fill = FALSE)

#   3- ¿Cómo influye la edad en la cantidad de puntos anotados?
modelo_pregunta3 <- lm(PTS~Age,data=df)
modelo_pregunta3

plot(data=df, 
     PTS~Age, 
     main="Edad vs Puntos Anotados",
     xlab="Edad", 
     ylab="Puntos Anotados", 
     col="green3",
     pch=19)
abline(modelo_pregunta3)
summary(modelo_pregunta3)
anova(modelo_pregunta3)

#   4- ¿Influye en los puntos anotados la cantidad de minutos que el jugador estuvo
# en el campo de juego?
modelo_pregunta4 <- lm(PTS~MP,data=df)
modelo_pregunta4

plot(data=df, 
     PTS~MP, 
     main="Minutos Jugados vs Puntos Anotados",
     xlab="Minutos Jugados", 
     ylab="Puntos Anotados", 
     col="purple",
     cex.main = 3,  # Increase chart title font size
     cex.lab = 1.8,   # Increase axis title font size
     pch=19)
abline(modelo_pregunta4)
  summary(modelo_pregunta4)
anova(modelo_pregunta4)

#   5- ¿La performance histórica del jugador influye en el total de puntos anotados
# en esa temporada?
modelo_pregunta4 <- lm(PTS~perf,data=df)
modelo_pregunta4

plot(data=df, 
     PTS~MP, 
     main="Relación minutos jugados vs cantidad de puntos anotados",
     xlab="Minutos Jugados", 
     ylab="Puntos anotados", 
     col="deepskyblue3",
     pch=19)
abline(modelo_pregunta4)
summary(modelo_pregunta4)
anova(modelo_pregunta4)

perf_1 <- 0
perf_0 <- 0
total_points_perf <- dplyr::count(df_modified, df_modified$perf, sort = TRUE)
for (i in 1:length(df_modified)){
  if(df_modified[i,6]==1){perf_1 = perf_1 + df_modified[i,7]}
  else {perf_0 = perf_0 + df_modified[i,7]}
}
# pg_count 
# pf_count 
# sg_count 
avg_points_p <- data.frame(matrix(nrow=2, ncol=2))
avg_points_p[1,2] <- perf_1/250
avg_points_p[2,2] <- perf_0/132
avg_points_p[1,1] <- "Muy Buena"
avg_points_p[2,1] <- "Buena/Regular"

avg_points_p$X1 <- factor(avg_points_p$X1)

ggplot(avg_points_p, aes(x = X1, y = X2, fill = X1)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(X2, 2)), vjust = -0.5, color = "black", size = 3) +
  xlab("Performance") +
  ylab("Promedio de Puntos") +
  ggtitle("Promedio de puntos anotados por Performance") +
  theme_minimal() +
  labs(fill = "Performance") +
  theme(axis.title = element_text(size = 14),
        plot.title = element_text(size = 16)) +
  guides(fill = FALSE) 
# HACER ESTE GRAFICO PERO EN BASE A LA POSICION

