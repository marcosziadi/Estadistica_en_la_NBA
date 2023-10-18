# Librerias
install.packages("GGally")
install.packages("ggplot2")
install.packages("nortest")
install.packages("lmtest")
library(lmtest)
library(nortest)
library(GGally)
library(ggplot2)
#-------------------------------------------------------------------------------
data1 <- nba_2010
data2 <- players
df <- merge(data1,data2, by.x="Player", by.y="Player")
#-------------------------------------------------------------------------------
options(scipen=999)
options(warn=-1)
# 1) ¿Qué características presentan los jugadores estudiados?
# 1.1 Realice un análisis descriptivo univariado de las variables relevadas. ¿Hay valores
# atípicos? ¿Hay valores perdidos?


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
  

for (col in colnames(df)){
  print(paste("Valores perdidos encontrados en la columna", col, "=", sum(is.na(df[col]))))
}
# Hay 5 valores perdidos, y dichos valores se encuentran en la columna llamada "perf"
#-------------------------------------------------------------------------------
# 1.2 ¿Qué relación tienen las variables consideradas explicativas con los puntos
# totales anotadas? ¿Entre ellas presentan alguna relación?
# Variables consideradas explicativas: "Age","Pos","G","MP","perf","height","weight"


df_modified <- df
df_modified$Player <- NULL
df_modified$Tm <- NULL


# unique(df_modified['Pos'])
for(i in 1:nrow(df_modified)){
  if(df_modified[i,'Pos'] == 'PG'){df_modified[i,'Pos'] = 1}
  else if(df_modified[i,'Pos'] == 'PF-C'){df_modified[i,'Pos'] = 2}
  else if(df_modified[i,'Pos'] == 'SG-SF'){df_modified[i,'Pos'] = 3}
}
df_modified$Pos <- as.numeric(df_modified$Pos)
# PG    -> 1
# PF-C  -> 2
# SG-SF -> 3


ggcorr(df_modified, label = TRUE) 
cor(df_modified)
i=1
for (col in colnames(df_modified)){
  corr <- cor(x=df_modified,y=df_modified['PTS'])[i]
  print(paste("Correlacion entre PTS y", col, "=", corr))
  i <- i+1
}

























#-------------------------------------------------------------------------------

modelo1 <- lm(PTS~MP,data=df)
modelo1

plot(data=df, 
     PTS~MP, 
     main="Minutos Jugados vs Puntos Anotados",
     xlab="Minutos Jugados", 
     ylab="Puntos Anotados", 
     col="deepskyblue3",
     pch=19)
abline(modelo1)
summary(modelo1)

qt(0.975,nrow(df)-2)
# tcrit = 1.966145
# tobs = 43.478
# |tobs|>tcrit -> Rechazo H0
# En base a la evidencia muestral y con un nivel de significación del 5% es de
# esperar que la cantidad total de minutos jugados aporte significativamente a la
# explicacion de la cantidad total de puntos anotados

coef(modelo1)
# Ecuacion estimada: Y = -126.68 + 0.5X
# A medida que el numero de minutos jugados aumenta en 1 minutos, los puntos anotados
# totales aumenta en 0.5

# Coeficiente de correlacion: 0.8308
# El 83% de la variabilidad total de la cantidad de puntos anotados es explicada 
# por la relación lineal entre la cantidad de minutos jugados y los puntos anotados.

#Normalidad
qqnorm(modelo1$residuals,
       ylab="Residuos" ,
       xlab="Cuantiles Normales")
qqline(modelo1$residuals)


ad.test(modelo1$residuals)
lillie.test(modelo1$residuals)
# p_value < 0,05 -> No se cumple el supuesto de normalidad

#Variancia constante
plot(modelo1$residuals~modelo1$fitted.values,main="Gráfico de Residuos vs Valores
     Ajustados", ylab="Residuos",xlab="Predichos(yestimado)")
abline(0,0)
# No se cumple el supuesto de variancia constante
# No hay linealidad

# Correlación de los errores
# H0) Los errores no están correlacionados
# H1) Los errores están correlacionados
dwtest(modelo1,alternative = "two.sided")
# p_value>0.05 entonces no rechazo Ho. Los ei no están correlacionados
# No hay correlación


#sum(rstandard(modelo1)>3)
#rstudent(modelo1)
#hatvalues(modelo1)

infl <- influence.measures(modelo1)
infl
num_outliers <- sum(infl$is.inf)
num_outliers
outliers <- infl[infl$is.inf == TRUE, , drop = FALSE]
influencePlot(modelo1)

infl_indices
infl_obs <- df[infl_indices, ]
print(df[infl_indices, ])


bp_outliers = boxplot(df[,c('PTS','MP')])
outliers_df <- data.frame(matrix(ncol = ncol(df), nrow = 0))
colnames(outliers_df) <- colnames(df)
cont <- 1
for (i in 1:nrow(df)){
  if(df[i,9]>=bp_outliers$out[1]){
    for(c in 1:length(bp_outliers$out))
      if(df[i,9]==bp_outliers$out[c]){
        outliers_df[cont,] = df[i,]
        cont=cont+1
      }
  }
}
cat("Se encuentran observaciones atipicas, y son las siguientes:\n")
print(outliers_df)

























#-------------------------------------------------------------------------------

modelo2 <- lm(PTS~Age+G+MP+height+weight,data=df)
modelo2

anova(modelo2)
summary(modelo2)

modelo2perf <- lm(PTS~Age+G+MP+height+weight+perf,data=df)
#2
#test de regresion
#H0)B1=B2=B3=B4=B5=B6=0
#H1) Al menos un Bj es distinto de 0. j=1,2,3,4,5,6
anova(modelo2perf)
Fobs<-((77699+34666749+35967525+13749+87083+1902438)/6)/26779
qf(0.95,6,375)
#Fobs>Fcrit->RH0. En base a la evidencia muestral y con un nivel de 
# significación del 5% es de esperar que al menos una de las variables aporte  
# significativamente a la explicacion de la cantidad total de puntos anotados
#4
#Test parciales
summary(modelo2perf)
t_Age <- -3.256
t_G <- -5.009
t_MP <- 27.642
t_height <- -0.197
t_weight <- -0.053
t_perf <- 8.429
qt(0.975,375)
#Todas las variables aportan significativamente a la explicacion de la cantidad 
#de puntos, menos el peso y la altura

#4
#analisis de multicolinealidad
library(car)
vif(modelo2perf)
#no existe multicolinealidad en el modelo























#-------------------------------------------------------------------------------

modelo3 <- lm(PTS~Age+G+MP+perf + Age:perf+G:perf+MP:perf,data=df)
# Test de regresion
# H0) B1=B2=B3=B4=B5=B6=B7=0
# H1) Al menos un Bj es distinto de 0. j=1,2,3,4,5,6
anova(modelo3)
Fobs_m3<-((77699+34666749+35967525+1998262+31435+427499+1138852)/8)/22592
qf(0.95,8,373)
# Fobs>Fcrit->RH0. En base a la evidencia muestral y con un nivel de 
# significación del 5% es de esperar que al menos una de las variables aporte  
# significativamente a la explicacion de la cantidad total de puntos anotados

#Test parciales
summary(modelo3)
t_Age <- -3.401
t_G <- -2.430
t_MP <- 19.584
t_perf <- 0.557
t_Ageperf <- 0.015
t_Gperf <- -3.335
t_MPperf <- 7.100
qt(0.975,373)
# Todas las variables aportan significativamente a la explicacion de la cantidad 
# de puntos, menos el perf y la ageperf

#Analisis de Residuos
#Normalidad
qqnorm(modelo3$residuals,
       ylab="Residuos" ,
       xlab="Cuantiles Normales")
qqline(modelo1$residuals)


ad.test(modelo3$residuals)
lillie.test(modelo3$residuals)
# p_value < 0,05 -> No se cumple el supuesto de normalidad

#Variancia constante
plot(modelo3$residuals~modelo3$fitted.values,main="Gráfico de Residuos vs Valores
     Ajustados", ylab="Residuos",xlab="Predichos(yestimado)")
abline(0,0)
# No se cumple el supuesto de variancia constante
# No hay linealidad

# Correlación de los errores
# H0) Los errores no están correlacionados
# H1) Los errores están correlacionados
dwtest(modelo1,alternative = "two.sided")
# p_value>0.05 entonces no rechazo Ho. Los ei no están correlacionados
# No hay correlación

#FALTA: ¿Hay alguna observación atípica? Realice los
# gráficos de regresión parcial. Comente.
# 4.2 Si no se cumplen los supuestos plantear y realizar transformaciones para intentar
# solucionar el problema. 























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

