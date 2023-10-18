options(scipen=999)
options(warn=-1)
library(lmtest)
library(nortest)
library(car)
data1 <- nba_2010
data2 <- players
df <- merge(data1,data2, by.x="Player", by.y="Player")

unique(df$Pos)
df_modified <- df
df_modified$Player <- NULL
df_modified$Tm <- NULL
df_modified$Year <- NULL
df_modified$d1 <- ifelse(df$Pos=="PG",1,0)
df_modified$d2 <- ifelse(df$Pos=="PF-C",1,0)
df_modified$Pos <- NULL


# 2) Modelo 1
# En una primera instancia se desea estudiar la relación entre el total de minutos
# jugados y los puntos totales anotados. Para ello se decide ajustar un modelo de
# regresión lineal simple (Modelo 1).


# 2.1 Escribir la ecuación y los supuestos para el Modelo 1. (A MANO)
# ----
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
Anova(modelo1)
# ----

# 2.2 ¿Existe regresión? Plantear la hipótesis correspondiente, escribir la estadística de
# prueba y su distribución y concluir en términos del problema. 
# ----

qt(0.975,nrow(df)-2)
# tcrit = 1.966145
# tobs = 43.478
# |tobs|>tcrit -> Rechazo H0
# En base a la evidencia muestral y con un nivel de significación del 5% es de
# esperar que la cantidad total de minutos jugados aporte significativamente a la
# explicacion de la cantidad total de puntos anotados
# ----


# 2.3 Escribir la ecuación estimada e interpretar los parámetros estimados. 
# ----
coef(modelo1)
# Ecuacion estimada: Y = -126.68 + 0.5X
# A medida que el numero de minutos jugados aumenta en 1 minutos, los puntos anotados
# totales aumenta en 0.5
# ----


# 2.4 Comentar acerca del valor del coeficiente de determinación
# ----
summary(modelo1)
# Coeficiente de correlacion: 0.8308
# El 83% de la variabilidad total de la cantidad de puntos anotados es explicada 
# por la relación lineal entre la cantidad de minutos jugados y los puntos anotados.
# ----


# 2.5 Evaluar si se cumplen los supuestos del modelo. ¿Hay observaciones atípicas?
# ¿Corresponde a algún jugador particular? 
# ----
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

avPlots(modelo1)

residuos_estandarizados <- rstandard(modelo1) 
observaciones_atipicas_1 <- which(abs(residuos_estandarizados) > 3)
indices1 <- as.numeric(names(observaciones_atipicas_1))
df_outliers1 <- matrix()
df_outliers1 <- data.frame(matrix(ncol = ncol(df)))
colnames(df_outliers1) <- colnames(df)
for (i in 1:length(indices1)) {
  df_outliers1[i, ] <- df[indices1[i], ]
}
df_outliers1

residuos_studentizados <- rstudent(modelo1) 
observaciones_atipicas_2 <- which(abs(residuos_studentizados) > qt(0.975,380))
indices2 <- as.numeric(names(observaciones_atipicas_2))
df_outliers2 <- matrix()
df_outliers2 <- data.frame(matrix(ncol = ncol(df)))
colnames(df_outliers2) <- colnames(df)
for (i in 1:length(indices2)) {
  df_outliers2[i, ] <- df[indices2[i], ]
}
df_outliers2

leverage <- hatvalues(modelo1)
observaciones_atipicas_0 <- which(abs(leverage) > (4/387))
indices3 <- as.numeric(names(observaciones_atipicas_0))
df_outliers3 <- matrix()
df_outliers3 <- data.frame(matrix(ncol = ncol(df)))
colnames(df_outliers3) <- colnames(df)
for (i in 1:length(indices3)) {
  df_outliers3[i, ] <- df[indices3[i], ]
}
df_outliers3


# ----
