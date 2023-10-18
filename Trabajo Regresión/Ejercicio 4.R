options(scipen=999)
options(warn=-1)
# Librerias  ----
library(lmtest)
library(nortest)
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


# 4) Modelo 3
# Según el análisis de multicolinealidad y el resultado de los test parciales plantee un
# nuevo modelo llamado Modelo 3. Incorporar la variable dicotómica performance.
# Evaluar incorporar interacciones
# ----

modelo3 <- lm(PTS ~ (Age + G + MP) * (perf + d1 + d2)+perf:d1+perf:d2, data = df_modified)


# Test de paralelismo
# H0) B7=B8=B9=B10=B11=B12=B13=B14=B15=B16=0
# H1) Al menos un Bj es distinto de 0. j=7,8,9,10,11,12,13,14,15,16
anova(modelo3)
Fobs <- ((26864+20838+23442+567251+74688+89373+977515+223668+108014+24756+20135)/11)/20865
qf(0.95,11,369)
# Fobs>Fcrit->RH0. En base a la evidencia muestral y con un nivel de 
# significación del 5% se concluye que las rectas no son paralelas

# Test parciales
summary(modelo3)
qt(0.975,364) # 1.966414
# Las variables que aportan son:
# Age, G, MP, MP:d1 y MP:perf
modelo3_final <- lm(PTS~Age+G+MP+MP:d1+MP:perf,data=df_modified)
# ----

# 4.1 Estime el nuevo Modelo 3 y evalúe si se cumplen los supuestos del modelo
# mediante un análisis de residuos. ¿Hay alguna observación atípica? Realice los
# gráficos de regresión parcial. Comente.
# ----

#Test parciales
summary(modelo3_final)
qt(0.975,381)
# Todas las variables aportan significativamente a la explicacion de la cantidad 
# de puntos

#Analisis de Residuos
#Normalidad
qqnorm(modelo3_final$residuals,
       ylab="Residuos" ,
       xlab="Cuantiles Normales")
qqline(modelo3_final$residuals)


ad.test(modelo3_final$residuals)
lillie.test(modelo3_final$residuals)
# p_value < 0,05 -> No se cumple el supuesto de normalidad

#Variancia constante
plot(modelo3_final$residuals~modelo3_final$fitted.values,main="Gráfico de Residuos vs Valores
     Ajustados", ylab="Residuos",xlab="Predichos(yestimado)")
abline(0,0)
# No se cumple el supuesto de variancia constante
# No hay linealidad

# Correlación de los errores
# H0) Los errores no están correlacionados
# H1) Los errores están correlacionados
dwtest(modelo3_final,alternative = "two.sided")
# p_value>0.05 entonces no rechazo Ho. Los ei no están correlacionados
# No hay correlación

avPlots(modelo3_final)

#¿Hay alguna observación atípica? Realice los
# gráficos de regresión parcial. Comente.

residuos_estandarizados <- rstandard(modelo3_final) 
observaciones_atipicas_1 <- which(abs(residuos_estandarizados) > 3)
indices1 <- as.numeric(names(observaciones_atipicas_1))
df_outliers1 <- matrix()
df_outliers1 <- data.frame(matrix(ncol = ncol(df)))
colnames(df_outliers1) <- colnames(df)
for (i in 1:length(indices1)) {
  df_outliers1[i, ] <- df[indices1[i], ]
}
df_outliers1

residuos_studentizados <- rstudent(modelo3_final) 
observaciones_atipicas_2 <- which(abs(residuos_estandarizados) > qt(0.975,381))
indices2 <- as.numeric(names(observaciones_atipicas_2))
df_outliers2 <- matrix()
df_outliers2 <- data.frame(matrix(ncol = ncol(df)))
colnames(df_outliers2) <- colnames(df)
for (i in 1:length(indices2)) {
  df_outliers2[i, ] <- df[indices2[i], ]
}
df_outliers2

leverage <- hatvalues(modelo3_final)
observaciones_atipicas_0 <- which(abs(leverage) > (12/387))
indices3 <- as.numeric(names(observaciones_atipicas_0))
df_outliers3 <- matrix()
df_outliers3 <- data.frame(matrix(ncol = ncol(df)))
colnames(df_outliers3) <- colnames(df)
for (i in 1:length(indices3)) {
  df_outliers3[i, ] <- df[indices3[i], ]
}
df_outliers3

jugadores_coincidentes1 <- intersect(row.names(df_outliers1), row.names(df_outliers2))
jugadores_coincidentes2 <- intersect(row.names(df_outliers2), row.names(df_outliers3))
jugadores_coincidentes3 <- intersect(row.names(df_outliers1), row.names(df_outliers3))

# graficos de regresion parcial 
avPlots(modelo3_final)


# 4.2 Si no se cumplen los supuestos plantear y realizar transformaciones para intentar
# solucionar el problema. 
# ----

df_modified$log_pts <- log(df_modified$PTS)

modelotransf<-lm(log_pts~Age+G+MP+MP:d1+MP:perf,data=df_modified)

#Analisis de Residuos
#Normalidad
qqnorm(modelotransf$residuals,
       ylab="Residuos" ,
       xlab="Cuantiles Normales")
qqline(modelotransf$residuals)

ad.test(modelotransf$residuals)
# El p_value del test es mayor a 0.05

#Variancia constante
plot(modelotransf$residuals~modelotransf$fitted.values,main="Gr?fico de Residuos vs Valores
     Ajustados", ylab="Residuos",xlab="Predichos(yestimado)")
abline(0,0)
# Se cumple el supuesto de variancia constante

# Correlacion de los errores
# H0) Los errores no estan correlacionados
# H1) Los errores estan correlacionados
dwtest(modelotransf,alternative = "two.sided")
# p_value>0.05 entonces no rechazo Ho. Los ei no est?n correlacionados
# No hay correlacion
coef(modelotransf)
#Linealidad de los regresores
avPlots(modelotransf)
# ----