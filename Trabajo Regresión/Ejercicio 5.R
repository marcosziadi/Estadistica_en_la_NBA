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

# 5) Modelo final


# Luego de lo concluido en los puntos anteriores arribar a un Modelo final.


# 5.1 Escriba la ecuación estimada del Modelo final en forma general y según la variable
# performance. Interpretar los coeficientes en términos del problema.
df_modified$log_pts <- log(df_modified$PTS)
modelofinal <- lm(log_pts~Age+G+MP+MP:d1+MP:perf,data=df_modified)
coef(modelofinal)

# 5.2 Elija un coeficiente e interprete su intervalo de confianza del 95%.
conf_interval <- confint(modelofinal, level = 0.95)
# intervalo_age <- conf_interval["Age", ]
# print(intervalo_age)


# 5.3 Responda a las preguntas que se realizaron los periodistas según el modelo final
# estimado.

#  - ¿A mayor altura del jugador, mayor cantidad de puntos anotados?
# No esta altura en el modelo, no es una variable que aporte significativamente
# a la cantidad de puntos anotados

#  - ¿La posición de juego influye en los puntos anotados?
# No bajo el nuevo modelo (?)

#  - ¿Cómo influye la edad en la cantidad de puntos anotados?
# No esta edad en el modelo, no es una variable que aporte significativamente
# a la cantidad de puntos anotados

#  - ¿Influye en los puntos anotados la cantidad de minutos que el jugador estuvo
# en el campo de juego?
# Si

#  - ¿La performance histórica del jugador influye en el total de puntos anotados
# en esa temporada?
# Si


# 5.4 El entrenador del equipo Miami Heat quiere utilizar el modelo para estimar la
# cantidad de puntos que va a anotar al final de la temporada 2023 un jugador nuevo
# llamado Nicolas Sánchez. Este jugador mide 1.95mts, pesa 96 kg., su posición de
# juego es PG (Point Guard), presentó una performance regular en las temporadas
# previas y se cree que va a jugar 29 partidos con un total de 510 minutos. Presentar la
# estimación puntual y por intervalo del 95%.
# log_pts~Age+G+MP+perf+MP:perf
nuevo_jugador <- data.frame(Age = 22, G = 29, MP = 510, d1 = 1, perf = 0)
prediccion <- predict(modelofinal, newdata = nuevo_jugador, interval = "confidence", level = 0.95)
pts_jugador <- exp(prediccion)
pts_jugador
