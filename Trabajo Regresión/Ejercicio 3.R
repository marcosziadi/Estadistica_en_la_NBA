options(scipen=999)
options(warn=-1)
library(olsrr)
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


# 3) Modelo 2
# En una segunda instancia se desea estudiar la relación entre todas las variables
# relevadas y el total de puntos anotados. Para ello se decide ajustar un modelo de
# regresión lineal múltiple (Modelo 2). 
# ----

modelo2perf_marcos <- lm(PTS~Age+G+MP+height+weight+perf+d1+d2,data=df_modified)
summary(modelo2perf_marcos)
qt(0.975,386)
ols_step_forward_p(modelo2perf_marcos,pent=0.05)
ols_step_backward_p(modelo2perf_marcos,pent=0.05)
ols_step_both_p(modelo2perf_marcos,pent=0.05,prem=0.05)
# ----

# 3.2 ¿Existe regresión? Plantee las hipótesis correspondientes, arme el cuadro
# ANOVA e interprete en términos del problema. 
# ----

# Test de regresion
#H0)B1=B2=B3=B4=B5=B6=0
#H1) Al menos un Bj es distinto de 0. j=1,2,3,4,5,6
anova(modelo2perf_marcos)
Fobs<-((77699+34666749+35967525+13749+87083+1902438+144860+163238)/8)/26097
qf(0.95,8,378)
# Fobs>Fcrit->RH0. En base a la evidencia muestral y con un nivel de 
# significación del 5% es de esperar que al menos una de las variables aporte  
# significativamente a la explicacion de la cantidad total de puntos anotados
# ----


# 3.3 Estime los coeficientes del modelo y realice los test parciales. ¿Qué sugieren estos
# test? 
# ----

#Test parciales
summary(modelo2perf_marcos)
qt(0.975,373)
# Todas las variables aportan significativamente a la explicacion de la cantidad 
# de puntos, menos el peso y la altura
# ----


# 3.4 ¿Existe multicolinealidad?
# ----
library(car)
vif(modelo2perf_marcos)
# No existe multicolinealidad en el modelo
# ----
