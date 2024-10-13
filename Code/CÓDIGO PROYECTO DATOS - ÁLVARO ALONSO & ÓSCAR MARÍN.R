#Álvaro Enol Alonso Ortega
#Óscar Marín Esteban

library(MASS)
library(car)
library(nortest)
#install.packages("dplyr")

# Carga el paquete
library(dplyr)
# CARGAR LOS DATOS(CAMBIAR LA RUTA A LA CORRESPONDIENTE)
datos <- read.csv("C://Users//Usuario//Documents//Alvaro//Carrera//2-Segundo//Primer cuatrimetre//Métodos Estadísticos para Ingenieros de Datos//PROYECTO//Life-Expectancy-Data-Updated.csv")
head(datos)
summary(datos)
conjunto_0 <- datos%>%filter(Economy_status_Developed == 0)
conjunto_1 <- datos %>% filter(Economy_status_Developed == 1)
#quitamos estas 2 columnas, que ya no necesitamos
conjunto_0 <- conjunto_0[, -which(names(conjunto_0) == "Economy_status_Developed")]
conjunto_0 <- conjunto_0[, -which(names(conjunto_0) == "Economy_status_Developing")]

summary(conjunto_0)
summary(conjunto_1)

# Eliminar filas con valores NA
conjunto_0 <- na.omit(conjunto_0)
#variable respuesta
Y<- conjunto_0$Life_expectancy
library(e1071)

#hemos separado, porque se rigen por modelos diferentes, obviamente un pais desarrollado y sub son muy difernetes
columnas_numericas <- conjunto_0[, sapply(conjunto_0, is.numeric)]
for (i in 1:ncol(columnas_numericas)) {
  print(names(columnas_numericas)[i])
  print(skewness(columnas_numericas[, i]))
}
#miramos las que estén -2, 2, que son incidets_HIV, GPD_per_capita, population_mln
library(MASS)
# Ajustar un modelo lineal simple
x <- conjunto_0$Incidents_HIV
modelo <- lm(x ~ 1)
# Aplicar la función boxcox para encontrar el mejor lambda
b <- boxcox(modelo)
# Extraer el valor de lambda
lambda <- b$x[which.max(b$y)]
# Aplicar la transformación de Box-Cox
yIncidents_HIV <- (x^lambda - 1) / lambda
# Mostrar el valor de lambda
lambda
conjunto_0$Incidents_HIV<-yIncidents_HIV
skewness(conjunto_0$Incidents_HIV)
boxplot(conjunto_0$Incidents_HIV, range = 3)


library(MASS)
# Ajustar un modelo lineal simple
x <- conjunto_0$GDP_per_capita
modelo <- lm(x ~ 1)
# Aplicar la función boxcox para encontrar el mejor lambda
b <- boxcox(modelo)
# Extraer el valor de lambda
lambda <- b$x[which.max(b$y)]
# Aplicar la transformación de Box-Cox
yGPD <- (x^lambda - 1) / lambda
# Mostrar el valor de lambda
lambda
conjunto_0$GDP_per_capita<-yGPD
skewness(conjunto_0$GDP_per_capita)
boxplot(conjunto_0$GDP_per_capita, range = 3)

library(MASS)
# Ajustar un modelo lineal simple
x <- conjunto_0$Population_mln
modelo <- lm(x ~ 1)
# Aplicar la función boxcox para encontrar el mejor lambda
b <- boxcox(modelo)
# Extraer el valor de lambda
lambda <- b$x[which.max(b$y)]
# Aplicar la transformación de Box-Cox
yPopulation <- (x^lambda - 1) / lambda
# Mostrar el valor de lambda
lambda
conjunto_0$Population_mln<-yPopulation
skewness(conjunto_0$Population_mln)
boxplot(conjunto_0$Population_mln, range = 3)

datos_recortados<-conjunto_0

#Ejecutar 3 veces
# Identificar valores atípicos en la Life_expectancy
boxplot(datos_recortados$Life_expectancy, main="Boxplot de Life_expectancy", range = 3)

while (TRUE) {
  bp <- boxplot(datos_recortados$Life_expectancy, main="Boxplot sin outliers", range = 3)
  outliers <- bp$out
  
  if (length(outliers) == 0) {
    break  # Salir del bucle si no hay outliers
  }
  
  datos_recortados <- datos_recortados[!(datos_recortados$Life_expectancy %in% outliers), ]
}

# Identificar valores atípicos en la Infant_deaths
boxplot(datos_recortados$Infant_deaths, main="Boxplot de Infant_deaths", range = 3)
while (TRUE) {
  bp <- boxplot(datos_recortados$Infant_deaths, main="Boxplot sin outliers", range = 3)
  outliers <- bp$out
  
  if (length(outliers) == 0) {
    break  # Salir del bucle si no hay outliers
  }
  
  datos_recortados <- datos_recortados[!(datos_recortados$Infant_deaths %in% outliers), ]
}

# Identificar valores atípicos en la Under_five_deaths
boxplot(datos_recortados$Under_five_deaths, main="Boxplot de Under_five_deaths", range = 3)
while (TRUE) {
  bp <- boxplot(datos_recortados$Under_five_deaths, main="Boxplot sin outliers", range = 3)
  outliers <- bp$out
  
  if (length(outliers) == 0) {
    break  # Salir del bucle si no hay outliers
  }
  
  datos_recortados <- datos_recortados[!(datos_recortados$Under_five_deaths %in% outliers), ]
}

# Identificar valores atípicos en la Adult_mortality
boxplot(datos_recortados$Adult_mortality, main="Boxplot de Adult_mortality", range = 3)
while (TRUE) {
  bp <- boxplot(datos_recortados$Adult_mortality, main="Boxplot sin outliers", range = 3)
  outliers <- bp$out
  
  if (length(outliers) == 0) {
    break  # Salir del bucle si no hay outliers
  }
  
  datos_recortados <- datos_recortados[!(datos_recortados$Adult_mortality %in% outliers), ]
}

# Identificar valores atípicos en la Alcohol_consumption
boxplot(datos_recortados$Alcohol_consumption, main="Boxplot de Alcohol_consumption", range = 3)
while (TRUE) {
  bp <- boxplot(datos_recortados$Alcohol_consumption, main="Boxplot sin outliers", range = 3)
  outliers <- bp$out
  
  if (length(outliers) == 0) {
    break  # Salir del bucle si no hay outliers
  }
  
  datos_recortados <- datos_recortados[!(datos_recortados$Alcohol_consumption %in% outliers), ]
}

# Identificar valores atípicos en la Hepatitis_B
boxplot(datos_recortados$Hepatitis_B, main="Boxplot de Hepatitis_B", range = 3)
while (TRUE) {
  bp <- boxplot(datos_recortados$Hepatitis_B, main="Boxplot sin outliers", range = 3)
  outliers <- bp$out
  
  if (length(outliers) == 0) {
    break  # Salir del bucle si no hay outliers
  }
  
  datos_recortados <- datos_recortados[!(datos_recortados$Hepatitis_B %in% outliers), ]
}


# Identificar valores atípicos en la Measles
boxplot(datos_recortados$Measles, main="Boxplot de Measles", range = 3)
while (TRUE) {
  bp <- boxplot(datos_recortados$Measles, main="Boxplot sin outliers", range = 3)
  outliers <- bp$out
  
  if (length(outliers) == 0) {
    break  # Salir del bucle si no hay outliers
  }
  
  datos_recortados <- datos_recortados[!(datos_recortados$Measles %in% outliers), ]
}

# Identificar valores atípicos en la BMI
boxplot(datos_recortados$BMI, main="Boxplot de BMI", range = 3)
while (TRUE) {
  bp <- boxplot(datos_recortados$BMI, main="Boxplot sin outliers", range = 3)
  outliers <- bp$out
  
  if (length(outliers) == 0) {
    break  # Salir del bucle si no hay outliers
  }
  
  datos_recortados <- datos_recortados[!(datos_recortados$BMI %in% outliers), ]
}

# Identificar valores atípicos en la Polio
boxplot(datos_recortados$Polio, main="Boxplot de Polio", range = 3)
while (TRUE) {
  bp <- boxplot(datos_recortados$Polio, main="Boxplot sin outliers", range = 3)
  outliers <- bp$out
  
  if (length(outliers) == 0) {
    break  # Salir del bucle si no hay outliers
  }
  
  datos_recortados <- datos_recortados[!(datos_recortados$Polio %in% outliers), ]
}


# Identificar valores atípicos en la Diphtheria
boxplot(datos_recortados$Diphtheria, main="Boxplot de Diphtheria", range = 3)
while (TRUE) {
  bp <- boxplot(datos_recortados$Diphtheria, main="Boxplot sin outliers", range = 3)
  outliers <- bp$out
  
  if (length(outliers) == 0) {
    break  # Salir del bucle si no hay outliers
  }
  
  datos_recortados <- datos_recortados[!(datos_recortados$Diphtheria %in% outliers), ]
}

# Identificar valores atípicos en la Incidents_HIV
boxplot(datos_recortados$Incidents_HIV, main="Boxplot de Incidents_HIV", range = 3)
while (TRUE) {
  bp <- boxplot(datos_recortados$Incidents_HIV, main="Boxplot sin outliers", range = 3)
  outliers <- bp$out
  
  if (length(outliers) == 0) {
    break  # Salir del bucle si no hay outliers
  }
  
  datos_recortados <- datos_recortados[!(datos_recortados$Incidents_HIV %in% outliers), ]
}

# Identificar valores atípicos en la GDP_per_capita
boxplot(datos_recortados$GDP_per_capita, main="Boxplot de GDP_per_capita", range = 3)
while (TRUE) {
  bp <- boxplot(datos_recortados$GDP_per_capita, main="Boxplot sin outliers", range = 3)
  outliers <- bp$out
  
  if (length(outliers) == 0) {
    break  # Salir del bucle si no hay outliers
  }
  
  datos_recortados <- datos_recortados[!(datos_recortados$GDP_per_capita %in% outliers), ]
}

# Identificar valores atípicos en la Population_mln
boxplot(datos_recortados$Population_mln, main="Boxplot de Population_mln", range = 3)
while (TRUE) {
  bp <- boxplot(datos_recortados$Population_mln, main="Boxplot sin outliers", range = 3)
  outliers <- bp$out
  
  if (length(outliers) == 0) {
    break  # Salir del bucle si no hay outliers
  }
  
  datos_recortados <- datos_recortados[!(datos_recortados$Population_mln %in% outliers), ]
}

# Identificar valores atípicos en la Thinness_ten_nineteen_years
boxplot(datos_recortados$Thinness_ten_nineteen_years, main="Boxplot de Thinness_ten_nineteen_years", range = 3)
while (TRUE) {
  bp <- boxplot(datos_recortados$Thinness_ten_nineteen_years, main="Boxplot sin outliers", range = 3)
  outliers <- bp$out
  
  if (length(outliers) == 0) {
    break  # Salir del bucle si no hay outliers
  }
  
  datos_recortados <- datos_recortados[!(datos_recortados$Thinness_ten_nineteen_years %in% outliers), ]
}

# Identificar valores atípicos en la Thinness_five_nine_years
boxplot(datos_recortados$Thinness_five_nine_years, main="Boxplot de Thinness_five_nine_years", range = 3)
while (TRUE) {
  bp <- boxplot(datos_recortados$Thinness_five_nine_years, main="Boxplot sin outliers", range = 3)
  outliers <- bp$out
  
  if (length(outliers) == 0) {
    break  # Salir del bucle si no hay outliers
  }
  
  datos_recortados <- datos_recortados[!(datos_recortados$Thinness_five_nine_years %in% outliers), ]
}

# Identificar valores atípicos en la Schooling
boxplot(datos_recortados$Schooling, main="Boxplot de Schooling", range = 3)
while (TRUE) {
  bp <- boxplot(datos_recortados$Schooling, main="Boxplot sin outliers", range = 3)
  outliers <- bp$out
  
  if (length(outliers) == 0) {
    break  # Salir del bucle si no hay outliers
  }
  
  datos_recortados <- datos_recortados[!(datos_recortados$Schooling %in% outliers), ]
}
#1462

#COMPROBACION DE BOXPLOTS
boxplot(datos_recortados$Infant_deaths, main="Boxplot de Infant_deaths", range = 3)
boxplot(datos_recortados$Under_five_deaths, main="Boxplot de Under_five_deaths", range = 3)
boxplot(datos_recortados$Life_expectancy, main="Boxplot de Life_expectancy", range = 3)
boxplot(datos_recortados$Adult_mortality, main="Boxplot de Adult_mortality", range = 3)
boxplot(datos_recortados$Alcohol_consumption, main="Boxplot de Alcohol_consumption", range = 3)
boxplot(datos_recortados$Hepatitis_B, main="Boxplot de Hepatitis_B", range = 3)
boxplot(datos_recortados$Measles, main="Boxplot de Measles", range = 3)
boxplot(datos_recortados$BMI, main="Boxplot de BMI", range = 3)
boxplot(datos_recortados$Polio, main="Boxplot de Polio", range = 3)
boxplot(datos_recortados$Diphtheria, main="Boxplot de Diphtheria", range = 3)
boxplot(datos_recortados$Incidents_HIV, main="Boxplot de Incidents_HIV", range = 3)
boxplot(datos_recortados$GDP_per_capita, main="Boxplot de GDP_per_capita", range = 3)
boxplot(datos_recortados$Population_mln, main="Boxplot de Population_mln", range = 3)
boxplot(datos_recortados$Thinness_ten_nineteen_years, main="Boxplot de Thinness_ten_nineteen_years", range = 3)
boxplot(datos_recortados$Thinness_five_nine_years, main="Boxplot de Thinness_five_nine_years", range = 3)
boxplot(datos_recortados$Schooling, main="Boxplot de Schooling", range = 3)

#HEPATITIS_B Y DIPHTERIA TIENEN DE NUEVO
#EJECUTAMOS BUCLES DE ESOS 2, SALE UNO EN POLIO, EJECUTAMOS BUCLE DE POLIO
#TODOS LIMPIOS

library(heatmaply)
#lillie.test de todas las variables hecho, y no hay ninguna normal

# Calcula las correlaciones de Spearman
correlaciones <- cor(datos_recortados[, sapply(datos_recortados, is.numeric)], method = "spearman")

# Crea un mapa de calor con heatmaply
heatmaply(correlaciones, 
          main = "Correlaciones de Spearman",
          labRow = names(correlaciones),
          labCol = names(correlaciones))

corrTF=cor(datos_recortados[,-c(1,2)], method="spearman")
corrout = corrTF >0.7 | corrTF < -0.7
corrout

cor.test(datos_recortados$Infant_deaths, datos_recortados$Under_five_deaths,alternative = "two.sided", conf.level=0.95,mehod = "spearman")
#mirar codigo prac 4, para memoria, quitamos por test muy alto
datos_recortados <- datos_recortados[, -which(names(datos_recortados) == "Infant_deaths")]
corrTF=cor(datos_recortados[,-c(1,2)], method="spearman")
corrout = corrTF >0.7 | corrTF < -0.7
corrout

cor.test(datos_recortados$Adult_mortality, datos_recortados$Under_five_deaths,alternative = "two.sided", conf.level=0.95,mehod = "spearman")
#mirar codigo prac 4, para memoria, quitamos por test muy alto
datos_recortados <- datos_recortados[, -which(names(datos_recortados) == "Under_five_deaths")]
corrTF=cor(datos_recortados[,-c(1,2)], method="spearman")
corrout = corrTF >0.7 | corrTF < -0.7
corrout

cor.test(datos_recortados$Polio, datos_recortados$Diphtheria,alternative = "two.sided", conf.level=0.95,mehod = "spearman")
datos_recortados <- datos_recortados[, -which(names(datos_recortados) == "Diphtheria")]
corrTF=cor(datos_recortados[,-c(1,2)], method="spearman")
corrout = corrTF >0.7 | corrTF < -0.7
corrout

cor.test(datos_recortados$Hepatitis_B, datos_recortados$Polio,alternative = "two.sided", conf.level=0.95,mehod = "spearman")
datos_recortados <- datos_recortados[, -which(names(datos_recortados) == "Polio")]
corrTF=cor(datos_recortados[,-c(1,2)], method="spearman")
corrout = corrTF >0.7 | corrTF < -0.7
corrout



cor.test(datos_recortados$Thinness_five_nine_years, datos_recortados$Thinness_ten_nineteen_years,alternative = "two.sided", conf.level=0.95,mehod = "spearman")
datos_recortados <- datos_recortados[, -which(names(datos_recortados) == "Thinness_five_nine_years")]
corrTF=cor(datos_recortados[,-c(1,2)], method="spearman")
corrout = corrTF >0.7 | corrTF < -0.7
corrout

cor.test(datos_recortados$BMI, datos_recortados$GDP_per_capita,alternative = "two.sided", conf.level=0.95,mehod = "spearman")
datos_recortados <- datos_recortados[, -which(names(datos_recortados) == "BMI")]
corrTF=cor(datos_recortados[,-c(1,2)], method="spearman")
corrout = corrTF >0.7 | corrTF < -0.7
corrout

correlaciones <- cor(datos_recortados[, sapply(datos_recortados, is.numeric)], method = "spearman")
# Crea un mapa de calor con heatmaply
heatmaply(correlaciones, 
          main = "Correlaciones de Spearman",
          labRow = names(correlaciones),
          labCol = names(correlaciones))

# SELECCIONAR LAS VARIABLES MAS IMPORTANTES PARA EL MODELO
library(leaps)
library(lmtest)
names(datos_recortados)
datos_recortados=datos_recortados[,-c(1,2)]#quitamos las string
#por lo de subdesarrolado, no tendrá un impacto diferenciador con la respuesta
# Realizar la selección de modelos con el criterio de Mallows
seleccion <- regsubsets(Life_expectancy ~ .
                        , data = datos_recortados, nbest = 1, method = "exhaustive", nvmax=12)
summary(seleccion)
# Obtener el resumen de los modelos seleccionados
resumen <-summary(seleccion)
# Identificar el mejor modelo
mejor_modelo <- which.min(resumen$cp)
mejor_modelo


#la variable year no la incluimos, porque esta numerica no nos da ninguna diferenciadora, y si la dividimos
#nos va a separar el modelo en diferentes grupos
prueba_modelo<-lm(Life_expectancy ~Adult_mortality + Alcohol_consumption + Hepatitis_B + Incidents_HIV 
                  + GDP_per_capita + Population_mln + Schooling,data=datos_recortados)

summary(prueba_modelo)
#DIVIDIR ENTRE TRAIN Y TEST
set.seed(5)  # Para reproducir siempre igual, numero favorito profe
variables_modelo <- c("Life_expectancy", "Adult_mortality", "Alcohol_consumption", "Hepatitis_B", "Incidents_HIV" 
                     ,"GDP_per_capita", "Population_mln", "Schooling")
n <- nrow(datos_recortados)
n
indices_entrenamiento <- sample(1:n, 0.7 * n)
train <- datos_recortados[indices_entrenamiento, variables_modelo]
test <- datos_recortados[-indices_entrenamiento, variables_modelo]
nrow(train)
nrow(test)

#MODELO REGRESIÓN LINEAL MÚLTIPLE PONER EL CROSS-VALIDATION
# MODELO ENTRENAMIENTO
modelo_entrenamiento <- lm(Life_expectancy ~ Adult_mortality + Alcohol_consumption + Hepatitis_B + Incidents_HIV 
                           + GDP_per_capita + Population_mln + Schooling,data=train)
#Como ya hemos hecho lo de correlaciones, ya no sale mal
# VIF
library(car)
VIF <- vif(modelo_entrenamiento)
VIF
datosout <- train


# VALIDACION CRUZADA
#primero hace una matriz vacia para poner errores
#luego
library(leaps)
library(car)

predict.regsubsets <- function(object, newdata, id,...){
  form <-as.formula(object$call[[2]])
  mat <- model.matrix(form,newdata)
  coefi <- coef(object,id=id)
  xvars <- names(coefi)
  mat[,xvars]%*%coefi
}

n <- nrow(datosout)
k <- 5 # Número de grupos para la validación cruzada
set.seed(5)
best.fit.final <- regsubsets(Life_expectancy~.,data= datosout)
#no se incluia tampoco
summary(best.fit.final)
folds <- sample(x=1:k, size = nrow(datosout), replace = TRUE)
# Inicializar la matriz para almacenar los errores
cv.errors <- matrix(NA, k, 7, dimnames = list(NULL, paste(1:7)))

# Bucle para la validación cruzada
for (j in 1:k) {
  # Ajustar el modelo con regsubsets en el conjunto de entrenamiento
  best.fit <- regsubsets(Life_expectancy ~ ., data=datosout[folds != j, ])
  for (i in 1:7) {#numero de predictores
    # Predecir en el conjunto de prueba
    pred <- predict.regsubsets(best.fit, newdata=datosout[folds == j, ], id=i)
    # Calcular el error cuadrático medio para este subset
    cv.errors[j, i] <- mean((datosout$Life_expectancy[folds == j] - pred)^2)
    #y esto te hace un error de todos los modelos que salen
  }
}

# Calcular el error medio para cada tamaño de modelo
mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors
which.min(mean.cv.errors)
# Identificar el mejor modelo
best.model.size <- which.min(mean.cv.errors)
coef(best.fit.final, id=best.model.size)

# Diagnóstico adicional para el mejor modelo
model.cv <- lm(Life_expectancy ~ Adult_mortality + Alcohol_consumption + Hepatitis_B + Incidents_HIV 
               + GDP_per_capita + Population_mln + Schooling, data=datosout)#sale 7, pero es por year, pero antes
#no se incluia tampoco
summary(model.cv)
qqPlot(model.cv$residuals)
influenceIndexPlot(model.cv)
leverage=hatvalues(model.cv) #calculamos el valor de leverage
plot(leverage,type="h")
(leverage.alto=c(which(leverage>2)))
leverage[leverage.alto]
rest=rstudent(model.cv) #calculamos los residuos estudentizados
(outliers=c(which(rest>3),which(rest<(-3))))
rest[outliers] #obtenemos el valor del residuo estudentizado

summary(model.cv)
library(lmtest)
lillie.test(model.cv$residuals)
dwtest(model.cv)
bptest(model.cv)
#DIBUJOS(todo lo que han cambiado(a mejor))
boxplot(resid(model.cv), range = 3)
hist(model.cv$residuals)


#COMO NO DAN LOS TEST SACAMOS LOS OUTLIERS
# OUTLIERS DEL MODELO DE ENTRENAMIENTO (1 vez)
outlierTest(modelo)
influenceIndexPlot(modelo)

obs.out <- c(131.1)
datosout <- datosout[!(rownames(datosout) %in% obs.out), ]
rownames(datosout) = NULL


n <- nrow(datosout)
k <- 5 # Número de grupos para la validación cruzada
set.seed(5)
best.fit.final <- regsubsets(Life_expectancy~.,data= datosout)
#no se incluia tampoco
summary(best.fit.final)
folds <- sample(x=1:k, size = nrow(datosout), replace = TRUE)
# Inicializar la matriz para almacenar los errores
cv.errors <- matrix(NA, k, 7, dimnames = list(NULL, paste(1:7)))

# Bucle para la validación cruzada
for (j in 1:k) {
  # Ajustar el modelo con regsubsets en el conjunto de entrenamiento
  best.fit <- regsubsets(Life_expectancy ~ ., data=datosout[folds != j, ])
  for (i in 1:7) {#numero de predictores
    # Predecir en el conjunto de prueba
    pred <- predict.regsubsets(best.fit, newdata=datosout[folds == j, ], id=i)
    # Calcular el error cuadrático medio para este subset
    cv.errors[j, i] <- mean((datosout$Life_expectancy[folds == j] - pred)^2)
    #y esto te hace un error de todos los modelos que salen
  }
}

# Calcular el error medio para cada tamaño de modelo
mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors
which.min(mean.cv.errors)
# Identificar el mejor modelo
best.model.size <- which.min(mean.cv.errors)
coef(best.fit.final, id=best.model.size)

# Diagnóstico adicional para el mejor modelo
model.cv <- lm(Life_expectancy ~ Adult_mortality + Alcohol_consumption + Hepatitis_B + Incidents_HIV 
               + GDP_per_capita + Population_mln + Schooling, data=datosout)#sale 7, pero es por year, pero antes
#no se incluia tampoco
summary(model.cv)
qqPlot(model.cv$residuals)
influenceIndexPlot(model.cv)
leverage=hatvalues(model.cv) #calculamos el valor de leverage
plot(leverage,type="h")
(leverage.alto=c(which(leverage>2)))
leverage[leverage.alto]
rest=rstudent(model.cv) #calculamos los residuos estudentizados
(outliers=c(which(rest>3),which(rest<(-3))))
rest[outliers] #obtenemos el valor del residuo estudentizado

summary(model.cv)
library(lmtest)
lillie.test(model.cv$residuals)
dwtest(model.cv)
bptest(model.cv)

#DIBUJOS(todo lo que han cambiado(a mejor))
boxplot(resid(model.cv), range = 3)
hist(model.cv$residuals)



# OUTLIERS DEL MODELO DE ENTRENAMIENTO (2 vez)
obs.out <- c(1455)
datosout <- datosout[!(rownames(datosout) %in% obs.out), ]
rownames(datosout) = NULL


n <- nrow(datosout)
k <- 5 # Número de grupos para la validación cruzada
set.seed(5)
best.fit.final <- regsubsets(Life_expectancy~.,data= datosout)
#no se incluia tampoco
summary(best.fit.final)
folds <- sample(x=1:k, size = nrow(datosout), replace = TRUE)
# Inicializar la matriz para almacenar los errores
cv.errors <- matrix(NA, k, 7, dimnames = list(NULL, paste(1:7)))

# Bucle para la validación cruzada
for (j in 1:k) {
  # Ajustar el modelo con regsubsets en el conjunto de entrenamiento
  best.fit <- regsubsets(Life_expectancy ~ ., data=datosout[folds != j, ])
  for (i in 1:7) {#numero de predictores
    # Predecir en el conjunto de prueba
    pred <- predict.regsubsets(best.fit, newdata=datosout[folds == j, ], id=i)
    # Calcular el error cuadrático medio para este subset
    cv.errors[j, i] <- mean((datosout$Life_expectancy[folds == j] - pred)^2)
    #y esto te hace un error de todos los modelos que salen
  }
}

# Calcular el error medio para cada tamaño de modelo
mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors
which.min(mean.cv.errors)
# Identificar el mejor modelo
best.model.size <- which.min(mean.cv.errors)
coef(best.fit.final, id=best.model.size)

# Diagnóstico adicional para el mejor modelo
model.cv <- lm(Life_expectancy ~ Adult_mortality + Alcohol_consumption + Hepatitis_B + Incidents_HIV 
               + GDP_per_capita + Population_mln + Schooling, data=datosout)#sale 7, pero es por year, pero antes
#no se incluia tampoco
summary(model.cv)
qqPlot(model.cv$residuals)
influenceIndexPlot(model.cv)
leverage=hatvalues(model.cv) #calculamos el valor de leverage
plot(leverage,type="h")
(leverage.alto=c(which(leverage>2)))
leverage[leverage.alto]
rest=rstudent(model.cv) #calculamos los residuos estudentizados
(outliers=c(which(rest>3),which(rest<(-3))))
rest[outliers] #obtenemos el valor del residuo estudentizado

summary(model.cv)
library(lmtest)
lillie.test(model.cv$residuals)
dwtest(model.cv)
bptest(model.cv)

#DIBUJOS(todo lo que han cambiado(a mejor))
boxplot(resid(model.cv), range = 3)
hist(model.cv$residuals)



#3 vuelta
obs.out <- c(396)
datosout <- datosout[!(rownames(datosout) %in% obs.out), ]
rownames(datosout) = NULL

n <- nrow(datosout)
k <- 5 # Número de grupos para la validación cruzada
set.seed(5)
best.fit.final <- regsubsets(Life_expectancy~.,data= datosout)
#no se incluia tampoco
summary(best.fit.final)
folds <- sample(x=1:k, size = nrow(datosout), replace = TRUE)
# Inicializar la matriz para almacenar los errores
cv.errors <- matrix(NA, k, 7, dimnames = list(NULL, paste(1:7)))

# Bucle para la validación cruzada
for (j in 1:k) {
  # Ajustar el modelo con regsubsets en el conjunto de entrenamiento
  best.fit <- regsubsets(Life_expectancy ~ ., data=datosout[folds != j, ])
  for (i in 1:7) {#numero de predictores
    # Predecir en el conjunto de prueba
    pred <- predict.regsubsets(best.fit, newdata=datosout[folds == j, ], id=i)
    # Calcular el error cuadrático medio para este subset
    cv.errors[j, i] <- mean((datosout$Life_expectancy[folds == j] - pred)^2)
    #y esto te hace un error de todos los modelos que salen
  }
}

# Calcular el error medio para cada tamaño de modelo
mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors
which.min(mean.cv.errors)
# Identificar el mejor modelo
best.model.size <- which.min(mean.cv.errors)
coef(best.fit.final, id=best.model.size)

# Diagnóstico adicional para el mejor modelo
model.cv <- lm(Life_expectancy ~ Adult_mortality + Alcohol_consumption + Hepatitis_B + Incidents_HIV 
               + GDP_per_capita + Population_mln + Schooling, data=datosout)#sale 7, pero es por year, pero antes
#no se incluia tampoco
summary(model.cv)
qqPlot(model.cv$residuals)
influenceIndexPlot(model.cv)
leverage=hatvalues(model.cv) #calculamos el valor de leverage
plot(leverage,type="h")
(leverage.alto=c(which(leverage>2)))
leverage[leverage.alto]
rest=rstudent(model.cv) #calculamos los residuos estudentizados
(outliers=c(which(rest>3),which(rest<(-3))))
rest[outliers] #obtenemos el valor del residuo estudentizado

summary(model.cv)
library(lmtest)
lillie.test(model.cv$residuals)
dwtest(model.cv)
bptest(model.cv)

#DIBUJOS(todo lo que han cambiado(a mejor))
boxplot(resid(model.cv), range = 3)
hist(model.cv$residuals)



#4 vuelta
obs.out <- c(659)
datosout <- datosout[!(rownames(datosout) %in% obs.out), ]
rownames(datosout) = NULL

n <- nrow(datosout)
k <- 5 # Número de grupos para la validación cruzada
set.seed(5)
best.fit.final <- regsubsets(Life_expectancy~.,data= datosout)
#no se incluia tampoco
summary(best.fit.final)
folds <- sample(x=1:k, size = nrow(datosout), replace = TRUE)
# Inicializar la matriz para almacenar los errores
cv.errors <- matrix(NA, k, 7, dimnames = list(NULL, paste(1:7)))

# Bucle para la validación cruzada
for (j in 1:k) {
  # Ajustar el modelo con regsubsets en el conjunto de entrenamiento
  best.fit <- regsubsets(Life_expectancy ~ ., data=datosout[folds != j, ])
  for (i in 1:7) {#numero de predictores
    # Predecir en el conjunto de prueba
    pred <- predict.regsubsets(best.fit, newdata=datosout[folds == j, ], id=i)
    # Calcular el error cuadrático medio para este subset
    cv.errors[j, i] <- mean((datosout$Life_expectancy[folds == j] - pred)^2)
    #y esto te hace un error de todos los modelos que salen
  }
}

# Calcular el error medio para cada tamaño de modelo
mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors
which.min(mean.cv.errors)
# Identificar el mejor modelo
best.model.size <- which.min(mean.cv.errors)
coef(best.fit.final, id=best.model.size)

# Diagnóstico adicional para el mejor modelo
model.cv <- lm(Life_expectancy ~ Adult_mortality + Alcohol_consumption + Hepatitis_B + Incidents_HIV 
               + GDP_per_capita + Population_mln + Schooling, data=datosout)#sale 7, pero es por year, pero antes
#no se incluia tampoco
summary(model.cv)
qqPlot(model.cv$residuals)
influenceIndexPlot(model.cv)
leverage=hatvalues(model.cv) #calculamos el valor de leverage
plot(leverage,type="h")
(leverage.alto=c(which(leverage>2)))
leverage[leverage.alto]
rest=rstudent(model.cv) #calculamos los residuos estudentizados
(outliers=c(which(rest>3),which(rest<(-3))))
rest[outliers] #obtenemos el valor del residuo estudentizado

summary(model.cv)
library(lmtest)
lillie.test(model.cv$residuals)
dwtest(model.cv)
bptest(model.cv)

#DIBUJOS(todo lo que han cambiado(a mejor))
boxplot(resid(model.cv), range = 3)
hist(model.cv$residuals)


#5 vuelta
obs.out <- c(1279)
datosout <- datosout[!(rownames(datosout) %in% obs.out), ]
rownames(datosout) = NULL

n <- nrow(datosout)
k <- 5 # Número de grupos para la validación cruzada
set.seed(5)
best.fit.final <- regsubsets(Life_expectancy~.,data= datosout)
#no se incluia tampoco
summary(best.fit.final)
folds <- sample(x=1:k, size = nrow(datosout), replace = TRUE)
# Inicializar la matriz para almacenar los errores
cv.errors <- matrix(NA, k, 7, dimnames = list(NULL, paste(1:7)))

# Bucle para la validación cruzada
for (j in 1:k) {
  # Ajustar el modelo con regsubsets en el conjunto de entrenamiento
  best.fit <- regsubsets(Life_expectancy ~ ., data=datosout[folds != j, ])
  for (i in 1:7) {#numero de predictores
    # Predecir en el conjunto de prueba
    pred <- predict.regsubsets(best.fit, newdata=datosout[folds == j, ], id=i)
    # Calcular el error cuadrático medio para este subset
    cv.errors[j, i] <- mean((datosout$Life_expectancy[folds == j] - pred)^2)
    #y esto te hace un error de todos los modelos que salen
  }
}

# Calcular el error medio para cada tamaño de modelo
mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors
which.min(mean.cv.errors)
# Identificar el mejor modelo
best.model.size <- which.min(mean.cv.errors)
coef(best.fit.final, id=best.model.size)

# Diagnóstico adicional para el mejor modelo
model.cv <- lm(Life_expectancy ~ Adult_mortality + Alcohol_consumption + Hepatitis_B + Incidents_HIV 
               + GDP_per_capita + Population_mln + Schooling, data=datosout)#sale 7, pero es por year, pero antes
#no se incluia tampoco
summary(model.cv)
qqPlot(model.cv$residuals)
influenceIndexPlot(model.cv)
leverage=hatvalues(model.cv) #calculamos el valor de leverage
plot(leverage,type="h")
(leverage.alto=c(which(leverage>2)))
leverage[leverage.alto]
rest=rstudent(model.cv) #calculamos los residuos estudentizados
(outliers=c(which(rest>3),which(rest<(-3))))
rest[outliers] #obtenemos el valor del residuo estudentizado

summary(model.cv)
library(lmtest)
lillie.test(model.cv$residuals)
dwtest(model.cv)
bptest(model.cv)

#DIBUJOS(todo lo que han cambiado(a mejor))
boxplot(resid(model.cv), range = 3)
hist(model.cv$residuals)



#6 vuelta
obs.out <- c(944)
datosout <- datosout[!(rownames(datosout) %in% obs.out), ]
rownames(datosout) = NULL

n <- nrow(datosout)
k <- 5 # Número de grupos para la validación cruzada
set.seed(5)
best.fit.final <- regsubsets(Life_expectancy~.,data= datosout)
#no se incluia tampoco
summary(best.fit.final)
folds <- sample(x=1:k, size = nrow(datosout), replace = TRUE)
# Inicializar la matriz para almacenar los errores
cv.errors <- matrix(NA, k, 7, dimnames = list(NULL, paste(1:7)))

# Bucle para la validación cruzada
for (j in 1:k) {
  # Ajustar el modelo con regsubsets en el conjunto de entrenamiento
  best.fit <- regsubsets(Life_expectancy ~ ., data=datosout[folds != j, ])
  for (i in 1:7) {#numero de predictores
    # Predecir en el conjunto de prueba
    pred <- predict.regsubsets(best.fit, newdata=datosout[folds == j, ], id=i)
    # Calcular el error cuadrático medio para este subset
    cv.errors[j, i] <- mean((datosout$Life_expectancy[folds == j] - pred)^2)
    #y esto te hace un error de todos los modelos que salen
  }
}

# Calcular el error medio para cada tamaño de modelo
mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors
which.min(mean.cv.errors)
# Identificar el mejor modelo
best.model.size <- which.min(mean.cv.errors)
coef(best.fit.final, id=best.model.size)

# Diagnóstico adicional para el mejor modelo
model.cv <- lm(Life_expectancy ~ Adult_mortality + Alcohol_consumption + Hepatitis_B + Incidents_HIV 
               + GDP_per_capita + Population_mln + Schooling, data=datosout)#sale 7, pero es por year, pero antes
#no se incluia tampoco
summary(model.cv)
qqPlot(model.cv$residuals)
influenceIndexPlot(model.cv)
leverage=hatvalues(model.cv) #calculamos el valor de leverage
plot(leverage,type="h")
(leverage.alto=c(which(leverage>2)))
leverage[leverage.alto]
rest=rstudent(model.cv) #calculamos los residuos estudentizados
(outliers=c(which(rest>3),which(rest<(-3))))
rest[outliers] #obtenemos el valor del residuo estudentizado

summary(model.cv)
library(lmtest)
lillie.test(model.cv$residuals)
dwtest(model.cv)
bptest(model.cv)

#DIBUJOS(todo lo que han cambiado(a mejor))
boxplot(resid(model.cv), range = 3)
hist(model.cv$residuals)



#7 vuelta
obs.out <- c(643)
datosout <- datosout[!(rownames(datosout) %in% obs.out), ]
rownames(datosout) = NULL

n <- nrow(datosout)
k <- 5 # Número de grupos para la validación cruzada
set.seed(5)
best.fit.final <- regsubsets(Life_expectancy~.,data= datosout)
#no se incluia tampoco
summary(best.fit.final)
folds <- sample(x=1:k, size = nrow(datosout), replace = TRUE)
# Inicializar la matriz para almacenar los errores
cv.errors <- matrix(NA, k, 7, dimnames = list(NULL, paste(1:7)))

# Bucle para la validación cruzada
for (j in 1:k) {
  # Ajustar el modelo con regsubsets en el conjunto de entrenamiento
  best.fit <- regsubsets(Life_expectancy ~ ., data=datosout[folds != j, ])
  for (i in 1:7) {#numero de predictores
    # Predecir en el conjunto de prueba
    pred <- predict.regsubsets(best.fit, newdata=datosout[folds == j, ], id=i)
    # Calcular el error cuadrático medio para este subset
    cv.errors[j, i] <- mean((datosout$Life_expectancy[folds == j] - pred)^2)
    #y esto te hace un error de todos los modelos que salen
  }
}

# Calcular el error medio para cada tamaño de modelo
mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors
which.min(mean.cv.errors)
# Identificar el mejor modelo
best.model.size <- which.min(mean.cv.errors)
coef(best.fit.final, id=best.model.size)

# Diagnóstico adicional para el mejor modelo
model.cv <- lm(Life_expectancy ~ Adult_mortality + Alcohol_consumption + Hepatitis_B + Incidents_HIV 
               + GDP_per_capita + Population_mln + Schooling, data=datosout)#sale 7, pero es por year, pero antes
#no se incluia tampoco
summary(model.cv)
qqPlot(model.cv$residuals)
influenceIndexPlot(model.cv)
leverage=hatvalues(model.cv) #calculamos el valor de leverage
plot(leverage,type="h")
(leverage.alto=c(which(leverage>2)))
leverage[leverage.alto]
rest=rstudent(model.cv) #calculamos los residuos estudentizados
(outliers=c(which(rest>3),which(rest<(-3))))
rest[outliers] #obtenemos el valor del residuo estudentizado


summary(model.cv)
library(lmtest)
lillie.test(model.cv$residuals)
dwtest(model.cv)
bptest(model.cv)

#DIBUJOS(todo lo que han cambiado(a mejor))
boxplot(resid(model.cv), range = 3)
hist(model.cv$residuals)


#8 vuelta
obs.out <- c(69)
datosout <- datosout[!(rownames(datosout) %in% obs.out), ]
rownames(datosout) = NULL

n <- nrow(datosout)
k <- 5 # Número de grupos para la validación cruzada
set.seed(5)
best.fit.final <- regsubsets(Life_expectancy~.,data= datosout)
#no se incluia tampoco
summary(best.fit.final)
folds <- sample(x=1:k, size = nrow(datosout), replace = TRUE)
# Inicializar la matriz para almacenar los errores
cv.errors <- matrix(NA, k, 7, dimnames = list(NULL, paste(1:7)))

# Bucle para la validación cruzada
for (j in 1:k) {
  # Ajustar el modelo con regsubsets en el conjunto de entrenamiento
  best.fit <- regsubsets(Life_expectancy ~ ., data=datosout[folds != j, ])
  for (i in 1:7) {#numero de predictores
    # Predecir en el conjunto de prueba
    pred <- predict.regsubsets(best.fit, newdata=datosout[folds == j, ], id=i)
    # Calcular el error cuadrático medio para este subset
    cv.errors[j, i] <- mean((datosout$Life_expectancy[folds == j] - pred)^2)
    #y esto te hace un error de todos los modelos que salen
  }
}

# Calcular el error medio para cada tamaño de modelo
mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors
which.min(mean.cv.errors)
# Identificar el mejor modelo
best.model.size <- which.min(mean.cv.errors)
coef(best.fit.final, id=best.model.size)

# Diagnóstico adicional para el mejor modelo
model.cv <- lm(Life_expectancy ~ Adult_mortality + Alcohol_consumption + Hepatitis_B + Incidents_HIV 
               + GDP_per_capita + Population_mln + Schooling, data=datosout)#sale 7, pero es por year, pero antes
#no se incluia tampoco
summary(model.cv)
qqPlot(model.cv$residuals)
influenceIndexPlot(model.cv)
leverage=hatvalues(model.cv) #calculamos el valor de leverage
plot(leverage,type="h")
(leverage.alto=c(which(leverage>2)))
leverage[leverage.alto]
rest=rstudent(model.cv) #calculamos los residuos estudentizados
(outliers=c(which(rest>3),which(rest<(-3))))
rest[outliers] #obtenemos el valor del residuo estudentizado

summary(model.cv)
library(lmtest)
lillie.test(model.cv$residuals)
dwtest(model.cv)
bptest(model.cv)

#DIBUJOS(todo lo que han cambiado(a mejor))
boxplot(resid(model.cv), range = 3)
hist(model.cv$residuals)


#9 vuelta
obs.out <- c(361)
datosout <- datosout[!(rownames(datosout) %in% obs.out), ]
rownames(datosout) = NULL

n <- nrow(datosout)
k <- 5 # Número de grupos para la validación cruzada
set.seed(5)
best.fit.final <- regsubsets(Life_expectancy~.,data= datosout)
#no se incluia tampoco
summary(best.fit.final)
folds <- sample(x=1:k, size = nrow(datosout), replace = TRUE)
# Inicializar la matriz para almacenar los errores
cv.errors <- matrix(NA, k, 7, dimnames = list(NULL, paste(1:7)))

# Bucle para la validación cruzada
for (j in 1:k) {
  # Ajustar el modelo con regsubsets en el conjunto de entrenamiento
  best.fit <- regsubsets(Life_expectancy ~ ., data=datosout[folds != j, ])
  for (i in 1:7) {#numero de predictores
    # Predecir en el conjunto de prueba
    pred <- predict.regsubsets(best.fit, newdata=datosout[folds == j, ], id=i)
    # Calcular el error cuadrático medio para este subset
    cv.errors[j, i] <- mean((datosout$Life_expectancy[folds == j] - pred)^2)
    #y esto te hace un error de todos los modelos que salen
  }
}

# Calcular el error medio para cada tamaño de modelo
mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors
which.min(mean.cv.errors)
# Identificar el mejor modelo
best.model.size <- which.min(mean.cv.errors)
coef(best.fit.final, id=best.model.size)

# Diagnóstico adicional para el mejor modelo
model.cv <- lm(Life_expectancy ~ Adult_mortality + Alcohol_consumption + Hepatitis_B + Incidents_HIV 
               + GDP_per_capita + Population_mln + Schooling, data=datosout)#sale 7, pero es por year, pero antes
#no se incluia tampoco
summary(model.cv)
qqPlot(model.cv$residuals)
influenceIndexPlot(model.cv)
leverage=hatvalues(model.cv) #calculamos el valor de leverage
plot(leverage,type="h")
(leverage.alto=c(which(leverage>2)))
leverage[leverage.alto]
rest=rstudent(model.cv) #calculamos los residuos estudentizados
(outliers=c(which(rest>3),which(rest<(-3))))
rest[outliers] #obtenemos el valor del residuo estudentizado

summary(model.cv)
library(lmtest)
lillie.test(model.cv$residuals)
dwtest(model.cv)
bptest(model.cv)

#DIBUJOS(todo lo que han cambiado(a mejor))
boxplot(resid(model.cv), range = 3)
hist(model.cv$residuals)


#8 vuelta
obs.out <- c(843)
datosout <- datosout[!(rownames(datosout) %in% obs.out), ]
rownames(datosout) = NULL

n <- nrow(datosout)
k <- 5 # Número de grupos para la validación cruzada
set.seed(5)
best.fit.final <- regsubsets(Life_expectancy~.,data= datosout)
#no se incluia tampoco
summary(best.fit.final)
folds <- sample(x=1:k, size = nrow(datosout), replace = TRUE)
# Inicializar la matriz para almacenar los errores
cv.errors <- matrix(NA, k, 7, dimnames = list(NULL, paste(1:7)))

# Bucle para la validación cruzada
for (j in 1:k) {
  # Ajustar el modelo con regsubsets en el conjunto de entrenamiento
  best.fit <- regsubsets(Life_expectancy ~ ., data=datosout[folds != j, ])
  for (i in 1:7) {#numero de predictores
    # Predecir en el conjunto de prueba
    pred <- predict.regsubsets(best.fit, newdata=datosout[folds == j, ], id=i)
    # Calcular el error cuadrático medio para este subset
    cv.errors[j, i] <- mean((datosout$Life_expectancy[folds == j] - pred)^2)
    #y esto te hace un error de todos los modelos que salen
  }
}

# Calcular el error medio para cada tamaño de modelo
mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors
which.min(mean.cv.errors)
# Identificar el mejor modelo
best.model.size <- which.min(mean.cv.errors)
coef(best.fit.final, id=best.model.size)

# Diagnóstico adicional para el mejor modelo
model.cv <- lm(Life_expectancy ~ Adult_mortality + Alcohol_consumption + Hepatitis_B + Incidents_HIV 
               + GDP_per_capita + Population_mln + Schooling, data=datosout)#sale 7, pero es por year, pero antes
#no se incluia tampoco
summary(model.cv)
qqPlot(model.cv$residuals)
influenceIndexPlot(model.cv)
leverage=hatvalues(model.cv) #calculamos el valor de leverage
plot(leverage,type="h")
(leverage.alto=c(which(leverage>2)))
leverage[leverage.alto]
rest=rstudent(model.cv) #calculamos los residuos estudentizados
(outliers=c(which(rest>3),which(rest<(-3))))
rest[outliers] #obtenemos el valor del residuo estudentizado

summary(model.cv)
library(lmtest)
lillie.test(model.cv$residuals)
dwtest(model.cv)
bptest(model.cv)

#DIBUJOS(todo lo que han cambiado(a mejor))
boxplot(resid(model.cv), range = 3)
hist(model.cv$residuals)


#9 vuelta
obs.out <- c(245)
datosout <- datosout[!(rownames(datosout) %in% obs.out), ]
rownames(datosout) = NULL

n <- nrow(datosout)
k <- 5 # Número de grupos para la validación cruzada
set.seed(5)
best.fit.final <- regsubsets(Life_expectancy~.,data= datosout)
#no se incluia tampoco
summary(best.fit.final)
folds <- sample(x=1:k, size = nrow(datosout), replace = TRUE)
# Inicializar la matriz para almacenar los errores
cv.errors <- matrix(NA, k, 7, dimnames = list(NULL, paste(1:7)))

# Bucle para la validación cruzada
for (j in 1:k) {
  # Ajustar el modelo con regsubsets en el conjunto de entrenamiento
  best.fit <- regsubsets(Life_expectancy ~ ., data=datosout[folds != j, ])
  for (i in 1:7) {#numero de predictores
    # Predecir en el conjunto de prueba
    pred <- predict.regsubsets(best.fit, newdata=datosout[folds == j, ], id=i)
    # Calcular el error cuadrático medio para este subset
    cv.errors[j, i] <- mean((datosout$Life_expectancy[folds == j] - pred)^2)
    #y esto te hace un error de todos los modelos que salen
  }
}

# Calcular el error medio para cada tamaño de modelo
mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors
which.min(mean.cv.errors)
# Identificar el mejor modelo
best.model.size <- which.min(mean.cv.errors)
coef(best.fit.final, id=best.model.size)

# Diagnóstico adicional para el mejor modelo
model.cv <- lm(Life_expectancy ~ Adult_mortality + Alcohol_consumption + Hepatitis_B + Incidents_HIV 
               + GDP_per_capita + Population_mln + Schooling, data=datosout)#sale 7, pero es por year, pero antes
#no se incluia tampoco
summary(model.cv)
qqPlot(model.cv$residuals)
influenceIndexPlot(model.cv)
leverage=hatvalues(model.cv) #calculamos el valor de leverage
plot(leverage,type="h")
(leverage.alto=c(which(leverage>2)))
leverage[leverage.alto]
rest=rstudent(model.cv) #calculamos los residuos estudentizados
(outliers=c(which(rest>3),which(rest<(-3))))
rest[outliers] #obtenemos el valor del residuo estudentizado

summary(model.cv)
library(lmtest)
lillie.test(model.cv$residuals)
dwtest(model.cv)
bptest(model.cv)

#DIBUJOS(todo lo que han cambiado(a mejor))
boxplot(resid(model.cv), range = 3)
hist(model.cv$residuals)



#9 vuelta
obs.out <- c(683)
datosout <- datosout[!(rownames(datosout) %in% obs.out), ]
rownames(datosout) = NULL

n <- nrow(datosout)
k <- 5 # Número de grupos para la validación cruzada
set.seed(5)
best.fit.final <- regsubsets(Life_expectancy~.,data= datosout)
#no se incluia tampoco
summary(best.fit.final)
folds <- sample(x=1:k, size = nrow(datosout), replace = TRUE)
# Inicializar la matriz para almacenar los errores
cv.errors <- matrix(NA, k, 7, dimnames = list(NULL, paste(1:7)))

# Bucle para la validación cruzada
for (j in 1:k) {
  # Ajustar el modelo con regsubsets en el conjunto de entrenamiento
  best.fit <- regsubsets(Life_expectancy ~ ., data=datosout[folds != j, ])
  for (i in 1:7) {#numero de predictores
    # Predecir en el conjunto de prueba
    pred <- predict.regsubsets(best.fit, newdata=datosout[folds == j, ], id=i)
    # Calcular el error cuadrático medio para este subset
    cv.errors[j, i] <- mean((datosout$Life_expectancy[folds == j] - pred)^2)
    #y esto te hace un error de todos los modelos que salen
  }
}

# Calcular el error medio para cada tamaño de modelo
mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors
which.min(mean.cv.errors)
# Identificar el mejor modelo
best.model.size <- which.min(mean.cv.errors)
coef(best.fit.final, id=best.model.size)

# Diagnóstico adicional para el mejor modelo
model.cv <- lm(Life_expectancy ~ Adult_mortality + Alcohol_consumption + Hepatitis_B + Incidents_HIV 
               + GDP_per_capita + Population_mln + Schooling, data=datosout)#sale 7, pero es por year, pero antes
#no se incluia tampoco
summary(model.cv)
qqPlot(model.cv$residuals)
influenceIndexPlot(model.cv)
leverage=hatvalues(model.cv) #calculamos el valor de leverage
plot(leverage,type="h")
(leverage.alto=c(which(leverage>2)))
leverage[leverage.alto]
rest=rstudent(model.cv) #calculamos los residuos estudentizados
(outliers=c(which(rest>3),which(rest<(-3))))
rest[outliers] #obtenemos el valor del residuo estudentizado

summary(model.cv)
library(lmtest)
lillie.test(model.cv$residuals)
dwtest(model.cv)
bptest(model.cv)

#DIBUJOS(todo lo que han cambiado(a mejor))
boxplot(resid(model.cv), range = 3)
hist(model.cv$residuals)


#10 vuelta
obs.out <- c(717)
datosout <- datosout[!(rownames(datosout) %in% obs.out), ]
rownames(datosout) = NULL

n <- nrow(datosout)
k <- 5 # Número de grupos para la validación cruzada
set.seed(5)
best.fit.final <- regsubsets(Life_expectancy~.,data= datosout)
#no se incluia tampoco
summary(best.fit.final)
folds <- sample(x=1:k, size = nrow(datosout), replace = TRUE)
# Inicializar la matriz para almacenar los errores
cv.errors <- matrix(NA, k, 7, dimnames = list(NULL, paste(1:7)))

# Bucle para la validación cruzada
for (j in 1:k) {
  # Ajustar el modelo con regsubsets en el conjunto de entrenamiento
  best.fit <- regsubsets(Life_expectancy ~ ., data=datosout[folds != j, ])
  for (i in 1:7) {#numero de predictores
    # Predecir en el conjunto de prueba
    pred <- predict.regsubsets(best.fit, newdata=datosout[folds == j, ], id=i)
    # Calcular el error cuadrático medio para este subset
    cv.errors[j, i] <- mean((datosout$Life_expectancy[folds == j] - pred)^2)
    #y esto te hace un error de todos los modelos que salen
  }
}

# Calcular el error medio para cada tamaño de modelo
mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors
which.min(mean.cv.errors)
# Identificar el mejor modelo
best.model.size <- which.min(mean.cv.errors)
coef(best.fit.final, id=best.model.size)

# Diagnóstico adicional para el mejor modelo
model.cv <- lm(Life_expectancy ~ Adult_mortality + Alcohol_consumption + Hepatitis_B + Incidents_HIV 
               + GDP_per_capita + Population_mln + Schooling, data=datosout)#sale 7, pero es por year, pero antes
#no se incluia tampoco
summary(model.cv)
qqPlot(model.cv$residuals)
influenceIndexPlot(model.cv)
leverage=hatvalues(model.cv) #calculamos el valor de leverage
plot(leverage,type="h")
(leverage.alto=c(which(leverage>2)))
leverage[leverage.alto]
rest=rstudent(model.cv) #calculamos los residuos estudentizados
(outliers=c(which(rest>3),which(rest<(-3))))
rest[outliers] #obtenemos el valor del residuo estudentizado

summary(model.cv)
library(lmtest)
lillie.test(model.cv$residuals)
dwtest(model.cv)
bptest(model.cv)

#DIBUJOS(todo lo que han cambiado(a mejor))
boxplot(resid(model.cv), range = 3)
hist(model.cv$residuals)


#11 vuelta
obs.out <- c(846)
datosout <- datosout[!(rownames(datosout) %in% obs.out), ]
rownames(datosout) = NULL

n <- nrow(datosout)
k <- 5 # Número de grupos para la validación cruzada
set.seed(5)
best.fit.final <- regsubsets(Life_expectancy~.,data= datosout)
#no se incluia tampoco
summary(best.fit.final)
folds <- sample(x=1:k, size = nrow(datosout), replace = TRUE)
# Inicializar la matriz para almacenar los errores
cv.errors <- matrix(NA, k, 7, dimnames = list(NULL, paste(1:7)))

# Bucle para la validación cruzada
for (j in 1:k) {
  # Ajustar el modelo con regsubsets en el conjunto de entrenamiento
  best.fit <- regsubsets(Life_expectancy ~ ., data=datosout[folds != j, ])
  for (i in 1:7) {#numero de predictores
    # Predecir en el conjunto de prueba
    pred <- predict.regsubsets(best.fit, newdata=datosout[folds == j, ], id=i)
    # Calcular el error cuadrático medio para este subset
    cv.errors[j, i] <- mean((datosout$Life_expectancy[folds == j] - pred)^2)
    #y esto te hace un error de todos los modelos que salen
  }
}

# Calcular el error medio para cada tamaño de modelo
mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors
which.min(mean.cv.errors)
# Identificar el mejor modelo
best.model.size <- which.min(mean.cv.errors)
coef(best.fit.final, id=best.model.size)

# Diagnóstico adicional para el mejor modelo
model.cv <- lm(Life_expectancy ~ Adult_mortality + Alcohol_consumption + Hepatitis_B + Incidents_HIV 
               + GDP_per_capita + Population_mln + Schooling, data=datosout)#sale 7, pero es por year, pero antes
#no se incluia tampoco
summary(model.cv)
qqPlot(model.cv$residuals)
influenceIndexPlot(model.cv)
leverage=hatvalues(model.cv) #calculamos el valor de leverage
plot(leverage,type="h")
(leverage.alto=c(which(leverage>2)))
leverage[leverage.alto]
rest=rstudent(model.cv) #calculamos los residuos estudentizados
(outliers=c(which(rest>3),which(rest<(-3))))
rest[outliers] #obtenemos el valor del residuo estudentizado

summary(model.cv)
library(lmtest)
lillie.test(model.cv$residuals)
dwtest(model.cv)
bptest(model.cv)

#DIBUJOS(todo lo que han cambiado(a mejor))
boxplot(resid(model.cv), range = 3)
hist(model.cv$residuals)


#12 vuelta
obs.out <- c(653)
datosout <- datosout[!(rownames(datosout) %in% obs.out), ]
rownames(datosout) = NULL

n <- nrow(datosout)
k <- 5 # Número de grupos para la validación cruzada
set.seed(5)
best.fit.final <- regsubsets(Life_expectancy~.,data= datosout)
#no se incluia tampoco
summary(best.fit.final)
folds <- sample(x=1:k, size = nrow(datosout), replace = TRUE)
# Inicializar la matriz para almacenar los errores
cv.errors <- matrix(NA, k, 7, dimnames = list(NULL, paste(1:7)))

# Bucle para la validación cruzada
for (j in 1:k) {
  # Ajustar el modelo con regsubsets en el conjunto de entrenamiento
  best.fit <- regsubsets(Life_expectancy ~ ., data=datosout[folds != j, ])
  for (i in 1:7) {#numero de predictores
    # Predecir en el conjunto de prueba
    pred <- predict.regsubsets(best.fit, newdata=datosout[folds == j, ], id=i)
    # Calcular el error cuadrático medio para este subset
    cv.errors[j, i] <- mean((datosout$Life_expectancy[folds == j] - pred)^2)
    #y esto te hace un error de todos los modelos que salen
  }
}

# Calcular el error medio para cada tamaño de modelo
mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors
which.min(mean.cv.errors)
# Identificar el mejor modelo
best.model.size <- which.min(mean.cv.errors)
coef(best.fit.final, id=best.model.size)

# Diagnóstico adicional para el mejor modelo
model.cv <- lm(Life_expectancy ~ Adult_mortality + Alcohol_consumption + Hepatitis_B + Incidents_HIV 
               + GDP_per_capita + Population_mln + Schooling, data=datosout)#sale 7, pero es por year, pero antes
#no se incluia tampoco
summary(model.cv)
qqPlot(model.cv$residuals)
influenceIndexPlot(model.cv)
leverage=hatvalues(model.cv) #calculamos el valor de leverage
plot(leverage,type="h")
(leverage.alto=c(which(leverage>2)))
leverage[leverage.alto]
rest=rstudent(model.cv) #calculamos los residuos estudentizados
(outliers=c(which(rest>3),which(rest<(-3))))
rest[outliers] #obtenemos el valor del residuo estudentizado

summary(model.cv)
library(lmtest)
lillie.test(model.cv$residuals)
dwtest(model.cv)
bptest(model.cv)

#DIBUJOS(todo lo que han cambiado(a mejor))
boxplot(resid(model.cv), range = 3)
hist(model.cv$residuals)


#13 vuelta
obs.out <- c(189)
datosout <- datosout[!(rownames(datosout) %in% obs.out), ]
rownames(datosout) = NULL

n <- nrow(datosout)
k <- 5 # Número de grupos para la validación cruzada
set.seed(5)
best.fit.final <- regsubsets(Life_expectancy~.,data= datosout)
#no se incluia tampoco
summary(best.fit.final)
folds <- sample(x=1:k, size = nrow(datosout), replace = TRUE)
# Inicializar la matriz para almacenar los errores
cv.errors <- matrix(NA, k, 7, dimnames = list(NULL, paste(1:7)))

# Bucle para la validación cruzada
for (j in 1:k) {
  # Ajustar el modelo con regsubsets en el conjunto de entrenamiento
  best.fit <- regsubsets(Life_expectancy ~ ., data=datosout[folds != j, ])
  for (i in 1:7) {#numero de predictores
    # Predecir en el conjunto de prueba
    pred <- predict.regsubsets(best.fit, newdata=datosout[folds == j, ], id=i)
    # Calcular el error cuadrático medio para este subset
    cv.errors[j, i] <- mean((datosout$Life_expectancy[folds == j] - pred)^2)
    #y esto te hace un error de todos los modelos que salen
  }
}

# Calcular el error medio para cada tamaño de modelo
mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors
which.min(mean.cv.errors)
# Identificar el mejor modelo
best.model.size <- which.min(mean.cv.errors)
coef(best.fit.final, id=best.model.size)

# Diagnóstico adicional para el mejor modelo
model.cv <- lm(Life_expectancy ~ Adult_mortality + Alcohol_consumption + Hepatitis_B + Incidents_HIV 
               + GDP_per_capita + Population_mln + Schooling, data=datosout)#sale 7, pero es por year, pero antes
#no se incluia tampoco
summary(model.cv)
qqPlot(model.cv$residuals)
influenceIndexPlot(model.cv)
leverage=hatvalues(model.cv) #calculamos el valor de leverage
plot(leverage,type="h")
(leverage.alto=c(which(leverage>2)))
leverage[leverage.alto]
rest=rstudent(model.cv) #calculamos los residuos estudentizados
(outliers=c(which(rest>3),which(rest<(-3))))
rest[outliers] #obtenemos el valor del residuo estudentizado

summary(model.cv)
library(lmtest)
lillie.test(model.cv$residuals)
dwtest(model.cv)
bptest(model.cv)

#DIBUJOS(todo lo que han cambiado(a mejor))
boxplot(resid(model.cv), range = 3)
hist(model.cv$residuals)


#14 vuelta
obs.out <- c(1094)
datosout <- datosout[!(rownames(datosout) %in% obs.out), ]
rownames(datosout) = NULL

n <- nrow(datosout)
k <- 5 # Número de grupos para la validación cruzada
set.seed(5)
best.fit.final <- regsubsets(Life_expectancy~.,data= datosout)
#no se incluia tampoco
summary(best.fit.final)
folds <- sample(x=1:k, size = nrow(datosout), replace = TRUE)
# Inicializar la matriz para almacenar los errores
cv.errors <- matrix(NA, k, 7, dimnames = list(NULL, paste(1:7)))

# Bucle para la validación cruzada
for (j in 1:k) {
  # Ajustar el modelo con regsubsets en el conjunto de entrenamiento
  best.fit <- regsubsets(Life_expectancy ~ ., data=datosout[folds != j, ])
  for (i in 1:7) {#numero de predictores
    # Predecir en el conjunto de prueba
    pred <- predict.regsubsets(best.fit, newdata=datosout[folds == j, ], id=i)
    # Calcular el error cuadrático medio para este subset
    cv.errors[j, i] <- mean((datosout$Life_expectancy[folds == j] - pred)^2)
    #y esto te hace un error de todos los modelos que salen
  }
}

# Calcular el error medio para cada tamaño de modelo
mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors
which.min(mean.cv.errors)
# Identificar el mejor modelo
best.model.size <- which.min(mean.cv.errors)
coef(best.fit.final, id=best.model.size)

# Diagnóstico adicional para el mejor modelo
model.cv <- lm(Life_expectancy ~ Adult_mortality + Alcohol_consumption + Hepatitis_B + Incidents_HIV 
               + GDP_per_capita + Population_mln + Schooling, data=datosout)#sale 7, pero es por year, pero antes
#no se incluia tampoco
summary(model.cv)
qqPlot(model.cv$residuals)
influenceIndexPlot(model.cv)
leverage=hatvalues(model.cv) #calculamos el valor de leverage
plot(leverage,type="h")
(leverage.alto=c(which(leverage>2)))
leverage[leverage.alto]
rest=rstudent(model.cv) #calculamos los residuos estudentizados
(outliers=c(which(rest>3),which(rest<(-3))))
rest[outliers] #obtenemos el valor del residuo estudentizado

summary(model.cv)
library(lmtest)
lillie.test(model.cv$residuals)
dwtest(model.cv)
bptest(model.cv)


#15 vuelta
obs.out <- c(1094)
datosout <- datosout[!(rownames(datosout) %in% obs.out), ]
rownames(datosout) = NULL

n <- nrow(datosout)
k <- 5 # Número de grupos para la validación cruzada
set.seed(5)
best.fit.final <- regsubsets(Life_expectancy~.,data= datosout)
#no se incluia tampoco
summary(best.fit.final)
folds <- sample(x=1:k, size = nrow(datosout), replace = TRUE)
# Inicializar la matriz para almacenar los errores
cv.errors <- matrix(NA, k, 7, dimnames = list(NULL, paste(1:7)))

# Bucle para la validación cruzada
for (j in 1:k) {
  # Ajustar el modelo con regsubsets en el conjunto de entrenamiento
  best.fit <- regsubsets(Life_expectancy ~ ., data=datosout[folds != j, ])
  for (i in 1:7) {#numero de predictores
    # Predecir en el conjunto de prueba
    pred <- predict.regsubsets(best.fit, newdata=datosout[folds == j, ], id=i)
    # Calcular el error cuadrático medio para este subset
    cv.errors[j, i] <- mean((datosout$Life_expectancy[folds == j] - pred)^2)
    #y esto te hace un error de todos los modelos que salen
  }
}

# Calcular el error medio para cada tamaño de modelo
mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors
which.min(mean.cv.errors)
# Identificar el mejor modelo
best.model.size <- which.min(mean.cv.errors)
coef(best.fit.final, id=best.model.size)

# Diagnóstico adicional para el mejor modelo
model.cv <- lm(Life_expectancy ~ Adult_mortality + Alcohol_consumption + Hepatitis_B + Incidents_HIV 
               + GDP_per_capita + Population_mln + Schooling, data=datosout)#sale 7, pero es por year, pero antes
#no se incluia tampoco
summary(model.cv)
qqPlot(model.cv$residuals)
influenceIndexPlot(model.cv)
leverage=hatvalues(model.cv) #calculamos el valor de leverage
plot(leverage,type="h")
(leverage.alto=c(which(leverage>2)))
leverage[leverage.alto]
rest=rstudent(model.cv) #calculamos los residuos estudentizados
(outliers=c(which(rest>3),which(rest<(-3))))
rest[outliers] #obtenemos el valor del residuo estudentizado

summary(model.cv)
library(lmtest)
lillie.test(model.cv$residuals)
dwtest(model.cv)
bptest(model.cv)


#16 vuelta
obs.out <- c(435)
datosout <- datosout[!(rownames(datosout) %in% obs.out), ]
rownames(datosout) = NULL

n <- nrow(datosout)
k <- 5 # Número de grupos para la validación cruzada
set.seed(5)
best.fit.final <- regsubsets(Life_expectancy~.,data= datosout)
#no se incluia tampoco
summary(best.fit.final)
folds <- sample(x=1:k, size = nrow(datosout), replace = TRUE)
# Inicializar la matriz para almacenar los errores
cv.errors <- matrix(NA, k, 7, dimnames = list(NULL, paste(1:7)))

# Bucle para la validación cruzada
for (j in 1:k) {
  # Ajustar el modelo con regsubsets en el conjunto de entrenamiento
  best.fit <- regsubsets(Life_expectancy ~ ., data=datosout[folds != j, ])
  for (i in 1:7) {#numero de predictores
    # Predecir en el conjunto de prueba
    pred <- predict.regsubsets(best.fit, newdata=datosout[folds == j, ], id=i)
    # Calcular el error cuadrático medio para este subset
    cv.errors[j, i] <- mean((datosout$Life_expectancy[folds == j] - pred)^2)
    #y esto te hace un error de todos los modelos que salen
  }
}

# Calcular el error medio para cada tamaño de modelo
mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors
which.min(mean.cv.errors)
# Identificar el mejor modelo
best.model.size <- which.min(mean.cv.errors)
coef(best.fit.final, id=best.model.size)

# Diagnóstico adicional para el mejor modelo
model.cv <- lm(Life_expectancy ~ Adult_mortality + Alcohol_consumption + Hepatitis_B + Incidents_HIV 
               + GDP_per_capita + Population_mln + Schooling, data=datosout)#sale 7, pero es por year, pero antes
#no se incluia tampoco
summary(model.cv)
qqPlot(model.cv$residuals)
influenceIndexPlot(model.cv)
leverage=hatvalues(model.cv) #calculamos el valor de leverage
plot(leverage,type="h")
(leverage.alto=c(which(leverage>2)))
leverage[leverage.alto]
rest=rstudent(model.cv) #calculamos los residuos estudentizados
(outliers=c(which(rest>3),which(rest<(-3))))
rest[outliers] #obtenemos el valor del residuo estudentizado

summary(model.cv)
library(lmtest)
lillie.test(model.cv$residuals)
dwtest(model.cv)
bptest(model.cv)


#17 vuelta
obs.out <- c(737)
datosout <- datosout[!(rownames(datosout) %in% obs.out), ]
rownames(datosout) = NULL

n <- nrow(datosout)
k <- 5 # Número de grupos para la validación cruzada
set.seed(5)
best.fit.final <- regsubsets(Life_expectancy~.,data= datosout)
#no se incluia tampoco
summary(best.fit.final)
folds <- sample(x=1:k, size = nrow(datosout), replace = TRUE)
# Inicializar la matriz para almacenar los errores
cv.errors <- matrix(NA, k, 7, dimnames = list(NULL, paste(1:7)))

# Bucle para la validación cruzada
for (j in 1:k) {
  # Ajustar el modelo con regsubsets en el conjunto de entrenamiento
  best.fit <- regsubsets(Life_expectancy ~ ., data=datosout[folds != j, ])
  for (i in 1:7) {#numero de predictores
    # Predecir en el conjunto de prueba
    pred <- predict.regsubsets(best.fit, newdata=datosout[folds == j, ], id=i)
    # Calcular el error cuadrático medio para este subset
    cv.errors[j, i] <- mean((datosout$Life_expectancy[folds == j] - pred)^2)
    #y esto te hace un error de todos los modelos que salen
  }
}

# Calcular el error medio para cada tamaño de modelo
mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors
which.min(mean.cv.errors)
# Identificar el mejor modelo
best.model.size <- which.min(mean.cv.errors)
coef(best.fit.final, id=best.model.size)

# Diagnóstico adicional para el mejor modelo
model.cv <- lm(Life_expectancy ~ Adult_mortality + Alcohol_consumption + Hepatitis_B + Incidents_HIV 
               + GDP_per_capita + Population_mln + Schooling, data=datosout)#sale 7, pero es por year, pero antes
#no se incluia tampoco
summary(model.cv)
qqPlot(model.cv$residuals)
influenceIndexPlot(model.cv)
leverage=hatvalues(model.cv) #calculamos el valor de leverage
plot(leverage,type="h")
(leverage.alto=c(which(leverage>2)))
leverage[leverage.alto]
rest=rstudent(model.cv) #calculamos los residuos estudentizados
(outliers=c(which(rest>3),which(rest<(-3))))
rest[outliers] #obtenemos el valor del residuo estudentizado

summary(model.cv)
library(lmtest)
lillie.test(model.cv$residuals)
dwtest(model.cv)
bptest(model.cv)


#18 vuelta
obs.out <- c(415)
#415
datosout <- datosout[!(rownames(datosout) %in% obs.out), ]
rownames(datosout) = NULL

n <- nrow(datosout)
k <- 5 # Número de grupos para la validación cruzada
set.seed(5)
best.fit.final <- regsubsets(Life_expectancy~.,data= datosout)
#no se incluia tampoco
summary(best.fit.final)
folds <- sample(x=1:k, size = nrow(datosout), replace = TRUE)
# Inicializar la matriz para almacenar los errores
cv.errors <- matrix(NA, k, 7, dimnames = list(NULL, paste(1:7)))

# Bucle para la validación cruzada
for (j in 1:k) {
  # Ajustar el modelo con regsubsets en el conjunto de entrenamiento
  best.fit <- regsubsets(Life_expectancy ~ ., data=datosout[folds != j, ])
  for (i in 1:7) {#numero de predictores
    # Predecir en el conjunto de prueba
    pred <- predict.regsubsets(best.fit, newdata=datosout[folds == j, ], id=i)
    # Calcular el error cuadrático medio para este subset
    cv.errors[j, i] <- mean((datosout$Life_expectancy[folds == j] - pred)^2)
    #y esto te hace un error de todos los modelos que salen
  }
}

# Calcular el error medio para cada tamaño de modelo
mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors
which.min(mean.cv.errors)
# Identificar el mejor modelo
best.model.size <- which.min(mean.cv.errors)
coef(best.fit.final, id=best.model.size)

# Diagnóstico adicional para el mejor modelo
model.cv <- lm(Life_expectancy ~ Adult_mortality + Alcohol_consumption + Hepatitis_B + Incidents_HIV 
               + GDP_per_capita + Population_mln + Schooling, data=datosout)#sale 7, pero es por year, pero antes
#no se incluia tampoco
summary(model.cv)
qqPlot(model.cv$residuals)
influenceIndexPlot(model.cv)
leverage=hatvalues(model.cv) #calculamos el valor de leverage
plot(leverage,type="h")
(leverage.alto=c(which(leverage>2)))
leverage[leverage.alto]
rest=rstudent(model.cv) #calculamos los residuos estudentizados
(outliers=c(which(rest>3),which(rest<(-3))))
rest[outliers] #obtenemos el valor del residuo estudentizado

summary(model.cv)
lillie.test(model.cv$residuals)
dwtest(model.cv)
bptest(model.cv)


#DIBUJOS(todo lo que han cambiado(a mejor))
boxplot(resid(model.cv), range = 3)
hist(model.cv$residuals)


#Como sigue sin dar aplicamos box-cox
x<-datosout$Life_expectancy
modelo <- lm(x ~ 1)
# Aplicar la función boxcox para encontrar el mejor lambda
b <- boxcox(modelo)
# Extraer el valor de lambda
lambda <- b$x[which.max(b$y)]
# Aplicar la transformación de Box-Cox
ynewtrain <- (x^lambda - 1) / lambda
datosout$Life_expectancy = ynewtrain
# Mostrar el valor de lambda
lambda

x<-test$Life_expectancy
modelo <- lm(x ~ 1)
# Aplicar la función boxcox para encontrar el mejor lambda
b <- boxcox(modelo)
# Aplicar la transformación de Box-Cox
ynewtest <- (x^lambda - 1) / lambda
test$Life_expectancy = ynewtest


# Asumiendo que 'train' es tu conjunto de datos y 'Life_expectancy' tu variable dependiente
n <- nrow(datosout)
k <- 5 # Número de grupos para la validación cruzada
set.seed(5)
best.fit.final <- regsubsets(Life_expectancy~.,data= datosout)
#no se incluia tampoco
summary(best.fit.final)
folds <- sample(x=1:k, size = nrow(datosout), replace = TRUE)
# Inicializar la matriz para almacenar los errores
cv.errors <- matrix(NA, k, 7, dimnames = list(NULL, paste(1:7)))

# Bucle para la validación cruzada
for (j in 1:k) {
  # Ajustar el modelo con regsubsets en el conjunto de entrenamiento
  best.fit <- regsubsets(Life_expectancy ~ ., data=datosout[folds != j, ])
  for (i in 1:7) {#numero de predictores
    # Predecir en el conjunto de prueba
    pred <- predict.regsubsets(best.fit, newdata=datosout[folds == j, ], id=i)
    # Calcular el error cuadrático medio para este subset
    cv.errors[j, i] <- mean((datosout$Life_expectancy[folds == j] - pred)^2)
    #y esto te hace un error de todos los modelos que salen
  }
}

# Calcular el error medio para cada tamaño de modelo
mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors
which.min(mean.cv.errors)
# Identificar el mejor modelo
best.model.size <- which.min(mean.cv.errors)
coef(best.fit.final, id=best.model.size)

# Diagnóstico adicional para el mejor modelo
model.cv <- lm(Life_expectancy ~ Adult_mortality + Alcohol_consumption + Hepatitis_B + Incidents_HIV 
               + GDP_per_capita + Population_mln + Schooling, data=datosout)#sale 7, pero es por year, pero antes
#no se incluia tampoco
summary(model.cv)
qqPlot(model.cv$residuals)
influenceIndexPlot(model.cv)
leverage=hatvalues(model.cv) #calculamos el valor de leverage
plot(leverage,type="h")
(leverage.alto=c(which(leverage>2)))
leverage[leverage.alto]
rest=rstudent(model.cv) #calculamos los residuos estudentizados
(outliers=c(which(rest>3),which(rest<(-3))))
rest[outliers] #obtenemos el valor del residuo estudentizado

summary(model.cv)
library(lmtest)
lillie.test(model.cv$residuals)
dwtest(model.cv)
bptest(model.cv)


#DIBUJOS(todo lo que han cambiado(a mejor))
boxplot(resid(model.cv), range = 3)
hist(model.cv$residuals)

y_original_train <- ifelse(lambda == 0, exp(ynewtrain) - 1, 
                           (datosout$Life_expectancy * lambda + 1)^(1/lambda) - 1)

# Revertir la transformación Box-Cox en datos de prueba
y_original_test <- ifelse(lambda == 0, exp(ynewtest) - 1, 
                          (test$Life_expectancy * lambda + 1)^(1/lambda) - 1)
# Hacer predicciones en el conjunto test
prediccion <- predict(model.cv, newdata = test)
prediccion_original <- ifelse(lambda == 0, exp(prediccion) - 1, (prediccion * lambda + 1)^(1/lambda) - 1)
# Extraer los valores reales de la variable dependiente del conjunto test
actual <- y_original_test
# Calcular el Error Cuadrático Medio (ECM)
ECM <- mean((prediccion_original - actual)^2)
# Mostrar el ECM
ECM
sqrt(ECM)

#HEMOS LOGRADO NORMALIDAD E INDEPENDENCIA, PERO NO HOMOCEDASTICIDAD

#MODELO REGULARIZADO(3)

#install.packages("glmnet")
library(glmnet)
# Prepara los datos
x_train <- as.matrix(datosout[, -which(names(train) == "Life_expectancy")])
y_train <- datosout$Life_expectancy
x_test <- as.matrix(test[, -which(names(test) == "Life_expectancy")])
y_test <- test$Life_expectancy
#RIDGE
# Establece un rango de valores lambda para probar
grid <- 10^seq(10, -2, length=100)
# Ajusta el modelo Ridge en el conjunto de entrenamiento
ridge_mod <- glmnet(x_train, y_train, alpha=0, lambda=grid, thresh=1e-12)
# Realiza la validación cruzada para encontrar el mejor lambda
set.seed(5)  # para resultados reproducibles
cv_out <- cv.glmnet(x_train, y_train, alpha=0)
# Encuentra el mejor lambda
best_lambda <- cv_out$lambda.min
# Ajusta el modelo Ridge en el conjunto completo usando el mejor lambda
ridge_pred <- predict(ridge_mod, s=best_lambda, newx=x_test)
# Calcula el ECM en el conjunto de prueba
ridge_pred_original <- ifelse(lambda == 0, exp(ridge_pred) - 1, 
                     (ridge_pred * lambda + 1)^(1/lambda) - 1)
ecm <- mean((ridge_pred_original - y_original_test)^2)
ecm
sqrt(ecm)

ridge_coef <- coef(ridge_mod, s = best_lambda)
# Convierte la matriz dispersa a un formato más manejable
ridge_coef <- as.matrix(ridge_coef)
# Extrae los nombres de las variables y sus coeficientes asociados
coef_df <- data.frame(
  variable = rownames(ridge_coef),
  coefficient = ridge_coef[,1]
)
# Muestra las variables con coeficientes diferentes de cero
non_zero_coef <- coef_df[coef_df$coefficient != 0, ]
print(non_zero_coef)



#LASSO
# Establece un rango de valores lambda para probar con Lasso
grid <- 10^seq(10, -2, length=100)
# Ajusta el modelo Lasso en el conjunto de entrenamiento
lasso_mod <- glmnet(x_train, y_train, alpha=1, lambda=grid)
# Realiza la validación cruzada para encontrar el mejor lambda para Lasso
set.seed(1)  # Establece una semilla para reproducibilidad
cv_lasso_out <- cv.glmnet(x_train, y_train, alpha=1)
# Encuentra el mejor lambda
best_lambda_lasso <- cv_lasso_out$lambda.min
# Ajusta el modelo Lasso en el conjunto completo usando el mejor lambda
lasso_pred <- predict(lasso_mod, s=best_lambda_lasso, newx=x_test)
lasso_pred_original<-ifelse(lambda == 0, exp(lasso_pred) - 1, 
                                (lasso_pred * lambda + 1)^(1/lambda) - 1)

# Calcula el ECM en el conjunto de prueba
ecm_lasso <- mean((lasso_pred_original - y_original_test)^2)
# Mostrar el ECM para Lasso
ecm_lasso
sqrt(ecm_lasso)
lasso_coef <- coef(lasso_mod, s = best_lambda_lasso)
# Convertir la matriz dispersa en una matriz normal
lasso_coef_matrix <- as.matrix(lasso_coef)
# Crea un data frame con los nombres de las variables y los coeficientes
lasso_coef_df <- data.frame(variable = rownames(lasso_coef_matrix), 
                            coefficient = lasso_coef_matrix[,1])
# Filtrar las variables con coeficientes distintos de cero
lasso_coef_df <- lasso_coef_df[lasso_coef_df$coefficient != 0, ]
# Imprimir las variables que tienen coeficientes distintos de cero
print(lasso_coef_df)



#ELASTIC NET
# Establecer un rango de valores lambda para probar
grid <- 10^seq(10, -2, length=100)
# Ajustar el modelo Elastic Net en el conjunto de entrenamiento
# con alpha=0.9 que le da más peso a la penalización Lasso
set.seed(25)  # Establece una semilla para reproducibilidad
elasnet_mod <- glmnet(x_train, y_train, alpha=0.9, lambda=grid)
# Realizar validación cruzada para encontrar el mejor lambda para Elastic Net
cv_elasnet_out <- cv.glmnet(x_train, y_train, alpha=0.9)
# Encontrar el mejor lambda a partir de la validación cruzada
best_lambda_elasnet <- cv_elasnet_out$lambda.min
# Predecir utilizando el modelo Elastic Net en el conjunto de prueba
elasnet_pred <- predict(elasnet_mod, s=best_lambda_elasnet, newx=x_test)
# Revertir la transformación Box-Cox en las predicciones del modelo Elastic Net
elasnet_pred_original <- ifelse(lambda == 0, exp(elasnet_pred) - 1, 
                       (elasnet_pred * lambda + 1)^(1/lambda) - 1)
# Calcular el ECM en el conjunto de prueba
ecm_elasnet <- mean((elasnet_pred_original - y_original_test)^2)
# Mostrar el ECM para Elastic Net
ecm_elasnet
sqrt(ecm_elasnet)

#NCOMPS
# Cargar las librerías necesarias
library(factoextra)
# Escalar los datos seleccionados
datos_escalados <- scale(datosout)
# Unir de nuevo la variable dependiente
train_scaled <- cbind(datos_escalados, Life_expectancy = datosout$Life_expectancy)
# Realizar PCA
pca_resultado <- prcomp(train_scaled, scale. = TRUE)
# Graficar la varianza explicada
plot(pca_resultado, type = "l", main = "Varianza explicada por cada componente principal")
# Calcular y graficar la varianza acumulada
var_explicada <- summary(pca_resultado)$importance[2,]
ncomps <- which(cumsum(var_explicada) >= 0.7)[1]
# Graficar la varianza acumulada
plot(cumsum(var_explicada), xlab = "Componente Principal", ylab = "Varianza Acumulada", type = "b", 
     main = "Varianza Acumulada por Componente Principal")
# Mostrar el número de componentes seleccionados
ncomps


#MODELO PCR
library(pls)
#SOLO HACIA FALTA PLS, ESTABA BIEN
#ncomp es 3
# Ajustar un modelo de PCR
pcr_fit <- pcr(Life_expectancy ~ ., data=datosout, scale=TRUE, ncomp=3, validation="CV")
coef(pcr_fit)

# Resumen del modelo PCR
summary(pcr_fit)
# Validación cruzada del modelo PCR
validationplot(pcr_fit, val.type="MSEP")
MSEP(pcr_fit)
# Predecir utilizando el modelo PCR en el conjunto de prueba
pcr_pred_test <- predict(pcr_fit, test, ncomp=3)
pcr_pred_test_original <- ifelse(lambda == 0, exp(pcr_pred_test) 
- 1, (pcr_pred_test * lambda + 1)^(1/lambda) - 1)
# Calcular ECM para el conjunto de prueba
pcr_ecm_test <- mean((pcr_pred_test_original - y_original_test)^2)
# Mostrar ECM
pcr_ecm_test
sqrt(pcr_ecm_test)

#MODELO PLS
# Ajustar un modelo de PLS en el conjunto de entrenamiento
pls_fit <- plsr(Life_expectancy ~ ., data=datosout, scale=TRUE, ncomp=2, validation="CV")
# Resumen del modelo PLS
summary(pls_fit)
# Validación cruzada del modelo PLS
validationplot(pls_fit, val.type="MSEP")
# Predecir utilizando el modelo PLS en el conjunto de prueba
pls_pred_test <- predict(pls_fit, test, ncomp=2)
pls_pred_test_original <- ifelse(lambda == 0, exp(pls_pred_test) - 1, 
(pls_pred_test * lambda + 1)^(1/lambda) - 1)
# Calcular ECM para el conjunto de prueba
pls_ecm_test <- mean((pls_pred_test_original - y_original_test)^2)
# Mostrar ECM
pls_ecm_test
sqrt(pls_ecm_test)



#REGRESION LOGÍSTICA
mediana_vida <- median(datos_recortados$Life_expectancy)
mediana_vida
# Crear la variable binaria basada en la mediana
datos_recortados$Life_expectancy <- ifelse(datos_recortados$Life_expectancy < mediana_vida, 0, 1)
datos_recortados$Life_expectancy <- as.factor(datos_recortados$Life_expectancy)
head(datos_recortados)
seleccion <- regsubsets(Life_expectancy ~ .
                        , data = datos_recortados, nbest = 1, method = "exhaustive", nvmax = 10)
summary(seleccion)
# Obtener el resumen de los modelos seleccionados
resumen <-summary(seleccion)
# Identificar el mejor modelo
mejor_modelo <- which.min(resumen$cp)
mejor_modelo
library(caret)
library(MASS)

#DIVIDIR ENTRE TRAIN Y TEST
set.seed(5)  # Para reproducir siempre igual, numero favorito profe
variables_modelo_logistico <- c("Life_expectancy",
                                "Adult_mortality", "Alcohol_consumption", 
                                "Hepatitis_B", "Measles","Incidents_HIV ","GDP_per_capita ",
                                "Population_mln ", "Thinness_ten_nineteen_years","Schooling")
# Dividir los datos en conjuntos de entrenamiento y prueba
n<-nrow(datos_recortados)
indices_entrenamiento <- sample(1:n, 0.7 * n)
train_logistico <- datos_recortados[indices_entrenamiento,]
test_logistico <- datos_recortados[-indices_entrenamiento,]

# Ajustar el modelo de regresión logística utilizando los datos de entrenamiento
modelo_logistico <- glm(Life_expectancy ~ Adult_mortality + Alcohol_consumption + 
                        Hepatitis_B + Measles + Incidents_HIV + GDP_per_capita +
                        Population_mln + Thinness_ten_nineteen_years + Schooling,
                        data = train_logistico, 
                        family = binomial)

# Realizar validación cruzada
control_cv <- trainControl(method = "cv", number = 10)  # 10-fold cross-validation
modelo_cv <- train(Life_expectancy ~Adult_mortality + Alcohol_consumption + 
                     Hepatitis_B + Measles + Incidents_HIV + GDP_per_capita +
                     Population_mln + Thinness_ten_nineteen_years + Schooling, 
                   data = train_logistico, 
                   method = "glm", 
                   family = binomial, 
                   trControl = control_cv)

# Realizar predicciones en el conjunto de prueba
predicciones_prob <- predict(modelo_logistico, newdata = test_logistico, type = "response")
predicciones_clase <- ifelse(predicciones_prob > 0.5, 1, 0)

# Calcular la matriz de confusión y la precisión
matriz_confusion <- confusionMatrix(as.factor(predicciones_clase), as.factor(test_logistico$Life_expectancy))
accuracy <- sum(diag(matriz_confusion$table)) / sum(matriz_confusion$table)

# Imprimir los resultados
print(summary(modelo_logistico))
print(modelo_cv)
print(matriz_confusion)
print(accuracy)

