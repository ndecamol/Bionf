# Natalia de Castro_Trabajo2.R

# Trabajo final Bioinformática - Curso 25/26

# Análisis de parámetros biomédicos por tratamiento

getwd()

# 1. Cargar librerías (si necesarias) y datos del archivo "datos_biomed.csv". (0.5 pts)

datos_biomed <- read.csv("datos_biomed.csv")

# 2. Exploración inicial con las funciones head(), summary(), dim() y str(). ¿Cuántas variables hay? ¿Cuántos tratamientos? (0.5 pts)

head(datos_biomed)     # Muestra las 6 primeras filas, así nos aseguramos que el archivo se cargó bien y vemos cómo se llaman las variables.

summary(datos_biomed)  # Da un resumen estadístico de cada variable, por ejemplo, para datos numéricos muestra el mínimo, el máximo, el promedio etc.

dim(datos_biomed)      # Vemos dos números, el primero cuántas filas y el segundo cuántas columnas

str(datos_biomed)      # Entender de qué tipo es cada variable y cómo R la está interpretando.

# 3. Una gráfica que incluya todos los boxplots por tratamiento. (1 pt)

# Crear la gráfica con boxplots por tratamiento

par(mfrow = c(1, 3))  # Divide la pantalla en 1 fila y 3 columnas

# Boxplot para Glucosa por tratamiento

boxplot(Glucosa ~ Tratamiento, data = datos_biomed, main = "Glucosa por Tratamiento")# "~" esto significa separado por, por lo tanto le estamos diciendo "muéstrame la Glucosa SEPARADA por Tratamiento"

# Boxplot para Presión por tratamiento

boxplot(Presion ~ Tratamiento, data= datos_biomed, main = "Presión por Tratamiento")


# Boxplot para Colesterol por tratamiento

boxplot(Colesterol ~ Tratamiento, data = datos_biomed, main = "Colesterol por Tratamiento")
        

# 4. Realiza un violin plot (investiga qué es). (1 pt)

# Un violinplot es un gráfico que muestra la densidad de datos, es decir, cuántos datos hay de cada valor, cómo se concentran los datos. Combina un histograma con un boxplot.

install.packages("vioplot")

# Descargamos la librería para violin plots porque aún no la hemos descargado

library(vioplot)

# Preparar pantalla para 3 gráficos

par(mfrow = c(1, 3))

# Creamos los 3 violin plots y les ponemos color. Usamos la misma estructura que hemos usado para hacer boxplot.

vioplot(Glucosa ~ Tratamiento, data = datos_biomed, main = "Glucosa",
        col = c("lightblue"))

vioplot(Presion ~ Tratamiento, data = datos_biomed, main = "Presión",
        col = c("lightgreen"))

vioplot(Colesterol ~ Tratamiento, data = datos_biomed, main = "Colesterol",
        col = c("pink"))


# 5. Realiza un gráfico de dispersión "Glucosa vs Presión". Emplea legend() para incluir una leyenda en la parte inferior derecha. (1 pt)

# Crear el gráfico de dispersión con colores por tratamiento

plot(datos_biomed$Glucosa, datos_biomed$Presion,
     
     #usamos la función "lab()" para añadir títulos
     
     xlab = "Glucosa", #variable glucosa en el eje X 
     
     ylab = "Presión",#variable presión en el eje y
     
     main = "Glucosa vs Presión", #así ponemos el título principal del gráfico
     
     pch = 16,#define la forma de los puntos, esta indica que son sólidos y redondos
     
     col = ifelse(datos_biomed$Tratamiento == "FarmacoA", "lightblue", # ¿el tratamiento es elfármaco A? si sí, entonces color azul, si no, continúa con el siguiente "if else"
                  ifelse(datos_biomed$Tratamiento == "FarmacoB", "lightgreen", "pink"))) #¿El tratamiento es "FarmacoB"? Si sí, entonces color verde, si no, entonces color rosa.

# Añadir leyenda en la parte inferior derecha

legend("bottomright", #Posiciona la leyenda en la esquina inferior derecha del gráfico 
       
       legend = c("FarmacoA", "FarmacoB", "Placebo"),
       
       col = c("lightblue", "lightgreen", "pink"),
       
       pch = 16,
       
       title = "Tratamiento") #título de la leyenda


# 6. Realiza un facet Grid (investiga qué es): Colesterol vs Presión por tratamiento. (1 pt)

#Es una técnica de visualización que divide el gráfico en varios paneles en función de una o más variables categóricas. Cada panel muestra la misma relación entre variables, pero para diferentes categorías.

library(ggplot2) #cargamos la librería

ggplot(datos_biomed, aes(x = Colesterol, y = Presion)) + #seleccionamos qué variables van el eje X y cuáles en el eje y de la gráfica
  
  geom_point(aes(color = Tratamiento), size = 2) + #geom_point añade puntos al gráfico, puntos de tamaño 2 con un color u otro dependiendo del tipo de tratamiento empleado
  
  facet_grid(. ~ Tratamiento) +  # Filas ~ Columnas, así dividimos el gráfico en varios paneles
  
  labs(title = "Colesterol vs Presión por Tratamiento", x = "Nivel de Colesterol (mg/dL)", y = "Presión Arterial (mmHg)") 
  
  

# 7. Realiza un histogramas para cada variable. (0.5 pts)
  
#Como he reiniciado Rstudio tenemos que "desconectar" el paquete ggplot2 de la sesión de R, con "unload" eliminamos las funciones ggplot2 de la memoria, después lo volvemos a cargar, así evitamos interrupciones.
  
detach("package:ggplot2", unload = TRUE)

library(ggplot2) #lo volvemos a cargar

# Histograma de Glucosa

  ggplot(datos_biomed, aes(x = Glucosa)) +
    
  geom_histogram(binwidth = 10, #"binwidth" es la función que dibuja las barras del histograma, le hemos puesto un ancho de 10.
                 
                 fill = "lightblue", #"color" es el color del borde de las barras
                 
                 color = "black", alpha = 0.7) + # "alpha" es la transparencia de las barras, siendo "1" opaco y "0" totalmente transparente.
    
  labs(title = "Distribución de Glucosa", 
       
       x = "Nivel de Glucosa (mg/dL)", #eje X
       
       y = "Frecuencia")  #eje Y
    
    
# Histograma de Presión
    
ggplot(datos_biomed, aes(x = Presion)) +
    
  geom_histogram(binwidth = 5, fill = "lightgreen", color = "black", alpha = 0.7) +
    
  labs(title = "Distribución de Presión Arterial", x = "Presión Arterial (mmHg)", y = "Frecuencia") 
  
# Histograma de Colesterol
    
ggplot(datos_biomed, aes(x = Colesterol)) +
    
  geom_histogram(binwidth = 20, fill = "pink", color = "black", alpha = 0.7) +
    
  labs(title = "Distribución de Colesterol", x = "Nivel de Colesterol (mg/dL)", y = "Frecuencia") 


# 8. Crea un factor a partir del tratamiento. Investiga factor(). (1 pt)

#Un factor es una variable que almacena datos categóricos, de esta forma R no lee los datos como texto sino como categorías organizadas.

datos_biomed$Tratamiento_factor <- factor(datos_biomed$Tratamiento)


# 9. Obtén la media y desviación estándar de los niveles de glucosa por tratamiento. Emplea aggregate() o apply(). (0.5 pts)

# Media de glucosa por tratamiento

#la función "aggregate" permite calcular estadísticas por grupos, hay que indicarle

media_glucosa <- aggregate(Glucosa ~ Tratamiento, #glucosa agrupada (~) por tratamiento 
                           
                           data = datos_biomed, # los datos
                           
                           FUN = mean) #la operación

print("MEDIA de Glucosa por Tratamiento:")

print(media_glucosa)

# Desviación estándar de glucosa por tratamiento

desv_glucosa <- aggregate(Glucosa ~ Tratamiento,data = datos_biomed,FUN = sd)

print("DESVIACIÓN ESTÁNDAR de Glucosa por Tratamiento:")

print(desv_glucosa)


# 10. Extrae los datos para cada tratamiento y almacenalos en una variable. Ejemplo todos los datos de Placebo en una variable llamada placebo. (1 pt)

#Con la función subset extraemos solo las filas que nos interesan de la tabla de datos.

# Extraer datos de Placebo

placebo <- subset(datos_biomed, Tratamiento == "Placebo") #Todos los pacientes que recibieron Placebo

# Extraer datos de FarmacoA

farmacoA <- subset(datos_biomed, Tratamiento == "FarmacoA") #Todos los pacientes que recibieron Fármaco A

# Extraer datos de FarmacoB

farmacoB <- subset(datos_biomed, Tratamiento == "FarmacoB") #Todos los pacientes que recibieron Fármaco B

# Para ver el resultado numérico utilizamos la función "nrow" 

cat("Placebo:", nrow(placebo), "pacientes\n")

cat("FarmacoA:", nrow(farmacoA), "pacientes\n")

cat("FarmacoB:", nrow(farmacoB), "pacientes\n")



# 11. Evalúa si los datos siguen una distribución normal y realiza una comparativa de medias acorde. (1 pt)

# Hacemos un test de normalidad (Shapiro Wilk)

shapiro_result <- by (datos_biomed$Glucosa, # "by()" aplica una función por grupos #datos_biomed$Glucosa es la variable a analizar
                     
                     datos_biomed$Tratamiento, #  esta es la variable que define los tres grupos, placebo, fármaco A y fáramco B
                     
                     shapiro.test) #Test de Shapiro-Wilk para normalidad

print("Normalidad por tratamiento:") #Muestra los p-values del test de Shapiro-Wilk para cada grupo.

print(shapiro_result)

# Decidir test

if(all(sapply(shapiro_result, #Extrae todos los p-values
              
              function(x) x$p.value > 0.05))) { #Verifica si TODOS los p-values son mayores a 0.05 (distribución normal)
  
  print("USAR ANOVA:") #Si todos los grupos son normales (p > 0.05) usamos ANOVA
  
  print(summary(aov(Glucosa ~ Tratamiento, data = datos_biomed)))
  
} else { #Si algún grupo no es normal (p ≤ 0.05) usamos Kruskal-Wallis 
  print("USAR KRUSKAL-WALLIS:")
  
  print(kruskal.test(Glucosa ~ Tratamiento, data = datos_biomed))
}


# 12. Realiza un ANOVA sobre la glucosa para cada tratamiento. (1 pt)

#Para poder hacer un anova nos tenemos que asegurar de que la distribución de datos es normal y que exista homogenicidad de varianzas

#Comprobamos que la distribución de los datos es normal

shapiro_result <- by(datos_biomed$Glucosa, datos_biomed$Tratamiento, shapiro.test)

print("=== PRUEBA DE NORMALIDAD (Shapiro-Wilk) por tratamiento ===")
print(shapiro_result)

#Comprobamos la homogenicidad de varianzas. Para que el ANOVA sea válido y confiable tenemos que asegurarnos de que los tres grupos de tratamiento para glucosa tienen varianzas similares, hacemos el "test de levene" para evitar conclusiones erróneas

install.packages("car")

library(car) #"car" tiene funciones avanzadas como "levene"

levene_test <- leveneTest(Glucosa ~ Tratamiento, data = datos_biomed) #Verifica si las varianzas de los tres grupos (FarmacoA, FarmacoB, Placebo) son iguales 

print("Prueba de Levene (homogeneidad de varianzas):")

print(levene_test)

#Decidir qué test usar según cómo sean las varianzas, usamos ANOVA CLÁSICO si hay homogenicidad de varianzas y WELCH ANOVA si NO hay homogenicidad de varianzas.

#Hipótesis del test: 

   #H₀ (Hipótesis nula): Las varianzas son iguales (homogéneas)

   #H₁ (Hipótesis alternativa): Las varianzas NO son iguales (heterogéneas)

   #p > 0.05 → "No tenemos evidencia suficiente para decir que las varianzas son diferentes", por tanto no rechazamos la hipótesis nula, podemos decir que las varianzas son iguales 

   #p ≤ 0.05 → "Tenemos evidencia estadística de que las varianzas son diferentes", por tanto en este caso rechazamos la hipótesis nula, podemos decir que las varianzas no son iguales


if(levene_test$"Pr(>F)"[1] > 0.05) { #No rechazamos hipótesis nula
  
  print("USAR ANOVA CLÁSICO (varianzas iguales)")
  
  anova_glucosa <- aov(Glucosa ~ Tratamiento, data = datos_biomed)
  
} else { #rechazamos hipótesis nula
  
  print("USAR WELCH ANOVA (varianzas desiguales)")
  
  resultado <- oneway.test(Glucosa ~ Tratamiento, data = datos_biomed)
  
}

# Si el ANOVA es significativo (hay diferencias reales entre al menos dos tratamientos), hacer post-hoc para saber exactamente entre qué tratamientos

#Hipótesis del ANOVA:
   
   #H₀: Todos los tratamientos tienen el mismo efecto

   #H₁: Al menos un tratamiento es diferente

   #p > 0.05 → No rechazamos H₀ → Concluimos que medias = iguales, esto indica que el ANOVA no es significativo 
   
   #p ≤ 0.05 → Rechazamos H₀ → Concluimos que medias ≠ iguales, esto indica que el ANOVA es significativo

if(summary(anova_glucosa)[[1]]$"Pr(>F)"[1] < 0.05) {  #hacemos el Post-Hoc solo si el anova es significativo, es decir, si p <= 0,05
  
  print("=== COMPARACIONES POST-HOC (Tukey HSD) ===")
  
  tukey_result <- TukeyHSD(anova_glucosa)
  
  print(tukey_result)
}



