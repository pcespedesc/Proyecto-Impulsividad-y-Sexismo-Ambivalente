#4.ANÁLISIS DESCRIPTIVOS 

# Resumen de cada base 
dfSummary(datos_m1[, c("EIP","SEXHOS","SEXBENE","EDAD","SEXO","GRUPO")])
dfSummary(datos_m2[, c("EIP","PATERPRO","DIGENERO","INTIMIDAD","EDAD","SEXO","GRUPO")])

#Descriptivos por grupo y sexo - Modelo 1 (objetivo específico 1)

#Crosstable por grupo
crosstable(datos_m1,
           c(EIP, SEXHOS, SEXBENE, EDAD),
           by = GRUPO,
           showNA = "no",
           funs = c(mean, sd)) %>%
  as_flextable(keep_id = FALSE)
#Los reclusos tienen más edad (media de 36 años) que los estudiantes (media de 22 años) y mayores puntajes en ambos tipos de sexismo que los estudiantes
#La impulsividad es similar entre ambos grupos. 

#Crosstable por sexo
crosstable(datos_m1,
           c(EIP, SEXHOS, SEXBENE, EDAD),
           by = SEXO,
           showNA = "no",
           funs = c(mean, sd)) %>%
  as_flextable(keep_id = FALSE)
#Hombres tienen puntajes ligeramente superiores en sexismo, aunque la diferencia es mínima
#La impulsividad es practicamente igual. En cuanto a la edad los hombres en promedio son mayores (29 vs 23)

#Descriptivos por grupo y sexo - Modelo 2 

#Crosstable por grupo
crosstable(datos_m2,
           c(EIP, PATERPRO, DIGENERO, INTIMIDAD, EDAD),
           by = GRUPO,
           showNA = "no",
           funs = c(mean, sd)) %>%
  as_flextable(keep_id = FALSE)
#Los reclusos puntúan consistentemente más alto en las tres subdimensiones del sexismo benevolente. 
#La diferencia más grande es en la subdimensión de paternalismo protector. 

#Crosstable por sexo
crosstable(datos_m2,
           c(EIP, PATERPRO, DIGENERO, INTIMIDAD, EDAD),
           by = SEXO,
           showNA = "no",
           funs = c(mean, sd)) %>%
  as_flextable(keep_id = FALSE)
#Las mujeres puntúan más bajo que los hombres en las subdimensiones de paternalismo e intimidad, pero ligeramente más alto
#en diferenciación de género. Aunque las diferencias son pequeñas. 

#Histogramas exploratorios de distribución de cada variable
par(mfrow = c(2,3))
hist(datos_m1$EIP,     main = "EIP",      xlab = "") #La mayoría de valores se concentran entre 0.5 y 1.5, con pocos casos en los extremos. 
hist(datos_m1$SEXHOS,  main = "SEXHOS",   xlab = "") #Los valores se concentran entre 10 y 25, con una concentración en los valores medios. 
hist(datos_m1$SEXBENE, main = "SEXBENE",  xlab = "") #Es la que más se aproxima a una distribución normal, con una mayor frecuencia entre 16 y 24
hist(datos_m2$PATERPRO,main = "PATERPRO", xlab = "") #Mayor concentración entre 5 y 9 
hist(datos_m2$DIGENERO,main = "DIGENERO", xlab = "") #Concentración principal entre 4 y 7
hist(datos_m2$INTIMIDAD,main = "INTIMIDAD",xlab = "") #Distribución cercana a la normalidad  con una mayor concentración entre 5 y 7
par(mfrow = c(1,1))

#Boxplots para explorar diferencias entre grupos y sexo 
boxplot(EIP ~ SEXO * GRUPO,      data = datos_m1, main = "EIP por sexo y grupo") #Medianas muy similares entre estudiantes y mujer reclusa, pero hombre recluso presenta la mediana más baja, aunque esta diferencia puede no ser muy grande
boxplot(SEXHOS ~ SEXO * GRUPO,   data = datos_m1, main = "Sexismo Hostil") #Los reclusos parecen presentar mayores niveles de sexismo hostil que los estudiantes. La mediana más alta es mujer reclusa mientras que la mediana más baja es mujer estudiante
boxplot(SEXBENE ~ SEXO * GRUPO,  data = datos_m1, main = "Sexismo Benevolente") #Las mujeres reclusas presentan la mediana más alta y los dos grupos de estudiantes muestran medianas menores. 
boxplot(PATERPRO ~ SEXO * GRUPO, data = datos_m2, main = "Paternalismo Protector") #Las mujeres reclusas tienen la mediana más alta, seguido de los hombres reclusos 
boxplot(DIGENERO ~ SEXO * GRUPO, data = datos_m2, main = "Diferenciación de Género") #Se destacan los outliers. Los reclusos tienen mayor diferenciación de género que los estudiantes. Entre estudiantes, los hombres muestran niveles más bajos
boxplot(INTIMIDAD ~ SEXO * GRUPO,data = datos_m2, main = "Intimidad Heterosexual") #Las medianas son muy parecidas en los grupos

