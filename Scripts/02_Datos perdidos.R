#3.MANEJO DE DATOS PERDIDOS 

# Se identifican cuántos y cuáles son los datos faltantes por variable
colSums(is.na(datos))
lapply(datos, function(x) which(is.na(x)))

#Se dejan solo las filas que tienen datos completos según los modelos (dados por los objetivos)

#MODELO 1 (Impulsividad y Escalas totales de sexismo hostil y benevolente)
#MODELO 2 (Impulsividad y Subdimensiones del sexismo benevolente)

# Se eliminan los casos en los que una persona tiene un dato perdido según el modelo
datos_m1 <- datos[complete.cases(datos[, c("EIP","SEXHOS","SEXBENE")]), ]
datos_m2 <- datos[complete.cases(datos[, c("EIP","PATERPRO","DIGENERO","INTIMIDAD")]), ]

cat("N modelo 1:", nrow(datos_m1), "\n")
cat("N modelo 2:", nrow(datos_m2), "\n")
