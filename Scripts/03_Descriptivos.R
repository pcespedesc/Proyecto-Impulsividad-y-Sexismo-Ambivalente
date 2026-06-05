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

#Crosstable por sexo
crosstable(datos_m1,
           c(EIP, SEXHOS, SEXBENE, EDAD),
           by = SEXO,
           showNA = "no",
           funs = c(mean, sd)) %>%
  as_flextable(keep_id = FALSE)

#Descriptivos por grupo y sexo - Modelo 2 

#Crosstable por grupo
crosstable(datos_m2,
           c(EIP, PATERPRO, DIGENERO, INTIMIDAD, EDAD),
           by = GRUPO,
           showNA = "no",
           funs = c(mean, sd)) %>%
  as_flextable(keep_id = FALSE)

#Crosstable por sexo
crosstable(datos_m2,
           c(EIP, PATERPRO, DIGENERO, INTIMIDAD, EDAD),
           by = SEXO,
           showNA = "no",
           funs = c(mean, sd)) %>%
  as_flextable(keep_id = FALSE)

#Histogramas exploratorios de distribución de cada variable
par(mfrow = c(2,3))
hist(datos_m1$EIP,     main = "EIP",      xlab = "")
hist(datos_m1$SEXHOS,  main = "SEXHOS",   xlab = "")
hist(datos_m1$SEXBENE, main = "SEXBENE",  xlab = "")
hist(datos_m2$PATERPRO,main = "PATERPRO", xlab = "")
hist(datos_m2$DIGENERO,main = "DIGENERO", xlab = "")
hist(datos_m2$INTIMIDAD,main = "INTIMIDAD",xlab = "")
par(mfrow = c(1,1))

#Boxplots para explorar diferencias entre grupos y sexo 
boxplot(EIP ~ SEXO * GRUPO,      data = datos_m1, main = "EIP por sexo y grupo")
boxplot(SEXHOS ~ SEXO * GRUPO,   data = datos_m1, main = "Sexismo Hostil")
boxplot(SEXBENE ~ SEXO * GRUPO,  data = datos_m1, main = "Sexismo Benevolente")
boxplot(PATERPRO ~ SEXO * GRUPO, data = datos_m2, main = "Paternalismo Protector")
boxplot(DIGENERO ~ SEXO * GRUPO, data = datos_m2, main = "Diferenciación de Género")
boxplot(INTIMIDAD ~ SEXO * GRUPO,data = datos_m2, main = "Intimidad Heterosexual")

