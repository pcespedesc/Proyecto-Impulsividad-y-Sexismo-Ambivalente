#7. TEORÍA DE RESPUESTA AL ÍTEM - MODELO DE SAMEJIMA

#Categorías de respuesta de ítems de la EIP
sapply(datos_afc_eip, function(x) sort(unique(na.omit(x)))) 

#modelo
fit_grm_eip <- mirt(
  data     = datos_afc_eip,
  model    = 1,
  itemtype = "graded",
  SE       = TRUE,
  verbose  = FALSE)

#resumen del modelo
summary(fit_grm_eip)

#parámetros
params_grm <- coef(fit_grm_eip, IRTpars = TRUE, simplify = TRUE)
round(params_grm$items, 3)
#EIP04 y EIP06 tienen discriminaciones negativas, lo que confirma lo observado en el AFC. Así como EIP15 que tiene una discriminación muy baja. 
#En EIP04, EIP06 y EIP11 los umbrales se comportan de manera anormal, lo que indica que los ítems no están funcionando bien. 
#Gráficos
plot(fit_grm_eip, type = "trace",
     main = "Curvas Características de los Ítems - EIP")
# Ítems como el 02, 05 y 10 muestran curvas ordenadas en donde cada categoría domuna en un rango de theta, pero EIP06 y EIP11 muestran curvas casi planas y superpuestas confirmando que no discriminan. 
# EIP04 muestra un patrón invertido. 
plot(fit_grm_eip, type = "infotrace",
     main = "Información por Ítem - EIP")
#EIP 04, 06, 09 y 11 aportan poca información o casi nula para estimar el rasgo, mientras que EIP02 y EIP10 son los más informativos complementados por EIP05 que aporta información sobre personas con poco nivel de rasgo 
plot(fit_grm_eip, type = "info",
     main = "Información Total del Test - EIP")
#Se puede ver que el test mide con mayor precisión a personas con impulsividad media-alta. 
plot(fit_grm_eip, type = "SE",
     main = "Error Estándar - EIP")
#El error mínimo se alcanza entre θ = -1 y 2, lo cual es consistente con el gráfico anterior.

# GRM para la Escala de Sexismo Ambivalente

#Sexismo hostil 

#Categorías de respuesta de ítems 
sapply(datos_afc_esa[, items_hostil], function(x) sort(unique(na.omit(x))))

#modelo
fit_grm_hostil <- mirt(
  data     = datos_afc_esa[, items_hostil],
  model    = 1,
  itemtype = "graded",
  SE       = TRUE,
  verbose  = FALSE)

#resumen del modelo
summary(fit_grm_hostil)

#parámetros
round(coef(fit_grm_hostil, IRTpars = TRUE, simplify = TRUE)$items,
  3)
#Todos los ítems tienen discriminación positiva a diferencia de EIP. ESA11 es el que más discrimina, seguido de ESA14, ESA05 Y ESA10. Los ítems con discriminación más baja 
#son el ESA04 y ESA18, que aunque distingan con menos precisión siguen funcionando. 
#Asimismo, todos tienen umbrales ordenados, e incluso los umbrales de b1 son muy negativos lo que significa que incluso personas con niveles bajos de sexismo hostil tienen alta probabilidad
#de responder en categorías distintas a la mínima, lo que va en concordancia con los descriptivos. 
       
#Gráficos

plot(fit_grm_hostil, type = "trace",
     main = "CCI - Sexismo Hostil")
#Todos los ítems muestran curvas ordenadas y separadas. SA14 y ESA05 tienen las curvas más bien definidas y separadas, consistente con sus altos parámetros de discriminación. 
#ESA04 y ESA18 muestran curvas algo más superpuestas, coherente con su menor discriminación.
plot(fit_grm_hostil, type = "info",
     main = "Información - Sexismo Hostil")
#La curva de información tiene un rango amplio de theta entre -2 y 2 con un pico máximo cerca de 0, lo que indica que la escala mide con precisión a personas 
#con niveles bajos como medios de sexismo hostil. 

#Sexismo benevolente 
#Categorías de respuesta de ítems 
sapply(datos_afc_esa[, items_benev], function(x) sort(unique(na.omit(x))))

#modelo
fit_grm_benev <- mirt(
  data     = datos_afc_esa[, items_benev],
  model    = 1,
  itemtype = "graded",
  SE       = TRUE,
  verbose  = FALSE)

#resumen del modelo
summary(fit_grm_benev)

#parámetros
round(coef(fit_grm_benev, IRTpars = TRUE, simplify = TRUE)$items,
  3)
#ESA12 es el ítem que más discrimina y tiene umbrales ordenados, siendo practicamente el que domina la medición del factor, lo cual es consistente con su carga de 0.92 en el AFC
#ESA09, ESA22, ESA01 funcionan bien y el resto tienen discriminación baja pero positiva a excepción del ESA13 que su valor de a=-1.13 y umbrales invertidos lo que confirma por tercera vez que funciona en dirección opuesta.
#ESA06 tiene discriminación casi nula, tambien consistente con lo observado anteriormente. Al igual que umbrales que se comportan de manera anómala.  
#ESA09 Y ESA08 tienen umbrales muy negativos, lo que significa que se necesita un nivel muy bajo de sexismo benevolente para responder en la categoría mínima, por lo que casi toda la muestra tiende a responder en categorías medias o altas. 

#Gráficos

plot(fit_grm_benev, type = "trace",
     main = "CCI - Sexismo Benevolente")
#Aquí se ve más heterogeneidad entre ítems. ESA12 tiene curvas muy juntas y con transiciones muy marcadas. ESA13 tiene un patrón invertido.
#ESA06 tiene curvas casi planas y superpuestas. ESA01, ESA03, ESA17 y ESA22 muestran curvas ordenadas similares a las del sexismo hostil. 
plot(fit_grm_benev, type = "info",
     main = "Información - Sexismo Benevolente")
#La curva muestra tres picos, lo que refleja la heterogeneidad de los ítems. La información se concentra en el rango negativo de θ (entre -2.5 y 0), lo que indica que la escala mide con mayor precisión a personas con niveles bajos a medios de sexismo benevolente. A partir de θ = 1 la información cae marcadamente, lo que significa que la escala tiene poca precisión para discriminar entre personas con niveles altos del rasgo. 
#Comparado con el sexismo hostil, cuya curva era más simétrica, esta escala tiene cobertura más asimétrica del rasgo. 
