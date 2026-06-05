# 6. ANÁLISIS FACTORIAL CONFIRMATORIO (AFC)
# Con el propósito de verificar la estructura según Vaamonde y Omar (2012)
# Es decir: Modelo con 3 subdimensiones para el sexismo benevolente
#           - Paternalismo Protector:   ESA03, ESA09, ESA17, ESA20
#           - Diferenciación de Género: ESA08, ESA19, ESA22
#           - Intimidad Heterosexual:   ESA01, ESA06, ESA12, ESA13
# Mientras que el sexismo hostil se trata como factor unitario (11 ítems)

#En EIP se asume de 1 factor porque se cuenta con el puntaje total en la base de datos

#Solo se toman los casos con respuestas completas (sin N/A)
datos_afc_eip <- data[complete.cases(data[, items_eip]), items_eip]
datos_afc_esa <- data[complete.cases(data[, items_esa]), items_esa]

#Conversión de ítems a datos númericos y que sean tratados como tal
datos_afc_eip <- lapply(datos_afc_eip, as.numeric) %>% as.data.frame()
datos_afc_esa <- lapply(datos_afc_esa, as.numeric) %>% as.data.frame()

#6.1 ANÁLISIS FACTORIAL DE EIP

modelo_afc_eip <- 'Impulsividad =~ EIP01 + EIP02 + EIP03 + EIP04 + EIP05 +
                  EIP06 + EIP07 + EIP08 + EIP09 + EIP10 +
                  EIP11 + EIP12 + EIP13 + EIP14 + EIP15'

fit_afc_eip <- cfa(
  model     = modelo_afc_eip,
  data      = datos_afc_eip,
  estimator = "WLSMV",
  ordered   = TRUE)

summary(fit_afc_eip, fit.measures = TRUE, standardized = TRUE)

fitMeasures(fit_afc_eip, c("cfi", "tli", "rmsea", "wrmr"))

semPaths(fit_afc_eip, what = "std", layout = "tree",
        title = FALSE, style = "ram",
        main = "AFC - Impulsividad (EIP)")

# 6.2 AFC SEXISMO 2 FACTORES  

modelo_afc_esa_2f <- '
  SexHostil =~ ESA02 + ESA04 + ESA05 + ESA07 + ESA10 +
               ESA11 + ESA14 + ESA15 + ESA16 + ESA18 + ESA21
 
  SexBenev  =~ ESA01 + ESA03 + ESA06 + ESA08 + ESA09 +
               ESA12 + ESA13 + ESA17 + ESA19 + ESA20 + ESA22'

fit_afc_esa_2f <- cfa(
  model     = modelo_afc_esa_2f,
  data      = datos_afc_esa,
  estimator = "WLSMV",
  ordered   = TRUE)

summary(fit_afc_esa_2f, fit.measures = TRUE, standardized = TRUE)

fitMeasures(fit_afc_esa_2f, c("cfi", "tli", "rmsea", "wrmr"))

semPaths(fit_afc_esa_2f, what = "std", layout = "tree",
         title = FALSE, style = "ram",
         main = "AFC - ESA (2 factores: Hostil y Benevolente)")

#6.3 AFC SEXISMO 3 SUBDIMENSIONES CORRELACIONADAS
modelo_afc_benev_3f <- 'Paternalismo =~ ESA03 + ESA09 + ESA17 + ESA20
  DifGenero    =~ ESA08 + ESA19 + ESA22
  Intimidad    =~ ESA01 + ESA06 + ESA12 + ESA13
  Paternalismo ~~ DifGenero + Intimidad
  DifGenero ~~ Intimidad'

items_benev_3f  <- c("ESA01","ESA03","ESA06","ESA08","ESA09",
                 "ESA12","ESA13","ESA17","ESA19","ESA20","ESA22")

fit_afc_benev_3f <- cfa(
  model     = modelo_afc_benev_3f,
  data      = datos_afc_esa[, items_benev_3f],
  estimator = "WLSMV",
  ordered   = TRUE)

summary(fit_afc_benev_3f, fit.measures = TRUE, standardized = TRUE)

fitMeasures(fit_afc_benev_3f, c("cfi", "tli", "rmsea", "wrmr"))

semPaths(fit_afc_benev_3f,
         what = "std",
         layout = "tree",
         style = "ram",
         nCharNodes = 6,
         residuals = FALSE,
         title = TRUE,
         main = "AFC - Sexismo Benevolente (3 factores correlacionados)")

#6.4 AFC SUBDIMENSIONES COMO INDICADORES DEL SEXISMO AMBIVALENTE 
modelo_afc_benev_2orden <- 'Paternalismo =~ ESA03 + ESA09 + ESA17 + ESA20
  DifGenero    =~ ESA08 + ESA19 + ESA22
  Intimidad    =~ ESA01 + ESA06 + ESA12 + ESA13
  SexBenev =~ Paternalismo + DifGenero + Intimidad'
fit_afc_benev_2orden <- cfa(
  model     = modelo_afc_benev_2orden,
  data      = datos_afc_esa[, items_benev],
  estimator = "WLSMV",
  ordered   = TRUE)

#Aquí sale el aviso de que hay varianzas negativas en los factores latentes, lo cual indica problemas con el modelo, así que se opta
#por conservar la estructura de tres factores correlacionados

