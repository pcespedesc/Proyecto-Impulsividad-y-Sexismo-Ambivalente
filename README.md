# Impulsividad como predictor del sexismo ambivalente en universitarios y reclusos colombianos
Este repositorio contiene el proceso de investigación desarrollado para responder a la pregunta: ¿En qué medida la impulsividad predice el sexismo ambivalente en universitarios y reclusos, y cómo varía esta relación según el sexo?.  
## Objetivos específicos 
Este trabajo se fundamenta en tres objetivos específicos 
1. Describir los niveles de impulsividad y sexismo ambivalente en la muestra total, por grupo poblacional (universitarios y reclusos) y por sexo.
2. Evaluar la capacidad predictiva de la impulsividad sobre el sexismo hostil, el sexismo benevolente y sus subdimensiones (paternalismo protector, diferenciación de género e intimidad heterosexual) en la muestra total.
3. Examinar si la relación predictiva entre impulsividad y sexismo varía según el grupo poblacional y el sexo.
## Variables 
### Impulsividad 
En este estudio la impulsividad se entiende como la tendencia de actuar de manera rápida, con escasa planificación y limitada consideración de las consecuencias de la propia conducta (Plutchik & Van Praag, 1989; Moeller et al., 2001). Se evaluó mediante la Escala de Impulsividad de Plutchik (EIP) - adaptado a hispanohablantes por Alcázar et al (2015), utilizada como variable predictora principal.
### Sexismo ambivalente: 
Corresponde a un sistema de actitudes hacia las mujeres compuesto por dos dimensiones complementarias: el sexismo hostil, asociado a formas explícitas de rechazo y antagonismo, y el sexismo benevolente, caracterizado por actitudes aparentemente positivas que contribuyen a mantener roles tradicionales de género (Glick & Fiske, 1996). Se evaluó mediante el Inventario de Sexismo Ambivalente - adaptado por Vaamonde y Omar (2012). 
#### Sexismo benevolente
A diferencia del sexismo hostil, el benevolente se organiza en tres subdimensiones: 
- **Paternalismo protector:** Creencia de que las mujeres deben ser protegidas y cuidadas por los hombres. (Glick y Fiske, 1996)
- **Diferenciación de género:** Creencia de que las mujeres son poseedoras de cualidades emocionales o domésticas que complementan las carencias masculinas. (Glick y Fiske, 1996)
 - **Intimidad heterosexual:** Deseo de cercanía e intimidad romántica entre hombres y mujeres basado en la creencia de que las relaciones románticas heterosexuales son esenciales para que hombres y mujeres alcancen la verdadera felicidad (Glick y Fiske, 1996; Vaamonde, Omar, 2012)
### Covariables y variables moderadoras 
- **Edad** (covariable)
- **Sexo** (moderadora)
- **Grupo** (moderadora)
    
## Método y análisis
El estudio corresponde a un diseño no experimental, transversal y de alcance predictivo, basado en datos secundarios recolectados en contextos naturales.

La muestra está conformada por 322 participantes, de los cuales 232 son estudiantes universitarios y 90 personas privadas de la libertad. El muestreo fue no probabilístico por conveniencia.
Los datos provienen de una base de una salida de campo de psicología jurídica de la Universidad Nacional de Colombia (2012–2016), recolectados mediante cuestionarios de autoinforme en contextos universitarios y penitenciarios. 

El análisis de datos incluyó la depuración de la base (recodificación de variables, manejo de valores perdidos y detección de valores atípicos), seguido de análisis descriptivos, análisis factorial confirmatorio (AFC), teoría de respuesta al ítem (IRT) y modelos de ecuaciones estructurales mediante PLS-SEM. También se realizaron análisis multigrupo según sexo y grupo poblacional.

## Estructura del repositorio 
- sexismo.sav → base de datos original
- scripts/ → scripts de R organizados por etapas del análisis:
  00_script_completo.R (ejecución total del análisis)
  01_preparación y depuración.R  
  02_datos perdidos.R  
  03_descriptivos.R
  04_Outliers.R 
  05_AFC.R  
  06_IRT.R  
  07_PLS_SEM.R  
- outputs/ → resultados (tablas y gráficos)


## Resultados principales

La impulsividad predice positiva y significativamente el sexismo hostil en la muestra total (β = 0.236, p < .001), aunque con un tamaño de efecto pequeño (R² = 5.6%). No se encontró relación significativa entre impulsividad y sexismo benevolente, ni con sus subdimensiones de paternalismo protector, diferenciación de género e intimidad heterosexual, resultado consistente en todos los modelos estimados.
Al controlar por edad, la impulsividad mantuvo su efecto sobre el sexismo hostil, mientras que la edad predijo significativamente el sexismo benevolente total (β = 0.162, p = .024) y el paternalismo protector (β = 0.266, p < .001), sugiriendo que las manifestaciones benevolentes del sexismo se asocian más con factores sociodemográficos que con rasgos de personalidad.
Los análisis multigrupo no evidenciaron diferencias estadísticamente significativas en las relaciones estructurales según grupo poblacional (reclusos vs. estudiantes) ni según sexo, aunque se observó una tendencia no significativa hacia una relación negativa entre impulsividad y sexismo benevolente en hombres que merece exploración en muestras más grandes.

## Requisitos del entorno
El análisis fue desarrollado en RStudio. Para la ejecución del proyecto se requiere instalar los siguientes paquetes:
- haven
- dplyr
- summarytools
- crosstable
- flextable
- Routliers
- lavaan
- semPlot
- mirt
- psych
- seminr
