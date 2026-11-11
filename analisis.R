system.time({

library(dplyr)
#########################################
#### Todas las posibles Combinaciones ###
#########################################
combinaciones_detalladas <- transacciones_dvf %>%
  mutate(type_local = if_else(is.na(type_local), "Sin Tipo", type_local)) %>%

  group_by(id_mutation, type_local) %>%
  summarise(cantidad = n(), .groups = "drop") %>%

  mutate(etiqueta = paste(cantidad, type_local)) %>%

  group_by(id_mutation) %>%
  arrange(type_local, .by_group = TRUE) %>%
  summarise(
    combinacion = paste(etiqueta, collapse = " + ")
  ) %>%
  ungroup() %>%
  count(combinacion, sort = TRUE)

# Visualizamos la tabla
View(combinaciones_detalladas)
sum(combinaciones_detalladas$n)

##########################################
### Eliminamos todos los duplicados####
##########################################
df_sin_duplicados <- transacciones_dvf[!duplicated(transacciones_dvf), ]

#Borra transacciones que incluyen un local comercial
df_limpio_sin_comercial <- df_sin_duplicados %>%
  group_by(id_mutation) %>%
  filter(!any(type_local == "Local industriel. commercial ou assimilé", na.rm = TRUE)) %>%
  ungroup()

#borra transacciones que incluyen como minimo una casa y un apartamento
df_limpio_sin_mixtos <- df_limpio_sin_comercial %>%
  group_by(id_mutation) %>%
  filter(!(any(type_local == "Maison", na.rm = TRUE) & any(type_local == "Appartement", na.rm = TRUE))) %>%
  ungroup()

#borra toda transaccion que incluye al menos dos viviendas(appartament,maison)
df_limpio_sin_dos_viviendas <- df_limpio_sin_mixtos %>%
  group_by(id_mutation) %>%
  filter(!(
    sum(type_local == "Maison", na.rm = TRUE) >= 2 |
      sum(type_local == "Appartement", na.rm = TRUE) >= 2
  )) %>%
  ungroup()


#elimina transacciones que solo consideran dependencias
df_limpio_sin_dependencia <- df_limpio_sin_dos_viviendas %>%
  group_by(id_mutation) %>%
  # Retenemos la transacción si NO ocurre que todas sus filas sean "Dépendance"
  filter(!all(type_local == "Dépendance", na.rm = TRUE)) %>%
  ungroup()

#Elimina transacciones que incluyan unicamente NA en tipo de propiedad
df_limpio_sin_puro_NA <- df_limpio_sin_dependencia %>%
  group_by(id_mutation) %>%
  # Conservamos la transacción si NO ocurre que todas sus filas sean NA en code_type_local
  filter(!all(is.na(code_type_local))) %>%
  ungroup()


#borra toda transaccion compuesta unicamente por dependencia Y NA
df_limpio_sin_DEPENDENCIA_NA <- df_limpio_sin_puro_NA %>%
  group_by(id_mutation) %>%
  # Conservamos la transacción si NO ocurre que todas sus filas sean (NA o Dépendance)
  filter(!all(is.na(code_type_local) | type_local == "Dépendance")) %>%
  ungroup()


#borrar toda transaccion que incluya al menos un NA Y tenga
#al menos un valor NA en code_nature_culture
paso_1 <- df_limpio_sin_DEPENDENCIA_NA %>%
  group_by(id_mutation) %>%
  # Nos QUEDAMOS solo con las transacciones que tienen al menos un NA
  filter(any(is.na(code_type_local))) %>%
  ungroup()

diagnostico <- paso_1 %>%
  filter(is.na(code_type_local)) %>%
  count(code_nature_culture)


ids_a_borrar <- paso_1 %>%
  # Miramos ÚNICAMENTE las filas que tienen texto vacío (o NA real) en tipo de local
  filter(code_type_local == "" | is.na(code_type_local)) %>%
  group_by(id_mutation) %>%
  summarise(tiene_blanco_cultura = any(code_nature_culture == "" | is.na(code_nature_culture))) %>%
  filter(tiene_blanco_cultura == TRUE) %>%
  pull(id_mutation)




df_limpio_prueba <- df_limpio_sin_DEPENDENCIA_NA %>%
  # Retenemos solo las transacciones cuyo ID NO esté en la lista de ids_a_borrar
  filter(!id_mutation %in% ids_a_borrar)


#ver posibles valores en lt1_surface_carrez para filas con na en propiedad)
paso_na <- df_limpio_prueba %>%
  group_by(id_mutation) %>%
  # Nos QUEDAMOS solo con las transacciones que tienen al menos un NA
  filter(any(is.na(code_type_local))) %>%
  ungroup()


ids_manuales <- c("2022-1142503", "2021-1153903", "2022-1140688","2022-1142506","2022-1143152")

#  Filtramos la base para quedarnos solo con los que NO están en esa lista
df_limpio_final <- df_limpio_prueba %>%
  filter(!id_mutation %in% ids_manuales)



###########################################################################
## TRATAMIENTO DE LAS VARIABLES PARA DEJAR UNA SOLA FILA POR TRANSACCIÓN ##
###########################################################################

#  Banderas (flags) a nivel de fila
df_limpio_final <- df_limpio_final %>%
  mutate(
    # Banderas por tipo de fila
    es_dependencia = !is.na(code_type_local) & code_type_local %in% c(3, 4),
    es_residencia  = !is.na(code_type_local) & !(code_type_local %in% c(3, 4)),
    es_terreno     = is.na(code_type_local),

    # Dummies por tipo específico de terreno
    has_jardin = ifelse(!is.na(nature_culture) & nature_culture %in% c("jardins", "J"), 1, 0),
    has_sol    = ifelse(!is.na(nature_culture) & nature_culture %in% c("sols", "S"), 1, 0),
    has_pres   = ifelse(!is.na(nature_culture) & nature_culture %in% c("prés", "P"), 1, 0),
    has_terres = ifelse(!is.na(nature_culture) & nature_culture %in% c("terres", "T"), 1, 0),
    has_verger = ifelse(!is.na(nature_culture) & nature_culture %in% c("vergers", "VI"), 1, 0)
  )

# Agregación a 1 fila por transacción -> df_modelamiento (Sin surface_dependances)
df_modelamiento <- df_limpio_final %>%
  group_by(id_mutation) %>%
  summarise(
    # --- Identificación y Fecha ---
    date_mutation      = first(date_mutation),
    numero_disposition = first(numero_disposition),
    nature_mutation    = first(nature_mutation),

    # --- Variable Respuesta (Y) ---
    valeur_fonciere    = first(valeur_fonciere),

    # --- Ubicación Geográfica ---
    code_postal      = first(code_postal),
    code_commune     = first(code_commune),
    nom_commune      = first(nom_commune),
    code_departement = first(code_departement),

    # --- Vivienda Principal y Piezas ---
    type_local = first(type_local[es_residencia]),
    piezas_max = max(nombre_pieces_principales, na.rm = TRUE),
    piezas_sum = sum(nombre_pieces_principales, na.rm = TRUE),

    # --- Superficie Habitable y Conteo de Dependencias ---
    surface_habitable = sum(surface_reelle_bati[es_residencia], na.rm = TRUE),
    n_dependances     = sum(es_dependencia),

    # --- Terrenos y Parcelas ---
    n_terrenos = sum(es_terreno),
    surface_terrain_total = ifelse(
      sum(es_terreno) > 0,
      {
        val <- sum(surface_terrain[es_terreno], na.rm = TRUE)
        if (all(is.na(surface_terrain[es_terreno]))) NA_real_ else val
      },
      0
    ),
    n_parcelles = n_distinct(id_parcelle, na.rm = TRUE),

    # --- Copropiedad (Máximo y Suma) ---
    nombre_lots_max = max(nombre_lots, na.rm = TRUE),
    nombre_lots_sum = sum(nombre_lots, na.rm = TRUE),

    # --- Dummies de Terreno ---
    has_jardin = max(has_jardin, na.rm = TRUE),
    has_sol    = max(has_sol, na.rm = TRUE),
    has_pres   = max(has_pres, na.rm = TRUE),
    has_terres = max(has_terres, na.rm = TRUE),
    has_verger = max(has_verger, na.rm = TRUE),

    .groups = "drop"
  ) %>%
  mutate(
    # Limpieza técnica de -Inf en agregaciones
    piezas_max      = ifelse(is.infinite(piezas_max), NA, piezas_max),
    nombre_lots_max = ifelse(is.infinite(nombre_lots_max), NA, nombre_lots_max),

    # Dummy de terreno no especificado
    has_terreno_unspecified = ifelse(
      n_terrenos > 0 & (has_jardin + has_sol + has_pres + has_terres + has_verger == 0),
      1,
      0
    )
  )

######################################
## TRATAMIENTO DE VALORES FALTANTES ##
######################################

library(tidyr)

# Diagnóstico de NAs por variable
na_summary <- df_modelamiento %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "cant_na") %>%
  mutate(
    total_filas = nrow(df_modelamiento),
    pct_na = round((cant_na / total_filas) * 100, 2)
  ) %>%
  arrange(desc(cant_na))

# Visualizar el resumen completo de las 25 variables
print(na_summary, n = 26)


## PRIMERA ELIMINACIÓN: LOS 23 DATOS QUE NO TIENEN VARIABLE RESPUESTA

df_modelamiento1 <- df_modelamiento %>%
  filter(!is.na(valeur_fonciere) & valeur_fonciere > 0)

## SEGUNDA ELIMINACIÓN (REEMPLAZO): los 2 NA en piezas_max

# Paso 2: Imputación condicional por la MEDIA de piezas según type_local (MAR)

df_modelamiento2 <- df_modelamiento1 %>%
  group_by(type_local) %>%
  mutate(
    piezas_max = ifelse(is.na(piezas_max), round(mean(piezas_max, na.rm = TRUE)), piezas_max),
    piezas_sum = ifelse(is.na(piezas_sum), round(mean(piezas_sum, na.rm = TRUE)), piezas_sum)
  ) %>%
  ungroup()


###########################
### Variables Continuas ###
###########################
## Librerias
library(ggplot2)
library(gridExtra)

## Creamos la variables precio_m2 = valor / superficie habitable (temporal para ver si es consistente con lo que vale)
df_modelamiento3 <- df_modelamiento2 %>%
  mutate(precio_m2 = valeur_fonciere / surface_habitable)


## 1-. Varibles continuas
b1 <- ggplot(df_modelamiento3, aes(x= type_local, y = valeur_fonciere, fill = type_local))+
  geom_boxplot(outlier.colour = "red", outlier.alpha = 0.3)+
  scale_y_log10(labels = scales::dollar_format(prefix="€", big.mark="."))+
  theme_minimal()+
  theme(legend.position = "none")+
  labs(title="Precio de Venta", y="Euros (escala log10)", x ="")

b2 <- ggplot(df_modelamiento3, aes(x= type_local, y = surface_habitable, fill = type_local))+
  geom_boxplot(outlier.colour = "red", outlier.alpha = 0.3)+
  scale_y_log10()+
  theme_minimal()+
  theme(legend.position = "none")+
  labs(title="Superficie habitable", y="Metros cuadrados m^2 (escala log10)", x ="")

b3 <- ggplot(df_modelamiento3, aes(x= type_local, y = precio_m2, fill = type_local))+
  geom_boxplot(outlier.colour = "red", outlier.alpha = 0.3)+
  scale_y_log10(labels = scales::dollar_format(prefix="€", big.mark="."))+
  theme_minimal()+
  theme(legend.position = "none")+
  labs(title="Precio por m^2 de Control", y="Euros/m^2 (escala log10)", x ="")
grid.arrange(b1,b2,b3,ncol=3)

print(b1)
print(b2)
print(b3)



contar_outliers <- function(x){
  q1 = quantile(x,0.25,na_rm = TRUE)
  q3 = quantile(x,0.75,na_rm = TRUE)
  iqr  = q3 - q1
  sum(x<(q1-1.5 *iqr) | x > (13+1.5*iqr), na.rm = TRUE)
}

cat("Outliers estadísticos en Precio:", contar_outliers(df_modelamiento3$valeur_fonciere), "\\n")
cat("Outliers estadísticos en Superficie Habitable:", contar_outliers(df_modelamiento3$surface_habitable), "\\n")
cat("Outliers estadísticos en Precio/m²:", contar_outliers(df_modelamiento3$precio_m2), "\\n")


cat("1. Precios no comerciales ( < 1.000 €):", sum(df_modelamiento3$valeur_fonciere < 1000, na.rm = TRUE), "\n")
cat("2. Metros no habitables (< 8 m^2):", sum(df_modelamiento3$surface_habitable < 8, na.rm = TRUE), "\n")
cat("3. Precios por m² irrisorios (< 150 €/m^2):", sum(df_modelamiento3$precio_m2 < 1000, na.rm = TRUE), "\n")
cat("4. Precios por m² aberrantes (>30.000 €/m^2):", sum(df_modelamiento3$precio_m2 > 30000, na.rm = TRUE), "\n")





#############################
### Casos Outliers a mano ###
#############################
# No podemos eliminar la 50000 seguin la regla de los quantiles,
# por lo que lo revisaremos a manos casos que pueden ser extremos si en realidad si son extremos


precios_altos_m2<-df_modelamiento3 %>%
  filter(precio_m2 == Inf) %>%
  select(id_mutation,nom_commune,nature_mutation, type_local, valeur_fonciere, surface_habitable,surface_terrain_total, precio_m2) %>%
  arrange(desc(precio_m2))


precios_superface<-df_modelamiento3 %>%
  filter(surface_habitable < 8) %>%
  select(id_mutation,nom_commune, type_local, valeur_fonciere, surface_habitable, precio_m2) %>%
  arrange(surface_habitable)


precios_bajos <- df_modelamiento3 %>%
  mutate(precio_m2=round(valeur_fonciere/surface_habitable,1)) %>%
  filter((surface_habitable >= 80 & valeur_fonciere <50000) | (precio_m2 < 150 & surface_habitable > 0)) %>%
  select(id_mutation, date_mutation, nom_commune, type_local,
         nature_mutation, valeur_fonciere, surface_habitable,
         precio_m2, surface_terrain_total, n_dependances,
         any_of(c("nombre_pieces_principales", "piezas_max")) )%>%
  arrange(precio_m2)


View(precios_superface)





###############################
### Base final para modelos ###
###############################

df_modelamiento_final <- df_modelamiento3 %>%
  filter(surface_habitable >= 8,) %>%
  mutate(log_valeur_fonciere = log(valeur_fonciere))
nrow(df_modelamiento_final)
#dejar solo transacciones que son consideradas venta
df_modelamiento_final_final <- df_modelamiento_final %>%
  filter(nature_mutation == "Vente")

#eliminar transacciones que tienen el valor del m2 <1000
df_modelamiento_listo <- df_modelamiento_final_final%>%
  filter(precio_m2 > 1000,)

#####################
### Tabla resumen ###
#####################

#1. Total de datos
total_datos <- n_distinct(transacciones_dvf$id_mutation)     # Número total de datos
t1  <- n_distinct(df_sin_duplicados$id_mutation)              # eliminación de duplicados
t2  <- n_distinct(df_limpio_sin_comercial$id_mutation)        # Eliminación de comerciales
t3  <- n_distinct(df_limpio_sin_mixtos$id_mutation)           # Eliminación casa-apartamento
t4  <- n_distinct(df_limpio_sin_dos_viviendas$id_mutation)    # Eliminacion 2o+ residencias por transacción
t5  <- n_distinct(df_limpio_sin_dependencia$id_mutation)      # Eliminacion solo dependencias
t6  <- n_distinct(df_limpio_sin_puro_NA$id_mutation)          # Conservala si NO todas sus filas son NA (no hay residencia)
t7  <- n_distinct(df_limpio_sin_DEPENDENCIA_NA$id_mutation)   # eliminacion dependencia+NA (No hay residencia principal)
t8  <- n_distinct(df_limpio_prueba$id_mutation)               # Se revisaron NA en type_code, si eran un terreno y luego si tenía algo construido
t9  <- n_distinct(df_limpio_final$id_mutation)                #
t10 <- n_distinct(df_modelamiento1$id_mutation)               # Eliminación de NA en valor de la transacción
t11 <- n_distinct(df_modelamiento2$id_mutation)               # No se eliminó nada, solo se reemplazaron 2 NA de numeros de pieza condicionando ciertas casas y tomando la media.
t12 <- n_distinct(df_modelamiento_final$id_mutation)         # Eliminación superficies < 8 m2
t13 <- n_distinct(df_modelamiento_final_final$id_mutation)   # Eliminación todo excepto "Vente"
t14 <- n_distinct(df_modelamiento_listo)                      # Eliminacion m2<1000
conteos_t <- c(total_datos, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14)

total_FILAS <- nrow(transacciones_dvf)     # Número total de datos
t_1  <- nrow(df_sin_duplicados)                      # eliminación de duplicados
t_2  <- nrow(df_limpio_sin_comercial)        # Eliminación de comerciales
t_3  <- nrow(df_limpio_sin_mixtos)           # Eliminación casa-apartamento
t_4  <- nrow(df_limpio_sin_dos_viviendas)    # Eliminacion 2o+ residencias por transacción
t_5  <- nrow(df_limpio_sin_dependencia)      # Eliminacion solo dependencias
t_6  <- nrow(df_limpio_sin_puro_NA)          # Conservala si NO todas sus filas son NA (no hay residencia)
t_7  <- nrow(df_limpio_sin_DEPENDENCIA_NA)   # eliminacion dependencia+NA (No hay residencia principal)
t_8  <- nrow(df_limpio_prueba)               # Se revisaron NA en type_code, si eran un terreno y luego si tenía algo construido
t_9  <- nrow(df_limpio_final)                #
t_10 <- nrow(df_modelamiento1)               # Eliminación de NA en valor de la transacción
t_11 <- nrow(df_modelamiento2)
t_12 <- nrow(df_modelamiento_final)        # Eliminación superficies < 8 m2
t_13 <- nrow(df_modelamiento_final_final)  # Eliminación todo excepto "Vente"
t_14 <- nrow(df_modelamiento_listo) # eliminacion m2 <1000
conteos_tfilas <-c(total_FILAS,t_1,t_2,t_3,t_4,t_5,t_6,t_7,t_8,t_9,t_10,t_11, t_12, t_13, t_14)

tabla_resumen_1 <- tibble( paso= 0:14,
                           Criterio_de_eliminación = c("Base Original",
                                                       "Eliminación duplicados",
                                                       "Eliminación de locales comerciales e industriales",
                                                       "Eliminación de transaccines mixtas (Maison+Appartement)",
                                                       "Eliminación ventas en bloque (>=2 residencias)",
                                                       "Eliminación de transacciones de solo dependencia",
                                                       "Eliminacion de transacciones con solo NA en tipo local",
                                                       "Eliminación de combinacion dependecia + NA",
                                                       "Eliminacion de registros sin tipo y sin cultivo catastral",
                                                       "Eliminación manual casos atípicos",
                                                       "Eliminación de datos de transaccion con precio nulo o <=0",
                                                       "Reemplazo de datos faltantes en cantidad de piezas",
                                                       "Eliminación superficies < 8 m2",
                                                       "Eliminación todo excepto Vente",
                                                       "Eliminación de precios de M**2 < 1000 €"),
                           Número_de_filas = conteos_tfilas,
                           Transacciones_Conservadas = conteos_t,
                           Transacciones_Descartadas = c(0,-diff(conteos_t)),
                           Porcentaje_perdida_paso = c(0, round(100*(-diff(conteos_t))/lag(conteos_t)[-1],2)),
                           Porcentaje_muestra_inicial= round(100*conteos_t/ total_datos,2)
)
print(tabla_resumen_1)
View(tabla_resumen_1)
#limpieza de variables y datos
rm(combinaciones_detalladas,df_limpio_final,df_limpio_prueba,df_limpio_sin_puro_NA,
   df_limpio_sin_dependencia,df_limpio_sin_comercial,df_limpio_sin_DEPENDENCIA_NA,df_limpio_sin_mixtos
   ,df_modelamiento1,df_modelamiento,df_modelamiento_final,df_modelamiento2,df_modelamiento3,
   df_modelamiento_final_final,df_limpio_sin_dos_viviendas,diagnostico,precios_superface,precios_altos_m2,precios_bajos,
   df_sin_duplicados, paso_1, paso_na,b1,b2,b3,conteos_t,conteos_tfilas
   ,ids_a_borrar,ids_manuales,t_1,t_2,t_3,t_4,t_5,t_6,t_7,t_8,t_9,t_10,t1,t2,t3,t4,t5
   ,t6,t7,t8,t9,t10,t11,t12,t13,t14,t_11,t_12,t_13,t_14,na_summary,total_datos,total_FILAS,contar_outliers)

library(glmnet)

df_modelamiento_listo$año <- substr(df_modelamiento_listo$date_mutation, 1, 4)
coordenadas_recuperadas <- transacciones_dvf %>%
  select(id_mutation, latitude, longitude) %>%
  # Agrupamos para asegurar que quede 1 sola coordenada por transacción
  group_by(id_mutation) %>%
  summarise(
    latitude = first(latitude[!is.na(latitude)]),
    longitude = first(longitude[!is.na(longitude)]),
    .groups = "drop"
  )

# 2. Unir las coordenadas a tu base de modelamiento final
df_modelamiento_listo <- df_modelamiento_listo %>%
  left_join(coordenadas_recuperadas, by = "id_mutation")
cat("Transacciones sin latitud/longitud:", sum(is.na(df_modelamiento_listo$latitude)), "\n")

# REEMPLAZO DE LOS DATOS FALTANTES (MEDIANA DE LA COMUNA)--------------------

# Imputación por la mediana geográfica de la comuna
df_modelamiento_listo <- df_modelamiento_listo %>%
  group_by(code_commune, nom_commune) %>%
  mutate(
    latitude = ifelse(
      is.na(latitude),
      median(latitude, na.rm = TRUE),
      latitude
    ),
    longitude = ifelse(
      is.na(longitude),
      median(longitude, na.rm = TRUE),
      longitude
    )
  ) %>%
  ungroup() %>%

  mutate(
    latitude = ifelse(
      is.na(latitude),
      median(latitude, na.rm = TRUE),
      latitude
    ),
    longitude = ifelse(
      is.na(longitude),
      median(longitude, na.rm = TRUE),
      longitude
    )
  )




###################################################
#Creacion de particion de desarrollo y de reserva##
###################################################

df_desarrollo <- df_modelamiento_listo %>%
  filter(año %in% c(2021, 2022)) %>%
  select(
    id_mutation,
    log_valeur_fonciere,
    type_local, surface_habitable, piezas_max, n_dependances,
    surface_terrain_total, n_parcelles, nombre_lots_max,
    has_jardin, has_sol, has_pres, has_terres, has_verger, has_terreno_unspecified, latitude, longitude
  )

df_reserva <- df_modelamiento_listo %>%
  filter(año == 2023) %>%
  select(
    id_mutation,
    log_valeur_fonciere,
    type_local, surface_habitable, piezas_max, n_dependances,
    surface_terrain_total, n_parcelles, nombre_lots_max,
    has_jardin, has_sol, has_pres, has_terres, has_verger, has_terreno_unspecified, latitude, longitude
  )


set.seed(4554)
indices_entrenamiento <- sample(1:nrow(df_desarrollo), size = 0.8 * nrow(df_desarrollo))
df_entrenamiento <- df_desarrollo[indices_entrenamiento, ]
df_validacion    <- df_desarrollo[-indices_entrenamiento, ]

x_entrenamiento <- model.matrix(log_valeur_fonciere ~ . - id_mutation, data = df_entrenamiento)[, -1]
y_entrenamiento <- df_entrenamiento$log_valeur_fonciere

x_validacion <- model.matrix(log_valeur_fonciere ~ . - id_mutation, data = df_validacion)[, -1]
y_validacion <- df_validacion$log_valeur_fonciere

x_reserva <- model.matrix(log_valeur_fonciere ~ . - id_mutation, data = df_reserva)[, -1]
y_reserva <- df_reserva$log_valeur_fonciere


#############################
#########MODELO LASSO########
#############################

modelo_lasso_cv <- cv.glmnet(x_entrenamiento, y_entrenamiento, alpha = 1, standardize = TRUE)
mejor_lambda <- modelo_lasso_cv$lambda.min
print(mejor_lambda)
##predicciones con validacion
predicciones_validacion <- predict(modelo_lasso_cv, s = mejor_lambda, newx = x_validacion)

df_resultados_validacion <- df_validacion %>%
  select(id_mutation, log_valeur_fonciere) %>%
  mutate(
    prediccion_log = as.vector(predicciones_validacion),
    precio_real_euros = exp(log_valeur_fonciere),
    precio_predicho_euros = exp(prediccion_log)
  )
#predicciones con reserva
predicciones_reserva <- predict(modelo_lasso_cv, s = mejor_lambda, newx = x_reserva)

df_resultados_reserva <- df_reserva %>%
  select(id_mutation, log_valeur_fonciere) %>%
  mutate(
    prediccion_log = as.vector(predicciones_reserva),
    precio_real_euros = exp(log_valeur_fonciere),
    precio_predicho_euros = exp(prediccion_log)
  )

##### Variables eliminadas de Lasso #####

matriz_coef<- as.matrix(coef(modelo_lasso_cv,s=mejor_lambda))
df_coeficientes <- data.frame( Variable = rownames(matriz_coef),
                               Coeficientes = matriz_coef[,1],
                               stringsAsFactors = FALSE)

df_conservadas <- df_coeficientes %>%
  filter(Coeficientes != 0)

df_eliminadas <- df_coeficientes %>%
  filter(Coeficientes == 0 )
print( "Variables conservadas del modelo \n")
print(df_conservadas)
cat("\n")
print( "Variables eliminadas del modelo \n")
print(df_eliminadas)

#Función para calcular las métricas de evaluación
calcular_metricas <- function(real, predicho) {
  rss <- sum((real - predicho)^2)
  tss <- sum((real - mean(real))^2)

  r2 <- 1 - (rss / tss)
  rmse <- sqrt(mean((real - predicho)^2))

  return(c(R2 = round(r2, 4), RMSE = round(rmse, 4)))
}

metricas_validacion <- calcular_metricas(
  real = df_resultados_validacion$log_valeur_fonciere,
  predicho = df_resultados_validacion$prediccion_log
)


metricas_reserva <- calcular_metricas(
  real = df_resultados_reserva$log_valeur_fonciere,
  predicho = df_resultados_reserva$prediccion_log
)

tabla_comparativa <- bind_rows(
  data.frame(Conjunto = "Validación (2021-2022)", R2 = metricas_validacion["R2"], RMSE = metricas_validacion["RMSE"]),
  data.frame(Conjunto = "Reserva (2023)", R2 = metricas_reserva["R2"], RMSE = metricas_reserva["RMSE"])
)

print(tabla_comparativa)


df_evaluacion <- df_resultados_reserva %>%
  left_join(
    df_modelamiento_listo %>%
      select(id_mutation, code_departement, nom_commune, type_local),
    by = "id_mutation"
  ) %>%
  mutate(error_abs_euros = abs(precio_real_euros - precio_predicho_euros))

# 2. MAE y RMSE Global en Euros
metricas_globales_euros <- df_evaluacion %>%
  summarise(
    MAE_Euros = mean(error_abs_euros),
    RMSE_Euros = sqrt(mean((precio_real_euros - precio_predicho_euros)^2)),
    Mediana_Error_Euros = median(error_abs_euros) # Agregada porque el RMSE se infla con valores altos
  )
print(metricas_globales_euros)


errores_tipo <- df_evaluacion %>%
  group_by(type_local) %>%
  summarise(
    N_Transacciones = n(),
    MAE_Euros = mean(error_abs_euros),
    RMSE_Euros = sqrt(mean((precio_real_euros - precio_predicho_euros)^2))
  )
print("=== ERRORES POR TIPO DE VIVIENDA ===")
print(errores_tipo)

# examen de alto valor
umbral_alto_valor_lasso <- quantile(df_evaluacion$precio_real_euros, 0.95)
evaluacion_alto_valor_lasso <- df_evaluacion %>%
  mutate(
    categoria_precio = ifelse(precio_real_euros >= umbral_alto_valor_lasso,
                              "Top 5% (Alto Valor)",
                              "Normal (95% restante)")
  ) %>%
  group_by(categoria_precio) %>%
  summarise(
    N_Transacciones = n(),
    Precio_Medio = round(mean(precio_real_euros), 0),
    MAE_Euros = round(mean(error_abs_euros), 0),
    RMSE_Euros = round(sqrt(mean((precio_real_euros - precio_predicho_euros)^2)), 0),
    Mediana_Error = round(median(error_abs_euros), 0)
  )

print("=== DESEMPEÑO EN VENTAS DE ALTO VALOR (LASSO) ===")
print(evaluacion_alto_valor_lasso)

########################
######MODELO_GAM########
########################
library(mgcv)
set.seed(4554)
modelo_gam <- gam(
  log_valeur_fonciere ~ type_local +
    s(surface_habitable, k=10) +
    s(surface_terrain_total, k=10) +
    piezas_max +
    n_dependances +
    n_parcelles +
    nombre_lots_max +
    has_jardin + has_sol + has_pres + has_terres + has_verger + has_terreno_unspecified + s(longitude,latitude) ,
  data = df_entrenamiento,
  method = "REML"
)

summary(modelo_gam)

# Validación (2021-2022)
df_resultados_validacion_gam <- df_validacion %>%
  mutate(
    prediccion_log = as.vector(predict(modelo_gam, newdata = df_validacion)),
    precio_real_euros = exp(log_valeur_fonciere),
    precio_predicho_euros = exp(prediccion_log),
    error_abs_euros = abs(precio_real_euros - precio_predicho_euros)
  )

# Reserva / Prueba (2023)
df_resultados_reserva_gam <- df_reserva %>%
  mutate(
    prediccion_log = as.vector(predict(modelo_gam, newdata = df_reserva)),
    precio_real_euros = exp(log_valeur_fonciere),
    precio_predicho_euros = exp(prediccion_log),
    error_abs_euros = abs(precio_real_euros - precio_predicho_euros)
  )
#metricas calculo
calcular_metricas_gam <- function(df) {
  real_log <- df$log_valeur_fonciere
  pred_log <- df$prediccion_log
  real_eur <- df$precio_real_euros
  pred_eur <- df$precio_predicho_euros

  rss <- sum((real_log - pred_log)^2)
  tss <- sum((real_log - mean(real_log))^2)
  r2  <- 1 - (rss / tss)
  rmse_log <- sqrt(mean((real_log - pred_log)^2))

  mae_eur  <- mean(abs(real_eur - pred_eur))
  rmse_eur <- sqrt(mean((real_eur - pred_eur)^2))
  med_ae_eur <- median(abs(real_eur - pred_eur))

  return(c(
    R2_log = round(r2, 4),
    RMSE_log = round(rmse_log, 4),
    MAE_Euros = round(mae_eur, 2),
    RMSE_Euros = round(rmse_eur, 2),
    Mediana_AE_Euros = round(med_ae_eur, 2)
  ))
}

metricas_val_gam <- calcular_metricas_gam(df_resultados_validacion_gam)
metricas_res_gam <- calcular_metricas_gam(df_resultados_reserva_gam)

tabla_comparativa_gam <- bind_rows(
  data.frame(Conjunto = "Validación (GAM)", t(metricas_val_gam)),
  data.frame(Conjunto = "Reserva 2023 (GAM)", t(metricas_res_gam))
)

print("=== DESEMPEÑO GLOBAL DEL MODELO GAM ===")
print(tabla_comparativa_gam)

df_evaluacion_gam <- df_resultados_reserva_gam %>%
  left_join(
    df_modelamiento_listo %>% select(id_mutation, code_departement, nom_commune),
    by = "id_mutation"
  )

# Desglose de errores por Tipo de Local (Maison vs Appartement)
errores_tipo_gam <- df_evaluacion_gam %>%
  group_by(type_local) %>%
  summarise(
    N_Transacciones = n(),
    MAE_Euros = round(mean(error_abs_euros), 2),
    RMSE_Euros = round(sqrt(mean((precio_real_euros - precio_predicho_euros)^2)), 2),
    Mediana_Error_Euros = round(median(error_abs_euros), 2)
  )

print("=== ERRORES POR TIPO DE VIVIENDA (RESERVA 2023) ===")
print(errores_tipo_gam)

par(mfrow = c(1, 2))
plot(modelo_gam, select = 1, shade = TRUE, col = "darkblue", main = "Efecto Superficie Habitable")
plot(modelo_gam, select = 2, shade = TRUE, col = "darkgreen", main = "Efecto Superficie Terreno")
par(mfrow = c(1, 1)) # Restablecer ventana gráfica

par(mfrow = c(1, 3)) # Ponemos 3 gráficos en una misma fila
plot(modelo_gam, scheme = 2, shade = TRUE, shade.col = "lightblue", seWithMean = TRUE)

concurvity(modelo_gam, full = TRUE)

#EXAMEN ALTO VALOR GAM
umbral_alto_valor_gam <- quantile(df_evaluacion_gam$precio_real_euros, 0.95)

evaluacion_alto_valor_gam <- df_evaluacion_gam %>%
  mutate(
    categoria_precio = ifelse(precio_real_euros >= umbral_alto_valor_gam,
                              "Top 5% (Alto Valor)",
                              "Normal (95% restante)")
  ) %>%
  group_by(categoria_precio) %>%
  summarise(
    N_Transacciones = n(),
    Precio_Medio = round(mean(precio_real_euros), 0),
    MAE_Euros = round(mean(error_abs_euros), 0),
    RMSE_Euros = round(sqrt(mean((precio_real_euros - precio_predicho_euros)^2)), 0),
    Mediana_Error = round(median(error_abs_euros), 0)
  )

print("=== DESEMPEÑO EN VENTAS DE ALTO VALOR (GAM) ===")
print(evaluacion_alto_valor_lasso)


#############################################
#####modelo_xgboost##########################
#############################################
library(xgboost)

dtrain <- xgb.DMatrix(data = x_entrenamiento, label = y_entrenamiento)
dval   <- xgb.DMatrix(data = x_validacion, label = y_validacion)
dtest  <- xgb.DMatrix(data = x_reserva, label = y_reserva)

parametros_xgb <- list(
  max_depth = 6,
  eta = 0.05,
  objective = "reg:squarederror"
)

set.seed(4554)
modelo_xgb <- xgb.train(
  params = parametros_xgb,
  data = dtrain,
  nrounds = 3000,
  evals = list(train = dtrain, val = dval),
  early_stopping_rounds = 50,
  print_every_n = 100
)


cat("\nEl modelo se detuvo óptimamente en la iteración:", modelo_xgb$best_iteration, "\n")

df_resultados_reserva_xgb <- df_reserva %>%
  mutate(
    prediccion_log = predict(modelo_xgb, newdata = dtest),
    precio_real_euros = exp(log_valeur_fonciere),
    precio_predicho_euros = exp(prediccion_log),
    error_abs_euros = abs(precio_real_euros - precio_predicho_euros)
  )

metricas_res_xgb <- calcular_metricas(
  real = df_resultados_reserva_xgb$log_valeur_fonciere,
  predicho = df_resultados_reserva_xgb$prediccion_log
)

mae_eur_xgb <- mean(df_resultados_reserva_xgb$error_abs_euros)
rmse_eur_xgb <- sqrt(mean(df_resultados_reserva_xgb$error_abs_euros^2))
med_ae_eur_xgb <- median(df_resultados_reserva_xgb$error_abs_euros)

cat("\n=== DESEMPEÑO XGBOOST EN RESERVA (2023) ===\n")
cat("R2 (log):", metricas_res_xgb["R2"], "\n")
cat("RMSE (log):", metricas_res_xgb["RMSE"], "\n")
cat("MAE (Euros): €", round(mae_eur_xgb, 0), "\n")
cat("RMSE (Euros): €", round(rmse_eur_xgb, 0), "\n")
cat("Mediana Error Absoluto: €", round(med_ae_eur_xgb, 0), "\n\n")

# examen de alto valor xgboost
umbral_alto_valor_xgb <- quantile(df_resultados_reserva_xgb$precio_real_euros, 0.95)

evaluacion_alto_valor_xgb <- df_resultados_reserva_xgb %>%
  mutate(
    categoria_precio = ifelse(precio_real_euros >= umbral_alto_valor_xgb,
                              "Top 5% (Alto Valor)",
                              "Normal (95% restante)")
  ) %>%
  group_by(categoria_precio) %>%
  summarise(
    N_Transacciones = n(),
    Precio_Medio = round(mean(precio_real_euros), 0),
    MAE_Euros = round(mean(error_abs_euros), 0),
    RMSE_Euros = round(sqrt(mean((precio_real_euros - precio_predicho_euros)^2)), 0),
    Mediana_Error = round(median(error_abs_euros), 0)
  )

print("=== DESEMPEÑO EN VENTAS DE ALTO VALOR (XGBOOST) ===")
print(evaluacion_alto_valor_xgb)


# Gráfico de Importancia de Variables
importancia_xgb <- xgb.importance(feature_names = colnames(x_entrenamiento), model = modelo_xgb)
xgb.plot.importance(importancia_xgb, top_n = 10, main = "Importancia de Variables - XGBoost", col = "darkred")


#############################################
######localizaciones con pocos antecedentes##
#############################################
conteo_comunas_entrenamiento <- df_modelamiento_listo %>%
  filter(año %in% c("2021", "2022")) %>%
  group_by(nom_commune) %>%
  summarise(ventas_historicas = n(), .groups = "drop")

df_evaluacion_xgb <- df_resultados_reserva_xgb %>%
  left_join(
    df_modelamiento_listo %>% select(id_mutation, nom_commune),
    by = "id_mutation"
  )

df_errores_todos <- bind_rows(
  df_evaluacion %>%
    select(id_mutation, nom_commune, precio_real_euros, precio_predicho_euros, error_abs_euros) %>%
    mutate(Modelo = "1. LASSO"),

  df_evaluacion_gam %>%
    select(id_mutation, nom_commune, precio_real_euros, precio_predicho_euros, error_abs_euros) %>%
    mutate(Modelo = "2. GAM"),

  df_evaluacion_xgb %>%
    select(id_mutation, nom_commune, precio_real_euros, precio_predicho_euros, error_abs_euros) %>%
    mutate(Modelo = "3. XGBoost")
)

evaluacion_localizaciones_modelos <- df_errores_todos %>%
  left_join(conteo_comunas_entrenamiento, by = "nom_commune") %>%
  mutate(
    ventas_historicas = ifelse(is.na(ventas_historicas), 0, ventas_historicas),

    categoria_volumen = case_when(
      ventas_historicas <= 10 ~ "1. Pocos antecedentes (<= 10 ventas)",
      ventas_historicas <= 50 ~ "2. Antecedentes medios (11-50 ventas)",
      TRUE ~ "3. Muchos antecedentes (> 50 ventas)"
    )
  ) %>%
  group_by(Modelo, categoria_volumen) %>%
  summarise(
    N_Transacciones_2023 = n(),
    MAE_Euros = round(mean(error_abs_euros), 0),
    RMSE_Euros = round(sqrt(mean((precio_real_euros - precio_predicho_euros)^2)), 0),
    .groups = "drop"
  ) %>%
  arrange(categoria_volumen, Modelo)

# Imprimir la tabla final
print("=== DESEMPEÑO POR VOLUMEN HISTÓRICO DE LA COMUNA (COMPARATIVA) ===")
print(evaluacion_localizaciones_modelos)


########################################
#### Modelos sin longitud y latitud ####
########################################}

df_modelamiento_listo <- df_modelamiento_listo %>%
  mutate(type_local = as.factor(type_local))
df_sin_ll <- df_modelamiento_listo %>%
  filter(año %in% c(2021, 2022)) %>%
  select(id_mutation, log_valeur_fonciere,
         type_local, surface_habitable,
         piezas_max, n_dependances,
         surface_terrain_total,
         n_parcelles, nombre_lots_max,
         has_jardin, has_sol, has_pres,
         has_terres, has_verger,
         has_terreno_unspecified)
df_reserva_sin_ll <- df_modelamiento_listo %>%
  filter(año == 2023) %>%
  select(id_mutation, log_valeur_fonciere,
         type_local, surface_habitable,
         piezas_max, n_dependances,
         surface_terrain_total,
         n_parcelles, nombre_lots_max,
         has_jardin, has_sol, has_pres,
         has_terres, has_verger,
         has_terreno_unspecified)

df_entrenamiento_sin_ll <- df_sin_ll[indices_entrenamiento,]
df_validacion_sin_ll <- df_sin_ll[-indices_entrenamiento,]

################################
## matrices para Lasso y XGboost sin coordenadas
################################

x_ent_sin_ll <- model.matrix(log_valeur_fonciere ~ . -id_mutation, data = df_entrenamiento_sin_ll)[,-1]
y_ent_sin_ll <- df_entrenamiento_sin_ll$log_valeur_fonciere

x_val_sin_ll <- model.matrix(log_valeur_fonciere ~. - id_mutation, df_validacion_sin_ll)[,-1]
y_val_sin_ll <- df_validacion_sin_ll$log_valeur_fonciere


x_res_sin_ll <- model.matrix(log_valeur_fonciere ~ . -id_mutation, data = df_reserva_sin_ll)[,-1]
y_res_sin_ll <- df_reserva_sin_ll$log_valeur_fonciere

#####################################
### modelo con regularizacion Lasso##
#####################################
set.seed(4554)
modelo_lasso_sin_ll <- cv.glmnet(x_ent_sin_ll, y_ent_sin_ll, alpha = 1,
                                 standardize = T )
mejor_lambda_sin_ll <- modelo_lasso_sin_ll$lambda.min


pred_val_lasso_sin_ll <- as.vector(predict(modelo_lasso_sin_ll, s=mejor_lambda_sin_ll, newx = x_val_sin_ll))
pred_res_lasso_sin_ll <- as.vector(predict(modelo_lasso_sin_ll, s = mejor_lambda_sin_ll,newx = x_res_sin_ll))

matriz_coef_sin_ll <- as.matrix(coef(modelo_lasso_sin_ll,s=mejor_lambda_sin_ll))
df_coeficientes_sin_ll <- data.frame( Variable = rownames(matriz_coef_sin_ll),
                               Coeficientes = matriz_coef_sin_ll[,1],
                               stringsAsFactors = FALSE)

df_conservadas <- df_coeficientes_sin_ll %>%
  filter(Coeficientes != 0)

df_eliminadas <- df_coeficientes_sin_ll %>%
  filter(Coeficientes == 0 )
print( "Variables conservadas del modelo \n")
print(df_conservadas)
cat("\n")
print( "Variables eliminadas del modelo \n")
print(df_eliminadas)





##########################
### modelo gam sin ll ###
##########################

modelo_gam_sin_ll <- gam( log_valeur_fonciere ~ type_local +s(surface_habitable,k=10)+
                            s(surface_terrain_total, k=10)+
                            piezas_max + n_dependances +n_parcelles + nombre_lots_max+
                            has_jardin + has_sol +has_pres + has_terres + has_verger +
                            has_terreno_unspecified,
                          data = df_entrenamiento_sin_ll,
                          method = "REML")



pred_val_gam_sin_ll <- as.vector(predict(modelo_gam_sin_ll,newdata = df_validacion_sin_ll))
pred_res_gam_sin_ll <- as.vector(predict(modelo_gam_sin_ll, newdata = df_reserva_sin_ll))



###############################################
### Modelo XGBoost sin latitud y longitud ####
###############################################
df_ent_sin_ll <-xgb.DMatrix(data = x_ent_sin_ll, label = y_ent_sin_ll)
df_val_sin_ll <- xgb.DMatrix(data = x_val_sin_ll, label = y_val_sin_ll)
df_prueba_sin_ll <- xgb.DMatrix(data = x_res_sin_ll,label = y_res_sin_ll)


params_xgb_sin_ll <- list(max_depth = 6,
                          eta = 0.005,
                          objective = "reg:squarederror")



set.seed(4554)
modelo_xgb_sin_ll <- xgb.train(params = params_xgb_sin_ll,
                               data = df_ent_sin_ll,
                               nrounds = 3000,
                               evals = list(train=df_ent_sin_ll,val= df_val_sin_ll),
                               early_stopping_rounds = 50,
                               verbose = 0)


pred_val_xgb_sg <-  predict(modelo_xgb_sin_ll, newdata = df_val_sin_ll)
pred_res_xgb_sg <-  predict(modelo_xgb_sin_ll, newdata = df_prueba_sin_ll)




#########################################
#### Modelo sin logaritmo del precio ####
#########################################


df_desarrollo_sin_log <- df_modelamiento_listo %>%
  filter(año %in% c(2021,2022)) %>%
  select(id_mutation, valeur_fonciere,
         type_local, surface_habitable,
         piezas_max, n_dependances,
         surface_terrain_total,
         n_parcelles, nombre_lots_max,
         has_jardin, has_sol, has_pres,
         has_terres, has_verger,
         has_terreno_unspecified, latitude, longitude)

df_reserva_sin_log <- df_modelamiento_listo %>%
  filter(año == 2023) %>%
  select(id_mutation, valeur_fonciere,
         type_local, surface_habitable,
         piezas_max, n_dependances,
         surface_terrain_total,
         n_parcelles, nombre_lots_max,
         has_jardin, has_sol, has_pres,
         has_terres, has_verger,
         has_terreno_unspecified, latitude ,longitude)


df_entrenamiento_sin_log <- df_desarrollo_sin_log[indices_entrenamiento,]
df_validacion_sin_log <- df_desarrollo_sin_log[-indices_entrenamiento,]

#matrices lasso y xgboost en euros directo

x_ent_sin_log <- model.matrix(valeur_fonciere ~.-id_mutation, data = df_entrenamiento_sin_log)[,-1]
y_ent_sin_log <- df_entrenamiento_sin_log$valeur_fonciere

x_val_sin_log <- model.matrix(valeur_fonciere ~.-id_mutation, data = df_validacion_sin_log)[,-1]
y_val_sin_log <- df_validacion_sin_log$valeur_fonciere

x_res_sin_log <- model.matrix(valeur_fonciere ~.-id_mutation, data = df_reserva_sin_log)[,-1]
y_res_sin_log <- df_reserva_sin_log$valeur_fonciere


####################################
### Modelo Lasso sin logaritmo ####
####################################
set.seed(4554)
modelo_lasso_sin_log <- cv.glmnet(x_ent_sin_log,y_ent_sin_log, alpha=1,
                                  standardize = T)
mejor_lambda_sin_log <- modelo_lasso_sin_log$lambda.min


pred_val_lasso_sin_log  <- as.vector(predict(modelo_lasso_sin_log, s = mejor_lambda_sin_log, newx = x_val_sin_log))
pred_res_lasso_sin_log  <-  as.vector(predict(modelo_lasso_sin_log, s = mejor_lambda_sin_log, newx = x_res_sin_log))
pred_log_lasso_sin_con_log <- log(pred_val_lasso_sin_log)

matriz_coef_sin_log <- as.matrix(coef(modelo_lasso_sin_log,s=mejor_lambda_sin_log))
df_coeficientes_sin_log <- data.frame( Variable = rownames(matriz_coef_sin_log),
                                      Coeficientes = matriz_coef_sin_log[,1],
                                      stringsAsFactors = FALSE)

df_conservadas <- df_coeficientes_sin_log %>%
  filter(Coeficientes != 0)

df_eliminadas <- df_coeficientes_sin_log %>%
  filter(Coeficientes == 0 )
print( "Variables conservadas del modelo \n")
print(df_conservadas)
cat("\n")
print( "Variables eliminadas del modelo \n")
print(df_eliminadas)

###########################
### GAM sin logaritmo ####
###########################
modelo_gam_sin_log <- gam(valeur_fonciere ~ type_local + s(surface_habitable,k=10) +
                            s(surface_terrain_total,k=10)+ piezas_max + n_dependances
                          + n_parcelles + nombre_lots_max + has_jardin + has_sol
                          + has_pres + has_terres + has_verger + has_terreno_unspecified
                          + latitude + longitude, data = df_entrenamiento_sin_log,
                          method = "REML")


pred_val_gam_sin_log <- predict(modelo_gam_sin_log, newdata = df_validacion_sin_log)
pred_reserva_gam_sin_log <- predict(modelo_gam_sin_log,newdata = df_reserva_sin_log)
mae_gam_sin_log <- mean(abs(df_reserva_sin_log$valeur_fonciere-pred_reserva_gam_sin_log))
rmse_gam_sin_log <- sqrt(mean((df_reserva_sin_log$valeur_fonciere-pred_reserva_gam_sin_log)^2))

cat("MAE en Euros (GAM sin log): €", round(mae_gam_sin_log, 0), "\\n")
cat("RMSE en Euros (GAM sin log): €", round(rmse_gam_sin_log, 0), "\\n")
###################
## XGBoost sin log
###################

df_ent_sin_log <- xgb.DMatrix(data = x_ent_sin_log, label = y_ent_sin_log)
df_val_sin_log <- xgb.DMatrix(data = x_val_sin_log, label = y_val_sin_log)
df_res_sin_log <- xgb.DMatrix(data = x_res_sin_log, label = y_res_sin_log)

params_xgb_sin_log <- list(max_depth = 6 ,
                           eta=0.05,
                           objective= "reg:squarederror")
set.seed(4554)
modelo_xgb_sin_log <- xgb.train(params = params_xgb_sin_log,
                                data = df_ent_sin_log,
                                nrounds = 3000,
                                evals = list(train = df_ent_sin_log , val = df_val_sin_log),
                                early_stopping_rounds = 50,
                                verbose = 0)
pred_res_xgb_sin_log  <-  predict(modelo_xgb_sin_log, newdata = df_res_sin_log)


####################################
### Funciones para cada modelos ####
####################################
funcion_para_metricas_casos <- function(real_log, pred_log, es_log = T) {
  if (es_log) {
    real_eur <- exp(real_log)
    pred_eur <- exp(pred_log)

    rss <- sum((real_log - pred_log)^2)
    tss <- sum((real_log - mean(real_log))^2)
    r2_log <-  round(1- (rss/tss),4)
    rmse_log <- round(sqrt(mean((real_log - pred_log)^2)),4)
  } else{
    real_eur <-  real_log
    pred_eur <- pred_log
    r2_log <- NA_real_
    rmse_log <- NA_real_
  }

  err_abs <- abs(real_eur - pred_eur)
  mae_eur <- round(mean(err_abs), 2)
  rmse_eur <- round(sqrt(mean((real_eur - pred_eur)^2)),2)
  med_ae_eur <-  round(median(err_abs),2)

  return(data.frame(
    R2_log = r2_log,
    RMSE_log = rmse_log,
    MAE_euros = mae_eur,
    RMSE_euros = rmse_eur,
    Mediana_AE_euros = med_ae_eur
  ))
}
pred_res_xgb_caso1 <-  predict(modelo_xgb, newdata = dtest)
pred_val_xgb_caso1 <- predict(modelo_xgb, newdata = dval)

#############################################
#### tabla  comparativa datos de prueba ####
#############################################
tabla_comparativa_prueba <- bind_rows(
  # Casos de la log + lati/long
  data.frame(Caso = "Con Log + Con Lat/Long", Modelo = "Lasso", funcion_para_metricas_casos(y_reserva, predicciones_reserva, es_log = TRUE)),
  data.frame(Caso = "Con Log + Con Lat/Long", Modelo = "GAM", funcion_para_metricas_casos(y_reserva, df_resultados_reserva_gam$prediccion_log, es_log = TRUE)),
  data.frame(Caso = "Con Log + Con Lat/Long", Modelo = "XGBoost", funcion_para_metricas_casos(y_reserva, pred_res_xgb_caso1, es_log = TRUE)),

  # Caso sin log (Euros directos) + long + lati
  data.frame(Caso = "Sin Log (Euros) + Con Lat/Long", Modelo = "Lasso", funcion_para_metricas_casos(y_res_sin_log, pred_res_lasso_sin_log, es_log = FALSE)),
  data.frame(Caso = "Sin Log (Euros) + Con Lat/Long", Modelo = "GAM", funcion_para_metricas_casos(y_res_sin_log, pred_reserva_gam_sin_log, es_log = FALSE)),
  data.frame(Caso = "Sin Log (Euros) + Con Lat/Long", Modelo = "XGBoost", funcion_para_metricas_casos(y_res_sin_log, pred_res_xgb_sin_log, es_log = FALSE)),


  # casos con log + sin (latitud y longitud)

  data.frame(Caso = "Con Log + Sin Lat/Long", Modelo = "Lasso", funcion_para_metricas_casos(y_res_sin_ll, pred_res_lasso_sin_ll, es_log = TRUE)),
  data.frame(Caso = "Con Log + Sin Lat/Long", Modelo = "GAM", funcion_para_metricas_casos(y_res_sin_ll, pred_res_gam_sin_ll, es_log = TRUE)),
  data.frame(Caso = "Con Log + Sin Lat/Long", Modelo = "XGBoost", funcion_para_metricas_casos(y_res_sin_ll, pred_res_xgb_sg, es_log = TRUE))


)

print(tabla_comparativa_prueba)





tabla_comparativa_validacion <- bind_rows(
  # Casos de la log + lati/long
  data.frame(Caso = "Con Log + Con Lat/Long", Modelo = "Lasso", funcion_para_metricas_casos(y_validacion, predicciones_validacion, es_log = TRUE)),
  data.frame(Caso = "Con Log + Con Lat/Long", Modelo = "GAM", funcion_para_metricas_casos(y_validacion, df_resultados_validacion_gam$prediccion_log, es_log = TRUE)),
  data.frame(Caso = "Con Log + Con Lat/Long", Modelo = "XGBoost", funcion_para_metricas_casos(y_validacion, pred_val_xgb_caso1, es_log = TRUE)),

  # Caso sin log (Euros directos) + long + lati
  data.frame(Caso = "Sin Log (Euros) + Con Lat/Long", Modelo = "Lasso", funcion_para_metricas_casos(y_val_sin_log, pred_val_lasso_sin_log, es_log = FALSE)),
  data.frame(Caso = "Sin Log (Euros) + Con Lat/Long", Modelo = "GAM", funcion_para_metricas_casos(y_val_sin_log, pred_val_gam_sin_log, es_log = FALSE)),
  data.frame(Caso = "Sin Log (Euros) + Con Lat/Long", Modelo = "XGBoost", funcion_para_metricas_casos(y_val_sin_log, predict(modelo_xgb_sin_log,newdata=df_val_sin_log), es_log = FALSE)),


  # casos con log + sin (latitud y longitud)

  data.frame(Caso = "Con Log + Sin Lat/Long", Modelo = "Lasso", funcion_para_metricas_casos(y_val_sin_ll, pred_val_lasso_sin_ll, es_log = TRUE)),
  data.frame(Caso = "Con Log + Sin Lat/Long", Modelo = "GAM", funcion_para_metricas_casos(y_val_sin_ll, pred_val_gam_sin_ll, es_log = TRUE)),
  data.frame(Caso = "Con Log + Sin Lat/Long", Modelo = "XGBoost", funcion_para_metricas_casos(y_val_sin_ll, pred_val_xgb_sg, es_log = TRUE))


)




#######################
### Referencia
#######################
# 1. Calcular la referencia en Entrenamiento
referencia_tipo  <-  df_entrenamiento %>% group_by(type_local) %>% summarise(mediana_log = median(log_valeur_fonciere))
# 2. Asignar la predicción fija a la base de Prueba (Reserva)
df_validacion_ref <-  df_validacion %>% left_join(referencia_tipo, by = "type_local")

pred_validacion_referencia <-  df_validacion_ref$mediana_log

metricas_ref <-  funcion_para_metricas_casos( real_log = df_validacion$log_valeur_fonciere, pred_log = pred_validacion_referencia, es_log = TRUE )
# Creamos la fila de Referencia y la unimos a tu
tabla_fila_referencia  <-  data.frame( Caso = " Referencia (Mediana por tipo)", Modelo = "Baseline", metricas_ref )

tabla_final_con_referencia <-  bind_rows(tabla_fila_referencia, tabla_comparativa_validacion)
print(tabla_final_con_referencia)


###### Mejor modelo XGBoost

df_residuos_xgb <- df_reserva %>%
  mutate( pred_log = pred_res_xgb_caso1,
          real_log = log_valeur_fonciere,
          res_log = real_log - pred_log,
          # Residuo en escala Log
          real_eur = exp(real_log),
          pred_eur = exp(pred_log),
          res_eur = real_eur - pred_eur)

# Residuos vs valores predichos
library(ggplot2)
library(gridExtra)

g1 <-  ggplot(df_residuos_xgb, aes(x = pred_log, y = res_log)) +
  geom_point(alpha = 0.3, color = "#2c3e50") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 1) +
  geom_smooth(method = "loess", color = "blue", se = FALSE) +
  theme_minimal() +
  labs( title = "1. Residuos vs. Predichos (Escala Log)", x = "Log(Precio Predicho)", y = "Residuo Log (Real - Predicho)" )

#Histograma y Densidad de Residuos (Simetría del Error)

g2 <-  ggplot(df_residuos_xgb, aes(x = res_log)) +
  geom_histogram(aes(y = ..density..), bins = 50, fill = "#3498db", color = "white", alpha = 0.7) +
  geom_density(color = "darkblue", size = 1) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", size = 1) +
  theme_minimal() +
  labs( title = "2. Distribución de los Residuos (Log)", x = "Residuo Log", y = "Densidad" )


# Q-Q Plot (Evaluación de Normalidad y Colas Pesadas)
g3  <-  ggplot(df_residuos_xgb, aes(sample = res_log)) +
  stat_qq(color = "#2c3e50", alpha = 0.4) +
  stat_qq_line(color = "red", linetype = "dashed", size = 1) +
  theme_minimal() +
  labs( title = "3. Gráfico Q-Q Normal de Residuos", x = "Cuantiles Teóricos", y = "Cuantiles de los Residuos" )



g4 <- ggplot(df_residuos_xgb, aes(x = longitude, y = latitude, color = res_log)) +
  geom_point(alpha = 0.6, size = 1.5) +
  scale_color_gradient2( low = "#e74c3c", mid = "#f1c40f", high = "#2ecc71", midpoint = 0, name = "Residuo Log" ) +
  theme_minimal() + labs( title = "4. Distribución Espacial del Error", x = "Longitud", y = "Latitud" )
grid.arrange(g1, g2, g3, ncol = 3)




ggplot(df_modelamiento_listo, aes(x = longitude, y = latitude, color = log_valeur_fonciere)) +
  geom_point(alpha = 0.5, size = 1.2) + scale_color_viridis_c(option = "magma", name = "Log(Precio)") +
  theme_minimal() + labs( title = "Distribución Geográfica de Transacciones Inmobiliarias", subtitle = "Departamento de Rhône, Francia", x = "Longitud", y = "Latitud" ) +
  coord_quickmap()


library(maps)
mapa_francia <-  map_data("france")
mapa_rhone <-  mapa_francia %>% filter(region == "Rhone")

ggplot() +
  geom_polygon(data = mapa_francia, aes(x = long, y = lat, group = group), fill = "#181818", color = "#333333") +
  geom_point(data = df_modelamiento_listo, aes(x = longitude, y = latitude, color = log_valeur_fonciere), alpha = 0.6, size = 1) +
  scale_color_viridis_c(name = "Log(Precio)", option = "viridis") +
  scale_color_gradient( name = "Log(Precio)", low = "#f1c40f",
                        high = "#ffffff" ) +
  coord_fixed(xlim = c(4.3, 5.3), ylim = c(45.4, 46.3), ratio = 1.3) +
  theme_minimal() +
  labs( title = "Ubicación de Propiedades en Rhône (Francia)", x = "Longitud", y = "Latitud" )


ggplot() +
  geom_polygon( data = mapa_francia,
                aes(x = long, y = lat, group = group),
                fill = "#181818", color = "#333333" ) +
  geom_polygon( data = mapa_rhone,
                aes(x = long, y = lat, group = group),
                fill = "#f1c40f",
                color = "#f39c12",
                size = 0.8 ) +
  geom_point( data = df_modelamiento_listo,
              aes(x = longitude, y = latitude), color = "gray",
              alpha = 0.7, size = 1.1 ) +
  coord_fixed(xlim = c(4.3, 5.3), ylim = c(45.4, 46.3), ratio = 1.3) +
  theme_minimal() +
  theme( plot.background = element_rect(fill = "black", color = "black"),
         panel.background = element_rect(fill = "black", color = "black"),
         panel.grid.major = element_line(color = "#222222", linetype = "dotted"),
         panel.grid.minor = element_blank(), plot.title = element_text(color = "white", face = "bold", size = 14),
         axis.title = element_text(color = "white"), axis.text = element_text(color = "#cccccc") ) +
  labs( title = "Ubicación de Propiedades en el Departamento de Rhône (Francia)", x = "Longitud", y = "Latitud" )

resultado <- sum(1:10000000)
})

