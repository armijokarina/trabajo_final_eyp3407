library(dplyr)
#########################################
#### Todas las posibles Combinaciones ###
#########################################
combinaciones_detalladas <- transacciones_dvf %>%
  # 1. Para que los espacios vacíos no queden raros, les ponemos un nombre claro
  mutate(type_local = if_else(is.na(type_local), "Sin Tipo", type_local)) %>%

  # 2. Agrupamos por transacción Y por tipo de local para contar cuántos hay de cada uno
  group_by(id_mutation, type_local) %>%
  summarise(cantidad = n(), .groups = "drop") %>%

  # 3. Creamos la etiqueta específica (Ej: "2 Appartement", "1 Maison")
  mutate(etiqueta = paste(cantidad, type_local)) %>%

  # 4. Volvemos a agrupar solo por transacción para unir todas sus etiquetas
  group_by(id_mutation) %>%
  # Ordenamos alfabéticamente para que "A + B" sea igual a "B + A"
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
### Eliminamos Todas las duplicaciones####
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
  # 1. Miramos ÚNICAMENTE las filas que tienen texto vacío (o NA real) en tipo de local
  filter(code_type_local == "" | is.na(code_type_local)) %>%

  # 2. Agrupamos por transacción para evaluarlas
  group_by(id_mutation) %>%

  # 3. Verificamos si en este subgrupo aislado existe al menos un texto vacío en cultura
  summarise(tiene_blanco_cultura = any(code_nature_culture == "" | is.na(code_nature_culture))) %>%

  # 4. Filtramos las transacciones que cumplen la condición
  filter(tiene_blanco_cultura == TRUE) %>%

  # 5. Extraemos los ID a una lista
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

# 2. Filtramos la base para quedarnos solo con los que NO están en esa lista
df_limpio_final <- df_limpio_prueba %>%
  filter(!id_mutation %in% ids_manuales)



###########################################################################
## TRATAMIENTO DE LAS VARIABLES PARA DEJAR UNA SOLA FILA POR TRANSACCIÓN ##
###########################################################################

# 1. Banderas (flags) a nivel de fila
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

# 2. Agregación a 1 fila por transacción -> df_modelamiento (Sin surface_dependances)
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


#####################
### Tabla resumen ###
#####################

#1. Total de datos
total_datos <- n_distinct(transacciones_dvf$id_mutation)     # Número total de datos
t1  <- n_distinct(df_sin_duplicados$id_mutation)                      # eliminación de duplicados
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
conteos_t <-c(total_datos,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11)

#View(transacciones_dvf)
# Justificación: revisamos los NA vimos que no tenían números de pieza. Si tiene superficie construida como puedo
# justificar que es una casa o apartamento y no local comercial.

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
conteos_tfilas <-c(total_FILAS,t_1,t_2,t_3,t_4,t_5,t_6,t_7,t_8,t_9,t_10,t_11)

tabla_resumen_1 <- tibble( paso= 0:11,
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
                                                       "Reemplazo de datos faltantes en cantidad de piezas"),
                           Número_de_filas = conteos_tfilas,
                           Transacciones_Conservadas = conteos_t,
                           Transacciones_Descartadas = c(0,-diff(conteos_t)),
                           Porcentaje_perdida_paso = c(0, round(100*(-diff(conteos_t))/lag(conteos_t)[-1],2)),
                           Porcentaje_muestra_inicial= round(100*conteos_t/ total_datos,2)
)
print(tabla_resumen_1)
View(tabla_resumen_1)


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

##

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
cat("3. Precios por m² irrisorios (< 150 €/m^2):", sum(df_modelamiento_final_final_final$precio_m2 < 1000, na.rm = TRUE), "\n")
cat("4. Precios por m² aberrantes (>30.000 €/m^2):", sum(df_modelamiento3$precio_m2 > 30000, na.rm = TRUE), "\n")





#############################
### Casos Outliers a mano ###
#############################
# No podemos eliminar la 50000 seguin la regla de los quantiles,
# por lo que lo revisaremos a manos casos que pueden ser extremos si en realidad si son extremos
# en uno de los docimentos que no se considera las casa menores a 8 m^2, aunque los revisé
# View(df_modelamiento3)


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
#eliminar transacciones con un valor menor a 1000 euros
df_modelamiento_final_final_final <- df_modelamiento_final_final%>%
  filter(valeur_fonciere>= 1000,)
#eliminar transacciones que tienen el valor del m2 <1000
df_modelamiento_listo <- df_modelamiento_final_final_final%>%
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
t14 <- n_distinct(df_modelamiento_final_final_final$id_mutation) #  Precios >= 1000
t15 <- n_distinct(df_modelamiento_listo)                      # Eliminacion m2<1000
conteos_t <- c(total_datos, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15)

#View(transacciones_dvf)
# Justificación: revisamos los NA vimos que no tenían números de pieza. Si tiene superficie construida como puedo
# justificar que es una casa o apartamento y no local comercial.

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
t_14 <- nrow(df_modelamiento_final_final_final) #  Precios >= 1000
t_15 <- nrow(df_modelamiento_listo) # eliminacion m2 <1000
conteos_tfilas <-c(total_FILAS,t_1,t_2,t_3,t_4,t_5,t_6,t_7,t_8,t_9,t_10,t_11, t_12, t_13, t_14, t_15)

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
                                                       "Eliminación de precios bajos (< 1000 €)",
                                                       "Eliminación de precios de M**2 < 1000 €"),
                           Número_de_filas = conteos_tfilas,
                           Transacciones_Conservadas = conteos_t,
                           Transacciones_Descartadas = c(0,-diff(conteos_t)),
                           Porcentaje_perdida_paso = c(0, round(100*(-diff(conteos_t))/lag(conteos_t)[-1],2)),
                           Porcentaje_muestra_inicial= round(100*conteos_t/ total_datos,2)
)
print(tabla_resumen_1)
View(tabla_resumen_1)

library(glmnet)

df_modelamiento_final_final_final$año <- substr(df_modelamiento_final_final_final$date_mutation, 1, 4)

df_desarrollo <- df_modelamiento_final_final_final %>%
  filter(año %in% c(2021, 2022)) %>%
  select(
    id_mutation,
    log_valeur_fonciere,
    type_local, surface_habitable, piezas_max, n_dependances,
    surface_terrain_total, n_parcelles, nombre_lots_max,
    has_jardin, has_sol, has_pres, has_terres, has_verger, has_terreno_unspecified
  )

df_reserva <- df_modelamiento_final_final_final %>%
  filter(año == 2023) %>%
  select(
    id_mutation,
    log_valeur_fonciere,
    type_local, surface_habitable, piezas_max, n_dependances,
    surface_terrain_total, n_parcelles, nombre_lots_max,
    has_jardin, has_sol, has_pres, has_terres, has_verger, has_terreno_unspecified
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

modelo_lasso_cv <- cv.glmnet(x_entrenamiento, y_entrenamiento, alpha = 1, standardize = TRUE)
mejor_lambda <- modelo_lasso_cv$lambda.min
print(mejor_lambda)

predicciones_validacion <- predict(modelo_lasso_cv, s = mejor_lambda, newx = x_validacion)

df_resultados_validacion <- df_validacion %>%
  select(id_mutation, log_valeur_fonciere) %>%
  mutate(
    prediccion_log = as.vector(predicciones_validacion),
    precio_real_euros = exp(log_valeur_fonciere),
    precio_predicho_euros = exp(prediccion_log)
  )

predicciones_reserva <- predict(modelo_lasso_cv, s = mejor_lambda, newx = x_reserva)

df_resultados_reserva <- df_reserva %>%
  select(id_mutation, log_valeur_fonciere) %>%
  mutate(
    prediccion_log = as.vector(predicciones_reserva),
    precio_real_euros = exp(log_valeur_fonciere),
    precio_predicho_euros = exp(prediccion_log)
  )


# 1. Función para calcular las métricas de evaluación
calcular_metricas <- function(real, predicho) {
  # Suma de cuadrados de los residuos (RSS)
  rss <- sum((real - predicho)^2)
  # Suma total de cuadrados (TSS)
  tss <- sum((real - mean(real))^2)

  # Fórmulas matemáticas
  r2 <- 1 - (rss / tss)
  rmse <- sqrt(mean((real - predicho)^2))

  return(c(R2 = round(r2, 4), RMSE = round(rmse, 4)))
}

# 2. Calcular las métricas en la base de VALIDACIÓN (escala log)
metricas_validacion <- calcular_metricas(
  real = df_resultados_validacion$log_valeur_fonciere,
  predicho = df_resultados_validacion$prediccion_log
)

# 3. Calcular las métricas en la base de RESERVA (escala log)
metricas_reserva <- calcular_metricas(
  real = df_resultados_reserva$log_valeur_fonciere,
  predicho = df_resultados_reserva$prediccion_log
)

# 4. Unir todo en una tabla comparativa para visualizar
tabla_comparativa <- bind_rows(
  data.frame(Conjunto = "Validación (2021-2022)", R2 = metricas_validacion["R2"], RMSE = metricas_validacion["RMSE"]),
  data.frame(Conjunto = "Reserva (2023)", R2 = metricas_reserva["R2"], RMSE = metricas_reserva["RMSE"])
)

print(tabla_comparativa)


df_evaluacion <- df_resultados_reserva %>%
  left_join(
    df_modelamiento_final_final_final %>%
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
