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

  # 5. Contamos cuántas veces se repite cada combinación final en la base completa
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
  # Retenemos la transacción completa solo si NINGUNA de sus filas es un local comercial
  filter(!any(type_local == "Local industriel. commercial ou assimilé", na.rm = TRUE)) %>%
  ungroup()

#borra transacciones que incluyen como minimo una casa y un apartamento
df_limpio_sin_mixtos <- df_limpio_sin_comercial %>%
  group_by(id_mutation) %>%
  # Retenemos la transacción si NO se cumple que tenga (Maison Y Appartement al mismo tiempo)
  filter(!(any(type_local == "Maison", na.rm = TRUE) & any(type_local == "Appartement", na.rm = TRUE))) %>%
  ungroup()

#borra toda transaccion que incluye al menos dos viviendas(appartament,maison)
df_limpio_sin_dos_viviendas <- df_limpio_sin_mixtos %>%
  group_by(id_mutation) %>%
  # Retenemos la transacción si NO se cumple que tenga (2 o más Casas O 2 o más Departamentos)
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
