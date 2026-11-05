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
df_limpio <- transacciones_dvf[!duplicated(transacciones_dvf), ]

combinaciones_detalladas <- df_limpio %>%
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

###########################
#### Eliminación todas ####
###########################

df_limpio_sin_comercial <- df_limpio %>%
  group_by(id_mutation) %>%
  # Retenemos la transacción completa solo si NINGUNA de sus filas es un local comercial
  filter(!any(type_local == "Local industriel. commercial ou assimilé", na.rm = TRUE)) %>%
  ungroup()

combinaciones_detalladas <- df_limpio_sin_comercial %>%
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
