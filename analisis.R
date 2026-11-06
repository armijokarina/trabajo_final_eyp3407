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
#### Comentario oooo

#borra transacciones que incluyen como minimo una casa y un apartamento
df_limpio_sin_mixtos <- df_limpio_sin_comercial %>%
  group_by(id_mutation) %>%
  # Retenemos la transacción si NO se cumple que tenga (Maison Y Appartement al mismo tiempo)
  filter(!(any(type_local == "Maison", na.rm = TRUE) & any(type_local == "Appartement", na.rm = TRUE))) %>%
  ungroup()

combinaciones_detalladas <- df_limpio_sin_mixtos %>%
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

#borra toda transaccion que incluye al menos dos viviendas(appartament,maison)
df_limpio_sin_dos_viviendas <- df_limpio_sin_mixtos %>%
  group_by(id_mutation) %>%
  # Retenemos la transacción si NO se cumple que tenga (2 o más Casas O 2 o más Departamentos)
  filter(!(
    sum(type_local == "Maison", na.rm = TRUE) >= 2 |
      sum(type_local == "Appartement", na.rm = TRUE) >= 2
  )) %>%
  ungroup()

combinaciones_detalladas <- df_limpio_sin_dos_viviendas %>%
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

#elimina transacciones que solo consideran dependencias
df_limpio_sin_dependencia <- df_limpio_sin_dos_viviendas %>%
  group_by(id_mutation) %>%
  # Retenemos la transacción si NO ocurre que todas sus filas sean "Dépendance"
  filter(!all(type_local == "Dépendance", na.rm = TRUE)) %>%
  ungroup()

combinaciones_detalladas <- df_limpio_sin_dependencia %>%
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



df_limpio_sin_puro_NA <- df_limpio_sin_dependencia %>%
  group_by(id_mutation) %>%
  # Conservamos la transacción si NO ocurre que todas sus filas sean NA en code_type_local
  filter(!all(is.na(code_type_local))) %>%
  ungroup()

combinaciones_detalladas <- df_limpio_sin_puro_NA %>%
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


#borra toda transaccion compuesta unicamente por dependencia Y NA
df_limpio_sin_DEPENDENCIA_NA <- df_limpio_sin_puro_NA %>%
  group_by(id_mutation) %>%
  # Conservamos la transacción si NO ocurre que todas sus filas sean (NA o Dépendance)
  filter(!all(is.na(code_type_local) | type_local == "Dépendance")) %>%
  ungroup()



combinaciones_detalladas <- df_limpio_sin_DEPENDENCIA_NA %>%
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

print(diagnostico)


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

# Revisamos cuántas transacciones vamos a eliminar (¡ahora sí debería dar un número mayor a cero!)
length(ids_a_borrar)


df_limpio_prueba <- df_limpio_sin_DEPENDENCIA_NA %>%
  # Retenemos solo las transacciones cuyo ID NO esté en la lista de ids_a_borrar
  filter(!id_mutation %in% ids_a_borrar)

combinaciones_detalladas <- df_limpio_prueba %>%
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

#ver posibles valores en lt1_surface_carrez para filas con na en propiedad)
paso_na <- df_limpio_prueba %>%
  group_by(id_mutation) %>%
  # Nos QUEDAMOS solo con las transacciones que tienen al menos un NA
  filter(any(is.na(code_type_local))) %>%
  ungroup()
diagnostico <- paso_na %>%
  filter(is.na(code_type_local)) %>%
  count(lot1_surface_carrez)

print(diagnostico)





ids_manuales <- c("2022-1142503", "2021-1153903", "2022-1140688")

# 2. Filtramos la base para quedarnos solo con los que NO están en esa lista
df_limpio_final <- df_limpio_prueba %>%
  filter(!id_mutation %in% ids_manuales)
