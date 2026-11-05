library(dplyr)

transacciones_complejas <- transacciones_dvf %>%
  group_by(id_mutation) %>%
  # Retenemos solo los id_mutation que tienen al menos una fila de cada uno de estos 3 tipos
  filter(
    any(type_local == "Maison", na.rm = TRUE) &
      any(type_local == "Dépendance", na.rm = TRUE) &
      any(type_local == "Local industriel. commercial ou assimilé", na.rm = TRUE)
  ) %>%
  ungroup()

# Para visualizar rápidamente los resultados en la consola o en un visor
View(transacciones_complejas)



## lO


base_etiquetada <- transacciones_dvf %>%
  group_by(id_mutation) %>%
  mutate(
    # Detectamos qué contiene la transacción completa
    tiene_comercial = any(type_local == "Local industriel. commercial ou assimilé", na.rm = TRUE),
    tiene_app = any(type_local == "Appartement", na.rm = TRUE),
    tiene_maison = any(type_local == "Maison", na.rm = TRUE),

    # Asignamos el motivo de exclusión (el orden importa, evalúa de arriba hacia abajo)
    motivo_exclusion = case_when(
      tiene_comercial ~ "Eliminado: Contiene local comercial",
      tiene_app & tiene_maison ~ "Eliminado: Mezcla Appartement + Maison",
      TRUE ~ "Conservado"
    )
  ) %>%
  ungroup()

# 2. Cuantificar las pérdidas (Guarda esta tabla para tu informe)
resumen_perdidas <- base_etiquetada %>%
  group_by(motivo_exclusion) %>%
  summarise(
    filas_borradas = n(),
    transacciones_afectadas = n_distinct(id_mutation)
  )

print(resumen_perdidas)

# 3. Filtrar la base final y limpiar las columnas auxiliares
base_limpia <- base_etiquetada %>%
  filter(motivo_exclusion == "Conservado") %>%
  # Eliminamos las variables temporales que creamos en el paso 1
  select(-tiene_comercial, -tiene_app, -tiene_maison, -motivo_exclusion)
print(base_limpia)



library(dplyr)

filas_na <- base_limpia %>%
  filter(is.na(code_type_local))

# Para visualizar el grupo
View(filas_na)


base_etiquetada_na <- base_limpia %>%
  group_by(id_mutation) %>%
  mutate(
    # Evaluamos si TODA la transacción cumple la condición de ser puro NA en tipo y superficie
    es_terreno_vacio = all(is.na(code_type_local)) & all(is.na(lot1_surface_carrez)),

    # Asignamos una etiqueta
    estado_filtro = if_else(es_terreno_vacio, "Borrado: Solo NA y sin superficie Carrez", "Conservado")
  ) %>%
  ungroup()

# 2. Creamos el conteo de qué se borró y con cuántas transacciones te quedas
resumen_filtro_na <- base_etiquetada_na %>%
  group_by(estado_filtro) %>%
  summarise(
    total_filas = n(),
    total_transacciones = n_distinct(id_mutation) # Cuenta los id_mutation únicos
  )

# Imprimimos la tabla para que la uses en tu informe
print(resumen_filtro_na)
