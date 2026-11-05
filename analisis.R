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

resumen_combinaciones <- base_limpia %>%
  # 1. Filtramos los registros sin tipo de local para que no ensucien el texto
  filter(!is.na(type_local)) %>%

  # 2. Agrupamos por identificador de transacción
  group_by(id_mutation) %>%

  # 3. Extraemos los tipos únicos de esa transacción, los ordenamos alfabéticamente
  # y los unimos en una sola frase separados por " + "
  summarise(
    combinacion = paste(sort(unique(type_local)), collapse = " + ")
  ) %>%

  # 4. Quitamos la agrupación por si acaso
  ungroup() %>%

  # 5. Contamos cuántas veces aparece cada combinación en la base completa
  count(combinacion, sort = TRUE)

# Visualizamos la tabla resultante
View(resumen_combinaciones)
