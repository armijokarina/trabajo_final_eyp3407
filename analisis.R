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


