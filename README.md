# Trabajo Final: Análisis Estadístico Reproducible (Caso 09)
**Curso:** Tópicos Aplicados en Estadística (EYP3407)  
**Semestre:** Segundo Semestre 2026  

## Integrantes del Grupo
* Francisca Sepúlveda
* Alexander Pinto
* Karina Armijo
* Pía Mella
##  Descripción del Proyecto y Caso Asignado
Este repositorio contiene el código, los datos y el informe final 
correspondientes al **Caso 09**, enfocado en la predicción del valor de 
compraventas residenciales a partir de la base de datos 
*Demandes de valeurs foncières géolocalisées* (DVF) para el departamento de 
Rhône (código 69), Francia, durante el período 2021–2023. El flujo incluye 
la depuración y reconstrucción de la unidad transacción, la partición temporal, 
el ajuste de modelos de referencia, LASSO, GAM y XGBoost, y la evaluación de su 
desempeño fuera del ajuste

## Estructura del Repositorio

* `analisis.R`: Script principal en R que automatiza todo el flujo de trabajo (depuración, ingeniería de características, partición temporal, ajuste de modelos LASSO, GAM y XGBoost, y cálculo de métricas de desempeño).
* `Informe.pdf`: Código fuente y documento final compilado del informe de investigación.
* `datos/`: Carpeta destinada a almacenar los datos fuente comprimidos requeridos para la ejecución.
* `.gitignore`: Archivo de configuración para excluir archivos temporales o pesados innecesarios del control de versiones.

##  Entorno y Requisitos de Software
El análisis fue desarrollado completamente en el lenguaje **R**. Asegúrate de 
tener instalado las siguientes librerías necesarias para la ejecución del 
script:
  * `dplyr`
* `tidyr`
* `ggplot2`
* `gridExtra`
* `glmnet`
* `mgcv`
* `xgboost`

##  Instrucciones para acceder a los datos

Los datos se encuentran almacenados en el archivo comprimido `datos.zip`, el cual debe ser descomprimido antes de comenzar. En su interior se encuentra el archivo `transacciones_dvf.csv`, que debe cargarse en R en formato CSV, especificando que la primera fila contiene los nombres de las variables (`header = TRUE`). Una vez realizado este procedimiento, se podrá comenzar con la ejecución del código.


