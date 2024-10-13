# Proyecto de Predicción de Esperanza de Vida

Este proyecto se centra en el análisis de los factores que influyen en la esperanza de vida a nivel mundial mediante la selección y evaluación de diversos modelos estadísticos y de machine learning. A partir de un conjunto de datos con información proveniente de fuentes reconocidas (OMS, Naciones Unidas, Banco Mundial, y "Our World in Data"), se busca desarrollar un modelo predictivo preciso que permita comprender mejor los determinantes de la esperanza de vida en distintos países.

## Objetivo del Proyecto

El objetivo principal es identificar el modelo que mejor prediga la esperanza de vida, con el fin de comprender la relación entre distintas variables sociodemográficas, económicas y de salud con la longevidad de la población.

## Estructura del Proyecto

El proyecto está organizado en las siguientes etapas:

1. **Preparación de los Datos**:
   - Limpieza y preprocesamiento de los datos.
   - Exploración y visualización inicial de los datos para detectar patrones.

2. **Selección de Modelos**:
   - Evaluación de diferentes algoritmos, incluyendo regresión lineal, árboles de decisión, random forest, y redes neuronales.
   - Optimización de hiperparámetros y validación cruzada.

3. **Evaluación de Modelos**:
   - Comparación de métricas de rendimiento como el error cuadrático medio (RMSE) y la puntuación R^2.
   - Análisis de la importancia de las características en cada modelo.

4. **Implementación del Modelo Final**:
   - Entrenamiento del modelo seleccionado con los mejores hiperparámetros.
   - Implementación de un pipeline para la predicción de nuevos datos.

5. **Visualización de Resultados**:
   - Gráficos de predicción vs. valores reales.
   - Mapas y gráficos interactivos para mostrar la variación de la esperanza de vida entre países.

## Requisitos

Para ejecutar este proyecto, asegúrate de tener instaladas las siguientes bibliotecas de Python:

- `pandas`
- `numpy`
- `matplotlib`
- `seaborn`
- `scikit-learn`
- `xgboost`
- `tensorflow` (opcional, para redes neuronales)

Para instalarlas, ejecuta:

```bash
pip install pandas numpy matplotlib seaborn scikit-learn xgboost tensorflow

## Uso

Para ejecutar el proyecto, sigue estos pasos:

1. Clona este repositorio:

   ```bash
   git clone https://github.com/usuario/proyecto-esperanza-vida.git

2. Navega al directorio del proyecto:
  ```bash
  cd proyecto-esperanza-vida

3. Ejecuta el script principal:
  R CÓDIGO PROYECTO DATOS - ÁLVARO ALONSO & ÓSCAR MARÍN.R

## Estructura del Repositorio

El proyecto está organizado en la siguiente estructura de carpetas:

```bash
proyecto-esperanza-vida/
├── data/                 # Datos crudos y preprocesados
├── notebooks/            # Jupyter notebooks con análisis exploratorio
├── src/                  # Código fuente de los modelos y funciones auxiliares
├── models/               # Modelos entrenados
├── results/              # Gráficos y reportes de resultados
└── README.md             # Este archivo

## Conjunto de Datos

El conjunto de datos utilizado contiene información sobre:

- Esperanza de vida (años)
- Producto Interno Bruto (PIB) per cápita
- Gasto en salud per cápita
- Tasa de mortalidad infantil
- Educación (años de escolaridad)
- Otros indicadores socioeconómicos

Los datos están disponibles en el directorio `data/` y se recomienda revisar el archivo `data/README.md` para más detalles sobre las fuentes y la estructura del conjunto de datos.

## Licencia

Este proyecto está bajo la Licencia MIT. Para más detalles, consulta el archivo `LICENSE`.
