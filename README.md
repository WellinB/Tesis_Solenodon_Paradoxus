# Tesis_Solenodon_Paradoxus

Código y materiales reproducibles asociados a la tesis:

**Uso del hábitat por *Solenodon paradoxus* en Jaiquí Picado Arriba, Jaiquí Picado Abajo y Jaiquipicao, Santiago y Valverde, República Dominicana: análisis espacial y estadística multivariada.**

## Descripción

Este repositorio contiene los scripts y cuadernos utilizados para el procesamiento de datos, análisis estadístico, análisis espacial y modelación predictiva de la tesis.

La variable de respuesta principal corresponde a la detección o no detección de evidencias de forrajeo de *Solenodon paradoxus* en segmentos de los recorridos muestreados.

## Estructura del repositorio

- `R/`: scripts utilizados para análisis descriptivos, análisis espacial y modelos lineales generalizados.
- `Python/`: cuadernos utilizados para el modelo predictivo Random Forest y su validación.
- `datos/`: archivos de entrada necesarios para reproducir los análisis.
- `resultados/`: tablas y resultados generados por los análisis.
- `figuras/`: figuras producidas durante el análisis.

## Software

### Python

Los análisis predictivos fueron realizados en:

- Python 3.12.10
- Jupyter Notebook
- scikit-learn 1.9.0

El cuaderno principal es:

`Python/01_modelo_predictivo.ipynb`

Este archivo contiene:

- preparación de los predictores;
- ajuste del modelo Random Forest;
- ponderación de clases;
- optimización de hiperparámetros mediante RandomizedSearchCV;
- validación cruzada anidada;
- validación espacial;
- cálculo de ROC-AUC y Average Precision;
- importancia de variables por permutación;
- gráficos de dependencia parcial.

Para garantizar la reproducibilidad de los procedimientos aleatorios se utilizaron semillas fijas (`random_state`), incluyendo los valores 123 y 456 según el procedimiento de validación.

### R

Los análisis estadísticos y espaciales incluyeron:

- estadística descriptiva;
- análisis espacial de las evidencias;
- análisis de agrupamiento mediante join-count;
- modelo lineal generalizado binomial;
- odds ratios e intervalos de confianza;
- diagnóstico de multicolinealidad mediante VIF;
- autocorrelación espacial de residuos mediante Moran's I;
- validación cruzada del GLM;
- ROC-AUC y Precision-Recall.

Los scripts correspondientes se encuentran en la carpeta `R/`.

## Reproducibilidad

Para reproducir los análisis se recomienda:

1. Descargar o clonar este repositorio.
2. Mantener la estructura de carpetas original.
3. Instalar las dependencias indicadas para R y Python.
4. Colocar los archivos de entrada en la carpeta `datos/`.
5. Ejecutar primero los scripts de preparación de datos.
6. Ejecutar posteriormente los análisis estadísticos y predictivos.

## Autora

Wellin Del Carmen Brito Jáquez

Universidad Autónoma de Santo Domingo  
Escuela de Biología

## Uso

Este repositorio forma parte de una investigación académica de tesis. Los resultados deben interpretarse considerando las limitaciones y el alcance descritos en el documento final de la investigación.
