# Simulador de ozono — Regresión Lineal Simple

App Shiny que acompaña el Trabajo Final de Regresión Lineal Simple (Bloque 501).

Modelo: `lm(Ozone ~ Temp)` estimado con el dataset de calidad del aire de Nueva York
(mayo–septiembre de 1973, dataset `airquality` de R), con la misma división 80/20
(`set.seed(123)`) y preparación de datos del informe.

## Pestañas

- **Simulador**: ozono predicho e intervalo de predicción al 95 % para una temperatura dada.
- **Exploración**: dispersión de Ozone contra Temp, Wind y Solar.R.
- **Modelo**: coeficientes, R², RSE y `summary()`.
- **Evaluación**: MAE y RMSE en el conjunto de prueba.
- **Supuestos**: gráficos de residuos y pruebas de Breusch-Pagan, Durbin-Watson y Shapiro-Wilk.

## Ejecutar localmente

```r
install.packages(c("shiny", "lmtest"))
shiny::runApp()
```
