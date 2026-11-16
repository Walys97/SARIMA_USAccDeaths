# 🚗 Muertes Accidentales Mensuales en EE. UU. — Modelo SARIMA

Aplicación individual del curso **Series de Tiempo** — Universidad de Antioquia.  
**Autora:** Walys Vera Herrera

---

## 📊 Datos

Serie **USAccDeaths** (librería `datasets` de R): total mensual de muertes accidentales en Estados Unidos entre enero de 1973 y diciembre de 1978 (72 observaciones, frecuencia mensual, S = 12).

## 🔎 Fases de modelación (metodología Box-Jenkins)

1. Análisis descriptivo: tabla Buys-Ballot, perfil estacional y descomposición
2. Transformación de Box-Cox
3. Prueba de estacionariedad (Dickey-Fuller) y número de diferencias
4. Identificación con ACF y PACF
5. Identificación con criterios de información (AIC y BIC) y `auto.arima()`
6. Estimación de dos modelos tentativos
7. Diagnóstico residual y pruebas de normalidad
8. Selección final del modelo
9. Pronóstico a 24 meses (1979 – 1980)
10. Evaluación del pronóstico fuera de muestra (70 % / 30 %)
11. Aplicación Shiny interactiva

## ✅ Resultados principales

**Modelo seleccionado:** SARIMA(0,1,1)×(0,1,1)<sub>12</sub>

$$(1-B)(1-B^{12})\,Y_t = (1 - 0.4303\,B)(1 - 0.5528\,B^{12})\,a_t$$

| Criterio | Resultado |
|---|---|
| AIC / BIC | Menores entre los 36 modelos evaluados |
| Parámetros | θ₁ = −0.4303 (p = 0.0009) · Θ₁ = −0.5528 (p = 0.003) |
| Ljung-Box | p = 0.358 → residuos ruido blanco |
| Shapiro-Wilk | p = 0.328 → residuos normales |
| MAPE fuera de muestra | **6.19 %** |

## 📁 Archivos

| Archivo |
