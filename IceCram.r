# ==============================================================================
# CLASE: AUTOCORRELACIÓN TEMPORAL EN MODELOS ECONOMÉTRICOS
# Aplicación Práctica: Dataset "Icecream" (Hildreth y Lu, 1960)
# ==============================================================================
# Contexto:
# Modelamos el consumo per cápita de helado ('cons', pintas) en función de:
# - 'price': Precio del helado ($ por pinta)
# - 'income': Ingreso familiar semanal ($)
# - 'temp': Temperatura media exterior (°F)
# Los datos abarcan 30 periodos consecutivos de 4 semanas (frecuencia regular).
#
# Secuencia metodológica:
# 1. Ajuste OLS Ingenuo.
# 2. Diagnóstico formal de perturbaciones (Serie, ACF, Durbin-Watson, Breusch-Godfrey).
# 3. Ruta 1: Inferencia Robusta Newey-West (HAC).
# 4. Ruta 2: Mínimos Cuadrados Generalizados (GLS con corAR1).
# 5. Inferencia Estadística: Likelihood Ratio Test (LRT) y Blanqueamiento.
# 6. Tabla comparativa final para proyección en clase.
# ==============================================================================

# Instalar si es necesario:
# install.packages(c("AER", "lmtest", "sandwich", "nlme"))

library(AER)       # Contiene el dataset Icecream
library(lmtest)    # dwtest, bgtest, coeftest
library(sandwich)  # NeweyWest (matriz HAC)
library(nlme)      # gls, corAR1

# ------------------------------------------------------------------------------
# 1. CARGA Y PREPARACIÓN DE DATOS
# ------------------------------------------------------------------------------
data("Icecream", package = "AER")

# Añadimos un índice temporal explícito de 1 a n
Icecream$t <- 1:nrow(Icecream)

head(Icecream)
cat("Número total de observaciones (n):", nrow(Icecream), "\n")

# ------------------------------------------------------------------------------
# 2. MODELO OLS BASE (INGENUO)
# ------------------------------------------------------------------------------
mod_ols <- lm(cons ~ price + income + temp, data = Icecream)
summary(mod_ols)

# Comentario docente:
# Notar la aparente alta significancia de 'income' (p ~ 0.008) y 'temp' (p < 0.001).
# OLS asume E[e e'] = sigma^2 * I. Si hay memoria serial, el error estándar
# convencional está subestimado.

# ------------------------------------------------------------------------------
# 3. DIAGNÓSTICO DE RESIDUOS DE OLS
# ------------------------------------------------------------------------------
par(mfrow = c(1, 2))

# A) Gráfico temporal de residuos
plot(Icecream$t, residuals(mod_ols), type = "b", pch = 19, col = "steelblue",
     main = "Residuos OLS en el Tiempo", ylab = "Residuo", xlab = "Periodo (t)")
abline(h = 0, lty = 2, col = "firebrick")

# B) Función de Autocorrelación (ACF)
acf(residuals(mod_ols), main = "ACF de Residuos OLS")
par(mfrow = c(1, 1))

# Pruebas estadísticas formales:
# Durbin-Watson (H0: rho = 0 frente a H1: rho > 0)
dw_test <- dwtest(mod_ols)
print(dw_test)
# DW ~ 1.02 << 2 (p-valor < 0.001) confirma fuerte autocorrelación serial positiva.

# Breusch-Godfrey (evalúa órdenes superiores, ej. orden 2)
bg_test <- bgtest(mod_ols, order = 2)
print(bg_test)

# ------------------------------------------------------------------------------
# 4. RUTA 1: INFERENCIA ROBUSTA NEWEY-WEST (HAC)
# ------------------------------------------------------------------------------
# Filosofía: Conserva exactamente los coeficientes beta de OLS y recalcula
# la matriz de varianzas-covarianzas tipo sándwich con kernel de Bartlett.
vcov_hac <- NeweyWest(mod_ols)
mod_hac  <- coeftest(mod_ols, vcov = vcov_hac)
print(mod_hac)

# Comentario docente:
# Observar 'income': el coeficiente sigue siendo 0.003308, pero el error estándar
# sube de 0.00117 a 0.00150, haciendo que el p-valor pase de 0.008 a 0.033.

# ------------------------------------------------------------------------------
# 5. RUTA 2: MÍNIMOS CUADRADOS GENERALIZADOS (GLS)
# ------------------------------------------------------------------------------
# Filosofía: Modela explícitamente el proceso estocástico AR(1) de los residuos
# y aplica la matriz de transformación P para recuperar la eficiencia (BLUE).
mod_gls <- gls(cons ~ price + income + temp, 
               data = Icecream,
               correlation = corAR1(form = ~ t),
               method = "ML") # Permite comparativa de verosimilitud
summary(mod_gls)

# Modelo OLS equivalente estimado por ML para contrastes directos
mod_ols_ml <- gls(cons ~ price + income + temp, 
                  data = Icecream, 
                  method = "ML")

# ------------------------------------------------------------------------------
# 6. INFERENCIA Y EVALUACIÓN ESTRUCTURAL
# ------------------------------------------------------------------------------
# Likelihood Ratio Test (LRT) formal: H0: rho = 0 frente a H1: modelo con AR(1)
lrt <- anova(mod_ols_ml, mod_gls)
print(lrt)
# L.Ratio = 6.18, p-valor = 0.0129 -> Se rechaza H0; la estructura AR(1) mejora el ajuste.

# Diagnóstico de blanqueamiento: los residuos normalizados (e* = P * e)
# deben comportarse como ruido blanco
par(mfrow = c(1, 2))
acf(residuals(mod_ols, type = "response"), main = "OLS: Residuos con Memoria")
acf(residuals(mod_gls, type = "normalized"), main = "GLS: Residuos Normalizados")
par(mfrow = c(1, 1))

# ------------------------------------------------------------------------------
# 7. TABLA COMPARATIVA FINAL PARA EL AULA
# ------------------------------------------------------------------------------
vars <- c("(Intercept)", "price", "income", "temp")

resumen <- data.frame(
  Variable = rep(vars, 3),
  Metodo   = rep(c("1. OLS Ingenuo", "2. OLS + Newey-West (HAC)", "3. GLS (corAR1)"), each = 4),
  Beta     = c(coef(mod_ols), 
               coef(mod_ols), 
               coef(mod_gls)),
  SE       = c(summary(mod_ols)$coefficients[, 2],
               mod_hac[, 2],
               summary(mod_gls)$tTable[, 2]),
  t_stat   = c(summary(mod_ols)$coefficients[, 3],
               mod_hac[, 3],
               summary(mod_gls)$tTable[, 3]),
  p_val    = c(summary(mod_ols)$coefficients[, 4],
               mod_hac[, 4],
               summary(mod_gls)$tTable[, 4])
)

resumen$Beta   <- round(resumen$Beta, 4)
resumen$SE     <- round(resumen$SE, 4)
resumen$t_stat <- round(resumen$t_stat, 3)
resumen$p_val  <- round(resumen$p_val, 4)

print(resumen)