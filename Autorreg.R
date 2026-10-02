# ==============================================================================
# AUTOCORRELACIÓN TEMPORAL: 

library(lmtest)    # Diagnóstico (dwtest, bgtest) e inferencia (coeftest)
library(sandwich)  # Matriz de covarianza robusta HAC (NeweyWest)
library(nlme)      # Mínimos Cuadrados Generalizados (gls, corAR1)

# ------------------------------------------------------------------------------
# 1. GENERACIÓN DEL PROCESO AR(1)
# ------------------------------------------------------------------------------
set.seed(123)
n <- 120
t <- 1:n

x <- rnorm(n, mean = 5, sd = 1.5)

# Proceso autorregresivo: e_t = 0.75 * e_{t-1} + u_t
e <- numeric(n)
u <- rnorm(n, mean = 0, sd = 1)
for (i in 2:n) {
  e[i] <- 0.75 * e[i - 1] + u[i]
}

# Modelo poblacional verdadero: beta_0 = 2.0, beta_1 = 0.2
y <- 2.0 + 0.2 * x + e
datos <- data.frame(t = t, y = y, x = x)

# ------------------------------------------------------------------------------
# 2. MODELO OLS INGENUO Y DIAGNÓSTICO DE RESIDUOS
# ------------------------------------------------------------------------------
mod_ols <- lm(y ~ x, data = datos)
summary(mod_ols)

# Inspección visual de residuos
par(mfrow = c(1, 2))
plot(datos$t, residuals(mod_ols), type = "b", pch = 19, col = "steelblue",
     main = "Residuos OLS en el Tiempo", ylab = "Residuo", xlab = "t")
abline(h = 0, lty = 2, col = "firebrick")

acf(residuals(mod_ols), main = "ACF de Residuos OLS")
par(mfrow = c(1, 1))

# Pruebas formales de autocorrelación serial
dwtest(mod_ols)           # Durbin-Watson (AR(1))
bgtest(mod_ols, order = 1) # Breusch-Godfrey
