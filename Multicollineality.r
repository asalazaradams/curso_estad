# ==============================================================================
# SCRIPT DE CLASE: DIAGNÓSTICO Y SOLUCIÓN DE MULTICOLINEALIDAD EN R
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. INSTALACIÓN Y CARGA DE PAQUETES
# ------------------------------------------------------------------------------
paquetes <- c("ISLR2", "faraway", "car", "corrplot", "leaps", "stargazer")
nuevos <- paquetes[!(paquetes %in% installed.packages()[, "Package"])]
if (length(nuevos) > 0) install.packages(nuevos)

library(ISLR2)     # Contiene el dataset Credit
library(faraway)   # Contiene el dataset fat (Bodyfat)
library(car)       # Función vif() para factores de inflación de varianza
library(corrplot)  # Gráficos de matriz de correlación
library(leaps)     # Selección exhaustiva de subconjuntos (regsubsets)
library(stargazer) # Tablas comparativas de modelos


# ==============================================================================
# CASO 1: DATASET 'Credit' (COLINEALIDAD BIPARTITA EXTREMA)
# Pregunta: ¿Aporta tener 'Limit' y 'Rating' a la vez para explicar 'Balance'?
# ==============================================================================

# 1.1 Exploración y Matriz de Correlación
# Filtramos únicamente predictores numéricos continuos de interés
vars_cred <- Credit[, c("Balance", "Income", "Limit", "Rating", "Cards", "Age", "Education")]

# Cálculo de la matriz de correlación de Pearson
matriz_cor_cred <- cor(vars_cred)
round(matriz_cor_cred, 3)

# Visualización: notar la correlación casi perfecta (r = 0.996) entre Limit y Rating
corrplot(matriz_cor_cred, 
         method = "circle", 
         type = "upper", 
         tl.col = "black", 
         title = "Caso Credit: Matriz de Correlación",
         mar = c(0, 0, 2, 0))


# 1.2 Estimación de Modelos: Conjunto vs. Restringidos
# Modelo saturado (ambas variables compitiendo)
mod_ambos  <- lm(Balance ~ Income + Limit + Rating + Age + Education, data = Credit)

# Modelos aislados (dejando solo una de las variables colineales)
mod_limit  <- lm(Balance ~ Income + Limit + Age + Education, data = Credit)
mod_rating <- lm(Balance ~ Income + Rating + Age + Education, data = Credit)


# 1.3 Diagnóstico Formal mediante VIF
# Valores mayores a 5-10 indican problemas graves; aquí superan 160-200
cat("\n--- Diagnóstico VIF del Modelo Conjunto (Credit) ---\n")
print(vif(mod_ambos))

cat("\n--- Diagnóstico VIF tras dejar solo 'Limit' ---\n")
print(vif(mod_limit))


# 1.4 Comparación de Tablas: Errores Estándar y R2
# Nota didáctica: El error estándar de Limit pasa de 0.033 a 0.015 al aislarlo.
# El R2 ajustado apenas pasa de 0.873 a 0.871 (no se pierde capacidad explicativa).
stargazer(mod_ambos, mod_limit, mod_rating, 
          type = "text", 
          keep.stat = c("n", "rsq", "adj.rsq", "f"),
          column.labels = c("Ambos", "Solo Limit", "Solo Rating"),
          model.numbers = FALSE)


# ==============================================================================
# CASO 2: DATASET 'fat' (MULTICOLINEALIDAD MULTIDIMENSIONAL Y SIGNOS ABSURDOS)
# Pregunta: ¿Aumentar el volumen corporal reduce la grasa? (Efecto subibaja)
# ==============================================================================

data(fat)

# 2.1 Modelo Saturado y Detección de Síntomas
mod_fat_full <- lm(siri ~ age + weight + height + neck + chest + 
                          abdomen + hip + thigh + knee + ankle + 
                          biceps + forearm + wrist, 
                    data = fat)

cat("\n--- Resumen del Modelo Saturado (Fat) ---\n")
summary(mod_fat_full)

cat("\n--- Diagnóstico VIF Modelo Saturado (Fat) ---\n")
print(vif(mod_fat_full))
# Observar:
# 1) Signo negativo en 'weight' o 'knee' (contraintuitivo por colinealidad).
# 2) VIFs elevados (weight, abdomen y hip superan con creces 10).


# 2.2 Solución A: Selección por Pasos mediante AIC (step)
# El algoritmo elimina covariables redundantes cuyo aporte marginal es inferior al castigo del AIC
mod_fat_step <- step(mod_fat_full, direction = "both", trace = 0)

cat("\n--- Resumen del Modelo Seleccionado por AIC (Stepwise) ---\n")
summary(mod_fat_step)

cat("\n--- VIF del Modelo Seleccionado por Stepwise ---\n")
print(vif(mod_fat_step))
# Los VIF descienden de manera sustancial y los signos se estabilizan.


# 2.3 Solución B: Selección Exhaustiva de Subconjuntos (leaps)
# Comparación formal: R2 Ajustado vs. Criterio de Información Bayesiano (BIC)
subsets_fat <- regsubsets(siri ~ age + weight + height + neck + chest + 
                                 abdomen + hip + thigh + knee + ankle + 
                                 biceps + forearm + wrist, 
                          data = fat, 
                          nvmax = 13)

resumen_subsets <- summary(subsets_fat)

# Identificación de los modelos óptimos bajo cada criterio
n_opt_adjr2 <- which.max(resumen_subsets$adjr2)
n_opt_bic   <- which.min(resumen_subsets$bic)

cat(paste("\nNúmero de predictores óptimos según R2 Ajustado:", n_opt_adjr2, "\n"))
cat(paste("Número de predictores óptimos según BIC (más estricto):", n_opt_bic, "\n"))


# 2.4 Gráfico Comparativo: R2 Ajustado vs. BIC
# Nota didáctica: R2 ajustado retiene más variables (|t| > 1); BIC es más parsimonioso
par(mfrow = c(1, 2))

# Panel 1: R2 Ajustado
plot(resumen_subsets$adjr2, type = "b", pch = 19, 
     xlab = "Número de Predictores", ylab = "R2 Ajustado", 
     main = "Máximo R2 Ajustado")
points(n_opt_adjr2, resumen_subsets$adjr2[n_opt_adjr2], 
       col = "red", cex = 2, pch = 20)

# Panel 2: BIC
plot(resumen_subsets$bic, type = "b", pch = 19, 
     xlab = "Número de Predictores", ylab = "Criterio BIC", 
     main = "Mínimo BIC (Parsimonia)")
points(n_opt_bic, resumen_subsets$bic[n_opt_bic], 
       col = "blue", cex = 2, pch = 20)

par(mfrow = c(1, 1)) # Restaurar panel gráfico


# 2.5 Variables Retenidas en Cada Modelo Óptimo
cat("\n--- Coeficientes elegidos por Máximo R2 Ajustado ---\n")
print(coef(subsets_fat, n_opt_adjr2))

cat("\n--- Coeficientes elegidos por Mínimo BIC ---\n")
print(coef(subsets_fat, n_opt_bic))