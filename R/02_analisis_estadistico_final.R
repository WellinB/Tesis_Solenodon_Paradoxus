# =============================================================================
# 02_analisis_estadistico_final.R
# Tesis: Uso del hábitat por Solenodon paradoxus
#
# Este script reconstruye de forma reproducible los análisis estadísticos
# finales a partir de los archivos procesados de la tesis.
#
# IMPORTANTE:
# - Ejecutar desde la RAÍZ del repositorio.
# - No contiene rutas absolutas de la computadora personal.
# - Se fija una semilla (123) para que los procedimientos aleatorios puedan
#   repetirse de forma idéntica a partir de esta versión del repositorio.
# =============================================================================

# 0. PAQUETES -----------------------------------------------------------------

paquetes <- c(
  "sf",
  "dplyr",
  "tidyr",
  "ggplot2",
  "broom",
  "car",
  "pROC",
  "PRROC",
  "spdep"
)

faltantes <- paquetes[
  !vapply(paquetes, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))
]

if (length(faltantes) > 0) {
  stop(
    "Faltan estos paquetes: ",
    paste(faltantes, collapse = ", "),
    "\nInstálelos antes de continuar con install.packages()."
  )
}

library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)
library(car)
library(pROC)
library(PRROC)
library(spdep)

set.seed(123)

# Crear carpetas de salida si no existen
dir.create("resultados", showWarnings = FALSE, recursive = TRUE)
dir.create("figuras", showWarnings = FALSE, recursive = TRUE)


# 1. LOCALIZACIÓN DE ARCHIVOS --------------------------------------------------
# Los archivos de entrada pueden colocarse en datos/ (recomendado para GitHub)
# o permanecer en resultados/ para mantener compatibilidad con la estructura
# original del proyecto local.

buscar_archivo <- function(nombre) {
  candidatos <- c(
    file.path("datos", nombre),
    file.path("resultados", nombre)
  )
  existe <- file.exists(candidatos)
  if (!any(existe)) {
    stop(
      "No se encontró el archivo '", nombre,
      "'. Colóquelo en datos/ o resultados/."
    )
  }
  candidatos[which(existe)[1]]
}

ruta_modelo <- buscar_archivo("datos_modelo_forrajeo.rds")
ruta_segmentos <- buscar_archivo("segmentos_objetivo1.rds")
ruta_evidencias <- buscar_archivo("evidencias_objetivo1.rds")


# 2. CARGA DE DATOS ------------------------------------------------------------

datos_modelo <- readRDS(ruta_modelo)
segmentos_obj1 <- readRDS(ruta_segmentos)
evidencias_obj1 <- readRDS(ruta_evidencias)

cat("\n--- DIMENSIONES ---\n")
cat("datos_modelo: ", nrow(datos_modelo), " filas\n", sep = "")
cat("segmentos_objetivo1: ", nrow(segmentos_obj1), " filas\n", sep = "")
cat("evidencias_objetivo1: ", nrow(evidencias_obj1), " filas\n", sep = "")

# Para los modelos no necesitamos la geometría
datos_df <- if (inherits(datos_modelo, "sf")) {
  sf::st_drop_geometry(datos_modelo)
} else {
  as.data.frame(datos_modelo)
}


# 3. COMPROBACIONES BÁSICAS ----------------------------------------------------

variables_necesarias <- c(
  "deteccion_forrajeo",
  "dist_carretera_m",
  "dist_impact_m",
  "elev_mean",
  "pend_mean",
  "chm_mean",
  "forma_relieve"
)

faltan_variables <- setdiff(variables_necesarias, names(datos_df))

if (length(faltan_variables) > 0) {
  stop(
    "Faltan variables necesarias en datos_modelo_forrajeo.rds: ",
    paste(faltan_variables, collapse = ", ")
  )
}

# Asegurar respuesta binaria 0/1
datos_df <- datos_df %>%
  mutate(
    deteccion_forrajeo = as.integer(as.character(deteccion_forrajeo)),
    forma_relieve = tolower(trimws(as.character(forma_relieve)))
  )

if (!all(na.omit(unique(datos_df$deteccion_forrajeo)) %in% c(0L, 1L))) {
  stop("deteccion_forrajeo debe estar codificada como 0/1.")
}

# Referencia del factor: neutral
datos_df <- datos_df %>%
  mutate(
    forma_relieve = factor(
      forma_relieve,
      levels = c("neutral", "positivo", "negativo")
    )
  )

cat("\n--- DETECCIONES / NO DETECCIONES ---\n")
print(table(datos_df$deteccion_forrajeo, useNA = "ifany"))

prevalencia <- mean(datos_df$deteccion_forrajeo == 1, na.rm = TRUE)
cat("Prevalencia de detección: ", round(prevalencia, 6), "\n", sep = "")


# 4. OBJETIVO 2: ESTADÍSTICA DESCRIPTIVA --------------------------------------

variables_continuas <- c(
  "dist_carretera_m",
  "dist_impact_m",
  "elev_mean",
  "pend_mean",
  "chm_mean"
)

descriptivos <- datos_df %>%
  select(deteccion_forrajeo, all_of(variables_continuas)) %>%
  pivot_longer(
    cols = all_of(variables_continuas),
    names_to = "variable",
    values_to = "valor"
  ) %>%
  group_by(deteccion_forrajeo, variable) %>%
  summarise(
    n = sum(!is.na(valor)),
    media = mean(valor, na.rm = TRUE),
    desviacion_estandar = sd(valor, na.rm = TRUE),
    mediana = median(valor, na.rm = TRUE),
    minimo = min(valor, na.rm = TRUE),
    maximo = max(valor, na.rm = TRUE),
    .groups = "drop"
  )

write.csv(
  descriptivos,
  "resultados/objetivo2_estadisticos_descriptivos.csv",
  row.names = FALSE
)

# Relieve: frecuencia total, detecciones y proporciones
tabla_relieve <- datos_df %>%
  filter(!is.na(forma_relieve)) %>%
  group_by(forma_relieve) %>%
  summarise(
    total_segmentos = n(),
    segmentos_con_deteccion = sum(deteccion_forrajeo == 1, na.rm = TRUE),
    segmentos_sin_deteccion = sum(deteccion_forrajeo == 0, na.rm = TRUE),
    tasa_deteccion = segmentos_con_deteccion / total_segmentos,
    .groups = "drop"
  ) %>%
  mutate(
    porcentaje_entre_detecciones =
      segmentos_con_deteccion / sum(segmentos_con_deteccion)
  )

write.csv(
  tabla_relieve,
  "resultados/objetivo2_forma_relieve.csv",
  row.names = FALSE
)

# Boxplots principales
etiquetas_vars <- c(
  dist_carretera_m = "Distancia a carretera (m)",
  dist_impact_m = "Distancia a impacto antrópico (m)",
  elev_mean = "Elevación media (m)",
  pend_mean = "Pendiente media (°)",
  chm_mean = "Altura media del dosel (m)"
)

for (v in variables_continuas) {
  p <- ggplot(
    datos_df,
    aes(
      x = factor(
        deteccion_forrajeo,
        levels = c(0, 1),
        labels = c("Sin detección", "Con detección")
      ),
      y = .data[[v]]
    )
  ) +
    geom_boxplot(outlier.alpha = 0.35) +
    labs(
      x = NULL,
      y = etiquetas_vars[[v]]
    ) +
    theme_minimal(base_size = 12)

  ggsave(
    filename = file.path("figuras", paste0("objetivo2_", v, ".png")),
    plot = p,
    width = 7,
    height = 5,
    dpi = 300
  )
}


# 5. OBJETIVO 1: JOIN-COUNT CON kNN SIMÉTRICO ---------------------------------
# Se evalúa la agregación espacial de segmentos con detección mediante vecinos
# más cercanos (k = 4, 8 y 12). La matriz de vecindad se hace simétrica.
# La significancia se obtiene mediante 999 permutaciones Monte Carlo.
#
# La función siguiente cuenta aristas no dirigidas 1-1 (detección-detección)
# y 0-0 (no detección-no detección).

if (!inherits(segmentos_obj1, "sf")) {
  stop("segmentos_objetivo1.rds debe ser un objeto sf con geometría.")
}

if (!"deteccion_forrajeo" %in% names(segmentos_obj1)) {
  stop("segmentos_objetivo1.rds no contiene deteccion_forrajeo.")
}

segmentos_obj1$deteccion_forrajeo <-
  as.integer(as.character(segmentos_obj1$deteccion_forrajeo))

# Centroides para construir la vecindad kNN
centroides <- suppressWarnings(st_centroid(segmentos_obj1))
coords <- st_coordinates(centroides)

contar_aristas <- function(y, nb, valor = 1L) {
  total <- 0L
  for (i in seq_along(nb)) {
    vecinos <- nb[[i]]
    if (length(vecinos) == 0) next
    total <- total + sum(y[i] == valor & y[vecinos] == valor, na.rm = TRUE)
  }
  total / 2
}

join_count_mc <- function(y, coords, k, nsim = 999, semilla = 123) {
  knn <- spdep::knearneigh(coords, k = k)
  nb <- spdep::knn2nb(knn, sym = TRUE)

  obs_11 <- contar_aristas(y, nb, 1L)
  obs_00 <- contar_aristas(y, nb, 0L)

  set.seed(semilla)
  sim_11 <- numeric(nsim)
  sim_00 <- numeric(nsim)

  for (s in seq_len(nsim)) {
    y_perm <- sample(y, replace = FALSE)
    sim_11[s] <- contar_aristas(y_perm, nb, 1L)
    sim_00[s] <- contar_aristas(y_perm, nb, 0L)
  }

  data.frame(
    k = k,
    categoria = c("deteccion-deteccion", "no_deteccion-no_deteccion"),
    observado = c(obs_11, obs_00),
    media_simulada = c(mean(sim_11), mean(sim_00)),
    p_monte_carlo = c(
      (sum(sim_11 >= obs_11) + 1) / (nsim + 1),
      (sum(sim_00 >= obs_00) + 1) / (nsim + 1)
    ),
    nsim = nsim
  )
}

y_obj1 <- segmentos_obj1$deteccion_forrajeo

resultados_join_count <- bind_rows(
  join_count_mc(y_obj1, coords, k = 4, nsim = 999, semilla = 123),
  join_count_mc(y_obj1, coords, k = 8, nsim = 999, semilla = 123),
  join_count_mc(y_obj1, coords, k = 12, nsim = 999, semilla = 123)
)

write.csv(
  resultados_join_count,
  "resultados/objetivo1_join_count.csv",
  row.names = FALSE
)


# 6. OBJETIVO 3: PREPARACIÓN PARA EL GLM --------------------------------------

# Estandarización z de predictores continuos.
# Se recalcula aquí para que el script sea autosuficiente.
datos_df <- datos_df %>%
  mutate(
    dist_carretera_z = as.numeric(scale(dist_carretera_m)),
    dist_impact_z = as.numeric(scale(dist_impact_m)),
    elev_z = as.numeric(scale(elev_mean)),
    pend_z = as.numeric(scale(pend_mean)),
    chm_z = as.numeric(scale(chm_mean))
  )

datos_glm <- datos_df %>%
  select(
    deteccion_forrajeo,
    dist_carretera_z,
    dist_impact_z,
    elev_z,
    pend_z,
    chm_z,
    forma_relieve
  ) %>%
  drop_na()

cat("\nFilas utilizadas en GLM: ", nrow(datos_glm), "\n", sep = "")


# 7. GLM BINOMIAL --------------------------------------------------------------

formula_glm <- deteccion_forrajeo ~
  dist_carretera_z +
  dist_impact_z +
  elev_z +
  pend_z +
  chm_z +
  forma_relieve

modelo_glm <- glm(
  formula_glm,
  data = datos_glm,
  family = binomial(link = "logit")
)

cat("\n--- RESUMEN GLM ---\n")
print(summary(modelo_glm))

# Guardar el modelo
saveRDS(modelo_glm, "resultados/modelo_glm_forrajeo_reproducible.rds")

# Coeficientes
coeficientes_glm <- broom::tidy(modelo_glm)
write.csv(
  coeficientes_glm,
  "resultados/objetivo3_coeficientes_glm.csv",
  row.names = FALSE
)

# Odds ratios e IC 95 %
odds_ratios <- broom::tidy(
  modelo_glm,
  conf.int = TRUE,
  conf.level = 0.95,
  exponentiate = TRUE
)

write.csv(
  odds_ratios,
  "resultados/objetivo3_odds_ratios_ic95.csv",
  row.names = FALSE
)

# Comparación global contra modelo nulo
modelo_nulo <- glm(
  deteccion_forrajeo ~ 1,
  data = datos_glm,
  family = binomial(link = "logit")
)

comparacion_global <- anova(modelo_nulo, modelo_glm, test = "Chisq")
capture.output(
  comparacion_global,
  file = "resultados/objetivo3_prueba_global_modelo.txt"
)


# 8. MULTICOLINEALIDAD: VIF / GVIF --------------------------------------------

vif_crudo <- car::vif(modelo_glm)

if (is.matrix(vif_crudo)) {
  tabla_vif <- data.frame(
    variable = rownames(vif_crudo),
    vif_crudo,
    row.names = NULL,
    check.names = FALSE
  )

  # Para términos con >1 grado de libertad se incluye el GVIF ajustado.
  if (all(c("GVIF", "Df") %in% colnames(vif_crudo))) {
    tabla_vif$GVIF_ajustado <- with(
      tabla_vif,
      GVIF^(1 / (2 * Df))
    )
  }
} else {
  tabla_vif <- data.frame(
    variable = names(vif_crudo),
    VIF = as.numeric(vif_crudo)
  )
}

write.csv(
  tabla_vif,
  "resultados/objetivo3_vif.csv",
  row.names = FALSE
)


# 9. R² DE TJUR ----------------------------------------------------------------

prob_aparentes <- predict(modelo_glm, type = "response")

r2_tjur <- mean(
  prob_aparentes[datos_glm$deteccion_forrajeo == 1],
  na.rm = TRUE
) -
  mean(
    prob_aparentes[datos_glm$deteccion_forrajeo == 0],
    na.rm = TRUE
  )

write.csv(
  data.frame(R2_Tjur = r2_tjur),
  "resultados/objetivo3_r2_tjur.csv",
  row.names = FALSE
)


# 10. ROC-AUC APARENTE ---------------------------------------------------------

roc_aparente <- pROC::roc(
  response = datos_glm$deteccion_forrajeo,
  predictor = prob_aparentes,
  quiet = TRUE,
  direction = "<"
)

auc_aparente <- as.numeric(pROC::auc(roc_aparente))

write.csv(
  data.frame(ROC_AUC_aparente = auc_aparente),
  "resultados/objetivo3_auc_aparente.csv",
  row.names = FALSE
)


# 11. VALIDACIÓN CRUZADA ESTRATIFICADA DE 5 PARTICIONES -----------------------
# Cada modelo se ajusta con cuatro particiones y el ROC-AUC se calcula
# EXCLUSIVAMENTE sobre la partición dejada para evaluación.

crear_folds_estratificados <- function(y, k = 5, semilla = 123) {
  set.seed(semilla)

  folds <- integer(length(y))

  idx_1 <- which(y == 1)
  idx_0 <- which(y == 0)

  folds[idx_1] <- sample(rep(seq_len(k), length.out = length(idx_1)))
  folds[idx_0] <- sample(rep(seq_len(k), length.out = length(idx_0)))

  folds
}

fold_id <- crear_folds_estratificados(
  datos_glm$deteccion_forrajeo,
  k = 5,
  semilla = 123
)

pred_oof <- rep(NA_real_, nrow(datos_glm))
auc_folds <- numeric(5)

for (f in 1:5) {
  train <- datos_glm[fold_id != f, , drop = FALSE]
  test <- datos_glm[fold_id == f, , drop = FALSE]

  mod_f <- glm(
    formula_glm,
    data = train,
    family = binomial(link = "logit")
  )

  pred_f <- predict(
    mod_f,
    newdata = test,
    type = "response"
  )

  pred_oof[fold_id == f] <- pred_f

  roc_f <- pROC::roc(
    response = test$deteccion_forrajeo,
    predictor = pred_f,
    quiet = TRUE,
    direction = "<"
  )

  auc_folds[f] <- as.numeric(pROC::auc(roc_f))
}

tabla_cv <- data.frame(
  fold = 1:5,
  ROC_AUC = auc_folds
)

resumen_cv <- data.frame(
  ROC_AUC_media = mean(auc_folds),
  ROC_AUC_DE = sd(auc_folds)
)

write.csv(
  tabla_cv,
  "resultados/objetivo3_auc_cv_folds.csv",
  row.names = FALSE
)

write.csv(
  resumen_cv,
  "resultados/objetivo3_auc_cv_resumen.csv",
  row.names = FALSE
)


# 12. PRECISION-RECALL CON PREDICCIONES FUERA DE MUESTRA ----------------------

scores_pos <- pred_oof[datos_glm$deteccion_forrajeo == 1]
scores_neg <- pred_oof[datos_glm$deteccion_forrajeo == 0]

pr_cv <- PRROC::pr.curve(
  scores.class0 = scores_pos,
  scores.class1 = scores_neg,
  curve = TRUE
)

pr_auc_cv <- pr_cv$auc.integral

write.csv(
  data.frame(
    PR_AUC_CV = pr_auc_cv,
    prevalencia_base = prevalencia
  ),
  "resultados/objetivo3_pr_auc_cv.csv",
  row.names = FALSE
)


# 13. FOREST PLOT DE ODDS RATIOS ----------------------------------------------

forest_df <- odds_ratios %>%
  filter(term != "(Intercept)") %>%
  mutate(
    etiqueta = dplyr::recode(
      term,
      "dist_carretera_z" = "Distancia a carretera",
      "dist_impact_z" = "Distancia a impacto antrópico",
      "elev_z" = "Elevación",
      "pend_z" = "Pendiente",
      "chm_z" = "Altura del dosel",
      "forma_relievepositivo" = "Relieve positivo",
      "forma_relievenegativo" = "Relieve negativo",
      .default = term
    )
  )

p_forest <- ggplot(
  forest_df,
  aes(
    x = estimate,
    y = reorder(etiqueta, estimate)
  )
) +
  geom_vline(
    xintercept = 1,
    linetype = "dashed"
  ) +
  geom_point(size = 2.4) +
  geom_errorbarh(
    aes(
      xmin = conf.low,
      xmax = conf.high
    ),
    height = 0.18
  ) +
  scale_x_log10() +
  labs(
    x = "Odds ratio (escala logarítmica)",
    y = NULL
  ) +
  theme_minimal(base_size = 12)

ggsave(
  "figuras/objetivo3_forest_plot_odds_ratios.png",
  p_forest,
  width = 8,
  height = 5.5,
  dpi = 300
)


# 14. AUTOCORRELACIÓN ESPACIAL DE RESIDUOS: MORAN'S I -------------------------
# Se vinculan los residuos a los segmentos por id_segmento cuando el identificador
# está disponible en ambas bases. Si no está disponible, esta sección se omite
# con un mensaje explícito para evitar emparejar filas de forma incorrecta.

if (
  "id_segmento" %in% names(datos_df) &&
  "id_segmento" %in% names(segmentos_obj1)
) {
  datos_residuos <- datos_df %>%
    select(id_segmento) %>%
    mutate(residuo_deviance = residuals(modelo_glm, type = "deviance"))

  segmentos_res <- segmentos_obj1 %>%
    left_join(datos_residuos, by = "id_segmento") %>%
    filter(!is.na(residuo_deviance))

  centroides_res <- suppressWarnings(st_centroid(segmentos_res))
  coords_res <- st_coordinates(centroides_res)

  knn4 <- spdep::knearneigh(coords_res, k = 4)
  nb4 <- spdep::knn2nb(knn4, sym = TRUE)
  lw4 <- spdep::nb2listw(
    nb4,
    style = "W",
    zero.policy = TRUE
  )

  moran_res <- spdep::moran.test(
    segmentos_res$residuo_deviance,
    lw4,
    zero.policy = TRUE
  )

  capture.output(
    moran_res,
    file = "resultados/objetivo3_moran_residuos.txt"
  )
} else {
  message(
    "Moran's I no fue ejecutado: falta id_segmento en una de las bases."
  )
}


# 15. RESUMEN DE RESULTADOS EN CONSOLA ----------------------------------------

cat("\n============================================================\n")
cat("RESUMEN DEL ANÁLISIS REPRODUCIBLE\n")
cat("============================================================\n")
cat("n GLM: ", nrow(datos_glm), "\n", sep = "")
cat("Detecciones: ", sum(datos_glm$deteccion_forrajeo == 1), "\n", sep = "")
cat("No detecciones: ", sum(datos_glm$deteccion_forrajeo == 0), "\n", sep = "")
cat("ROC-AUC aparente: ", round(auc_aparente, 4), "\n", sep = "")
cat(
  "ROC-AUC CV media (DE): ",
  round(mean(auc_folds), 4),
  " (",
  round(sd(auc_folds), 4),
  ")\n",
  sep = ""
)
cat("PR AUC CV: ", round(pr_auc_cv, 4), "\n", sep = "")
cat("Prevalencia base: ", round(prevalencia, 4), "\n", sep = "")
cat("R² de Tjur: ", round(r2_tjur, 4), "\n", sep = "")


# 16. INFORMACIÓN DE LA SESIÓN -------------------------------------------------
# Registra la versión de R y de los paquetes utilizados.

capture.output(
  sessionInfo(),
  file = "resultados/sessionInfo_R.txt"
)

cat("\nAnálisis finalizado. Revise las carpetas resultados/ y figuras/.\n")
