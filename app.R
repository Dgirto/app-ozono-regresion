# =====================================================================
#  APP SHINY — Simulador de ozono (complemento del Trabajo Final)
#  Regresión Lineal Simple: Ozone (ppb) ~ Temp (°F)
#
#  Reproduce EXACTAMENTE el mismo modelo del script trabajo_final_regresion.R
#  (misma semilla, misma división 80/20 y misma preparación de datos).
#
#  Cómo ejecutarla en RStudio:
#    1. install.packages("shiny")   # una sola vez (incluye bslib)
#    2. Abrir este archivo y presionar el botón "Run App".
#  El archivo B_calidad_aire.csv debe estar en la misma carpeta que app.R.
# =====================================================================

library(shiny)
library(bslib)

# ---------------------------------------------------------------------
# 1. Datos y modelo (idéntico al script del informe)
# ---------------------------------------------------------------------
datos <- read.csv("B_calidad_aire.csv")

set.seed(123)
n         <- nrow(datos)
idx_train <- sort(sample(1:n, round(0.8 * n)))
train <- datos[idx_train, ]
test  <- datos[-idx_train, ]

train_prep <- train[!is.na(train$Ozone) & !is.na(train$Temp), ]
test_prep  <- test[!is.na(test$Ozone)  & !is.na(test$Temp), ]

modelo <- lm(Ozone ~ Temp, data = train_prep)
b0 <- unname(coef(modelo)[1])
b1 <- unname(coef(modelo)[2])

rango_T <- range(train_prep$Temp)
r2      <- summary(modelo)$r.squared
rse     <- summary(modelo)$sigma
ic_b1   <- confint(modelo)["Temp", ]

pred_test <- predict(modelo, newdata = test_prep)
err_test  <- test_prep$Ozone - pred_test
MAE  <- mean(abs(err_test))
RMSE <- sqrt(mean(err_test^2))

residuos  <- residuals(modelo)
ajustados <- fitted(modelo)

# Pruebas de supuestos (lmtest es opcional para la app)
tiene_lmtest <- requireNamespace("lmtest", quietly = TRUE)
p_bp <- if (tiene_lmtest) lmtest::bptest(modelo)$p.value else NA
p_dw <- if (tiene_lmtest) lmtest::dwtest(modelo)$p.value else NA
p_sw <- shapiro.test(residuos)$p.value

nombres_mes <- c("5" = "mayo", "6" = "junio", "7" = "julio", "8" = "agosto", "9" = "septiembre")
f_c <- function(f) (f - 32) * 5 / 9

# Nivel referencial (definido por el grupo a partir de la distribución
# de los datos; NO es una norma oficial de calidad del aire)
nivel_ozono <- function(o) {
  if (o < 30) list(txt = "Bajo", col = "success")
  else if (o < 60) list(txt = "Moderado", col = "warning")
  else list(txt = "Alto", col = "danger")
}

col_pts <- rgb(0.2, 0.4, 0.7, 0.6)

# ---------------------------------------------------------------------
# 2. Interfaz
# ---------------------------------------------------------------------
ui <- page_navbar(
  title = "Ozono vs Temperatura · NY 1973",
  fillable = FALSE,
  theme = bs_theme(version = 5, bootswatch = "flatly"),

  # ----- Pestaña 1: Simulador -----
  nav_panel(
    "Simulador",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        sliderInput("temp", "Temperatura máxima pronosticada (°F)",
                    min = 40, max = 110, value = 85, step = 1),
        uiOutput("temp_c"),
        checkboxInput("banda", "Mostrar banda de predicción al 95 %", TRUE),
        hr(),
        helpText(sprintf("Rango observado en entrenamiento: %d a %d °F.",
                         rango_T[1], rango_T[2]),
                 "Fuera de ese rango el modelo extrapola.")
      ),
      uiOutput("alerta"),
      layout_columns(
        col_widths = c(4, 4, 4),
        uiOutput("vb_pred"),
        uiOutput("vb_nivel"),
        uiOutput("vb_int")
      ),
      card(
        card_header("Datos de entrenamiento, recta estimada y predicción"),
        plotOutput("g_sim", height = "420px", fill = FALSE)
      ),
      card(
        card_body(
          uiOutput("explica")
        )
      )
    )
  ),

  # ----- Pestaña 2: Exploración (EDA) -----
  nav_panel(
    "Exploración",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        selectInput("xcand", "Variable candidata a X",
                    c("Temperatura (Temp, °F)" = "Temp",
                      "Viento (Wind, mph)" = "Wind",
                      "Radiación solar (Solar.R, langleys)" = "Solar.R")),
        checkboxInput("recta_eda", "Dibujar recta de mínimos cuadrados", FALSE),
        hr(),
        helpText("Solo se usa el conjunto de entrenamiento (como en la Fase 4).")
      ),
      layout_columns(
        col_widths = c(7, 5),
        card(card_header("Diagrama de dispersión"), plotOutput("g_eda", height = "380px", fill = FALSE)),
        card(card_header("Histograma de Ozone"), plotOutput("g_hist", height = "380px", fill = FALSE))
      ),
      card(card_body(uiOutput("txt_eda")))
    )
  ),

  # ----- Pestaña 3: Modelo -----
  nav_panel(
    "Modelo",
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      value_box("Pendiente (b1)", sprintf("%.3f ppb/°F", b1), theme = "primary"),
      value_box("IC 95 % de b1", sprintf("[%.2f ; %.2f]", ic_b1[1], ic_b1[2]), theme = "secondary"),
      value_box("R²", sprintf("%.1f %%", 100 * r2), theme = "info"),
      value_box("RSE", sprintf("%.1f ppb", rse), theme = "secondary")
    ),
    card(
      card_header(sprintf("Ozono estimado = %.2f + %.3f × Temp", b0, b1)),
      verbatimTextOutput("resumen")
    )
  ),

  # ----- Pestaña 4: Evaluación -----
  nav_panel(
    "Evaluación",
    layout_columns(
      col_widths = c(4, 4, 4),
      value_box("MAE (prueba)", sprintf("%.1f ppb", MAE), theme = "primary"),
      value_box("RMSE (prueba)", sprintf("%.1f ppb", RMSE), theme = "primary"),
      value_box("RMSE / RSE", sprintf("%.2f", RMSE / rse), theme = "info")
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(card_header("Prueba: ozono real vs predicho"), plotOutput("g_test", height = "380px", fill = FALSE)),
      card(card_header("Días del conjunto de prueba (20)"), tableOutput("tabla_test"))
    )
  ),

  # ----- Pestaña 5: Supuestos -----
  nav_panel(
    "Supuestos",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        radioButtons("sup", "Supuesto a revisar",
                     c("Linealidad" = "lin",
                       "Varianza constante" = "var",
                       "Independencia" = "ind",
                       "Normalidad" = "nor"))
      ),
      card(card_header(textOutput("tit_sup")), plotOutput("g_sup", height = "420px", fill = FALSE)),
      card(card_body(uiOutput("txt_sup")))
    )
  )
)

# ---------------------------------------------------------------------
# 3. Servidor
# ---------------------------------------------------------------------
server <- function(input, output, session) {

  pred <- reactive({
    nd <- data.frame(Temp = input$temp)
    list(
      ci = predict(modelo, nd, interval = "confidence"),
      pi = predict(modelo, nd, interval = "prediction")
    )
  })
  fuera <- reactive(input$temp < rango_T[1] || input$temp > rango_T[2])

  output$temp_c <- renderUI(
    helpText(sprintf("Equivale a %.1f °C", f_c(input$temp)))
  )

  output$alerta <- renderUI({
    if (fuera()) {
      div(class = "alert alert-danger",
          strong("Extrapolación: "),
          sprintf("%d °F está fuera del rango observado (%d–%d °F). ", input$temp, rango_T[1], rango_T[2]),
          "La recta no fue validada con estas temperaturas; la predicción no es confiable.")
    } else if (pred()$ci[1, "fit"] < 0) {
      div(class = "alert alert-warning",
          "La recta predice ozono negativo, lo cual es imposible: es una limitación del modelo lineal en días frescos.")
    }
  })

  output$vb_pred <- renderUI({
    value_box("Ozono predicho", sprintf("%.1f ppb", pred()$ci[1, "fit"]),
              theme = if (fuera()) "danger" else "primary")
  })
  output$vb_nivel <- renderUI({
    nv <- nivel_ozono(pred()$ci[1, "fit"])
    value_box("Nivel referencial", nv$txt, theme = nv$col)
  })
  output$vb_int <- renderUI({
    p <- pred()$pi
    value_box("Intervalo de predicción 95 % (un día)",
              sprintf("[%.0f ; %.0f] ppb", p[1, "lwr"], p[1, "upr"]), theme = "secondary")
  })

  output$g_sim <- renderPlot({
    xs <- seq(min(40, input$temp), max(110, input$temp), length.out = 200)
    pb <- predict(modelo, data.frame(Temp = xs), interval = "prediction")
    plot(train_prep$Temp, train_prep$Ozone, pch = 19, col = col_pts,
         xlim = range(xs), ylim = range(c(-60, 180, pb)),
         xlab = "Temperatura máxima (°F)", ylab = "Ozono (ppb)")
    rect(rango_T[1], -1000, rango_T[2], 1000, col = rgb(0, 0.6, 0, 0.05), border = NA)
    if (input$banda) {
      polygon(c(xs, rev(xs)), c(pb[, "lwr"], rev(pb[, "upr"])),
              col = rgb(1, 0, 0, 0.08), border = NA)
    }
    abline(h = 0, lty = 3, col = "gray50")
    lines(xs, pb[, "fit"], col = "red", lwd = 2)
    abline(v = rango_T, lty = 2, col = "darkgreen")
    p <- pred()$pi
    segments(input$temp, p[1, "lwr"], input$temp, p[1, "upr"], col = "darkorange", lwd = 3)
    points(input$temp, p[1, "fit"], pch = 21, bg = "darkorange", cex = 2.2, lwd = 2)
    legend("topleft", bty = "n", cex = 0.85, y.intersp = 1.3, inset = 0.01,
           legend = c("Días de entrenamiento", "Recta estimada", "Predicción y su intervalo",
                      "Rango observado de Temp"),
           pch = c(19, NA, 21, NA), lty = c(NA, 1, 1, 2), lwd = c(NA, 2, 3, 1),
           col = c(col_pts, "red", "darkorange", "darkgreen"), pt.bg = c(NA, NA, "darkorange", NA))
  }, res = 96)

  output$explica <- renderUI({
    ci <- pred()$ci; pi <- pred()$pi
    tagList(
      p(HTML(sprintf(
        "Para un día con <b>%d °F</b> de máxima, el modelo estima en promedio <b>%.1f ppb</b> de ozono
         (IC 95 %% de la media: %.1f a %.1f ppb). Para <b>un día concreto</b>, el ozono podría estar
         entre <b>%.0f y %.0f ppb</b>: el intervalo de predicción es mucho más ancho porque incluye la
         variabilidad propia de cada día (RSE = %.1f ppb).",
        input$temp, ci[1, "fit"], ci[1, "lwr"], ci[1, "upr"], pi[1, "lwr"], pi[1, "upr"], rse))),
      p(HTML(sprintf(
        "Cada °F adicional suma en promedio <b>%.2f ppb</b>. Niveles referenciales definidos por el grupo
         (no son normas oficiales): bajo &lt; 30 ppb, moderado 30–60 ppb, alto ≥ 60 ppb.", b1)))
    )
  })

  # ---- Exploración ----
  etiquetas <- c(Temp = "Temperatura máxima (°F)", Wind = "Velocidad del viento (mph)",
                 Solar.R = "Radiación solar (langleys)")
  output$g_eda <- renderPlot({
    x <- train[[input$xcand]]
    plot(x, train$Ozone, pch = 19, col = col_pts,
         xlab = etiquetas[input$xcand], ylab = "Ozono (ppb)")
    if (input$recta_eda) abline(lm(train$Ozone ~ x), col = "red", lwd = 2)
  }, res = 96)
  output$g_hist <- renderPlot({
    hist(train$Ozone, breaks = 15, col = "steelblue", border = "white",
         main = "", xlab = "Ozono (ppb)", ylab = "Frecuencia")
    abline(v = mean(train$Ozone, na.rm = TRUE), col = "red", lwd = 2)
    abline(v = median(train$Ozone, na.rm = TRUE), col = "darkgreen", lwd = 2, lty = 2)
    legend("topright", c("Media", "Mediana"), col = c("red", "darkgreen"),
           lwd = 2, lty = c(1, 2), bty = "n")
  }, res = 96)
  output$txt_eda <- renderUI({
    r <- cor(train[[input$xcand]], train$Ozone, use = "complete.obs")
    txt <- switch(input$xcand,
      Temp = "Relación directa y la más fuerte: es la X elegida para el modelo. Se nota una ligera curvatura.",
      Wind = "Relación inversa: con poco viento el ozono se acumula. La nube es curvada y más dispersa.",
      Solar.R = "Relación directa pero débil y en forma de triángulo; además tiene valores faltantes.")
    p(HTML(sprintf("Correlación con Ozone (filas completas de este par): <b>r = %.3f</b>. %s", r, txt)))
  })

  # ---- Modelo ----
  output$resumen <- renderPrint(summary(modelo))

  # ---- Evaluación ----
  output$g_test <- renderPlot({
    lim <- range(c(pred_test, test_prep$Ozone))
    plot(pred_test, test_prep$Ozone, pch = 19, col = "darkorange", cex = 1.3,
         xlim = lim, ylim = lim, xlab = "Ozono predicho (ppb)", ylab = "Ozono real (ppb)")
    abline(0, 1, lty = 2)
  }, res = 96)
  output$tabla_test <- renderTable({
    fila <- as.integer(rownames(test_prep))
    data.frame(
      Fecha = paste(datos$Day[fila], nombres_mes[as.character(datos$Month[fila])]),
      `Temp (°F)` = test_prep$Temp,
      `Real (ppb)` = test_prep$Ozone,
      `Predicho (ppb)` = round(pred_test, 1),
      `Error (ppb)` = round(err_test, 1),
      check.names = FALSE
    )
  }, striped = TRUE, hover = TRUE, spacing = "xs", digits = 1)

  # ---- Supuestos ----
  output$tit_sup <- renderText(switch(input$sup,
    lin = "Residuos vs valores ajustados (con curva de tendencia)",
    var = "Residuos vs valores ajustados (dispersión)",
    ind = "Residuos en orden temporal",
    nor = "Gráfico Q-Q de los residuos"))

  output$g_sup <- renderPlot({
    switch(input$sup,
      lin = , var = {
        plot(ajustados, residuos, pch = 19, col = col_pts,
             xlab = "Valores ajustados (ppb)", ylab = "Residuos (ppb)")
        abline(h = 0, col = "red", lwd = 2)
        if (input$sup == "lin") lines(lowess(ajustados, residuos), col = "darkgreen", lwd = 3)
      },
      ind = {
        plot(residuos, type = "b", pch = 19, col = "gray40",
             xlab = "Orden de la medición (mayo a septiembre)", ylab = "Residuos (ppb)")
        abline(h = 0, col = "red", lwd = 2)
      },
      nor = {
        qqnorm(residuos, pch = 19, col = col_pts, main = "")
        qqline(residuos, col = "red", lwd = 2)
      })
  }, res = 96)

  fmt_p <- function(p) if (is.na(p)) "no disponible (instalar lmtest)" else format(signif(p, 3), scientific = p < 1e-4)
  output$txt_sup <- renderUI({
    switch(input$sup,
      lin = p(HTML("<b>No se cumple.</b> Los residuos forman una curva en U: positivos en días frescos y muy
                    calurosos, negativos en la zona media. La relación real entre temperatura y ozono es curva.")),
      var = p(HTML(sprintf("<b>Hay dudas.</b> La dispersión es menor para valores ajustados bajos y más amplia
                    a partir de unos 40 ppb. Breusch-Pagan: p = %s (no rechaza la varianza constante).", fmt_p(p_bp)))),
      ind = p(HTML(sprintf("<b>Se cumple, con reservas.</b> Son datos diarios consecutivos, pero no se ven rachas
                    largas del mismo signo. Durbin-Watson: p = %s.", fmt_p(p_dw)))),
      nor = p(HTML(sprintf("<b>No se cumple.</b> La cola derecha se separa de la línea (días de ozono extremo).
                    Shapiro-Wilk: p = %s.", fmt_p(p_sw)))))
  })
}

shinyApp(ui, server)
