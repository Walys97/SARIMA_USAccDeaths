# ==============================================================================
#  Aplicación Shiny: Muertes Accidentales Mensuales en EE. UU. (USAccDeaths)
#  Series de Tiempo · Universidad de Antioquia
#  Datos: librería datasets de R, enero 1973 - diciembre 1978
# ==============================================================================

#------------------------------------------------------------------------------
# Librerías
#------------------------------------------------------------------------------
# install.packages(c("shiny", "bslib", "forecast", "ggplot2", "tseries"))  # solo la primera vez
library(shiny)
library(bslib)
library(forecast)
library(ggplot2)
library(tseries)

#------------------------------------------------------------------------------
# Datos
#------------------------------------------------------------------------------
data("USAccDeaths")
accid <- USAccDeaths
n     <- length(accid)

ultimo    <- tail(accid, 1)
var_anual <- (sum(accid[(n-11):n]) / sum(accid[(n-23):(n-12)]) - 1) * 100
mes_max   <- month.abb[cycle(accid)[which.max(accid)]]

df_serie <- data.frame(Tiempo = as.numeric(time(accid)), Muertes = as.numeric(accid))

# Fechas reales de cada observación y formato de mes en español (independiente del idioma del PC)
meses_es   <- c("enero", "febrero", "marzo", "abril", "mayo", "junio", "julio",
                "agosto", "septiembre", "octubre", "noviembre", "diciembre")
fecha_es   <- function(f) paste(meses_es[as.integer(format(f, "%m"))], "de", format(f, "%Y"))
fechas_obs <- seq(as.Date("1973-01-01"), by = "month", length.out = n)
ultima_obs <- tail(fechas_obs, 1)
df_hist    <- data.frame(Fecha_Obj = fechas_obs, Fecha = fecha_es(fechas_obs), Muertes = as.numeric(accid))

tema <- theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        panel.grid.minor = element_blank())

#------------------------------------------------------------------------------
# Interfaz de usuario
#------------------------------------------------------------------------------
ui <- page_navbar(fillable = FALSE,
  title = "🚗 USAccDeaths · SARIMA",
  theme = bs_theme(bootswatch = "flatly", primary = "#E65100"),

  # ---------------- 1. Serie ----------------
  nav_panel("1. Serie",
    layout_columns(
      value_box(title = "Muertes en diciembre de 1978", value = format(ultimo, big.mark = ".", decimal.mark = ","),
                showcase = icon("car-burst"), theme = "primary"),
      value_box(title = "Variación anual 1978 vs. 1977", value = paste0(round(var_anual, 2), " %"),
                showcase = icon("arrow-trend-up"), theme = "success"),
      value_box(title = paste0("Máximo mensual (", mes_max, ")"), value = format(max(accid), big.mark = ".", decimal.mark = ","),
                showcase = icon("chart-line"), theme = "warning")
    ),
    card(card_header("Muertes accidentales mensuales en EE. UU. (1973 - 1978)"),
         plotOutput("g_serie", height = "380px")),
    layout_columns(
      card(card_header("Perfil estacional"),   plotOutput("g_season", height = "330px")),
      card(card_header("Promedio por mes"),    plotOutput("g_month",  height = "330px"))
    )
  ),

  # ---------------- 2. Últimas observaciones ----------------
  nav_panel("2. Últimas observaciones",
    layout_sidebar(
      sidebar = sidebar(
        numericInput("recent_n", "Número de observaciones recientes a inspeccionar:",
                     value = 12, min = 1, max = 36, step = 1),
        helpText("Inspección detallada de los meses más recientes de la serie USAccDeaths.")
      ),
      h4(textOutput("recentTitle")),
      layout_columns(
        card(card_header("Tabla"),   tableOutput("recentTable")),
        card(card_header("Gráfico"), plotOutput("recentPlot", height = "380px"))
      )
    )
  ),

  # ---------------- 3. Identificación ----------------
  nav_panel("3. Identificación",
    layout_sidebar(
      sidebar = sidebar(
        checkboxInput("log_id", "Transformación logarítmica", FALSE),
        radioButtons("dif", "Diferenciación:",
                     choices = c("Serie original" = "0", "(1-B)" = "1",
                                 "(1-B^12)" = "12", "(1-B)(1-B^12)" = "1_12"),
                     selected = "1_12"),
        sliderInput("lags", "Rezagos", min = 24, max = 60, value = 48, step = 12),
        hr(),
        h6("Prueba de Dickey-Fuller"),
        verbatimTextOutput("adf")
      ),
      card(card_header("Serie transformada"), plotOutput("g_dif", height = "250px")),
      layout_columns(
        card(card_header("ACF"),  plotOutput("g_acf",  height = "300px")),
        card(card_header("PACF"), plotOutput("g_pacf", height = "300px"))
      )
    )
  ),

  # ---------------- 4. Modelo ----------------
  nav_panel("4. Modelo",
    layout_sidebar(
      sidebar = sidebar(
        h6("Parte regular"),
        layout_columns(numericInput("p", "p", 0, 0, 3), numericInput("d", "d", 1, 0, 2), numericInput("q", "q", 1, 0, 3)),
        h6("Parte estacional (S = 12)"),
        layout_columns(numericInput("P", "P", 0, 0, 2), numericInput("D", "D", 1, 0, 1), numericInput("Q", "Q", 1, 0, 2)),
        checkboxInput("log_m", "Transformación logarítmica", FALSE),
        hr(),
        h5(textOutput("etiqueta"))
      ),
      layout_columns(
        card(card_header("Parámetros estimados"),  tableOutput("t_coef")),
        card(card_header("Criterios y pruebas"),   tableOutput("t_diag"))
      ),
      card(card_header("Serie observada vs. valores ajustados"), plotOutput("g_ajuste", height = "300px")),
      layout_columns(
        card(card_header("Residuos estandarizados"), plotOutput("g_res",  height = "260px")),
        card(card_header("ACF de residuos"),         plotOutput("g_racf", height = "260px")),
        card(card_header("Q-Q plot"),                plotOutput("g_qq",   height = "260px"))
      )
    )
  ),

  # ---------------- 5. Pronóstico ----------------
  nav_panel("5. Pronóstico",
    layout_sidebar(
      sidebar = sidebar(
        wellPanel(
          h5(strong("Última observación disponible:")),
          h4(fecha_es(ultima_obs), style = "color: #2c3e50; font-weight: bold;")
        ),
        helpText("Pronósticos fuera de muestra con el modelo definido en la pestaña 4."),
        hr(),
        numericInput("h", "Horizonte de pronóstico (meses a futuro):", value = 24, min = 1, max = 36, step = 1),
        sliderInput("nivel", "Nivel de confianza del intervalo (%):", min = 80, max = 99, value = 95, step = 1),
        hr(),
        downloadButton("downloadData", "Descargar Pronósticos (CSV)", class = "btn-primary")
      ),
      navset_card_tab(
        nav_panel("Gráfico de Pronóstico",
                  plotOutput("g_pron", height = "450px"),
                  verbatimTextOutput("modelSummary")),
        nav_panel("Tabla de Predicciones",
                  h4("Proyección Mensual Detallada"),
                  tableOutput("t_pron"))
      )
    )
  ),

  # ---------------- 6. Validación ----------------
  nav_panel("6. Validación",
    layout_sidebar(
      sidebar = sidebar(
        sliderInput("pct", "% de datos para entrenamiento", min = 60, max = 90, value = 70, step = 5),
        p("Se re-ajusta el modelo de la pestaña 4 con los datos de entrenamiento y se pronostica el periodo de prueba.")
      ),
      card(card_header("Pronóstico fuera de muestra vs. valores reales"), plotOutput("g_val", height = "400px")),
      card(card_header("Métricas de precisión"), tableOutput("t_val"))
    )
  )
)

#------------------------------------------------------------------------------
# Servidor
#------------------------------------------------------------------------------
servidor <- function(input, output, session) {

  # ---------- 1. Serie ----------
  output$g_serie <- renderPlot({
    ggplot(df_serie, aes(Tiempo, Muertes)) + geom_line(colour = "#1F4E79", linewidth = 0.9) +
      geom_point(data = subset(df_serie, round((Tiempo %% 1) * 12) == 6), colour = "#E65100", size = 3) +
      geom_vline(xintercept = 1973:1979, colour = "grey80", linetype = 2) +
      labs(x = "Año", y = "Número de muertes", caption = "Puntos naranjas: julio de cada año") + tema
  })
  output$g_season <- renderPlot(ggseasonplot(accid, year.labels = FALSE) + labs(title = NULL, y = "Muertes") + tema)
  output$g_month  <- renderPlot(ggsubseriesplot(accid) + labs(title = NULL, y = "Muertes") + tema)

  # ---------- 2. Últimas observaciones ----------
  output$recentTitle <- renderText(sprintf("Últimas %d Observaciones", min(input$recent_n, n)))

  output$recentTable <- renderTable({
    k  <- min(input$recent_n, n)
    df <- tail(df_hist, k)
    df <- df[order(df$Fecha_Obj, decreasing = TRUE), c("Fecha", "Muertes")]
    df
  }, align = "c", digits = 0)

  output$recentPlot <- renderPlot({
    k  <- min(input$recent_n, n)
    df <- tail(df_hist, k)
    ggplot(df, aes(x = Fecha_Obj, y = Muertes)) +
      geom_line(colour = "steelblue", linewidth = 1) +
      geom_point(colour = "steelblue", size = 3) +
      geom_text(aes(label = Muertes), vjust = -0.8, colour = "gray20", size = 3.5, fontface = "bold") +
      scale_x_date(breaks = df$Fecha_Obj, labels = substr(meses_es[as.integer(format(df$Fecha_Obj, "%m"))], 1, 3)) +
      labs(title = sprintf("Trayectoria Reciente (Últimos %d Meses)", k), x = "Mes", y = "Muertes") +
      tema + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })

  # ---------- 3. Identificación ----------
  serie_dif <- reactive({
    y <- if (input$log_id) log(accid) else accid
    switch(input$dif,
           "0"    = y,
           "1"    = diff(y),
           "12"   = diff(y, lag = 12),
           "1_12" = diff(diff(y), lag = 12))
  })
  output$g_dif  <- renderPlot(ggplot(data.frame(t = as.numeric(time(serie_dif())), y = as.numeric(serie_dif())), aes(t, y)) +
                                geom_line(colour = "#E65100") + geom_hline(yintercept = 0, linetype = 2) + labs(x = "Año", y = "") + tema)
  output$g_acf  <- renderPlot(ggAcf(serie_dif(),  lag.max = input$lags) + labs(title = NULL) + tema)
  output$g_pacf <- renderPlot(ggPacf(serie_dif(), lag.max = input$lags) + labs(title = NULL) + tema)
  output$adf    <- renderPrint({
    pv <- suppressWarnings(tseries::adf.test(serie_dif())$p.value)
    cat("p-valor =", round(pv, 4), "\n", ifelse(pv < 0.05, "-> Estacionaria", "-> No estacionaria"))
  })

  # ---------- 4. Modelo ----------
  ajustar <- function(y) {
    tryCatch(
      Arima(y, order = c(input$p, input$d, input$q),
            seasonal = list(order = c(input$P, input$D, input$Q), period = 12),
            lambda = if (input$log_m) 0 else NULL, biasadj = TRUE, method = "CSS-ML"),
      error = function(e) NULL)
  }

  modelo <- reactive(ajustar(accid))

  # Mismo modelo estimado con stats::arima (igual que en el notebook) para la tabla de
  # parámetros y sigma^2: p-valores con distribución t y sigma^2 de máxima verosimilitud
  modelo_st <- reactive({
    y <- if (input$log_m) log(accid) else accid
    tryCatch(stats::arima(y, order = c(input$p, input$d, input$q),
                          seasonal = list(order = c(input$P, input$D, input$Q), period = 12),
                          method = "CSS-ML"),
             error = function(e) NULL)
  })

  etiqueta_txt <- reactive(paste0("SARIMA(", input$p, ",", input$d, ",", input$q, ")x(",
                                   input$P, ",", input$D, ",", input$Q, ")12"))
  output$etiqueta <- renderText(etiqueta_txt())

  output$t_coef <- renderTable({
    m <- modelo_st(); validate(need(!is.null(m), "El modelo no se pudo estimar con esos órdenes. Prueba otros."))
    se <- sqrt(diag(m$var.coef))
    gl <- n - input$d - 12 * input$D - length(coef(m))       # grados de libertad (57 en el Modelo 1)
    data.frame(Parámetro = names(coef(m)), Estimación = coef(m), Error_Est = se,
               t = coef(m) / se, p_valor = 2 * pt(-abs(coef(m) / se), df = gl))
  }, digits = 4)

  residuos <- reactive({
    m <- modelo(); req(m)
    r <- as.numeric(residuals(m))
    r <- r[(input$d + 12 * input$D + 1):length(r)]
    r / sqrt(m$sigma2)
  })

  output$t_diag <- renderTable({
    m <- modelo(); req(m); r <- residuos(); ms <- modelo_st(); req(ms)
    k  <- input$p + input$q + input$P + input$Q
    lb <- Box.test(r, lag = 24, type = "Ljung-Box", fitdf = k)$p.value
    sw <- shapiro.test(r)$p.value
    data.frame(Medida = c("AIC", "BIC", "sigma^2", "Ljung-Box (lag 24) p-valor", "Shapiro-Wilk p-valor"),
               Valor  = c(m$aic, m$bic, ms$sigma2, lb, sw),
               Decisión = c("", "", "",
                            ifelse(lb > 0.05, "Ruido blanco ✔", "Autocorrelación ✘"),
                            ifelse(sw > 0.05, "Normalidad ✔", "No normal ✘")))
  }, digits = 4)

  output$g_ajuste <- renderPlot({
    m <- modelo(); req(m)
    autoplot(accid, series = "Observada") + autolayer(fitted(m), series = "Ajustada") +
      scale_colour_manual(values = c("Observada" = "black", "Ajustada" = "#E65100")) +
      labs(x = "Año", y = "Muertes", colour = "") + tema + theme(legend.position = "bottom")
  })
  output$g_res  <- renderPlot({
    r <- residuos()
    ggplot(data.frame(t = seq_along(r), r = r), aes(t, r)) + geom_line(colour = "#1F4E79") +
      geom_hline(yintercept = c(-2, 0, 2), linetype = c(2, 1, 2), colour = c("red", "grey50", "red")) +
      labs(x = "", y = "") + tema
  })
  output$g_racf <- renderPlot(ggAcf(residuos(), lag.max = 36) + labs(title = NULL) + tema)
  output$g_qq   <- renderPlot(ggplot(data.frame(r = residuos()), aes(sample = r)) +
                                stat_qq(colour = "#1F4E79") + stat_qq_line(colour = "red") + tema)

  # ---------- 5. Pronóstico ----------
  pron <- reactive({
    m <- modelo(); req(m)
    forecast::forecast(m, h = input$h, level = input$nivel)
  })
  fechas_fc <- reactive(seq(ultima_obs, by = "month", length.out = input$h + 1)[-1])

  output$g_pron <- renderPlot({
    f <- pron()
    df_fc <- data.frame(Fecha_Obj = fechas_fc(), Pronostico = as.numeric(f$mean),
                        Lower = as.numeric(f$lower[, 1]), Upper = as.numeric(f$upper[, 1]))
    ggplot() +
      geom_ribbon(data = df_fc, aes(x = Fecha_Obj, ymin = Lower, ymax = Upper), fill = "firebrick", alpha = 0.2) +
      geom_line(data = df_hist, aes(x = Fecha_Obj, y = Muertes, colour = "Histórico"), linewidth = 0.8) +
      geom_line(data = df_fc, aes(x = Fecha_Obj, y = Pronostico, colour = "Pronóstico"), linewidth = 1, linetype = "dashed") +
      geom_point(data = df_fc, aes(x = Fecha_Obj, y = Pronostico), colour = "firebrick", size = 2) +
      geom_vline(xintercept = as.numeric(ultima_obs), linetype = "dotted", colour = "black", linewidth = 0.8) +
      scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
      scale_colour_manual(values = c("Histórico" = "steelblue", "Pronóstico" = "firebrick")) +
      labs(title = sprintf("Pronóstico a %d Meses a Futuro (Modelo %s)", input$h, etiqueta_txt()),
           subtitle = sprintf("Información hasta %s | Intervalo de confianza al %d%%", fecha_es(ultima_obs), input$nivel),
           x = "Fecha", y = "Número de muertes", colour = "") +
      tema + theme(plot.subtitle = element_text(hjust = 0.5), legend.position = "bottom")
  })

  output$modelSummary <- renderPrint({
    cat("--- Resumen del Modelo", etiqueta_txt(), "Ajustado ---\n")
    print(modelo())
  })

  tabla_pron <- reactive({
    f  <- pron()
    df <- data.frame(Fecha = fecha_es(fechas_fc()),
                     Pron = round(as.numeric(f$mean), 0),
                     Li   = round(as.numeric(f$lower[, 1]), 0),
                     Ls   = round(as.numeric(f$upper[, 1]), 0))
    colnames(df) <- c("Fecha", "Pronóstico Puntual",
                      paste0("Límite Inf (", input$nivel, "%)"), paste0("Límite Sup (", input$nivel, "%)"))
    df
  })
  output$t_pron <- renderTable(tabla_pron(), align = "c", digits = 0)

  output$downloadData <- downloadHandler(
    filename = function() paste0("pronostico_USAccDeaths_", gsub("[^0-9x]", "", etiqueta_txt()), ".csv"),
    content  = function(file) write.csv(tabla_pron(), file, row.names = FALSE, fileEncoding = "UTF-8")
  )

  # ---------- 6. Validación ----------
  validacion <- reactive({
    n_tr <- round(n * input$pct / 100)
    tr <- window(accid, end = time(accid)[n_tr]); te <- window(accid, start = time(accid)[n_tr + 1])
    m  <- ajustar(tr); req(m)
    list(f = forecast::forecast(m, h = length(te), level = 95), te = te)
  })
  output$g_val <- renderPlot({
    v <- validacion()
    autoplot(v$f, fcol = "#E65100", shadecols = "#FFCC80") +
      geom_line(data = data.frame(x = as.numeric(time(v$te)), y = as.numeric(v$te)),
                aes(x, y), colour = "black", linewidth = 0.8) +
      labs(title = NULL, x = "Año", y = "Muertes") + tema
  })
  output$t_val <- renderTable({
    v <- validacion(); a <- forecast::accuracy(v$f, v$te)
    data.frame(Conjunto = c("Entrenamiento", "Prueba"), a[, c("RMSE", "MAE", "MAPE")])
  }, digits = 3)
}

#------------------------------------------------------------------------------
# Ejecución de la Aplicación Shiny
#------------------------------------------------------------------------------
shinyApp(ui = ui, server = servidor)