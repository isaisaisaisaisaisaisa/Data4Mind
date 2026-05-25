# =============================================================================
# TABLERO INTERACTIVO — SALUD MENTAL ADOLESCENTE
# Demostración Técnica de Arquitectura de Monitoreo
#
# NOTA ÉTICA Y DE GOBERNANZA:
# El dataset utilizado es sintético; los patrones que se exhiben sirven para
# demostrar la arquitectura del sistema, no como hallazgos epidemiológicos.
#
# Paquetes requeridos:
#   install.packages(c("shiny","shinydashboard","shinydashboardPlus",
#                      "ggplot2","dplyr","plotly","DT","scales","viridis"))
# =============================================================================

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)
library(DT)
library(scales)
library(viridis)

# -----------------------------------------------------------------------------
# 1. CARGA Y PREPARACIÓN DE DATOS
# -----------------------------------------------------------------------------

DATA_PATH <- "Teen_Mental_Health.csv"

df_raw <- read.csv(DATA_PATH, stringsAsFactors = FALSE)

df <- df_raw %>%
  mutate(
    gender                   = factor(gender,
                                      levels = c("male","female"),
                                      labels = c("Masculino","Femenino")),
    platform_usage           = factor(platform_usage),
    social_interaction_level = factor(social_interaction_level,
                                      levels = c("low","medium","high"),
                                      labels = c("Baja","Media","Alta")),
    sleep_quality            = factor(sleep_quality,
                                      levels = c("Poor","Fair","Good"),
                                      labels = c("Deficiente","Regular","Buena")),
    digital_wellbeing_flag   = factor(digital_wellbeing_flag,
                                      levels = c("At Risk","Moderate","Healthy"),
                                      labels = c("En riesgo","Moderado","Saludable")),
    depression_label         = factor(depression_label)
  )

# Paleta corporativa 
PAL_WELLBEING <- c("En riesgo" = "#e05c5c", "Moderado" = "#f5a623", "Saludable" = "#50c878")
PAL_GENDER    <- c("Masculino" = "#4fa3e0", "Femenino"  = "#e07cb0")
PAL_SLEEP     <- c("Deficiente" = "#e05c5c", "Regular" = "#f5a623", "Buena" = "#50c878")

tema_dashboard <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.background  = element_rect(fill = "#1e2130", color = NA),
      panel.background = element_rect(fill = "#1e2130", color = NA),
      panel.grid.major = element_line(color = "#2d3250", size = 0.4),
      panel.grid.minor = element_blank(),
      text             = element_text(color = "#c8cfe8"),
      axis.text        = element_text(color = "#9aa3c2"),
      axis.title       = element_text(color = "#c8cfe8", face = "bold"),
      legend.background = element_rect(fill = "#1e2130", color = NA),
      legend.text      = element_text(color = "#c8cfe8"),
      legend.title     = element_text(color = "#c8cfe8", face = "bold"),
      plot.title       = element_text(color = "#ffffff", face = "bold", size = 14),
      plot.subtitle    = element_text(color = "#9aa3c2", size = 11),
      strip.text       = element_text(color = "#c8cfe8", face = "bold"),
      strip.background = element_rect(fill = "#2d3250", color = NA)
    )
}

# -----------------------------------------------------------------------------
# 2. UI
# -----------------------------------------------------------------------------

ui <- dashboardPage(
  skin = "black",
  
  # ── HEADER ──────────────────────────────────────────────────────────────────
  dashboardHeader(
    title = tags$span(
      tags$img(src = "https://www.svgrepo.com/show/530440/mind.svg",
               height = "28px", style = "margin-right:8px;filter:invert(1);"),
      "Data4Mind"
    ),
    titleWidth = 280
  ),
  
  # ── SIDEBAR ─────────────────────────────────────────────────────────────────
  dashboardSidebar(
    width = 280,
    tags$div(
      style = "padding:12px 16px; background:#151824; border-bottom:1px solid #2d3250;",
      tags$p(
        style = "color:#e05c5c; font-size:11px; font-weight:700; margin:0 0 4px 0;",
        "⚠ AVISO ÉTICO"
      ),
      tags$p(
        style = "color:#9aa3c2; font-size:10px; margin:0; line-height:1.4;",
        "Dataset sintético — patrones para demostración técnica, no hallazgos epidemiológicos."
      )
    ),
    sidebarMenu(
      menuItem("Portada",            tabName = "portada",    icon = icon("home")),
      menuItem("Resumen Ejecutivo",  tabName = "resumen",    icon = icon("chart-pie")),
      menuItem("Uso Digital",        tabName = "digital",    icon = icon("mobile-alt")),
      menuItem("Sueño & Bienestar",  tabName = "sueno",      icon = icon("bed")),
      menuItem("Rendimiento & Estrés",tabName = "academico", icon = icon("graduation-cap")),
      menuItem("Análisis por Perfil",tabName = "perfil",     icon = icon("users")),
      menuItem("Tabla de Datos",     tabName = "tabla",      icon = icon("table")),
      menuItem("Ficha Técnica",      tabName = "ficha",      icon = icon("file-alt"))
    ),
    tags$hr(style = "border-color:#2d3250;"),
    # Filtros globales
    tags$div(
      style = "padding:0 16px;",
      tags$p(style = "color:#9aa3c2; font-size:11px; margin-bottom:6px;", "FILTROS GLOBALES"),
      selectInput("fil_gender", "Género",
                  choices = c("Todos", levels(df$gender)), selected = "Todos"),
      sliderInput("fil_age", "Rango de edad",
                  min = 13, max = 19, value = c(13, 19), step = 1),
      selectInput("fil_wellbeing", "Estado de bienestar",
                  choices = c("Todos", levels(df$digital_wellbeing_flag)), selected = "Todos")
    )
  ),
  
  # ── BODY ────────────────────────────────────────────────────────────────────
  dashboardBody(
    
    # CSS personalizado
    tags$head(
      tags$style(HTML("
        body, .wrapper { background-color: #151824 !important; }
        .skin-midnight .main-header .navbar,
        .skin-midnight .main-header .logo { background-color:#1e2130 !important; border-bottom:1px solid #2d3250; }
        .skin-midnight .sidebar { background-color:#1a1d2e !important; }
        .skin-midnight .treeview-menu { background-color:#151824 !important; }
        .content-wrapper { background-color:#151824 !important; }
        .box { background:#1e2130; border-top:none; border:1px solid #2d3250; border-radius:8px; }
        .box-header { color:#ffffff !important; }
        .box-title { color:#c8cfe8 !important; font-weight:700; }
        .value-box { border-radius:8px; border:1px solid #2d3250; }
        .small-box { border-radius:8px; }
        .small-box h3, .small-box p { color:#fff !important; }
        .info-box { background:#1e2130; border:1px solid #2d3250; border-radius:8px; }
        .info-box-text, .info-box-number { color:#c8cfe8 !important; }
        .nav-tabs-custom { background:#1e2130; border:1px solid #2d3250; border-radius:8px; }
        .nav-tabs-custom .nav-tabs li.active a { color:#fff; background:#2d3250; }
        select, input[type='text'] { background:#2d3250 !important; color:#c8cfe8 !important; border:1px solid #3d4570 !important; }
        .irs--shiny .irs-bar { background:#4fa3e0; }
        .irs--shiny .irs-handle { background:#4fa3e0; border-color:#4fa3e0; }
        .irs--shiny .irs-from,.irs--shiny .irs-to,.irs--shiny .irs-single { background:#4fa3e0; }
        .disclaimer-box { background:#2a1a1a; border:1px solid #e05c5c;
                          border-radius:8px; padding:16px; margin-bottom:20px; }
        .disclaimer-box p { color:#f0a0a0; margin:0; font-size:13px; line-height:1.6; }
        .disclaimer-box strong { color:#e05c5c; }
        .metric-badge { display:inline-block; padding:4px 10px; border-radius:4px;
                        font-size:11px; font-weight:700; }
      "))
    ),
    
    tabItems(
      
      # ================================================================
      # PESTAÑA 1: PORTADA
      # ================================================================
      tabItem(tabName = "portada",
              fluidRow(
                column(12,
                       tags$div(
                         style = "text-align:center; padding:40px 20px 20px;",
                         tags$h1(style = "color:#ffffff; font-size:36px; font-weight:800; margin-bottom:8px;",
                                 "MindScope Analytics"),
                         tags$h2(style = "color:#4fa3e0; font-size:20px; font-weight:400; margin-bottom:30px;",
                                 "Plataforma de Monitoreo de Bienestar Mental en Adolescentes"),
                         tags$div(
                           style = "max-width:780px; margin:0 auto;",
                           tags$div(
                             class = "disclaimer-box",
                             tags$p(
                               tags$strong("⚠ DECLARACIÓN DE DATOS Y GOBERNANZA:"),
                               tags$br(),
                               "El dataset utilizado en esta demostración es ", tags$strong("sintético."),
                               " Los patrones que se exhiben sirven para demostrar la arquitectura del sistema,",
                               tags$strong(" no como hallazgos epidemiológicos."),
                               " Este tablero ilustra cómo se vería una plataforma de monitoreo real si contara",
                               " con datos clínicos validados y señal estadística real. Ninguna cifra aquí",
                               " debe interpretarse ni citarse como evidencia de salud pública."
                             )
                           )
                         )
                       )
                )
              ),
              fluidRow(
                column(4,
                       tags$div(
                         style = "background:#1e2130; border:1px solid #2d3250; border-radius:8px; padding:24px; text-align:center; margin:8px;",
                         tags$div(style = "font-size:36px; margin-bottom:8px;", "🏗️"),
                         tags$h4(style = "color:#4fa3e0; font-weight:700;", "Arquitectura demostrada"),
                         tags$p(style = "color:#9aa3c2; font-size:13px;",
                                "Dashboard reactivo con filtros globales, visualizaciones interactivas y análisis multidimensional.")
                       )
                ),
                column(4,
                       tags$div(
                         style = "background:#1e2130; border:1px solid #2d3250; border-radius:8px; padding:24px; text-align:center; margin:8px;",
                         tags$div(style = "font-size:36px; margin-bottom:8px;", "⚖️"),
                         tags$h4(style = "color:#f5a623; font-weight:700;", "Ética & Gobernanza"),
                         tags$p(style = "color:#9aa3c2; font-size:13px;",
                                "Declaración explícita de datos sintéticos, limitaciones metodológicas y uso responsable.")
                       )
                ),
                column(4,
                       tags$div(
                         style = "background:#1e2130; border:1px solid #2d3250; border-radius:8px; padding:24px; text-align:center; margin:8px;",
                         tags$div(style = "font-size:36px; margin-bottom:8px;", "📊"),
                         tags$h4(style = "color:#50c878; font-weight:700;", "16 Variables · 1,200 registros"),
                         tags$p(style = "color:#9aa3c2; font-size:13px;",
                                "Cobertura de sueño, uso digital, rendimiento académico, estrés, ansiedad y bienestar.")
                       )
                )
              ),
              fluidRow(
                column(12,
                       box(title = "Stack Tecnológico", width = 12,
                           tags$div(
                             style = "display:flex; flex-wrap:wrap; gap:12px; padding:8px;",
                             lapply(c("R 4.x","Shiny","ggplot2","plotly","dplyr","DT","shinydashboard"),
                                    function(tech) {
                                      tags$span(
                                        style = "background:#2d3250; color:#c8cfe8; padding:6px 14px;
                                    border-radius:20px; font-size:13px; font-weight:600;",
                                        tech
                                      )
                                    })
                           )
                       )
                )
              )
      ),
      
      # ================================================================
      # PESTAÑA 2: RESUMEN EJECUTIVO
      # ================================================================
      tabItem(tabName = "resumen",
              fluidRow(
                valueBoxOutput("vbox_total",    width = 3),
                valueBoxOutput("vbox_riesgo",   width = 3),
                valueBoxOutput("vbox_stress",   width = 3),
                valueBoxOutput("vbox_sleep",    width = 3)
              ),
              fluidRow(
                box(title = "Distribución de Bienestar Digital", width = 6,
                    plotlyOutput("plot_wellbeing_pie", height = 320)),
                box(title = "Puntuación de Riesgo Mental — Distribución", width = 6,
                    plotlyOutput("plot_risk_hist", height = 320))
              ),
              fluidRow(
                box(title = "Bienestar Digital por Género", width = 6,
                    plotlyOutput("plot_wellbeing_gender", height = 320)),
                box(title = "Riesgo Mental por Edad", width = 6,
                    plotlyOutput("plot_risk_age", height = 320))
              )
      ),
      
      # ================================================================
      # PESTAÑA 3: USO DIGITAL
      # ================================================================
      tabItem(tabName = "digital",
              fluidRow(
                box(title = "Horas Diarias en Redes Sociales por Plataforma", width = 7,
                    plotlyOutput("plot_sm_platform", height = 340)),
                box(title = "Plataformas más Usadas", width = 5,
                    plotlyOutput("plot_platform_pie", height = 340))
              ),
              fluidRow(
                box(title = "Tiempo de Pantalla antes de Dormir vs Calidad del Sueño", width = 6,
                    plotlyOutput("plot_screen_sleep", height = 320)),
                box(title = "Horas en Redes vs Nivel de Adicción", width = 6,
                    plotlyOutput("plot_sm_addiction", height = 320))
              ),
              fluidRow(
                box(title = "Uso Digital por Edad y Género", width = 12,
                    plotlyOutput("plot_sm_age_gender", height = 300))
              )
      ),
      
      # ================================================================
      # PESTAÑA 4: SUEÑO & BIENESTAR
      # ================================================================
      tabItem(tabName = "sueno",
              fluidRow(
                box(title = "Distribución de Calidad del Sueño", width = 4,
                    plotlyOutput("plot_sleep_dist", height = 300)),
                box(title = "Horas de Sueño por Bienestar Digital", width = 8,
                    plotlyOutput("plot_sleep_wellbeing", height = 300))
              ),
              fluidRow(
                box(title = "Sueño vs Puntuación de Riesgo Mental", width = 6,
                    plotlyOutput("plot_sleep_risk", height = 320)),
                box(title = "Calidad del Sueño por Plataforma", width = 6,
                    plotlyOutput("plot_sleep_platform", height = 320))
              )
      ),
      
      # ================================================================
      # PESTAÑA 5: RENDIMIENTO & ESTRÉS
      # ================================================================
      tabItem(tabName = "academico",
              fluidRow(
                box(title = "Rendimiento Académico vs Estrés", width = 6,
                    plotlyOutput("plot_acad_stress", height = 320)),
                box(title = "Rendimiento vs Ansiedad", width = 6,
                    plotlyOutput("plot_acad_anxiety", height = 320))
              ),
              fluidRow(
                box(title = "Actividad Física vs Nivel de Estrés", width = 6,
                    plotlyOutput("plot_activity_stress", height = 320)),
                box(title = "Mapa de Correlaciones", width = 6,
                    plotlyOutput("plot_corr_heatmap", height = 320))
              )
      ),
      
      # ================================================================
      # PESTAÑA 6: ANÁLISIS POR PERFIL
      # ================================================================
      tabItem(tabName = "perfil",
              fluidRow(
                box(title = "Interacción Social vs Riesgo Mental", width = 6,
                    plotlyOutput("plot_social_risk", height = 320)),
                box(title = "Nivel de Adicción por Grupo Etario", width = 6,
                    plotlyOutput("plot_addiction_age", height = 320))
              ),
              fluidRow(
                box(title = "Radar de Indicadores Promedio por Bienestar", width = 12,
                    plotlyOutput("plot_radar", height = 420))
              )
      ),
      
      # ================================================================
      # PESTAÑA 7: TABLA DE DATOS
      # ================================================================
      tabItem(tabName = "tabla",
              fluidRow(
                box(title = "Dataset Filtrado", width = 12,
                    DTOutput("tabla_datos"))
              )
      ),
      
      # ================================================================
      # PESTAÑA 8: FICHA TÉCNICA
      # ================================================================
      tabItem(tabName = "ficha",
              fluidRow(
                column(12,
                       tags$div(
                         style = "max-width:860px; margin:0 auto;",
                         
                         tags$div(
                           class = "disclaimer-box",
                           tags$p(
                             tags$strong("DECLARACIÓN DE DATOS SINTÉTICOS (Sección de Ética y Gobernanza):"),
                             tags$br(),
                             "El dataset utilizado es sintético; los patrones que se exhiben sirven para",
                             " demostrar la arquitectura del sistema, no como hallazgos epidemiológicos.",
                             " Esta plataforma ilustra cómo se vería un sistema de monitoreo de bienestar",
                             " adolescente si operara con datos clínicos reales. Las distribuciones y",
                             " correlaciones observadas son artefactos del proceso de generación de datos",
                             " y no deben utilizarse para inferencias, políticas públicas ni intervenciones."
                           )
                         ),
                         
                         box(title = "Descripción del Dataset", width = 12,
                             tags$table(
                               style = "width:100%; border-collapse:collapse; color:#c8cfe8; font-size:13px;",
                               tags$thead(
                                 tags$tr(
                                   tags$th(style="padding:8px; border-bottom:1px solid #2d3250; color:#4fa3e0;", "Variable"),
                                   tags$th(style="padding:8px; border-bottom:1px solid #2d3250; color:#4fa3e0;", "Tipo"),
                                   tags$th(style="padding:8px; border-bottom:1px solid #2d3250; color:#4fa3e0;", "Descripción")
                                 )
                               ),
                               tags$tbody(
                                 lapply(list(
                                   c("age","Numérica","Edad del adolescente (13–19 años)"),
                                   c("gender","Categórica","Género (masculino / femenino)"),
                                   c("daily_social_media_hours","Numérica","Horas diarias en redes sociales"),
                                   c("platform_usage","Categórica","Plataforma principal utilizada"),
                                   c("sleep_hours","Numérica","Horas de sueño por noche"),
                                   c("screen_time_before_sleep","Numérica","Horas de pantalla antes de dormir"),
                                   c("academic_performance","Numérica","Puntuación de rendimiento académico (0–5)"),
                                   c("physical_activity","Numérica","Horas de actividad física diaria"),
                                   c("social_interaction_level","Categórica","Nivel de interacción social (baja/media/alta)"),
                                   c("stress_level","Numérica","Nivel de estrés (0–10)"),
                                   c("anxiety_level","Numérica","Nivel de ansiedad (0–10)"),
                                   c("addiction_level","Numérica","Nivel de adicción al dispositivo (0–10)"),
                                   c("depression_label","Binaria","Indicador de depresión (0/1)"),
                                   c("mental_health_risk_score","Numérica","Puntuación compuesta de riesgo (0–30)"),
                                   c("sleep_quality","Categórica","Calidad del sueño (deficiente/regular/buena)"),
                                   c("digital_wellbeing_flag","Categórica","Estado de bienestar digital (en riesgo/moderado/saludable)")
                                 ), function(row) {
                                   tags$tr(
                                     style = "border-bottom:1px solid #2d3250;",
                                     tags$td(style="padding:8px; font-family:monospace; color:#f5a623;", row[1]),
                                     tags$td(style="padding:8px; color:#9aa3c2;", row[2]),
                                     tags$td(style="padding:8px;", row[3])
                                   )
                                 })
                               )
                             )
                         ),
                         
                         box(title = "Limitaciones Metodológicas", width = 12,
                             tags$ul(
                               style = "color:#c8cfe8; font-size:13px; line-height:2;",
                               tags$li("Los datos son generados sintéticamente y no provienen de cohortes clínicas ni encuestas poblacionales validadas."),
                               tags$li("Las correlaciones observadas no implican causalidad y pueden ser artefactos del generador."),
                               tags$li("El balance de clases (género, bienestar) refleja la configuración de generación, no prevalencias reales."),
                               tags$li("No existen datos longitudinales; todos los registros son de corte transversal."),
                               tags$li("Las variables de salud mental (ansiedad, estrés, depresión) son proxies sintéticos sin validación psicométrica.")
                             )
                         )
                       )
                )
              )
      )
      
    ) # fin tabItems
  ) # fin dashboardBody
) # fin dashboardPage


# -----------------------------------------------------------------------------
# 3. SERVER
# -----------------------------------------------------------------------------

server <- function(input, output, session) {
  
  # Datos filtrados reactivos
  datos <- reactive({
    d <- df
    if (input$fil_gender != "Todos")   d <- d %>% filter(gender == input$fil_gender)
    if (input$fil_wellbeing != "Todos") d <- d %>% filter(digital_wellbeing_flag == input$fil_wellbeing)
    d <- d %>% filter(age >= input$fil_age[1], age <= input$fil_age[2])
    d
  })
  
  # ── VALUE BOXES ────────────────────────────────────────────────────────────
  
  output$vbox_total <- renderValueBox({
    valueBox(nrow(datos()), "Registros totales", icon = icon("users"),
             color = "blue")
  })
  
  output$vbox_riesgo <- renderValueBox({
    pct <- round(mean(datos()$digital_wellbeing_flag == "En riesgo") * 100, 1)
    valueBox(paste0(pct, "%"), "En riesgo digital", icon = icon("exclamation-triangle"),
             color = "red")
  })
  
  output$vbox_stress <- renderValueBox({
    val <- round(mean(datos()$stress_level), 2)
    valueBox(val, "Estrés promedio (0–10)", icon = icon("brain"), color = "orange")
  })
  
  output$vbox_sleep <- renderValueBox({
    val <- round(mean(datos()$sleep_hours), 1)
    valueBox(paste0(val, "h"), "Sueño promedio", icon = icon("bed"), color = "green")
  })
  
  # ── RESUMEN ────────────────────────────────────────────────────────────────
  
  output$plot_wellbeing_pie <- renderPlotly({
    d <- datos() %>% count(digital_wellbeing_flag)
    plot_ly(d, labels = ~digital_wellbeing_flag, values = ~n, type = "pie",
            marker = list(colors = unname(PAL_WELLBEING[levels(df$digital_wellbeing_flag)]),
                          line = list(color = "#1e2130", width = 2)),
            textinfo = "label+percent",
            textfont = list(color = "#ffffff", size = 12)) %>%
      layout(paper_bgcolor = "#1e2130", font = list(color = "#c8cfe8"),
             showlegend = TRUE,
             legend = list(font = list(color = "#c8cfe8")))
  })
  
  output$plot_risk_hist <- renderPlotly({
    p <- ggplot(datos(), aes(x = mental_health_risk_score,
                             fill = digital_wellbeing_flag)) +
      geom_histogram(bins = 20, alpha = 0.85, color = "#1e2130") +
      scale_fill_manual(values = PAL_WELLBEING, name = "Bienestar") +
      labs(x = "Puntuación de Riesgo", y = "Frecuencia") +
      tema_dashboard()
    ggplotly(p) %>%
      layout(paper_bgcolor = "#1e2130", plot_bgcolor = "#1e2130",
             font = list(color = "#c8cfe8"),
             legend = list(font = list(color = "#c8cfe8")))
  })
  
  output$plot_wellbeing_gender <- renderPlotly({
    d <- datos() %>% count(gender, digital_wellbeing_flag)
    p <- ggplot(d, aes(x = gender, y = n, fill = digital_wellbeing_flag)) +
      geom_col(position = "fill", alpha = 0.9) +
      scale_fill_manual(values = PAL_WELLBEING, name = "Bienestar") +
      scale_y_continuous(labels = percent_format()) +
      labs(x = "Género", y = "Proporción") +
      tema_dashboard()
    ggplotly(p) %>%
      layout(paper_bgcolor = "#1e2130", plot_bgcolor = "#1e2130",
             font = list(color = "#c8cfe8"))
  })
  
  output$plot_risk_age <- renderPlotly({
    d <- datos() %>% group_by(age) %>%
      summarise(riesgo_prom = mean(mental_health_risk_score), .groups = "drop")
    p <- ggplot(d, aes(x = age, y = riesgo_prom)) +
      geom_line(color = "#4fa3e0", size = 1.2) +
      geom_point(color = "#4fa3e0", size = 3) +
      labs(x = "Edad", y = "Riesgo promedio") +
      tema_dashboard()
    ggplotly(p) %>%
      layout(paper_bgcolor = "#1e2130", plot_bgcolor = "#1e2130",
             font = list(color = "#c8cfe8"))
  })
  
  # ── USO DIGITAL ────────────────────────────────────────────────────────────
  
  output$plot_sm_platform <- renderPlotly({
    p <- ggplot(datos(), aes(x = platform_usage, y = daily_social_media_hours,
                             fill = platform_usage)) +
      geom_violin(alpha = 0.7, color = NA) +
      geom_boxplot(width = 0.12, fill = "#1e2130", color = "#c8cfe8", outlier.size = 1) +
      scale_fill_viridis_d(option = "C") +
      labs(x = "Plataforma", y = "Horas diarias") +
      tema_dashboard() + theme(legend.position = "none")
    ggplotly(p) %>%
      layout(paper_bgcolor = "#1e2130", plot_bgcolor = "#1e2130",
             font = list(color = "#c8cfe8"))
  })
  
  output$plot_platform_pie <- renderPlotly({
    d <- datos() %>% count(platform_usage)
    plot_ly(d, labels = ~platform_usage, values = ~n, type = "pie",
            marker = list(line = list(color = "#1e2130", width = 2)),
            textinfo = "label+percent",
            textfont = list(color = "#ffffff", size = 11)) %>%
      layout(paper_bgcolor = "#1e2130", font = list(color = "#c8cfe8"),
             legend = list(font = list(color = "#c8cfe8")))
  })
  
  output$plot_screen_sleep <- renderPlotly({
    p <- ggplot(datos(), aes(x = screen_time_before_sleep, y = sleep_hours,
                             color = sleep_quality)) +
      geom_point(alpha = 0.5, size = 1.8) +
      geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
      scale_color_manual(values = PAL_SLEEP, name = "Calidad sueño") +
      labs(x = "Pantalla antes de dormir (h)", y = "Horas de sueño") +
      tema_dashboard()
    ggplotly(p) %>%
      layout(paper_bgcolor = "#1e2130", plot_bgcolor = "#1e2130",
             font = list(color = "#c8cfe8"))
  })
  
  output$plot_sm_addiction <- renderPlotly({
    p <- ggplot(datos(), aes(x = daily_social_media_hours, y = addiction_level,
                             color = digital_wellbeing_flag)) +
      geom_point(alpha = 0.45, size = 1.6) +
      geom_smooth(method = "loess", se = FALSE, linewidth = 1.2) +
      scale_color_manual(values = PAL_WELLBEING, name = "Bienestar") +
      labs(x = "Horas en redes (diarias)", y = "Nivel de adicción") +
      tema_dashboard()
    ggplotly(p) %>%
      layout(paper_bgcolor = "#1e2130", plot_bgcolor = "#1e2130",
             font = list(color = "#c8cfe8"))
  })
  
  output$plot_sm_age_gender <- renderPlotly({
    d <- datos() %>% group_by(age, gender) %>%
      summarise(sm_avg = mean(daily_social_media_hours), .groups = "drop")
    p <- ggplot(d, aes(x = age, y = sm_avg, color = gender, group = gender)) +
      geom_line(size = 1.3) +
      geom_point(size = 3) +
      scale_color_manual(values = PAL_GENDER, name = "Género") +
      labs(x = "Edad", y = "Horas promedio en redes") +
      tema_dashboard()
    ggplotly(p) %>%
      layout(paper_bgcolor = "#1e2130", plot_bgcolor = "#1e2130",
             font = list(color = "#c8cfe8"))
  })
  
  # ── SUEÑO & BIENESTAR ──────────────────────────────────────────────────────
  
  output$plot_sleep_dist <- renderPlotly({
    d <- datos() %>% count(sleep_quality)
    p <- ggplot(d, aes(x = sleep_quality, y = n, fill = sleep_quality)) +
      geom_col(alpha = 0.9) +
      scale_fill_manual(values = PAL_SLEEP) +
      labs(x = NULL, y = "Frecuencia") +
      tema_dashboard() + theme(legend.position = "none")
    ggplotly(p) %>%
      layout(paper_bgcolor = "#1e2130", plot_bgcolor = "#1e2130",
             font = list(color = "#c8cfe8"))
  })
  
  output$plot_sleep_wellbeing <- renderPlotly({
    p <- ggplot(datos(), aes(x = digital_wellbeing_flag, y = sleep_hours,
                             fill = digital_wellbeing_flag)) +
      geom_boxplot(alpha = 0.8, outlier.color = "#c8cfe8", outlier.size = 1) +
      scale_fill_manual(values = PAL_WELLBEING) +
      labs(x = "Bienestar Digital", y = "Horas de sueño") +
      tema_dashboard() + theme(legend.position = "none")
    ggplotly(p) %>%
      layout(paper_bgcolor = "#1e2130", plot_bgcolor = "#1e2130",
             font = list(color = "#c8cfe8"))
  })
  
  output$plot_sleep_risk <- renderPlotly({
    p <- ggplot(datos(), aes(x = sleep_hours, y = mental_health_risk_score,
                             color = sleep_quality)) +
      geom_point(alpha = 0.45, size = 1.6) +
      geom_smooth(method = "lm", se = TRUE, alpha = 0.15, linewidth = 1) +
      scale_color_manual(values = PAL_SLEEP, name = "Calidad sueño") +
      labs(x = "Horas de sueño", y = "Riesgo mental") +
      tema_dashboard()
    ggplotly(p) %>%
      layout(paper_bgcolor = "#1e2130", plot_bgcolor = "#1e2130",
             font = list(color = "#c8cfe8"))
  })
  
  output$plot_sleep_platform <- renderPlotly({
    d <- datos() %>% count(platform_usage, sleep_quality)
    p <- ggplot(d, aes(x = platform_usage, y = n, fill = sleep_quality)) +
      geom_col(position = "fill", alpha = 0.9) +
      scale_fill_manual(values = PAL_SLEEP, name = "Calidad sueño") +
      scale_y_continuous(labels = percent_format()) +
      labs(x = "Plataforma", y = "Proporción") +
      tema_dashboard()
    ggplotly(p) %>%
      layout(paper_bgcolor = "#1e2130", plot_bgcolor = "#1e2130",
             font = list(color = "#c8cfe8"))
  })
  
  # ── RENDIMIENTO & ESTRÉS ───────────────────────────────────────────────────
  
  output$plot_acad_stress <- renderPlotly({
    p <- ggplot(datos(), aes(x = stress_level, y = academic_performance,
                             color = digital_wellbeing_flag)) +
      geom_point(alpha = 0.4, size = 1.6) +
      geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
      scale_color_manual(values = PAL_WELLBEING, name = "Bienestar") +
      labs(x = "Nivel de estrés", y = "Rendimiento académico") +
      tema_dashboard()
    ggplotly(p) %>%
      layout(paper_bgcolor = "#1e2130", plot_bgcolor = "#1e2130",
             font = list(color = "#c8cfe8"))
  })
  
  output$plot_acad_anxiety <- renderPlotly({
    p <- ggplot(datos(), aes(x = anxiety_level, y = academic_performance,
                             color = gender)) +
      geom_point(alpha = 0.4, size = 1.6) +
      geom_smooth(method = "loess", se = FALSE, linewidth = 1.2) +
      scale_color_manual(values = PAL_GENDER, name = "Género") +
      labs(x = "Nivel de ansiedad", y = "Rendimiento académico") +
      tema_dashboard()
    ggplotly(p) %>%
      layout(paper_bgcolor = "#1e2130", plot_bgcolor = "#1e2130",
             font = list(color = "#c8cfe8"))
  })
  
  output$plot_activity_stress <- renderPlotly({
    p <- ggplot(datos(), aes(x = physical_activity, y = stress_level,
                             color = digital_wellbeing_flag)) +
      geom_point(alpha = 0.4, size = 1.6) +
      geom_smooth(method = "loess", se = FALSE, linewidth = 1.2) +
      scale_color_manual(values = PAL_WELLBEING, name = "Bienestar") +
      labs(x = "Actividad física (h/día)", y = "Nivel de estrés") +
      tema_dashboard()
    ggplotly(p) %>%
      layout(paper_bgcolor = "#1e2130", plot_bgcolor = "#1e2130",
             font = list(color = "#c8cfe8"))
  })
  
  output$plot_corr_heatmap <- renderPlotly({
    num_vars <- c("daily_social_media_hours","sleep_hours","screen_time_before_sleep",
                  "academic_performance","physical_activity","stress_level",
                  "anxiety_level","addiction_level","mental_health_risk_score")
    labels_es <- c("Redes (h)","Sueño (h)","Pantalla previa",
                   "Rendimiento","Act. Física","Estrés",
                   "Ansiedad","Adicción","Riesgo Mental")
    mat <- cor(datos()[, num_vars], use = "complete.obs")
    colnames(mat) <- labels_es; rownames(mat) <- labels_es
    plot_ly(z = mat, x = labels_es, y = labels_es, type = "heatmap",
            colorscale = list(c(0,"#e05c5c"), c(0.5,"#1e2130"), c(1,"#50c878")),
            zmin = -1, zmax = 1,
            text = round(mat, 2), texttemplate = "%{text}",
            textfont = list(size = 9, color = "#ffffff"),
            hoverinfo = "x+y+z") %>%
      layout(paper_bgcolor = "#1e2130", plot_bgcolor = "#1e2130",
             font = list(color = "#c8cfe8"),
             xaxis = list(tickangle = -30),
             margin = list(l = 80, b = 80))
  })
  
  # ── ANÁLISIS POR PERFIL ────────────────────────────────────────────────────
  
  output$plot_social_risk <- renderPlotly({
    p <- ggplot(datos(), aes(x = social_interaction_level,
                             y = mental_health_risk_score,
                             fill = social_interaction_level)) +
      geom_violin(alpha = 0.7, color = NA) +
      geom_boxplot(width = 0.1, fill = "#1e2130", color = "#c8cfe8") +
      scale_fill_viridis_d(option = "D") +
      labs(x = "Interacción social", y = "Riesgo mental") +
      tema_dashboard() + theme(legend.position = "none")
    ggplotly(p) %>%
      layout(paper_bgcolor = "#1e2130", plot_bgcolor = "#1e2130",
             font = list(color = "#c8cfe8"))
  })
  
  output$plot_addiction_age <- renderPlotly({
    d <- datos() %>%
      mutate(grupo_edad = cut(age, breaks = c(12,14,16,19),
                              labels = c("13-14","15-16","17-19"))) %>%
      group_by(grupo_edad, digital_wellbeing_flag) %>%
      summarise(addict_prom = mean(addiction_level), .groups = "drop")
    p <- ggplot(d, aes(x = grupo_edad, y = addict_prom,
                       fill = digital_wellbeing_flag)) +
      geom_col(position = "dodge", alpha = 0.9) +
      scale_fill_manual(values = PAL_WELLBEING, name = "Bienestar") +
      labs(x = "Grupo etario", y = "Adicción promedio") +
      tema_dashboard()
    ggplotly(p) %>%
      layout(paper_bgcolor = "#1e2130", plot_bgcolor = "#1e2130",
             font = list(color = "#c8cfe8"))
  })
  
  output$plot_radar <- renderPlotly({
    d <- datos() %>%
      group_by(digital_wellbeing_flag) %>%
      summarise(
        Estrés      = mean(stress_level) / 10,
        Ansiedad    = mean(anxiety_level) / 10,
        Adicción    = mean(addiction_level) / 10,
        `Act.Física`= mean(physical_activity) / max(physical_activity),
        Rendimiento = mean(academic_performance) / 5,
        Sueño       = mean(sleep_hours) / 10,
        .groups = "drop"
      )
    cats <- c("Estrés","Ansiedad","Adicción","Act.Física","Rendimiento","Sueño")
    cols <- unname(PAL_WELLBEING[levels(df$digital_wellbeing_flag)])
    plt <- plot_ly(type = "scatterpolar", fill = "toself")
    for (i in seq_len(nrow(d))) {
      vals <- as.numeric(d[i, cats])
      plt <- plt %>% add_trace(
        r     = c(vals, vals[1]),
        theta = c(cats, cats[1]),
        name  = as.character(d$digital_wellbeing_flag[i]),
        line  = list(color = cols[i]),
        fillcolor = paste0(gsub("#","",cols[i]), "33") |>
          (\(x) paste0("#", substr(x,1,6), "33"))()
      )
    }
    plt %>% layout(
      paper_bgcolor = "#1e2130",
      polar = list(
        bgcolor = "#1e2130",
        radialaxis = list(visible = TRUE, range = c(0,1),
                          color = "#9aa3c2", gridcolor = "#2d3250"),
        angularaxis = list(color = "#c8cfe8", gridcolor = "#2d3250")
      ),
      font   = list(color = "#c8cfe8"),
      legend = list(font = list(color = "#c8cfe8"))
    )
  })
  
  # ── TABLA ──────────────────────────────────────────────────────────────────
  
  output$tabla_datos <- renderDT({
    d <- datos() %>%
      select(age, gender, platform_usage, daily_social_media_hours,
             sleep_hours, stress_level, anxiety_level, addiction_level,
             academic_performance, mental_health_risk_score,
             sleep_quality, digital_wellbeing_flag)
    datatable(d,
              options = list(
                pageLength = 15,
                scrollX = TRUE,
                dom = "Bfrtip",
                buttons = c("csv","excel"),
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().node()).css('background-color', '#1e2130');",
                  "}"
                )
              ),
              class = "display compact",
              rownames = FALSE
    ) %>%
      formatStyle(columns = colnames(d),
                  backgroundColor = "#1e2130",
                  color = "#c8cfe8") %>%
      formatStyle("digital_wellbeing_flag",
                  backgroundColor = styleEqual(
                    c("En riesgo","Moderado","Saludable"),
                    c("#3a1a1a","#3a2a10","#1a3a1a")
                  ),
                  color = styleEqual(
                    c("En riesgo","Moderado","Saludable"),
                    c("#e05c5c","#f5a623","#50c878")
                  ),
                  fontWeight = "bold")
  })
  
}

# -----------------------------------------------------------------------------
# 4. LANZAR LA APP
# -----------------------------------------------------------------------------

shinyApp(ui = ui, server = server)
