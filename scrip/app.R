library(shiny)
library(plotly)
library(tidyverse)
library(shinythemes)
library(scales)
library(DT)
library(openxlsx)
library(shinymanager)


# =========================
# 2. UI protegida
# =========================
ui <- 
  fluidPage(
    theme = shinytheme("darkly"),
    titlePanel(
      div(
        img(src = "https://universidadaustral.hiringroom.com/data/accounts/universidadaustral/microsite/ccaa99c6d26434124e55a55b75bb20ee.png",
            height = "60px", style = "margin-right:15px;"),
        "Ingresos Austral S.A."
      )
    ),
    
    # KPIs
    fluidRow(
      column(3, wellPanel(h4("Total"), textOutput("kpi_total"))),
      column(3, wellPanel(h4("Promedio"), textOutput("kpi_promedio"))),
      column(3, wellPanel(h4("Máximo"), textOutput("kpi_maximo"))),
      column(3, wellPanel(h4("Cantidad"), textOutput("kpi_cantidad")))
    ),
    
    sidebarLayout(
      sidebarPanel(
        selectInput("mes", "Selecciona el Mes:", choices = sort(unique(unido$Mes)), selected = sort(unique(unido$Mes))[3]),
        selectInput("unidad", "Selecciona la Unidad de Negocio:", choices = sort(unique(unido$Unidad_de_negocio)), selected = sort(unique(unido$Unidad_de_negocio))[1]),
        selectizeInput("agente", "Selecciona Agente(s):", choices = sort(unique(unido$Agente)), multiple = TRUE),
        sliderInput("monto", "Filtrar por Monto:",
                    min = 0, max = 20000000,
                    value = c(0, 20000000), step = 100000),
        actionButton("reset", "Borrar filtros"),
        br(), br(),
        h4("Medidas Estadísticas"),
        uiOutput("estadisticas_ui"),
        br(),
        downloadButton("descargar_excel", "Exportar Excel")
      ),
      mainPanel(
        plotlyOutput("grafico"),
        br(),
        h4("Tabla de datos filtrados con Totales"),
        DTOutput("tabla")
      )
    )
  )

# =========================
# 3. Server
# =========================
server <- function(input, output, session) {

  # Resetear filtros
  observeEvent(input$reset, {
    updateSelectInput(session, "mes", selected = sort(unique(unido$Mes))[3])
    updateSelectInput(session, "unidad", selected = sort(unique(unido$Unidad_de_negocio))[1])
    updateSelectizeInput(session, "agente", selected = character(0))
    updateSliderInput(session, "monto", value = c(0, 20000000))
  })
  
  # Datos filtrados
  datos_filtrados <- reactive({
    datos <- unido %>%
      filter(Mes == input$mes,
             Unidad_de_negocio == input$unidad,
             Monto >= input$monto[1], Monto <= input$monto[2])
    if (length(input$agente) > 0) {
      datos <- datos %>% filter(Agente %in% input$agente)
    }
    datos
  })
  
  # KPIs
  output$kpi_total <- renderText({
    datos <- datos_filtrados()
    format(sum(datos$Monto), big.mark = ".", decimal.mark = ",")
  })
  output$kpi_promedio <- renderText({
    datos <- datos_filtrados()
    format(round(mean(datos$Monto), 2), big.mark = ".", decimal.mark = ",")
  })
  output$kpi_maximo <- renderText({
    datos <- datos_filtrados()
    format(max(datos$Monto), big.mark = ".", decimal.mark = ",")
  })
  output$kpi_cantidad <- renderText({
    datos <- datos_filtrados()
    format(nrow(datos), big.mark = ".", decimal.mark = ",")
  })
  
  # Gráfico sin notación científica
  output$grafico <- renderPlotly({
    datos <- datos_filtrados()
    validate(need(nrow(datos) > 0, "No hay datos para los filtros seleccionados"))
    
    g <- ggplot(datos, aes(x = reorder(Agente, Monto), y = Monto,
                           text = paste("Agente:", Agente,
                                        "<br>Monto:", format(Monto, big.mark = ".", decimal.mark = ",")))) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(title = paste("Monto por Agente -", input$mes, "-", input$unidad),
           x = "Agente", y = "Monto") +
      scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",")) +
      theme_minimal()
    
    ggplotly(g, tooltip = "text") %>%
      layout(yaxis = list(title = "Monto", tickformat = ",.0f"))  # sin notación científica
  })
  
  # Estadísticas
  output$estadisticas_ui <- renderUI({
    datos <- datos_filtrados()
    if (nrow(datos) > 0) {
      total <- sum(datos$Monto)
      promedio <- mean(datos$Monto)
      mediana <- median(datos$Monto)
      maximo <- max(datos$Monto)
      minimo <- min(datos$Monto)
      desv <- sd(datos$Monto)
      cantidad <- nrow(datos)
      
      tagList(
        p(strong("Total:"), format(total, big.mark = ".", decimal.mark = ",")),
        p(strong("Promedio:"), format(round(promedio, 2), big.mark = ".", decimal.mark = ",")),
        p(strong("Mediana:"), format(mediana, big.mark = ".", decimal.mark = ",")),
        p(strong("Máximo:"), format(maximo, big.mark = ".", decimal.mark = ",")),
        p(strong("Mínimo:"), format(minimo, big.mark = ".", decimal.mark = ",")),
        p(strong("Desviación estándar:"), format(round(desv, 2), big.mark = ".", decimal.mark = ",")),
        p(strong("Cantidad de registros:"), format(cantidad, big.mark = ".", decimal.mark = ","))
      )
    } else {
      p("No hay datos para calcular estadísticas.")
    }
  })
  
  # Tabla con totales
  output$tabla <- renderDT({
    datos <- datos_filtrados()
    if (nrow(datos) > 0) {
      total <- sum(datos$Monto)
      resumen <- tibble(Agente = "TOTAL", Monto = total)
      datos_final <- bind_rows(datos, resumen) %>%
        mutate(Monto = format(Monto, big.mark = ".", decimal.mark = ","))
      
      datatable(datos_final,
                options = list(pageLength = 10),
                class = 'cell-border stripe hover order-column',
                style = 'bootstrap')
    } else {
      datatable(tibble(Mensaje = "No hay datos para los filtros seleccionados"),
                class = 'cell-border stripe hover',
                style = 'bootstrap')
    }
  })
  
  # Exportar Excel con dos hojas
  output$descargar_excel <- downloadHandler(
    filename = function() {
      paste("datos_filtrados_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      datos <- datos_filtrados()
      wb <- createWorkbook()
      addWorksheet(wb, "Datos Filtrados")
      writeData(wb, "Datos Filtrados", datos)
      
      # Hoja de estadísticas
      addWorksheet(wb, "Estadísticas")
      stats <- tibble(
        Total = sum(datos$Monto),
        Promedio = mean(datos$Monto),
        Mediana = median(datos$Monto),
        Máximo = max(datos$Monto),
        Mínimo = min(datos$Monto),
        Desviación = sd(datos$Monto),
        Cantidad = nrow(datos)
      )
      writeData(wb, "Estadísticas", stats)
      
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
}

# =========================
# 4. Lanzar la app
# =========================
shinyApp(ui = ui, server = server)