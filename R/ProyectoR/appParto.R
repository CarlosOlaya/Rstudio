library(shiny)
library(caret)
#install.packages("shiny")
#install.packages("caret")
#install.packages("ggplot2")
#install.packages("lattice")

# Cargar los datos y entrenar el modelo fuera del servidor Shiny para eficiencia
datos_parto <- read.csv("C:/Users/tito_/OneDrive - cecar.edu.co/R/ProyectoR/datos_parto.csv")
datos_parto_modelo <- datos_parto[c("Género", "Peso", "Talla", "Tiempo.de.Gestación", "Tipo.de.Parto",
                                    "Multiplicidad.de.Embarazo", "Grupo.Sanguíneo", "Factor.RH",
                                    "Edad.de.la.Madre", "Número.de.Hijos.Nacidos.Vivos",
                                    "Fecha.Anterior.del.Hijo.Nacido.Vivo", "Número.de.Embarazos",
                                    "Nombre.de.la.Administradora")]
datos_parto_modelo <- datos_parto_modelo[-which(datos_parto_modelo$Tipo.de.Parto == "INSTRUMENTADO"), ]
datos_parto_modelo <- datos_parto_modelo[-which(datos_parto_modelo$Factor.RH == ""), ]
datos_parto_modelo$Género <- as.factor(datos_parto_modelo$Género)
datos_parto_modelo$Tipo.de.Parto <- as.factor(datos_parto_modelo$Tipo.de.Parto)
datos_parto_modelo$Multiplicidad.de.Embarazo <- as.factor(datos_parto_modelo$Multiplicidad.de.Embarazo)
datos_parto_modelo$Grupo.Sanguíneo <- as.factor(datos_parto_modelo$Grupo.Sanguíneo)
datos_parto_modelo$Factor.RH <- as.factor(datos_parto_modelo$Factor.RH)
datos_parto_modelo$Nombre.de.la.Administradora <- as.factor(datos_parto_modelo$Nombre.de.la.Administradora)
datos_parto_modelo$Fecha.Anterior.del.Hijo.Nacido.Vivo <- as.Date(datos_parto_modelo$Fecha.Anterior.del.Hijo.Nacido.Vivo, format = "%d/%m/%Y")
datos_parto_modelo$Fecha.Anterior.del.Hijo.Nacido.Vivo <- as.integer(as.Date('2017-01-01') - datos_parto_modelo$Fecha.Anterior.del.Hijo.Nacido.Vivo)
datos_parto_modelo$Fecha.Anterior.del.Hijo.Nacido.Vivo[is.na(datos_parto_modelo$Fecha.Anterior.del.Hijo.Nacido.Vivo)] <- 0

datos_parto_modelo_final <- datos_parto_modelo[, c("Tipo.de.Parto", "Tiempo.de.Gestación", "Multiplicidad.de.Embarazo", 
                                                   "Edad.de.la.Madre", "Número.de.Hijos.Nacidos.Vivos", 
                                                   "Fecha.Anterior.del.Hijo.Nacido.Vivo")]

modelo_final <- glm(Tipo.de.Parto ~ ., data = datos_parto_modelo_final, family = binomial)

# Definir UI para la aplicación
ui <- fluidPage(
  includeCSS("C:/Users/tito_/OneDrive - cecar.edu.co/R/ProyectoR/stylesParto.css"),  # Vincular el archivo CSS aquí
  
  titlePanel("Predicción de Tipo de Parto"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("TiempoGestacion", "Tiempo de Gestación (semanas):", value = 40, min = 20, max = 45, step = 1),
      selectInput("MultiplicidadEmbarazo", "Multiplicidad de Embarazo:", choices = levels(datos_parto_modelo$Multiplicidad.de.Embarazo)),
      numericInput("EdadMadre", "Edad de la Madre (años):", value = 30, min = 15, max = 50, step = 1),
      numericInput("NumeroHijosVivos", "Número de Hijos Nacidos Vivos:", value = 0, min = 0, max = 20, step = 1),
      numericInput("FechaAnteriorHijoNacidoVivo", "Días desde la Fecha Anterior del Hijo Nacido Vivo:", value = 0, min = 0, step = 1),
      actionButton("predictButton", "Predecir")
    ),
    
    mainPanel(
      verbatimTextOutput("prediction")
    )
  )
)

# Definir la lógica del servidor
server <- function(input, output) {
  observeEvent(input$predictButton, {
    new_data <- data.frame(
      Tiempo.de.Gestación = input$TiempoGestacion,
      Multiplicidad.de.Embarazo = input$MultiplicidadEmbarazo,
      Edad.de.la.Madre = input$EdadMadre,
      Número.de.Hijos.Nacidos.Vivos = input$NumeroHijosVivos,
      Fecha.Anterior.del.Hijo.Nacido.Vivo = input$FechaAnteriorHijoNacidoVivo
    )
    
    pred <- predict(modelo_final, newdata = new_data, type = "response")
    tipo_parto <- ifelse(pred >= 0.5, "Natural", "Cesárea")
    
    output$prediction <- renderText({
      paste("Probabilidad de Parto Natural:", round(pred * 100, 2), "%\nTipo de Parto Predicho:", tipo_parto)
    })
  })
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)
#runApp("appParto.R")
