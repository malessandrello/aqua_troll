
library(shiny)
library(bslib)
library(tidyverse)
library(shinyWidgets)
library(DT)
library(caret)
library(smotefamily)
library(insight)
library(shinycssloaders)
library(ggplot2)
library(dplyr)
library(readxl)
library(tidyverse)
library(EnvStats)
library(plotly)
library(anytime)
library(insight)
library(ggsci)
library(paletteer)
library(patchwork)
library(tools)
library(rmarkdown)
library(shinydashboard)




ui  <- fluidPage(id = "page",
                 title = "Análsis de datos PdA",
                        fillable = FALSE,
                        tags$head(
                          tags$style(HTML("
                          
     body{
      background-color: #2EA1AC;
    }
      .card{
      font-family: Raleway,  Arial;
      font-weight: bold;
      }"

                          )
                          )
                        ),
titlePanel(HTML(paste0("<b style = 'font-family: Raleway, Sans-serif';>","Parámetros sonda Aqua Troll","</b>"))),

  hr(),

  fileInput(inputId = "subir", NULL, buttonLabel = "Subir archivo", accept = c(".csv")),

  hr(),

  downloadBttn("reporte", "Generar reporte"),

hr(),


)

      
  




server <- function(input, output, session) {
  
  datosAL <- reactive({
    req(input$subir)
    
    
        read_csv(input$subir$datapath, skip = 32) %>%
 
     
        
         separate(col = everything(), sep = ",", into = 
                                          c("Fecha", "Temperatura", "Bateria",
                                            "Presión", "Conductividad_real", "Conductividad_especifica",
                                            "salinidad", "resistividad", "densidad", "solidos",
                                            "turbidez", "oxigeno", "saturacion", "presion_ox",
                                            "fluorescencia", "temperatura", "voltaje", "presion",
                                            "profundidad")) %>%
        
        mutate(Fecha = as_datetime(Fecha),
                                      Temperatura = as.numeric(str_remove_all(Temperatura, "\"")),
                                      Bateria = as.numeric(str_remove_all(Bateria, "\"")),
                                      Presión = as.numeric(str_remove_all(Presión, "\"")),
                                      Conductividad_real = as.numeric(str_remove_all(Conductividad_real, "\"")),
                                      Conductividad_especifica = as.numeric(str_remove_all(Conductividad_especifica, "\"")),
                                      salinidad =as.numeric(str_remove_all(salinidad, "\"")),
                                      resistividad = as.numeric(str_remove_all(resistividad, "\"")),
                                      densidad = as.numeric(str_remove_all(densidad, "\"")),
                                      solidos = as.numeric(str_remove_all(solidos, "\"")),
                                      turbidez = as.numeric(str_remove_all(turbidez, "\"")),
                                      oxigeno = as.numeric(str_remove_all(oxigeno, "\"")),
                                      saturacion = as.numeric(str_remove_all(saturacion, "\"")),
                                      presion_ox = as.numeric(str_remove_all(presion_ox, "\"")),
                                      fluorescencia = as.numeric(str_remove_all(fluorescencia, "\"")),
                                      temperatura = as.numeric(str_remove_all(temperatura, "\"")),
                                      voltaje = as.numeric(str_remove_all(voltaje, "\"")),
                                      presion = as.numeric(str_remove_all(presion, "\"")),
                                      profundidad = as.numeric(str_remove_all(profundidad, "\""))) %>%
        
        
        
          mutate(hora = factor((hour(Fecha))),
                                      Día = factor((day(Fecha)))) %>% drop_na() %>%
          filter(profundidad>0) %>%
          rename("Temperatura (°C)" = Temperatura, "Conductividad específica (µS/cm)" = Conductividad_especifica,
                 "Turbidez (NTU)" = turbidez, "Fluorescencia de clorofila-a (RFU)" = fluorescencia,
                 "Oxígeno Disuelto (mg/l)" = oxigeno)
        

     

  })
  

    
    
    
    maxi <- reactive({
       datosAL() %>% select(`Clorofila a (μg/l)`) %>% max() %>% round(2)
    })
    
    
    
    output$reporte <- downloadHandler( filename = "reporte.html",
                                      content = function (file){
                                        tempReport <- file.path(tempdir(), "reporte.Rmd")
                                        file.copy("reporte.Rmd", tempReport, overwrite = TRUE)  
                                      
    params <- list(data = datosAL())
    
    
     rmarkdown::render(tempReport, output_file = file,
           params = params,
           envir = new.env(parent = globalenv()))
                                     })
}

shinyApp(ui = ui, server = server)
