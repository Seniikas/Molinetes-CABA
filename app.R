library(shiny)
library(shinythemes)
library(shinydashboard)
library(ggplot2)
library(readr)
library(shinyBS)
library(dplyr)
library(plotly)
library(DT)


#base de datos del 2018
dfMol2 <- read_csv("molinetesCompletoLimpioR2018.csv", 
                   col_types = cols(desde = col_time(format = "%H:%M:%S"), 
                                    fecha = col_date(format = "%Y-%m-%d")))

#base de datos del 2019
dfMol4 <- read_csv("molinetes19Preds.csv", 
                   col_types = cols(desde = col_time(format = "%H:%M:%S"), 
                                    fecha = col_date(format = "%Y-%m-%d")))
#correccion sobre la variable del archivo
dfMol4$tipoDia = ifelse(dfMol4$feriado == 1, "Feriado", ifelse(dfMol4$finde == 1, 'Finde','Día Hábil') )

#funcion para darle un valor hasta 50 al tamaño del scatter
rescaler <- function(x) (x-min(x))/(max(x) - min(x)) * 50

#rendondeo de la variable diferencia para que no quede una cifra con 8 decimales como el original
dfMol4$diferencia = round(abs(dfMol4$total- dfMol4$predicho))


#archivo con las estaciones, lineas y sus coordenadas correspondientes
coordenadas <- read_delim("coordenadas_6.csv", 
                          ";", escape_double = FALSE, 
                          trim_ws = TRUE)

#archivo con los scores y rmse anuales por estacion
scores = read_csv("scores19.csv", col_types = cols(X1 = col_skip()))

scores$score = round(scores$score,2)

#le agrego la linea que le corresponde a cada estacion para poder graficarlo
scores = scores %>%  inner_join(coordenadas)

#elimino la variable linea para que no quede duplicada al joinearla con la base 2018
coordenadas$linea = NULL

#agrego las latitudes y longitudes correspondientes a cada estacion en la base del 2018
dfMol2 = dfMol2 %>% inner_join(coordenadas)

##defino un entorno para poder utilizarlo en la parte dinamica de seleccion de año
anio = new.env()

assign('2018',dfMol2,envir = anio)
assign('2019',dfMol4,envir = anio)

ui = navbarPage(
  
  #titulo del dashboard
  span(icon = 'subway',"Subtes CABA"), theme = shinytheme("flatly"),
  
  
  #### GRAFICO DE TOTALES #####
  
  
  tabPanel('Movimiento de gente', value= 'graficoEstacion',
           sidebarLayout(
             sidebarPanel(
               #input para elegir el año a graficar
               selectInput('Anio',
                           label='Año',
                           selected = '2018',
                           choices = ls(anio)),
               
               #input para elegir la linea a graficar
               selectInput('Linea',
                           label='Seleccionar Linea',
                           selected = 'LineaA',
                           choices  = unique(dfMol2$linea)),
               
               #input para elegir la estacion a graficar. Es render porque depende de la eleccion de la linea
               uiOutput("secondSelection"),
               
               #input para elegir el rango de fecha a graficar. Es render porque depende de la eleccion del año
               uiOutput("dateSelection"),
               
               #checkbox para elegir si separa los graficos por dia o todo junto
               checkboxInput('facetsSem',
                             'Separar por dias',
                             value=TRUE),
               
               #checkbox para elegir si solo grafica dias hábiles
               checkboxInput('DiasHab',
                             'Solo dias Hábiles',
                             value=FALSE),
               
               #boton para graficar
               bsButton("Grafico", 
                        label = "Graficar", 
                        icon = icon("subway"), 
                        style = "success")
             ),
             mainPanel(
               plotOutput('scatters')
               
             )
           )
  ),
  
  
  
  #### GRAFICO EN EL MAPA #####  
  
  
  tabPanel('Mapa',
           sidebarLayout(
             
             
             sidebarPanel(
               
               #input para elegir la linea a graficar
               selectInput('Linea2',
                           label='Seleccionar Linea',
                           selected = 'LineaA',
                           choices  = unique(dfMol2$linea)
                           
                           
               ),
               
               #input para elegir la fecha a graficar
               dateInput('FechaMapa',
                         'Seleccionar fecha',
                         min=min(dfMol2$fecha),
                         max=max(dfMol2$fecha),
                         value = '2018-01-01'),
               
               #checkbox para elegir si grafica todas las lineas o una sola
               checkboxInput('TodasLineas',
                             'Todas las Lineas de Subte',
                             value=TRUE),
               
               #boton para graficar el mapa
               bsButton("Mapear", 
                        label = "Graficar", 
                        icon = icon("subway"), 
                        style = "success")
               
             ),
             mainPanel(
               
               plotlyOutput('densMapPlot')
             )
           )
  ),
  
  
  
  #### GRAFICO DE PREDICCIONES #####
  tabPanel('Prediccion',
           
           sidebarLayout(
             
             
             sidebarPanel(
               
               #input para elegir la linea a predecir
               selectInput('LineaPred',
                           label='Seleccionar Linea',
                           selected = 'LineaA',
                           choices  = unique(dfMol4$linea)),
               
               #input para elegir el mes de la prediccion
               selectInput('mesPred',
                           label = 'Seleccionar el mes',
                           selected = 'Enero',
                           choices = unique(dfMol4$mes)),
               
               #radiobutts para elegir si predice una linea completa o solo una estacion 
               radioButtons('tipoPred', 'Seleccione qué predecir:',
                            choices = list('Linea Completa'='linea', 'Estación única'='estacion'),
                            selected = 'linea'),
               
               #render input para elegir que estacion graficar. Depende de la linea que se elija arriba
               uiOutput("secondSelection2"),
               
               #input para seleccionar la fecha a predecir
               dateInput('FechaPred',
                         'Seleccionar fecha',
                         min=min(dfMol4$fecha),
                         max=max(dfMol4$fecha),
                         value = '2019-01-01'),
               
               #boton para graficar las predicciones
               bsButton("Predecir", 
                        label = "Graficar", 
                        icon = icon("subway"), 
                        style = "success"),
               
               #boton para graficar el rmse y score anual por estacion 
               bsButton("Rmse", 
                        label = "SC/RMSE", 
                        icon = icon("subway"), 
                        style = "success")),
             
             mainPanel(
               #el scatter pred va a graficar la prediccion o el score, dependendiendo de la eleccion
               plotlyOutput('scatterPred'),
               dataTableOutput("TablaPredicciones"),
               plotlyOutput('scatterRMSE')
               
             )
             
           )
           
  )
  
  
  
)


server = function(input, output) { 
  
  #creo una variable reactiva para que los graficos solo se actualicen al apretar el boton de graficar
  
  v <- reactiveValues(doPlot = FALSE)
  
  
  #se hace lo mismo con todos los botones: cuando se apreta el boton, la variable (booleana) se vuelve verdadera
  #lo que habilita a que siga el resto del codigo como se vera mas adelante
  
  observeEvent(input$Grafico, {
    # 0 will be coerced to FALSE
    # 1+ will be coerced to TRUE
    v$doPlot <-  input$Grafico
  })
  
  observeEvent(input$Predecir, {
    
    v$doPlot <-  input$Predecir
  })
  observeEvent(input$Rmse, {
    
    v$doPlot <-  input$Rmse
  })
  
  observeEvent(input$Mapear, {
    
    v$doPlot <-  input$Mapear
  })
  
  
  #esto es para que cuando se seleccione la pestaña, la variable se vuelva FALSE y no grafique hasta que se aprete el boton
  observeEvent(input$graficoEstacion, {
    v$doPlot <- FALSE
  })
  
  observeEvent(input$Prediccion, {
    v$doPlot <- FALSE
  })
  
  
  observeEvent(input$Mapa, {
    v$doPlot <- FALSE
  })
  
  
  
  #render de la eleccion de estacion para el grafico de totales
  
  output$secondSelection <- renderUI({
    selectInput(
      'Estacion',
      label='Seleccionar la estación',
      choices = (dfMol2 %>% filter(linea == input$Linea) %>% distinct(estacion) )
    )
  })
  
  
  #render para eleccion de rango de fechas para el grafico de totales
  output$dateSelection <- renderUI({
    
    
    
    dateRangeInput('Fecha',
                   'Seleccionar un rango de fechas',
                   min=min(anio[[input$Anio]]$fecha),
                   max=max(anio[[input$Anio]]$fecha),
                   start=min(anio[[input$Anio]]$fecha),
                   end=max(anio[[input$Anio]]$fecha))
    
  })
  
  
  #render para seleccion de estacion en predicciones
  output$secondSelection2 <- renderUI({
    selectInput(
      'EstacionPred',
      label='Seleccionar la estación',
      choices = (dfMol4 %>% filter(linea == input$LineaPred) %>% distinct(estacion) )
    )
  })
  
  
  
  ##### GRAFICOS DE TOTALES #######
  
  
  
  observeEvent(input$Grafico,{         
    
    ds <- reactive({
      
      
      input$Grafico
      
      isolate({
        
        
        if(input$facetsSem){
          
          
          if(!input$DiasHab){
            
            estac = anio[[input$Anio]] %>% filter(estacion == input$Estacion) %>%  filter (fecha >= input$Fecha[1] & fecha <= input$Fecha[2]) %>% group_by(tipoDia,dia,desde) %>% summarise(total = sum(total))
            estac$dia = factor(estac$dia, levels=c( "domingo" ,"lunes","martes" , "miércoles", "jueves" , "viernes" , "sábado"), labels=c("domingo" ,"lunes","martes" , "miércoles", "jueves" , "viernes" , "sábado"))
            estac
            
          }
          
          else {
            
            estac = anio[[input$Anio]] %>% filter(estacion == input$Estacion) %>%  filter (fecha >= input$Fecha[1] & fecha <= input$Fecha[2]) %>%  filter(tipoDia == 'Día Hábil') %>% group_by(tipoDia,dia,desde) %>% summarise(total = sum(total))
            estac$dia = factor(estac$dia, levels=c("lunes","martes" , "miércoles", "jueves" , "viernes"), labels=c("lunes","martes" , "miércoles", "jueves" , "viernes"))
            
            
            estac
            
          }
          
          
          
          
        }
        else {
          
          if(!input$DiasHab){
            
            estac = anio[[input$Anio]] %>% filter( estacion == input$Estacion) %>%  filter (fecha >= input$Fecha[1] & fecha <= input$Fecha[2]) %>% group_by(desde) %>% summarise(total = sum(total))
            
            estac
          }
          
          else{
            estac = anio[[input$Anio]]  %>% filter( estacion == input$Estacion & tipoDia == 'Día Hábil') %>%
              filter (fecha >= input$Fecha[1] & fecha <= input$Fecha[2]) %>% group_by(desde) %>% summarise(total = sum(total))
            
            estac
            
          }
        }
        
      })
    })
    
    
    
    output$scatters <- renderPlot({
      
      input$Grafico
      
      if(!v$doPlot) return()
      isolate({
        
        if(input$facetsSem){
          
          gr = ggplot(ds(),mapping = aes(x = desde))+ geom_point(aes( y = total,color =tipoDia, group= tipoDia),size = 1)+
            scale_fill_manual(values = sort(unique(dfMol2$desde))) + facet_grid(dia~.)+ theme(legend.title = element_blank()) + labs(x = "Hora", y = 'Total')
        }
        
        else {
          
          gr =  ggplot(ds(),mapping = aes(x = desde))+ geom_point(aes( y = total,color = 'red'),size = 3 )+ geom_line(aes( y = total,color = 'red'),size = 1)+
            scale_fill_manual(values = sort(unique(dfMol2$desde))) + theme(legend.position = "none")+ labs(x = "Hora", y = 'Total')
          
        }
        
        gr
        
      })
    })
    
  })
  
  
  
  
  
  
  
  ######### Predicciones #########
  
  
  observeEvent(input$Predecir,{
    
    #en el caso de querer predecir, el rmse no tiene que aparecer, por lo tanto no tiene que renderizar
    output$scatterRMSE = renderPlotly({})
    
    output$scatterPred <- renderPlotly({
      
      input$Predecir
      
      if(!v$doPlot) return()
      isolate({
        
        
        
        if(input$tipoPred == 'linea'){
          
          estacpred = dfMol4 %>%filter(linea == input$LineaPred & mes == input$mesPred) %>% group_by(estacion) %>%
            summarise(total = sum(total), predicho = round(sum(predicho)), diferencia = sum(diferencia))
          
          estacpred$diferencia = rescaler(estacpred$diferencia)
          
          
          
          grpl = plot_ly(data = estacpred, x = ~estacion, y= ~total,type = 'scatter', size = ~diferencia)
          
          
          
        }
        
        if(input$tipoPred == 'estacion'){
          
          estacpred = dfMol4 %>%filter(estacion == input$EstacionPred & fecha == input$FechaPred) %>% group_by(desde) %>%
            summarise(total = sum(total), predicho = round(sum(predicho)), diferencia )
          
          estacpred$diferencia = rescaler(estacpred$diferencia)
          
          
          grpl = plot_ly(data = estacpred, x = ~desde, y= ~total,type = 'scatter', size = ~diferencia)
          
          
        }
        
        grpl
        
        
      })
      
      
    })
    
    output$TablaPredicciones = renderDataTable({
      
      
      input$Predecir
      
      if(!v$doPlot) return()
      
      isolate({
        
        
        
        if(input$tipoPred == 'linea'){
          
          dfMol4 %>%filter(linea == input$LineaPred & mes == input$mesPred) %>% group_by(estacion) %>%
            summarise(total = sum(total), predicho = round(sum(predicho)), diferencia = sum(diferencia))
        }
        
        else{
          
          dfMol4 %>%filter(estacion == input$EstacionPred & fecha == input$FechaPred) %>% group_by(desde) %>%
            summarise(total = sum(total), predicho = round(sum(predicho)), diferencia)
          
        }
        
        
        
      })
      
    })
    
  })
  
  ######## GRAFICAR EL MAPA ######
  observeEvent(input$Mapear,{
    
    
    output$densMapPlot <- renderPlotly({
      
      input$Mapear
      
      if(!v$doPlot) return()
      isolate({
        
        
        if(input$TodasLineas){
          
          dfMol2 %>% filter(fecha == input$FechaMapa) %>% plot_ly(
            type = 'densitymapbox',
            lat= ~latitud,
            lon = ~longitud,
            z = ~total,
            coloraxis = 'coloraxis',
            radius = 10,width = 850, height = 600) %>% layout(
              mapbox = list(zoom = 11,
                            style="stamen-terrain",
                            center= list(lon=-58.45, lat=-34.598)), coloraxis = list(colorscale = "Jet"))
          
        }
        
        
        else{
          
          dfMol2 %>% filter(fecha == input$FechaMapa & linea == input$Linea2) %>% plot_ly(
            type = 'densitymapbox',
            lat= ~latitud,
            lon = ~longitud,
            z = ~total,
            coloraxis = 'coloraxis',
            radius = 10,width = 850, height = 600) %>% layout(
              mapbox = list(zoom = 11,
                            style="stamen-terrain",
                            center= list(lon=-58.45, lat=-34.598)), coloraxis = list(colorscale = "Jet"))
          
          
          
        }
        
        
      })
    })
    
    
    
  })##termina el observeEvente de Mapear
  
  
  
  
  
  
  ####### GRAFICAR RMSE #########
  
  observeEvent(input$Rmse, {
    
    #si quiero mostrar score y rmse, la tabla de predicciones no tiene que aparecer
    output$TablaPredicciones = renderDataTable({})
    
    output$scatterPred <- renderPlotly({
      input$Rmse
      
      if(!v$doPlot) return()
      isolate({
        
        
        scores %>% plot_ly(x = ~estacion, y = ~score, transforms = list(
          list(
            type = 'groupby',
            groups = scores$linea,
            legendgroup = scores$linea, 
            styles = list(
              list(target = 'LineaA', value = list(marker =list(color = 'ligthblue'))),
              list(target = 'LineaB', value = list(marker =list(color = 'red'))),
              list(target = 'LineaC', value = list(marker =list(color = 'blue'))),
              list(target = 'LineaD', value = list(marker =list(color = 'green'))),
              list(target = 'LineaE', value = list(marker =list(color = 'violet'))),
              list(target = 'LineaH', value = list(marker =list(color = 'yellow')))
            )))) %>% layout(xaxis = list(categoryorder = 'array', categoryarray = scores$estacion))
        
        
      })    
    })  
    
    
    output$scatterRMSE <- renderPlotly({
      input$Rmse
      
      if(!v$doPlot) return()
      isolate({
        
        
        scores %>% plot_ly(x = ~estacion, y = ~rmse, transforms = list(
          list(
            type = 'groupby',
            groups = scores$linea,
            legendgroup = scores$linea, 
            styles = list(
              list(target = 'LineaA', value = list(marker =list(color = 'ligthblue'))),
              list(target = 'LineaB', value = list(marker =list(color = 'red'))),
              list(target = 'LineaC', value = list(marker =list(color = 'blue'))),
              list(target = 'LineaD', value = list(marker =list(color = 'green'))),
              list(target = 'LineaE', value = list(marker =list(color = 'violet'))),
              list(target = 'LineaH', value = list(marker =list(color = 'yellow')))
            )))) %>% layout(xaxis = list(categoryorder = 'array', categoryarray = scores$estacion))
        
        
      })    
    }) 
    
    
  })
  
  
  
  
} 



shinyApp(ui=ui, server=server)


