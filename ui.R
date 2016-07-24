
actionLink <- function(inputId, ...) {
  tags$a(href='javascript:void',
         id=inputId,
         class='action-button',
         ...)
}

shinyUI(navbarPage(theme = "bootstrap.css",
  title="IMDB Movies",
  tabPanel("Filtros",
  fluidRow(
    column(width=3,
           wellPanel(width=550,
             #h5("Filtros"),
             sliderInput("year", "Año de Salida", 2000, 2016, value = c(2002, 2007)),
             sliderInput("rating", "Rating IMDB", 0, 10, value = c(0,10)),
             selectInput("genre", "Género de la película",
                         c(ungenre,'All'),selected = 'All'
             ),
             textInput("director", "Nombre del director",placeholder='ie. Alejandro G. Iñárritu'),
             textInput("actor", "Nombre del actor",placeholder='ie. Leonardo DiCaprio'),
             selectInput("country", "País de la película",
                         c(uncountry,'All'),selected = 'All'
             ),
             selectInput("language", "Lenguaje de la película",
                         c(unlanguage,'All'),selected = 'All'
             ))),
    column(9,
           plotlyOutput("plot1",height=500),
           wellPanel(
             span(strong("Estadísticas del filtro:"),
                  uiOutput("n_movies")
             ))))),
  tabPanel('Gráficos',
    sidebarLayout(
      sidebarPanel(
        h4("Bar Charts"),
        width=3,
      selectInput(inputId='grafico',
                  label='Selecciona el gráfico:',
                  choices = c('Género','País','Lenguaje','Histograma')
      )
      ),
      mainPanel(width=9,
        plotlyOutput('plot2',height = '550px')
      ))),
  tabPanel('Mapas',
    sidebarLayout(
     sidebarPanel(width=3,
        h4("Mapas"),
        selectInput(inputId='mapas',
        label='Selecciona una vista de mapa:',
        choices = c('Cant. de Películas',
                    'Cant. de Exitos',
                    'Prop. de Exitos',
                    'Resumen')
      )),
      mainPanel(width=9,
       plotlyOutput('plot3',height = '550px')
     )))
     
))