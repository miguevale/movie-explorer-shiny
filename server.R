
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  # Filtros para gráfico principal
  movies <- reactive({
    minyear <- input$year[1]
    maxyear <- input$year[2]
    minrating <- input$rating[1]
    maxrating <- input$rating[2]
    
    # Filtro de Fecha
    m <- mov %>%
      filter(
        Year >= minyear,
        Year <= maxyear,
        imdbRating >= minrating,
        imdbRating <= maxrating
      ) 
    
    # Filtro de Genero
    if (input$genre != "All") {
      a <- m %>% filter(Genre %like% input$genre)
      if(nrow(a)==0){
        m <- mov %>%
          filter(
            Year >= minyear,
            Year <= maxyear,
            imdbRating >= minrating,
            imdbRating <= maxrating,
            Country %like% input$country,
            Language %like% input$language
          ) 
      } else m <- a
    }
    # Filtro de Director
    if (!is.null(input$director) && input$director != "") {
      a <- m %>% filter(Director %like% input$director)
      if(nrow(a)==0){
        m <- mov %>%
          filter(
            Year >= minyear,
            Year <= maxyear,
            imdbRating >= minrating,
            imdbRating <= maxrating,
            Genre %like% input$genre,
            Country %like% input$country,
            Language %like% input$language
          ) 
      } else m <- a
    }
    # Filtro de Actor
    if (!is.null(input$actor) && input$actor != "") {
      a <- m %>% filter(Actors %like% input$actor)
      if(nrow(a)==0){
        m <- mov %>%
          filter(
            Year >= minyear,
            Year <= maxyear,
            imdbRating >= minrating,
            imdbRating <= maxrating,
            Genre %like% input$genre,
            Country %like% input$country,
            Language %like% input$language
          ) 
      } else m <- a
    }
    # Filtro de País
    if (input$country != "All") {
      a <- m %>% filter(Country %like% input$country)
      if(nrow(a)==0){
        m <- mov %>%
          filter(
            Year >= minyear,
            Year <= maxyear,
            imdbRating >= minrating,
            imdbRating <= maxrating,
            Genre %like% input$genre,
            Language %like% input$language
          ) 
      } else m <- a
    }
    # Filtro de Lenguaje
    if (input$language != 'All') {
      a <- m %>% filter(Language %like% input$language)
      if(nrow(a)==0){
        m <- mov %>%
          filter(
            Year >= minyear,
            Year <= maxyear,
            imdbRating >= minrating,
            imdbRating <= maxrating,
            Genre %like% input$genre,
            Country %like% input$country
          ) 
      } else m <- a
    }
   
    if(nrow(m)==0){
      m <- data_frame(Year=2005,imdbRating=5,Title=' </i>🚨 Error 404. <i>Filter not Found</i>😅 <i>',
                      Dir_PPAL='La busqueda que hiciste no arroja resultados 😔',
                      Act_PPAL='Por favor intenta usando otros parámetros',
                      Genre_PPAL='<b>!Gracias por pasar! 😇</b>',Country='Laura Romero 💁Miguel Valencia 🇨🇴',Lang_PPAL='Fede Navarro 🇸🇻, Sergio Ayala 👽',
                      Exito='',Runtime=100,Metascore='🤖🤖🤖🤖🤖🤖🤖🤖🤖🤖🤖🤖🤖🤖🤖🤖🤖🤖',Released='🌚')
    }
    m
  })
  
  plot1 <- reactive({
    minyear <- input$year[1]
    maxyear <- input$year[2]
    movies() %>% ggplot(aes(x=Year,y=imdbRating))+
      geom_jitter(aes(color=Exito,text=paste('<b>Título: <i>',Title,'</b></i>','<br>',
                                             'Director: <b>',Dir_PPAL,'</b>','<br>',
                                             'Protagonista: <b>', Act_PPAL,'</b>','<br>',
                                             'Género: ', Genre_PPAL,'<br>',
                                             'País: ', Country,'<br>',
                                             'Idioma: ', Lang_PPAL,'<br>',
                                             'Estreno: ', Released,'<br>',
                                             'Metascore: ', Metascore)))+
      theme_minimal()+
      coord_cartesian(ylim=c(0,10),xlim=c(minyear,maxyear))+
      scale_x_continuous(breaks = seq(minyear,maxyear,1))+
      scale_color_manual(values=c('#E63946','#1D3557'))+
      labs(x='',y='Rating imdb')+
      theme(axis.title=element_text(colour='#535353'),axis.text=element_text(colour='#535353'),
            legend.title=element_text(colour='#535353'),legend.text=element_text(colour='#535353'))
    ggplotly() %>% layout(font=list(family='Roboto'))
  })
  
  plot2 <- reactive({
    
    plot <- if(input$grafico=='Histograma'){
      plot21
    } else if(input$grafico=='Género'){
      plot22
    } else if(input$grafico=='País'){
      plot23
    } else plot24
      
    plot
    })
  
  plot3 <- reactive({
    
    plot <- if(input$mapas=='Cant. de Películas'){
      plot31
    } else if(input$mapas=='Cant. de Exitos'){
      plot32
    } else if(input$mapas=='Prop. de Exitos'){
      plot33
    } else plot34
    plot
  })
  
  plot4 <- reactive({
    plot33
  })
    
  output$plot1 <- renderPlotly({plot1()})
  output$plot2 <- renderPlotly({plot2()})
  output$plot3 <- renderPlotly({plot3()})
  output$plot4 <- renderPlotly({plot4()})
  output$n_movies <- renderUI({HTML(paste0('El número de películas es: <b><i> ',nrow(movies()), '</b></i> ',
                                       '- La calificación media es: <b><i> ', format(mean(movies()$imdbRating),digits=3),'</b></i> ',
                                       '- La duración media es: <b><i> ', format(mean(movies()$Runtime,na.rm = T),digits=3),' min </b></i><br>',
                                       'La proporción de éxito es: <b><i> ', format(as.numeric((movies()%>% 
                                                                                                   group_by(Exito) %>% 
                                                                                                   summarise(Count=n()) %>% 
                                                                                                   mutate(prop=Count/sum(Count)*100))[1,3]),digits=3),'%</b></i> ',
                                       '- El metascore medio es: <b><i> ', format(mean(as.numeric(movies()$Metascore),na.rm = T),digits=3,),'% </b></i><br>'))})
})
