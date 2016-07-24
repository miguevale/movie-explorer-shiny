library(stringr)
library(dplyr)
library(data.table)
library(countrycode)
library(ggplot2)
library(plotly)

#Read Data
mov <- read.csv('data_movies.csv',stringsAsFactors = F)[,1:15]

#Auxiliar Variables
mov$Runtime <- as.numeric(mov$Runtime)
mov$Exito <- as.factor(ifelse(mov$imdbRating>=7,'Exito','No Exito'))
##Genre
ppalgenre <-  str_split(mov$Genre,',')
mov$Genre_PPAL <- sapply(ppalgenre, "[", 1)
rm(ppalgenre)
##Country
ppalCountry <-  str_split(mov$Country,',')
mov$Country_PPAL <- sapply(ppalCountry, "[", 1)
rm(ppalCountry)
##Language
ppalLang <-  str_split(mov$Language,',')
mov$Lang_PPAL <- sapply(ppalLang, "[", 1)
rm(ppalLang)
##Principal Actor
ppalAct <-  str_split(mov$Actors,',')
mov$Act_PPAL <- sapply(ppalAct, "[", 1)
rm(ppalAct)
#Principal Director
ppalDir <-  str_split(mov$Director,',')
mov$Dir_PPAL <- sapply(ppalDir, "[", 1)
rm(ppalDir)
#Unified Data Frame
mov <- as_data_frame(mov)

#Unique Variables
ungenre <-  unique(str_trim(unlist((str_split(mov$Genre,',')))))
uncountry <-  unique(str_trim(unlist((str_split(mov$Country,',')))))
unlanguage <-  unique(str_trim(unlist((str_split(mov$Language,',')))))

#Plots
##Histogram
plot21 <- plot_ly(data= mov,x= imdbRating, type='histogram', marker=list(color='#E63946',opacity=0.7)) %>% 
          layout(title='Histograma del Rating',
                 margin=list(t=75),font=list(family='Roboto', color='#535353'),xaxis=list(tickvals=seq(1,10,0.5)))

##Barplot Genre
auxGenre <- mov %>%  group_by(Genre_PPAL,Exito) %>% 
  summarise(count=n()) %>% mutate(prop=100*count/sum(count)) %>% 
  ungroup() %>% arrange(desc(count)) %>% filter(Exito=='Exito')
genres <- auxGenre$Genre_PPAL[1:15]
auxGenre <- mov %>% filter(Genre_PPAL!='N/A') %>%  group_by(Genre_PPAL) %>% summarise(count=n()) %>% ungroup() %>% arrange(desc(count))
mov <- mov %>% mutate(Genero=Genre_PPAL)
mov$Genero <- ordered(mov$Genero,levels=auxGenre$Genre_PPAL)
plot22 <- ggplotly(mov %>% filter(Genre_PPAL %in% genres) %>% ggplot()+
                  geom_bar(aes(x=Genero,fill=Exito))+
                  theme_minimal(base_size = 10)+
                  scale_fill_manual(values =c('#E63946','#1D3557'))+
                  labs(x='',y='',title='Películas por Genero')+
                  theme(axis.title=element_text(colour='#535353'),axis.text=element_text(colour='#535353'),
                        legend.title=element_text(colour='#535353'),legend.text=element_text(colour='#535353'),
                        plot.title=element_text(colour='#535353'))) %>% 
          layout(font=list(family='Roboto', color='#535353'))
rm(auxGenre,genres)
mov$Genero <- NULL
##Plotly
#plot22 <- plot_ly(data= mov %>%  group_by(Genre_PPAL,Exito) %>% summarise(count=n()) %>% ungroup() %>% arrange(desc(count)),
#                  x=Genre_PPAL,
#                  y=count,
#                  group=Exito,
#                  type='bar') %>% layout(barmode='stack',xaxis=list(title='Genero'),yaxis=list(title='Cuenta'),margin=list(b=100))

#Barplot Country
auxCount <- mov %>%  group_by(Country_PPAL,Exito) %>% 
  summarise(count=n()) %>% mutate(prop=100*count/sum(count)) %>% 
  ungroup() %>% arrange(desc(prop)) %>% filter(prop<90&prop>56)
countries <- auxCount$Country_PPAL[1:16][-6] #name too long
auxCount <- mov %>%  group_by(Country_PPAL,Exito) %>% summarise(count=n()) %>% 
  mutate(prop=sum(count)/count) %>% ungroup() %>% filter(Exito=='Exito') %>% arrange(desc(prop))
mov <- mov %>% mutate(Pais=Country_PPAL)
mov$Pais <- ordered(mov$Pais,levels=auxCount$Country_PPAL)
plot23 <- ggplotly(mov %>% filter(Country_PPAL %in% countries) %>% ggplot()+
                     geom_bar(aes(x=Pais,fill=Exito),position='fill')+
                     theme_minimal(base_size = 10)+
                     scale_fill_manual(values =c('#E63946','#1D3557'))+
                     labs(x='',y='',title='Películas por País')+
                     theme(axis.title=element_text(colour='#535353'),axis.text=element_text(colour='#535353'),
                           legend.title=element_text(colour='#535353'),legend.text=element_text(colour='#535353'),
                           plot.title=element_text(colour='#535353')))%>% 
          layout(font=list(family='Roboto', color='#535353'))
rm(auxCount,countries)
mov$Pais <- NULL

##Plotly
#auxCount <- mov %>%  group_by(Country_PPAL,Exito) %>% 
#  summarise(count=n()) %>% mutate(prop=100*count/sum(count)) %>% 
#  ungroup() %>% arrange(desc(prop)) %>% filter(prop<90&prop>56)
#countries <- auxCount$Country_PPAL[1:47]
#plot23 <- layout(plot_ly(data= data.frame(mov) %>%  group_by(Country_PPAL,Exito) %>% 
#                           summarise(count=n()) %>% mutate(prop=100*count/sum(count)) %>% ungroup() %>% 
#                           arrange(desc(prop)) %>% filter(Country_PPAL %in% countries),
#                         x=Country_PPAL,
#                         y=prop,
#                         group=Exito,
#                         marker=list(color=c('black','blue','black','blue','black','blue','black','blue')),
#                         type='bar'),barmode='stack',xaxis=list(title='País'),yaxis=list(title='Cuenta'),
#                         margin=list(b=100))

#Barplot Language
auxLang <- mov %>%  group_by(Lang_PPAL,Exito) %>% 
  summarise(count=n()) %>% mutate(prop=100*count/sum(count)) %>% 
  ungroup() %>% arrange(desc(count)) %>% filter(Exito=='Exito',count>30)
languages <- auxLang$Lang_PPAL[1:15]
auxLang <- mov %>%  group_by(Lang_PPAL) %>% summarise(count=n()) %>% ungroup() %>% arrange(desc(count))
mov <- mov %>% mutate(Lenguaje=Lang_PPAL)
mov$Lenguaje <- ordered(mov$Lenguaje,levels=auxLang$Lang_PPAL)
plot24 <- ggplotly(mov %>%  filter(Lang_PPAL %in% languages) %>% ggplot()+
                     geom_bar(aes(x=Lenguaje,fill=Exito),position='fill')+
                     theme_minimal(base_size = 10)+
                     scale_fill_manual(values =c('#E63946','#1D3557'))+
                     labs(x='',y='',title='Películas por Lenguaje')+
                     theme(axis.title=element_text(colour='#535353'),axis.text=element_text(colour='#535353'),
                          legend.title=element_text(colour='#535353'),legend.text=element_text(colour='#535353'),
                          plot.title=element_text(colour='#535353')))%>% 
          layout(font=list(family='Roboto', color='#535353'))
rm(auxLang,languages)
mov$Lenguaje <- NULL

##Plotly
#plot24 <- layout(plot_ly(data= mov %>%  group_by(Lang_PPAL,Exito) %>% summarise(count=n()) %>% filter(count>100)  %>% ungroup() %>% arrange(desc(count)),
#                         x=Lang_PPAL,
#                         y=count,
#                         group=Exito,
#                         type='bar'),barmode='stack',xaxis=list(title='Language'),yaxis=list(title='Cuenta'),
#                         margin=list(b=100))

#Mapas
mov$CC <- countrycode(mov$Country_PPAL,'country.name','iso3c') #Country Code Variable
##Quantity of movies/country
plot31 <- plot_ly(data= mov %>%  group_by(Country_PPAL,CC) %>% summarise(count=n()) %>% ungroup() %>%
                    arrange(desc(count)),
                  z=count,text=Country_PPAL,locations=CC,type='choropleth',
                  color=sqrt(count),colors=c('#1D3557','#457B9D','#E63946'),
                  marker = list(line = list(color = toRGB("grey"), width = 0.5)),
                  colorbar = list(title = '')) %>%
          layout(title = 'Películas por País',
                 margin = list(t=75),
                 geo = list(
                   showframe = F,
                   showcoastlines = T,
                   projection = list(type = 'Mercator'),
                   coastlinecolor= toRGB('grey')),font=list(family='Roboto', color='#535353'))
#Quantity of Success/country
plot32 <- plot_ly(data= mov %>% filter(Exito=='Exito') %>%  group_by(Country_PPAL,CC) %>% summarise(count=n()) %>% 
                    ungroup() %>% arrange(desc(count)),
                  z = count, text = Country_PPAL, locations = CC, type = 'choropleth',
                  color = sqrt(count),colors = c('#1D3557','#457B9D','#E63946'), 
                  marker = list(line = list(color = toRGB("grey"), width = 0.5)),
                  colorbar = list(title = '')) %>%
          layout(title = 'Éxitos por País',
                 margin = list(t=75),
                 geo = list(
                    showframe = F,
                    showcoastlines = T,
                    projection = list(type = 'Mercator'),
                    coastlinecolor= toRGB('grey')),font=list(family='Roboto', color='#535353'))
#Prop of Success/Country
plot33 <- plot_ly(data= mov %>%  group_by(Country_PPAL,CC,Exito) %>% summarise(count=n()) %>% 
                    mutate(prop=(count/sum(count))*100) %>% ungroup() %>% arrange(desc(count)) %>% 
                    filter(Exito=='Exito'),
                  z = prop, text = Country_PPAL, locations = CC, type = 'choropleth',
                  color = prop, colors = c('#1D3557','#457B9D','#E63946'), 
                  marker = list(line = list(color = toRGB("grey"), width = 0.5)),
                  colorbar = list(ticksuffix='%',title = '')) %>%
          layout(title = 'Proporción de Éxitos',
                 margin = list(t=75),
                 geo = list(
                   showframe = F,
                   showcoastlines = T,
                   projection = list(type = 'Mercator'),
                   coastlinecolor= toRGB('grey')),font=list(family='Roboto', color='#535353'))
#Bubble Plot
plot34 <- plot_ly(data= mov %>%  group_by(Country_PPAL,CC,Exito) %>% summarise(count=n()) %>% 
                    mutate(prop=(count/sum(count))*100) %>% ungroup() %>% arrange(desc(count)),
                  locationmode = 'country names',type = 'scattergeo', mode = 'markers', locations = Country_PPAL,
                  text = paste0('<b>',count, ' Películas</b> <br>',
                               format(prop,digits=2),'%'),
                  color = Exito,marker = list(size = log(count)*sqrt(prop)), inherit = T,
                  colors = c('#E63946','#1D3557')) %>% 
          layout(title = 'Películas por País (Resumen)',
                 margin = list(t=75),
                 geo = list(
                   showframe = F,
                   showcoastlines = T,
                   projection = list(type = 'Mercator'),
                   coastlinecolor= toRGB('grey')),font=list(family='Roboto', color='#535353'))
