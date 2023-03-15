
library(shiny)
library(plotly)
library(tidyverse)
library(DT)


starwars_data = starwars %>%
  mutate(
    height = case_when(
      name == 'Finn' ~ as.integer(178),
      name == 'Rey' ~ as.integer(170),
      name == 'Poe Dameron' ~ as.integer(172),
      name == 'BB8' ~ as.integer(67),
      name == 'Captain Phasma' ~ as.integer(200),
      TRUE ~ height
    ),
    mass = case_when(
      name == 'Finn' ~ 73,
      name == 'Rey' ~ 54,
      name == 'Poe Dameron' ~ 80,
      name == 'BB8' ~ 18,
      name == 'Captain Phasma' ~ 76,
      TRUE ~ mass
    )
  )

starwars_data = starwars_data %>%
  tibble::rownames_to_column(var = 'id') %>%
  replace_na(list(hair_color="not applicable")) %>%
  replace_na(list(sex ='not applicable')) %>%
  replace_na(list(homeworld = 'unknown')) %>%
  rowwise() %>%
  mutate(L.filmów=length(films),
         L.pojazdów=length(vehicles),
         L.statków=length(starships)
  )


unique_haircolor=unique(unlist(str_split(unlist(starwars_data$hair_color),', ')))
unique_species=unique(unlist(str_split(unlist(starwars_data$species),', ')))

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
                    @import url('https://fonts.googleapis.com/css2?family=Poller+One&display=swap');
                    hr {border-top: 2px solid rgba(67,67,68,0.51);}
                    
                    @keyframes typewriter {
                        from {
                          width: 0;
                        }
                        to {
                          width: 610px;
                        }
                      }
                      @keyframes blinkTextCursor {
                        from {
                          border-right-color: hsl(0, 0%, 80%);
                        }
                        to {
                          border-right-color: transparent;
                        }
                      }
     

                    h2 {
                      font-family: 'Poller One', sans-serif;
                      font-size: 34px;
                      color: rgb(122, 59, 46);
                      animation: typewriter 4s steps(44) 1200ms 1 normal both, blinkTextCursor 500ms infinite;
                          letter-spacing: 4px;
                      overflow: hidden;
                      border-right: 2px solid hsl(0, 0%, 80%);
                      white-space: nowrap;
                    }
                    
                    .shiny-input-container {
                      color: #474747;
                    }
                    
                    .well {
                    background-color: rgba(67,67,68,0.51);
                    border: 1px solid #e3e3e3;
                    box-shadow: 0 0px 18px rgba(0,0,0,0.6);
                    color: rgb(122, 59, 46);
                    }
                    
                    .well:hover {
                      background-color: rgba(67,67,68,0.4);
                    }
                    
                    table.dataTable.display tbody td {
                        border-top: 1px solid rgb(122, 59, 46) !important;
                    }
                    
                    table.dataTable tr.selected td, table.dataTable td.selected {
                        background-color: red !important;
                    }
                    "))
  ),
  titlePanel("Star Wars Dashboard"),
  
  sidebarLayout(

    sidebarPanel(
      selectInput(inputId = "input_haircolor", label = "Hair color:", multiple = TRUE,
                  choices = unique_haircolor, color <- "red"),

      selectInput(inputId = "input_species", label = "Species:", multiple = TRUE,
                  choices = unique_species),
      hr(),
      uiOutput('description')
      ),
    mainPanel(
      plotlyOutput(outputId = "scatterplot"),
      dataTableOutput(outputId = "table")
    )
    
  )
)

srv <- function(input, output, session){
    sd_filtered = reactive({
      if(length(input$input_haircolor) > 0 & length(input$input_species) > 0){
        starwars_data %>%
          filter(str_detect(hair_color, paste("\\b" ,input$input_haircolor, "\\b", collapse = "|", sep=''))) %>%
          filter(str_detect(species, paste("\\b" ,input$input_species, "\\b", collapse = "|", sep='')))
      } else if (length(input$input_haircolor) > 0 & length(input$input_species) == 0) {
        starwars_data %>%
          filter(str_detect(hair_color, paste("\\b" ,input$input_haircolor, "\\b", collapse = "|", sep='')))
      } else if (length(input$input_haircolor) == 0 & length(input$input_species) > 0) {
        starwars_data %>%
          filter(str_detect(species, paste("\\b" ,input$input_species, "\\b", collapse = "|", sep='')))
      } else {
        starwars_data = starwars_data
      }
    })
    

  observe({
    if(!is.null(input$input_species))
      updateSelectInput(session, "input_haircolor",
                        choices=unique(unlist(str_split(filter(starwars_data, species %in% input$input_species)$hair_color,', '))),
                        selected = isolate(input$input_haircolor)
                        )
  })
   observe({  
    if(!is.null(input$input_haircolor))
      updateSelectInput(session, "input_species",
                        choices=unique(unlist(str_split(filter(starwars_data, hair_color %in% input$input_haircolor)$species,', '))),
                        selected = isolate(input$input_species) 
      )
   })
   observe({
    if(is.null(input$input_species))
      updateSelectInput(session, "input_haircolor", 
                        choices = unique(unlist(str_split(starwars_data$hair_color,', '))),
                        selected = isolate(input$input_haircolor)
      )
   })
   observe({  
    if(is.null(input$input_haircolor))
      updateSelectInput(session, "input_species", 
                        choices = unique(unlist(str_split(starwars_data$species,', '))),
                        selected = isolate(input$input_species)
      )
  })
   
   pal <- c("yellow", "blue", "brown", "black", "orange")
     
  output$scatterplot <-renderPlotly({
      plot_ly(data = sd_filtered(), source='wykresik') %>% 
        add_markers(
          x= ~height,
          y = ~homeworld,
          color = ~sex,
          colors = pal,
          key= ~id,
          mode = 'markers',
          size = 3,
          alpha = .9
        ) %>% 
        layout(dragmode='select',
               margin = list(l=80, r=120, b=90, t=0, pad=0),
               yaxis = list(title = '<b>Family Planet</b>'),
               xaxis = list(title = '<b>Height [cm]</b>')
               ) %>%
        event_register('plotly_selecting')
    })  
  
  selected_data = reactive({
    sel_data = NULL
    d = event_data("plotly_selected", source ='wykresik')
    if(!is.null(d)){
      sel_data = sd_filtered() %>% 
        filter(id %in% d$key)
    } else {
      sel_data = sd_filtered()
    }
    sel_data
  })

  output$table <- renderDataTable(
          selected_data()[, c('name', 'height', 'mass', 'hair_color', 'birth_year', 'L.filmów', 'L.pojazdów', 'L.statków')], 
          selection = 'single', 
          colnames=c('Name'='name', 'Height'='height', 'Mass'='mass', 'Hair color'='hair_color', 'Birth year'='birth_year', 'No. movies'='L.filmów', 'No. vehicles'='L.pojazdów', 'No. ships'='L.statków')
          )


 output$description = renderText({
   d = input$table_rows_selected
   
   if(!is.null(d)){
     paste0("Name: ", "<strong>", selected_data()[input$table_rows_selected, "name"],"</strong>", "<br>",
           "Gender: ","<strong>", selected_data()[input$table_rows_selected, "sex"], "</strong>", "<br>",
           "Birth year: ", "<strong>", selected_data()[input$table_rows_selected, "birth_year"], " BBY", "</strong>", "<br>",
           "Planet: ", "<strong>", selected_data()[input$table_rows_selected, "homeworld"],"</strong>", "<br>",
           "Species: ", "<strong>", selected_data()[input$table_rows_selected, "species"],"</strong>", "<br>",
           "Height: ", "<strong>", selected_data()[input$table_rows_selected, "height"]," cm", "</strong>", "<br>",
           "Mass: ", "<strong>", selected_data()[input$table_rows_selected, "mass"]," kg","</strong>", "<br>",
           "Hair color: ", "<strong>", selected_data()[input$table_rows_selected, "hair_color"],"</strong>", "<br>",
           "Skin color: ", "<strong>", selected_data()[input$table_rows_selected, "skin_color"],"</strong>", "<br>",
           "Eye color: ", "<strong>", selected_data()[input$table_rows_selected, "eye_color"],"</strong>", "<br>",
           "<br>",
           "Vehicles:", "<br>",
           "<strong>", paste(paste0("&#8226;","&#160;", "&#160;", "&#160;",unlist(selected_data()[input$table_rows_selected, "vehicles"]), "<br>"), collapse = ""),"</strong>", "<br>",
           "<br>",
           "Ships: ", "<br>",
           "<strong>", paste(paste0("&#8226;","&#160;", "&#160;", "&#160;",unlist(selected_data()[input$table_rows_selected, "starships"]), "<br>"), collapse = ""),"</strong>", "<br>",
           "<br>",
           "Movies: ", "<br>", 
           "<strong>", paste(paste0("&#8226;","&#160;", "&#160;", "&#160;",unlist(selected_data()[input$table_rows_selected, "films"]), "<br>"), collapse = ""),"</strong>"
          )
      }
  })
  
}

shinyApp(ui, srv)

