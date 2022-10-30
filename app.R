library(dplyr)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(glue)
library(plotly)
library(scales)

imdb <- read_csv("imdb_top_1000.csv")
imdb <- select(imdb, -c("Poster_Link", "Overview"))
imdb <- na.exclude(imdb)
imdb <- imdb |> mutate(
  Certificate = as.factor(Certificate),
  Genre = as.factor(Genre),
  Released_Year = as.numeric(Released_Year)
)

header <- dashboardHeader(title = "TOP IMDB MOVIES")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Movie Group Performance", 
             tabName = "tabtop", 
             icon = icon("chart-simple")),
    menuItem("Per Movie Performance", 
             tabName = "tabmid", 
             icon = icon("cubes-stacked")),
    menuItem("Data Table", 
             tabName = "tabbot", 
             icon = icon("table")),
    menuItem("Source Code", icon = icon("display"), 
             href = "https://github.com/SholahShoya/Capstone2_Algoritma")
  )
)

body <- dashboardBody(
  tabItems(

    #tab1
    tabItem(tabName = "tabtop",
            fluidPage(
              h1(tags$b("IMDB MOVIES PERFORMANCE PER GROUP")),
              br(),
              div(style = "text-align:justify",
                  p("This data shows us the performance
                    of movies on IMDB grouped by
                    Genre & Certificate, sorted by the value of
                    Revenues and Meta Score
                    earned by each group."
                   ),
                  br(),
              h3(tags$b("Certificate Interpretation:"))
              )
            ),
            fluidPage(
              box(width = 4,
                      height = 180,
                  background = "navy",
                      div(style = "text-align:justify",
                          p("A = Allowed for all"),
                          p("UA = Unrestricted public exhibition"),
                          p("U = General viewing for all ages"),
                          p("R = Passed only for persons 18 and over"))),
              box(width = 4,
                  height = 180,
                  background = "navy",
                  div(style = "text-align:justify",
                      p("G = General"),
                      p("PG-13 = Parental Guide for age under 13"),
                      p("PG = Parental Guide for age under 18"),
                      p("Passed = Passed for age over 18 (sexual content)"))),
              box(width = 4,
                  height = 180,
                  background = "navy",
                  div(style = "text-align:justify",
                      p("Approved = Movie is deemed as moral"),
                      p("TV/PG = Parental Guide suggested"),
                      p("U/A = Unrestriced public exhibition, subject to parental guide for age under 12"),
                      p("GP = All ages admitted, parental guidance suggested")))),

            fluidPage(
              box(width = 12,
                  background = "green",
                  selectInput(inputId = "choose_certi",
                              label = "Choose Certificate",
                              choices = unique(imdb$Certificate),
                              selected = "A")),
            fluidPage(
              tabBox(width = 12,
                     title = tags$b("IMDB"),
                     # id = "tabset1",
                     side = "right",
                     tabPanel(tags$b("Based on Revenue"),
                              plotlyOutput("rev_graph")
                     ),
                     tabPanel(tags$b("Based on Meta Score"),
                              plotlyOutput("met_graph")
                     )
                    )
                  )
                )
              ),
    
    # tab2
    tabItem(
      tabName = "tabmid",
      fluidPage(
        box(plotlyOutput("jitter"), 
            height = 445),
        
        box(background = "red",
          br(), 
          sliderInput("sliderIn", 
                      "Select Rating:", 
                      min = min(imdb$IMDB_Rating), 
                      max = max(imdb$IMDB_Rating), 
                      value = c(8.4, 8.5),
                      step = 0.1)
        ),
        div(style = "text-align:center",
            p(tags$b("MOVIE WITH THE HIGHEST REVENUE"))),
        valueBox(paste0("USD$", comma(max(imdb$Gross))), 
                 "Star Wars: Episode VII - The Force Awakens", 
                 icon = icon("money"),
                 color = "blue",
                 width = 6),
        div(style = "text-align:center",
            p(tags$b("MOVIE WITH THE HIGHEST RATING"))),
        valueBox(max(imdb$IMDB_Rating),
                 "The Shawshank Redemption",
                 icon = icon("star"),
                 color = "green",
                 width = 6)),
      
      ######
      
      fluidPage(
        box(plotlyOutput("jitter2"), 
            height = 446),
        box(background = "red",
            br(), 
            sliderInput("sliderIn2", 
                        "Select Meta Score:", 
                        min = min(imdb$Meta_score), 
                        max = max(imdb$Meta_score), 
                        value = c(57, 62),
                        step = 1)),
        div(style = "text-align:center",
            p(tags$b("MOVIE WITH THE HIGHEST META SCORE"))),
        valueBox(max(imdb$Meta_score), 
                 "The God Father", 
                 icon = icon("circle-arrow-up"),
                 color = "blue",
                 width = 6),
        div(style = "text-align:center",
            p(tags$b("MOVIE WITH THE HIGHEST VOTES"))),
            valueBox(comma(max(imdb$No_of_Votes)),
                     "The Shawshank Redemption",
                     icon = icon("heart"),
                     color = "green",
                     width = 6),
                      )
                   ),
    
    # tab3
    tabItem(
      tabName = "tabbot",
      fluidRow(
        box(width = 12,
            DT::dataTableOutput("table1"))
        )
      )
    )
  )

ui <- shinydashboard::dashboardPage(
  header = header,
  sidebar = sidebar,
  body = body, 
  skin = "blue"
)

server <- function(input, output) {
  
  #output tab1
  output$rev_graph <- renderPlotly({
    imdb_revenue <- 
      imdb |> 
      filter(Certificate == input$choose_certi) |> 
      group_by(Genre) |> 
      summarise(revenue = sum(Gross)) |> 
      arrange(desc(revenue)) |> 
      top_n(20)
    
    imdb_revenue <- 
      imdb_revenue |> 
      mutate(label = glue("Total Revenue : {comma(revenue)}
                      Genre Movie : {Genre}"))
    
    plot1 <- ggplot(imdb_revenue,
                    aes(x = revenue,
                        y = reorder(Genre,revenue),
                        text = label)) + 
      geom_col(aes(fill=revenue))+
      scale_fill_gradient(low = "orange", high = "red")+
      scale_x_continuous(labels = comma)+
      labs(
        title = "Top 20 Genre Based on Revenue",
        x = 'Revenue',
        y = "Genre")+
      theme_minimal()+
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            legend.position = "none",
            plot.title = element_text(hjust = 0.4))
    
    ggplotly(plot1, tooltip = "text")
  })
  
  output$met_graph <- renderPlotly({
    imdb_metascore <- 
      imdb |> 
      filter(Certificate == input$choose_certi) |> 
      group_by(Genre) |> 
      summarise(mscore = sum(Meta_score)) |> 
      arrange(desc(mscore)) |> 
      top_n(20)
    
    imdb_metascore <- 
      imdb_metascore |> 
      mutate(label = glue("Total Meta Score : {comma(mscore)}
                      Genre Movie : {Genre}"))
    
    plot2 <- ggplot(imdb_metascore,
                    aes(x = mscore,
                        y = reorder(Genre, mscore),
                        text = label)) + 
      geom_col(aes(fill=mscore))+
      scale_fill_gradient(low = "orange", high = "red")+
      scale_x_continuous(labels = comma)+
      labs(
        title = "Top 20 Genre Based on Meta Score",
        x = 'Meta Score',
        y = "Genre")+
      theme_minimal()+
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            legend.position = "none",
            plot.title = element_text(hjust = 0.4))
    
    ggplotly(plot2, tooltip = "text")
  })
  
  #page2
  output$jitter <- renderPlotly({
    
    ratinglow <- input$sliderIn[1]
    ratinghigh <- input$sliderIn[2]
    
  imdb_rating <- 
    imdb |> 
    filter(IMDB_Rating >= ratinglow & IMDB_Rating <= ratinghigh)
  
  imdb_rating <- 
    imdb_rating |> 
    mutate(label = glue("Rating = {IMDB_Rating}
                      Revenue = {comma(Gross)}
                      Movie Title = {Series_Title}"))
  
  plot_rating <- ggplot(imdb_rating, aes(x = IMDB_Rating,
                          y = Gross,
                          col = IMDB_Rating,
                          text = label
  ))+
    geom_jitter(aes(size = Gross, alpha = 0.4))+
    scale_y_continuous(labels = comma)+     
    labs(
      title = "Movie plot based on revenue and rating",
      x = 'Rating',
      y = "Revenue")+
    theme_minimal()+
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          legend.position = "none",
          plot.title = element_text(hjust = 0.4))
  
  ggplotly(plot_rating, tooltip = "text")
  })
  
  #plot jitter 2
  output$jitter2 <- renderPlotly({
    
    metascorelow <- input$sliderIn2[1]
    metascorehigh <- input$sliderIn2[2]
    
    
    imdb_vote <- 
      imdb |> 
      filter(Meta_score >= metascorelow & Meta_score <= metascorehigh)
    
    
    
    imdb_vote <- 
      imdb_vote |> 
      mutate(label = glue("Meta Score = {Meta_score}
                      Total Vote = {comma(No_of_Votes)}
                      Movie Title = {Series_Title}"))
    
    plot_meta <- ggplot(imdb_vote, 
                        aes(x = Meta_score,
                              y = No_of_Votes,
                              col = No_of_Votes,
                              text = label
    ))+
      geom_jitter(aes(size = Meta_score, alpha = 0.4))+
      scale_y_continuous(labels = comma)+     
      labs(
        title = "Movie plot based on meta score and total votes",
        x = 'Meta Score',
        y = "Total Vote")+
      theme_minimal()+
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            legend.position = "none",
            plot.title = element_text(hjust = 0.4))
    
    ggplotly(plot_meta, tooltip = "text")
    
  })
  # page3
  output$table1 <- DT::renderDataTable({
    
    imdb_certificate <-
      imdb
    
    DT::datatable(imdb_certificate, 
                  filter="top", 
                  selection="multiple", 
                  escape=FALSE, 
                  options = list(dom = 'ltipr', 
                                 scrollX = T))
  })
}

shinyApp(ui = ui, server = server)