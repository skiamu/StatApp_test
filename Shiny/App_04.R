# app from the country



server <- function(input, output, session) {
  
  # tab ----
  output$tab1 <- renderDataTable({ # df 'corresponding' to the primary cnt
    val <- round(runif(length(topics), 1, 11)) # the score for now is random
    data.frame(Topics = topics, Score = val, Level = lev(val))
  })
  output$tab2 <- renderDataTable({ # df 'corresponding' to the cnt to be compared
    if (! input$showCnt2) df <- NULL
    else {
      val <- round(runif(length(topics), 1, 11)) # the score for now is random
      df <- data.frame(Topics = topics, Score = val, Level = lev(val))
    }
  })
  
  # growth ---- 
  growthCnt <- reactive({
    getIndicators(myInd = 'GDP per capita growth (annual %)', myCnt = c(input$cnt,'World')) %>%
      select(Year,Value,CountryName)
  })
  
  growthCnt2 <- reactive({
    getIndicators(myInd = 'GDP per capita growth (annual %)', myCnt = c(input$cnt,input$cnt2,'World')) %>%
      select(Year,Value,CountryName)
  })
  
  output$plotG <- renderPlot({ 
    if (input$plotTextGrowth) df <- growthCnt2()
    else                      df <- growthCnt() 
    p <- ggplot(df, aes(x = Year, y = Value, colour = CountryName)) + 
      geom_point() + geom_line() + ggtitle('Growth')
    print(p)
    
  })
  
  output$textG <- renderText({ 'it is growing (text example)' })
  
  # map ----
  output$plotM <- renderPlot({ 
    if(input$cnt2 == input$cnt) p <- plotCnt(input$cnt)
    else                        p <- plotCnt(c(input$cnt,input$cnt2))
    print(p)
  })
  output$textM <- renderText({ 'Plot similar countries somehow (do nothing for now)' })
  
  # text ----
  output$textT1 <- renderText({  paste('1st country you have selected :', input$cnt) })
  output$textT2 <- renderText({  paste('2nd country you have selected :', input$cnt2) })
  
  output$plotT <- renderPlot({ 
    if(!input$plotText) p <- NULL 
    else p <- plotCnt(input$cnt2)
    print(p)
  })
  
}

ui <- pageWithSidebar(
  # headerPanel('Iris k-means clustering'),
  headerPanel('Selection from the country'),
  sidebarPanel(
    selectInput('cnt',  'Country',       unique(Indicators$CountryName), selected = 'Italy'),
    selectInput('cnt2', 'Country to be compared', unique(Indicators$CountryName), selected = 'Argentina'),
    selectInput('top', 'Topic', topics, selected = 'Agriculture')
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("All Topics", dataTableOutput('tab1'),
               checkboxInput('showCnt2', 'Show the country to be compared', FALSE),
               dataTableOutput('tab2')),
      tabPanel("GDP Growth", plotOutput("plotG"),
               textOutput('textG'),
               checkboxInput('plotTextGrowth','Show the Growth also for the other country',value = FALSE),
               checkboxInput('plotPrediction','Show the predictions (do nothing for now)',value = FALSE)
      ),
      
      tabPanel("Map", textOutput('textM'),
               plotOutput("plotM")),
      tabPanel("Text",textOutput("textT1"),
               textOutput("textT2"),
               checkboxInput('plotText','Show the Plot',value = FALSE),
               plotOutput('plotT') 
      )
    )
  )
)

shinyApp(ui = ui, server = server)