# app from the country 05

# store somewhere else
topics <- c("Agriculture",
            "Natural Resources",
            "Trade",
            "Productivity",
            "Telecom",
            "Some missing ones")

server <- function(input, output, session) {
  
  # growth ---- 
  growthCnt <- reactive({
    getIndicators(myInd = 'GDP per capita growth (annual %)', myCnt = c(input$cnt,'World')) %>%
      select(Year,Value,CountryName)
  })
  
  output$plotG <- renderPlot({
    p <- ggplot(growthCnt(), aes(x = Year, y = Value, colour = CountryName)) + 
      geom_point() + geom_line() + ggtitle('Growth')
    print(p)
  })
  
  pred <- reactive({input$showPred})
  output$textG <- renderText({
    if(pred()){
      'Prediction not (yet) available'
    }})

  # by topic ---- 
  top <- reactive({ input$top })
  output$textByTopicTemp <- renderText({ 'Think about what to put here' })
  
  # agriculture ----
  output$tabAgr <- renderTable({ cluAgr })
  output$textAgr <- renderText({ paste(input$cnt,'is in cluster',kmAgr$cluster[input$cnt]) })
  output$plotCT <- renderPlot({ 
    if(!input$macFlag) print(plotClusterMap(kmAgr$cluster, nCluAgr))
    else               print(plotClusterMap(kmAgr$cluster, nCluAgr, mac=T))
    })
  output$plotCR <- renderPlot({ 
    if (input$showCntRad) print(radarTopic(agrDC_s,kmAgr,cntRad=input$cnt,mac=input$macFlag))
    else                  print(radarTopic(agrDC_s,kmAgr,                 mac=input$macFlag)) 
    })
  simAgr <- reactive({input$showSimAgr})
  output$tabSimAgr <- renderTable({
    as.data.frame(t(getClu(names(simCnt(agrDC_s,input$cnt,input$numSimCnt)),kmAgr)))
    })
  output$textAgrTemp <- renderText({ paste('In another year would',input$cnt,'be in another cluster?') })
  yearsAgr <- reactive({ findYears(myIndAgr,input$cnt)$years })
  dcAgrCnt <- reactive({ findYears(myIndAgr,input$cnt)$dc })
  valAgrCnt <- reactive({ std(findValues(dcAgrCnt(),yearsAgr()),meanAgr,varAgr) })
  output$space1      <- renderText({ 'ora gli faccio stampare tutti valori e i rispettivi cluster' }) 
  output$space2      <- renderTable({ as.data.frame(valAgrCnt()) })
  output$space2.1    <- renderText({ paste('cluster in',input$yAgr,':',
                                           fdaPred(fdaAgr$a,fdaAgr$cc,data.matrix(t(valAgrCnt()[,input$yAgr])))) })
  output$space3      <- renderText({ 'This analysis was made by Fisher FDA and the classifier has a AERCV = ...' })
}

ui <- pageWithSidebar(
  headerPanel('Selection from the country'),
  sidebarPanel(
    checkboxInput('macFlag','Are you using a Mac?',value = FALSE),
    selectInput('cnt', 'Country', unique(Indicators$CountryName), selected = 'Italy')#,
    #selectInput('cnt2', 'Country to be compared', unique(Indicators$CountryName), selected = 'Argentina')
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("GDP Growth", 
               plotOutput("plotG"),
               checkboxInput('showPred','Show the predictions',value = FALSE),
               textOutput("textG")
               ),
      tabPanel("By topic",
               selectInput('top', 'Topic', topics, selected = 'Agriculture'),
               textOutput("textByTopicTemp")
               ),
      tabPanel("Agriculture",
               tableOutput('tabAgr'),
               textOutput("textAgr"), 
               plotOutput("plotCT"),
               checkboxInput('showCntRad','Show the selected country on the radarplot',value = FALSE),
               plotOutput("plotCR"),
               numericInput('numSimCnt', 'Show similar countries with their cluster', 3,
                            min = 1, max = 9),
               tableOutput("tabSimAgr"),
               textOutput("textAgrTemp"),
               selectInput('yAgr', 'Year', 1960:2015, 2010),
               textOutput("space1"),
               textOutput("space2.1"),
               tableOutput("space2"),
               textOutput("space3")
               ),
      tabPanel("Natural Resources"),
      tabPanel("Telecommunications"),
      tabPanel("Occupation"),
      tabPanel("Trade"),
      tabPanel("Production")
      
    )
  )
)

shinyApp(ui = ui, server = server)