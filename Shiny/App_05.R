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
    if(pred()){ 'Prediction not (yet) available' }
    })

  # by topic ---- 
  top <- reactive({ input$top })
  output$textByTopicTemp <- renderText({ 'Think about what to put here' })
  
  # agriculture ----
  # cluster witha  brief description
  output$tabAgr <- renderTable({ cluAgr })
  # cluster of the selected country
  output$textAgr <- renderText({ paste(input$cnt,'is in cluster',kmAgr$cluster[input$cnt]) })
  # map with cluters
  output$plotCT <- renderPlot({ 
    if(!input$macFlag) print(plotClusterMap(kmAgr$cluster, nCluAgr))
    else               print(plotClusterMap(kmAgr$cluster, nCluAgr, mac=T))
    })
  # radarchart with the cluster means
  output$plotCR <- renderPlot({ 
    if (input$showCntRad) print(radarTopic(agrDC_s,kmAgr,cntRad=input$cnt,mac=input$macFlag))
    else                  print(radarTopic(agrDC_s,kmAgr,                 mac=input$macFlag)) 
    })
  # how many similar countries do you want to show?
  simAgr <- reactive({input$showSimAgr})
  # similar countries
  output$tabSimAgr <- renderTable({
    as.data.frame(t(getClu(names(simCnt(agrDC_s,input$cnt,input$numSimCnt)),kmAgr)))
    })
  
  # fda introduction
  output$textAgrIntro1 <- renderText({ paste('If the values change, would',input$cnt,'be in another cluster?') })
  output$textAgrIntro2 <- renderText({ 'First will be able to select an year and the corresponding values will be displayed. So you can get an idea for reasonable values.' })
  output$textAgrIntro3 <- renderText({ 'Then you will have to set the values for your prediction.' })
  output$textAgrIntro4 <- renderText({ 'After that the predicted cluster will be shown.' })
  # show values from the previous year selected
  output$valYAgr <- renderTable({ findValues(findYears(myIndAgr,input$cnt)$dc,input$yAgr) })
  # input for prediction
  vv <- reactive({ (c(input$v1,input$v2,input$v3,input$v4,input$v5,input$v6)-meanAgr)/varAgr })
  # result of the prediction
  output$predAgr <- renderText({ paste('The predicted cluster is',
                                       fdaPred(fdaAgr$fComp,fdaAgr$center,vv()))})
  output$space1 <- renderText({ '. ' })
  output$space2 <- renderText({ '. ' })
  output$space3      <- renderText({ 'This analysis was made by Fisher FDA and the classifier has a AERCV = ... TUTTAVIA LA FDA NON IN QUESTO CASO NON MI SEMBRA SOLIDISSIMA :(' })
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
               textOutput("textAgrIntro1"),
               textOutput("textAgrIntro2"),
               textOutput("textAgrIntro3"),
               textOutput("textAgrIntro4"),
               selectInput('yAgr', 'Year', 1960:2015, 2010),
               tableOutput('valYAgr'),
               numericInput('v1',sort(myIndAgr)[1],0,step = 0.1),
               numericInput('v2',sort(myIndAgr)[2],0,step = 0.1),
               numericInput('v3',sort(myIndAgr)[3],0,step = 0.1),
               numericInput('v4',sort(myIndAgr)[4],0,step = 0.1),
               numericInput('v5',sort(myIndAgr)[5],0,step = 0.1),
               numericInput('v6',sort(myIndAgr)[6],0,step = 0.1),
               textOutput('predAgr'),
               textOutput('space1'),
               textOutput('space2'),
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