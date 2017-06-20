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
  output$tab.Agr <- renderTable({ clu.Agr })
  # cluster of the selected country
  output$text.Agr <- renderText({ paste(input$cnt,'is in cluster',km.Agr$cluster[input$cnt]) })
  # map with cluters
  output$plotMap.Agr <- renderPlot({ 
    if(!input$macFlag) print(plotClusterMap(km.Agr$cluster, nClu.Agr))
    else               print(plotClusterMap(km.Agr$cluster, nClu.Agr, mac=T))
    })
  # radarchart with the cluster means
  output$plotRad.Agr <- renderPlot({ 
    if (input$showCntRad) print(radarTopic(DCs.Agr,km.Agr,cntRad=input$cnt,mac=input$macFlag))
    else                  print(radarTopic(DCs.Agr,km.Agr,                 mac=input$macFlag)) 
    })
  # how many similar countries do you want to show?
  simAgr <- reactive({input$showSim.Agr})
  # similar countries
  output$tabSim.Agr <- renderTable({
    as.data.frame(t(getClu(names(simCnt(DCs.Agr,input$cnt,input$numSimCnt.Agr)),km.Agr)))
    })
  
  # fda introduction
  output$textIntro1.Agr <- renderText({ paste('If the values change, would',input$cnt,'be in another cluster?') })
  output$textIntro2.Agr <- renderText({ 'First will be able to select an year and the corresponding values will be displayed. So you can get an idea for reasonable values.' })
  output$textIntro3.Agr <- renderText({ 'Then you will have to set the values for your prediction.' })
  output$textIntro4.Agr <- renderText({ 'After that the predicted cluster will be shown.' })
  # show values from the previous year selected
  output$valY.Agr <- renderTable({ findValues(findYears(myInd.Agr,input$cnt)$dc,input$y.Agr) })
  # input for prediction
  vv <- reactive({ (c(input$v1.Agr,
                      input$v2.Agr,
                      input$v3.Agr,
                      input$v4.Agr,
                      input$v5.Agr,
                      input$v6.Agr)-mean.Agr)/var.Agr })
  # result of the prediction
  output$pred.Agr <- renderText({ paste('The predicted cluster is',
                                       fdaPred(fda.Agr$fComp,fda.Agr$center,vv()))})
  output$space1 <- renderText({ '. ' })
  output$space2 <- renderText({ '. ' })
  
  # technical details
  det.Agr <- reactive({input$showDet.Agr})
  output$textDet.Agr <- renderText({
    if(det.Agr()){ recap.Agr }
  })
  
  # telecom ----
  # cluster witha  brief description
  output$tab.Tel <- renderTable({ clu.Tel })
  # cluster of the selected country
  output$text.Tel <- renderText({ paste(input$cnt,'is in cluster',km.Tel$cluster[input$cnt]) })
  # map with cluters
  output$plotMap.Tel <- renderPlot({ 
    if(!input$macFlag) print(plotClusterMap(km.Tel$cluster, nClu.Tel))
    else               print(plotClusterMap(km.Tel$cluster, nClu.Tel, mac=T))
  })
  # radarchart with the cluster means
  output$plotRad.Tel <- renderPlot({ 
    if (input$showCntRad) print(radarTopic(DCs.Tel,km.Tel,cntRad=input$cnt,mac=input$macFlag))
    else                  print(radarTopic(DCs.Tel,km.Tel,                 mac=input$macFlag)) 
  })
  # how many similar countries do you want to show?
  simTel <- reactive({input$showSim.Tel})
  # similar countries
  output$tabSim.Tel <- renderTable({
    as.data.frame(t(getClu(names(simCnt(DCs.Tel,input$cnt,input$numSimCnt.Tel)),km.Tel)))
  })
  
  # fda introduction
  output$textIntro1.Tel <- renderText({ paste('If the values change, would',input$cnt,'be in another cluster?') })
  output$textIntro2.Tel <- renderText({ 'First will be able to select an year and the corresponding values will be displayed. So you can get an idea for reasonable values.' })
  output$textIntro3.Tel <- renderText({ 'Then you will have to set the values for your prediction.' })
  output$textIntro4.Tel <- renderText({ 'After that the predicted cluster will be shown.' })
  # show values from the previous year selected
  output$valY.Tel <- renderTable({ findValues(findYears(myInd.Tel,input$cnt)$dc,input$y.Tel) })
  # input for prediction
  vv <- reactive({ (c(input$v1.Tel,
                      input$v2.Tel,
                      input$v3.Tel,
                      input$v4.Tel,
                      input$v5.Tel,
                      input$v6.Tel)-mean.Tel)/var.Tel })
  # result of the prediction
  output$pred.Tel <- renderText({ paste('The predicted cluster is',
                                        fdaPred(fda.Tel$fComp,fda.Tel$center,vv()))})
  output$space1 <- renderText({ '. ' })
  output$space2 <- renderText({ '. ' })
  
  # technical details
  det.Tel <- reactive({input$showDet.Tel})
  output$textDet.Tel <- renderText({
    if(det.Tel()){ recap.Tel }
  })  
}

ui <- pageWithSidebar(
  headerPanel('Overview on a country'),
  sidebarPanel(
    checkboxInput('macFlag','Problem with some plots? (it could happen on Mac)',value = FALSE),
    selectInput('cnt', 'Choose the country', unique(Indicators$CountryName), selected = 'Italy')
  ),
  mainPanel(
    tabsetPanel(
      
      tabPanel("GDP Growth",          # ----
               plotOutput("plotG"),
               checkboxInput('showPred','Show the predictions',value = FALSE),
               textOutput("textG")
               ),
      
      tabPanel("By topic",            # ----
               selectInput('top', 'Topic', topics, selected = 'Agriculture'),
               textOutput("textByTopicTemp")
               ),
      
      tabPanel("Agriculture",         # ----
               tableOutput('tab.Agr'),
               textOutput("text.Agr"), 
               plotOutput("plotMap.Agr"),
               checkboxInput('showCntRad','Show the selected country on the radarplot',value = FALSE),
               plotOutput("plotRad.Agr"),
               numericInput('numSimCnt.Agr', 'Show similar countries with their cluster', 3,
                            min = 1, max = 9),
               tableOutput("tabSim.Agr"),
               textOutput("textIntro1.Agr"),
               textOutput("textIntro2.Agr"),
               textOutput("textIntro3.Agr"),
               textOutput("textIntro4.Agr"),
               selectInput('y.Agr', 'Year', 1960:2015, 2010),
               tableOutput('valY.Agr'),
               numericInput('v1.Agr',sort(myInd.Agr)[1],0,step = 0.1),
               numericInput('v2.Agr',sort(myInd.Agr)[2],0,step = 0.1),
               numericInput('v3.Agr',sort(myInd.Agr)[3],0,step = 0.1),
               numericInput('v4.Agr',sort(myInd.Agr)[4],0,step = 0.1),
               numericInput('v5.Agr',sort(myInd.Agr)[5],0,step = 0.1),
               numericInput('v6.Agr',sort(myInd.Agr)[6],0,step = 0.1),
               textOutput('pred.Agr'),
               checkboxInput('showDet.Agr','Show technical details',value = FALSE),
               textOutput("textDet.Agr")
               ),
      
      tabPanel("Natural Resources"),  # ----
      tabPanel("Telecommunications", # ----
               tableOutput('tab.Tel'),
               textOutput("text.Tel"), 
               plotOutput("plotMap.Tel"),
               checkboxInput('showCntRad','Show the selected country on the radarplot',value = FALSE),
               plotOutput("plotRad.Tel"),
               numericInput('numSimCnt.Tel', 'Show similar countries with their cluster', 3,
                            min = 1, max = 9),
               tableOutput("tabSim.Tel"),
               textOutput("textIntro1.Tel"),
               textOutput("textIntro2.Tel"),
               textOutput("textIntro3.Tel"),
               textOutput("textIntro4.Tel"),
               selectInput('y.Tel', 'Year', 1960:2015, 2010),
               tableOutput('valY.Tel'),
               numericInput('v1.Tel',sort(myInd.Tel)[1],0,step = 0.1),
               numericInput('v2.Tel',sort(myInd.Tel)[2],0,step = 0.1),
               numericInput('v3.Tel',sort(myInd.Tel)[3],0,step = 0.1),
               numericInput('v4.Tel',sort(myInd.Tel)[4],0,step = 0.1),
               numericInput('v5.Tel',sort(myInd.Tel)[5],0,step = 0.1),
               numericInput('v6.Tel',sort(myInd.Tel)[6],0,step = 0.1),
               textOutput('pred.Tel'),
               checkboxInput('showDet.Tel','Show technical details',value = FALSE),
               textOutput("textDet.Tel")
      ),
      tabPanel("Occupation"),         # ----
      tabPanel("Trade"),              # ----
      tabPanel("Production")          # ----
      # end ----
    )
  )
)

shinyApp(ui = ui, server = server)