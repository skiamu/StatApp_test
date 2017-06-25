# app from the country 05

# store somewhere else
topics <- c("Agriculture",
            "Natural Resources",
            "Trade",
            "Productivity",
            "Telecom",
            "Ease to start a business",
            "Economic Indicators")

server <- function(input, output, session) {
  
  # growth ----
  predS <- reactive({input$showPred}) 
  predC <- reactive({checkPred(input$cnt)}) 
  output$plotG <- renderPlot({
    if(predS() & predC()){
      p <- plot10yPred(input$cnt,flagPred = T)
    } else {
      p <- plot10yPred(input$cnt,flagPred = F)
    }
    print(p)
  })
  
  # growthCnt <- reactive({
  #   getIndicators(myInd = 'GDP per capita growth (annual %)', myCnt = c(input$cnt,'World')) %>%
  #     select(Year,Value,CountryName)
  # })
  # 
  # output$plotG <- renderPlot({
  #   p <- ggplot(growthCnt(), aes(x = Year, y = Value, colour = CountryName)) + 
  #     geom_point() + geom_line() + ggtitle('Growth')
  #   print(p)
  # })
  
  output$textG <- renderText({
    if(!predC()){ 'Prediction are not available for this country' }
    })

  # by topic ---- 
  top <- reactive({ input$top })
  output$textByTopicTemp <- renderText({ 'Think about what to put here' })
  
  # agriculture ----
  # cluster with a brief description
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
    if (input$showCntRad.Agr) print(radarTopic(DCs.Agr,km.Agr,cntRad=input$cnt,mac=input$macFlag))
    else                      print(radarTopic(DCs.Agr,km.Agr,                 mac=input$macFlag)) 
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
  vv.Agr <- reactive({ (c(input$v1.Agr,
                          input$v2.Agr,
                          input$v3.Agr,
                          input$v4.Agr,
                          input$v5.Agr,
                          input$v6.Agr)-mean.Agr)/var.Agr })
  # result of the prediction
  output$pred.Agr <- renderText({ paste('The predicted cluster is',
                                       fdaPred(fda.Agr$fComp,fda.Agr$center,vv.Agr()))})
  output$space1 <- renderText({ '. ' })
  output$space2 <- renderText({ '. ' })
  
  # technical details
  det.Agr <- reactive({input$showDet.Agr})
  output$textDet.Agr <- renderText({
    if(det.Agr()){ recap.Agr }
  })
  
  # natuarl resources ----
  # cluster with a brief description
  output$tab.Nat <- renderTable({ clu.Nat })
  # cluster of the selected country
  output$text.Nat <- renderText({ paste(input$cnt,'is in cluster',km.Nat$cluster[input$cnt]) })
  # map with cluters
  output$plotMap.Nat <- renderPlot({ 
    if(!input$macFlag) print(plotClusterMap(km.Nat$cluster, nClu.Nat))
    else               print(plotClusterMap(km.Nat$cluster, nClu.Nat, mac=T))
  })
  # radarchart with the cluster means
  output$plotRad.Nat <- renderPlot({ 
    if (input$showCntRad.Nat) print(radarTopic(DCs.Nat,km.Nat,cntRad=input$cnt,mac=input$macFlag))
    else                      print(radarTopic(DCs.Nat,km.Nat,                 mac=input$macFlag)) 
  })
  # how many similar countries do you want to show?
  simNat <- reactive({input$showSim.Nat})
  # similar countries
  output$tabSim.Nat <- renderTable({
    as.data.frame(t(getClu(names(simCnt(DCs.Nat,input$cnt,input$numSimCnt.Nat)),km.Nat)))
  })
  
  # fda introduction
  output$textIntro1.Nat <- renderText({ paste('If the values change, would',input$cnt,'be in another cluster?') })
  output$textIntro2.Nat <- renderText({ 'First will be able to select an year and the corresponding values will be displayed. So you can get an idea for reasonable values.' })
  output$textIntro3.Nat <- renderText({ 'Then you will have to set the values for your prediction.' })
  output$textIntro4.Nat <- renderText({ 'After that the predicted cluster will be shown.' })
  # show values from the previous year selected
  output$valY.Nat <- renderTable({ findValues(findYears(myInd.Nat,input$cnt)$dc,input$y.Nat) })
  # input for prediction
  vv.Nat <- reactive({ (c(input$v1.Nat,
                          input$v2.Nat,
                          input$v3.Nat,
                          input$v4.Nat,
                          input$v5.Nat)-mean.Nat)/var.Nat })
  # result of the prediction
  output$pred.Nat <- renderText({ paste('The predicted cluster is',
                                        fdaPred(fda.Nat$fComp,fda.Nat$center,vv.Nat()))})
  output$space1 <- renderText({ '. ' })
  output$space2 <- renderText({ '. ' })
  
  # technical details
  det.Nat <- reactive({input$showDet.Nat})
  output$textDet.Nat <- renderText({
    if(det.Nat()){ recap.Nat }
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
    if (input$showCntRad.Tel) print(radarTopic(DCs.Tel,km.Tel,cntRad=input$cnt,mac=input$macFlag))
    else                      print(radarTopic(DCs.Tel,km.Tel,                 mac=input$macFlag)) 
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
  vv.Tel <- reactive({ (c(input$v1.Tel,
                          input$v2.Tel,
                          input$v3.Tel,
                          input$v4.Tel,
                          input$v5.Tel)-mean.Tel)/var.Tel })
  # result of the prediction
  output$pred.Tel <- renderText({ paste('The predicted cluster is',
                                        fdaPred(fda.Tel$fComp,fda.Tel$center,vv.Tel()))})
  output$space1 <- renderText({ '. ' })
  output$space2 <- renderText({ '. ' })
  
  # technical details
  det.Tel <- reactive({input$showDet.Tel})
  output$textDet.Tel <- renderText({
    if(det.Tel()){ recap.Tel }
  })  
  # trade ----
  # cluster witha  brief description
  output$tab.Trd <- renderTable({ clu.Trd })
  # cluster of the selected country
  output$text.Trd <- renderText({ paste(input$cnt,'is in cluster',km.Trd$cluster[input$cnt]) })
  # map with cluters
  output$plotMap.Trd <- renderPlot({ 
    if(!input$macFlag) print(plotClusterMap(km.Trd$cluster, nClu.Trd))
    else               print(plotClusterMap(km.Trd$cluster, nClu.Trd, mac=T))
  })
  # radarchart with the cluster means
  output$plotRad.Trd <- renderPlot({ 
    if (input$showCntRad.Trd) print(radarTopic(DCs.Trd,km.Trd,cntRad=input$cnt,mac=input$macFlag))
    else                      print(radarTopic(DCs.Trd,km.Trd,                 mac=input$macFlag)) 
  })
  # how many similar countries do you want to show?
  simTrd <- reactive({input$showSim.Trd})
  # similar countries
  output$tabSim.Trd <- renderTable({
    as.data.frame(t(getClu(names(simCnt(DCs.Trd,input$cnt,input$numSimCnt.Trd)),km.Trd)))
  })
  
  # fda introduction
  output$textIntro1.Trd <- renderText({ paste('If the values change, would',input$cnt,'be in another cluster?') })
  output$textIntro2.Trd <- renderText({ 'First will be able to select an year and the corresponding values will be displayed. So you can get an idea for reasonable values.' })
  output$textIntro3.Trd <- renderText({ 'Then you will have to set the values for your prediction.' })
  output$textIntro4.Trd <- renderText({ 'After that the predicted cluster will be shown.' })
  # show values from the previous year selected
  output$valY.Trd <- renderTable({ findValues(findYears(myInd.Trd,input$cnt)$dc,input$y.Trd) })
  # input for prediction
  vv.Trd <- reactive({ (c(input$v1.Trd,
                          input$v2.Trd,
                          input$v3.Trd,
                          input$v4.Trd,
                          input$v5.Trd,
                          input$v6.Trd,
                          input$v7.Trd,
                          input$v8.Trd,
                          input$v9.Trd,
                          input$v10.Trd)-mean.Trd)/var.Trd })
  # result of the prediction
  output$pred.Trd <- renderText({ paste('The predicted cluster is',
                                        fdaPred(fda.Trd$fComp,fda.Trd$center,vv.Trd()))})
  output$space1 <- renderText({ '. ' })
  output$space2 <- renderText({ '. ' })
  
  # technical details
  det.Trd <- reactive({input$showDet.Trd})
  output$textDet.Trd <- renderText({
    if(det.Trd()){ recap.Trd }
  })
  
  # economic indicators ----
  # cluster witha  brief description
  output$tab.Ein <- renderTable({ clu.Ein })
  # cluster of the selected country
  output$text.Ein <- renderText({ paste(input$cnt,'is in cluster',km.Ein$cluster[input$cnt]) })
  # map with cluters
  output$plotMap.Ein <- renderPlot({ 
    if(!input$macFlag) print(plotClusterMap(km.Ein$cluster, nClu.Ein))
    else               print(plotClusterMap(km.Ein$cluster, nClu.Ein, mac=T))
  })
  # radarchart with the cluster means
  output$plotRad.Ein <- renderPlot({ 
    if (input$showCntRad.Ein) print(radarTopic(DCs.Ein,km.Ein,cntRad=input$cnt,mac=input$macFlag))
    else                      print(radarTopic(DCs.Ein,km.Ein,                 mac=input$macFlag)) 
  })
  # how many similar countries do you want to show?
  simEin <- reactive({input$showSim.Ein})
  # similar countries
  output$tabSim.Ein <- renderTable({
    as.data.frame(t(getClu(names(simCnt(DCs.Ein,input$cnt,input$numSimCnt.Ein)),km.Ein)))
  })
  
  # fda introduction
  output$textIntro1.Ein <- renderText({ paste('If the values change, would',input$cnt,'be in another cluster?') })
  output$textIntro2.Ein <- renderText({ 'First will be able to select an year and the corresponding values will be displayed. So you can get an idea for reasonable values.' })
  output$textIntro3.Ein <- renderText({ 'Then you will have to set the values for your prediction.' })
  output$textIntro4.Ein <- renderText({ 'After that the predicted cluster will be shown.' })
  # show values from the previous year selected
  output$valY.Ein <- renderTable({ findValues(findYears(myInd.Ein,input$cnt)$dc,input$y.Ein) })
  # input for prediction
  vv.Ein <- reactive({ (c(input$v1.Ein,
                          input$v2.Ein,
                          input$v3.Ein,
                          input$v4.Ein,
                          input$v5.Ein,
                          input$v6.Ein)-mean.Ein)/var.Ein })
  # result of the prediction
  output$pred.Ein <- renderText({ paste('The predicted cluster is',
                                        fdaPred(fda.Ein$fComp,fda.Ein$center,vv.Ein()))})
  output$space1 <- renderText({ '. ' })
  output$space2 <- renderText({ '. ' })
  
  # technical details
  det.Ein <- reactive({input$showDet.Ein})
  output$textDet.Ein <- renderText({
    if(det.Ein()){ recap.Ein }
  })
  
  # production ####
  # cluster witha  brief description
  output$tab.Prd <- renderTable({ clu.Prd })
  # cluster of the selected country
  output$text.Prd <- renderText({ paste(input$cnt,'is in cluster',km.Prd$cluster[input$cnt]) })
  # map with cluters
  output$plotMap.Prd <- renderPlot({ 
    if(!input$macFlag) print(plotClusterMap(km.Prd$cluster, nClu.Prd))
    else               print(plotClusterMap(km.Prd$cluster, nClu.Prd, mac=T))
  })
  # radarchart with the cluster means
  output$plotRad.Prd <- renderPlot({ 
    if (input$showCntRad.Prd) print(radarTopic(DCs.Prd,km.Prd,cntRad=input$cnt,mac=input$macFlag))
    else                      print(radarTopic(DCs.Prd,km.Prd,                 mac=input$macFlag)) 
  })
  # how many similar countries do you want to show?
  simPrd <- reactive({input$showSim.Prd})
  # similar countries
  output$tabSim.Prd <- renderTable({
    as.data.frame(t(getClu(names(simCnt(DCs.Prd,input$cnt,input$numSimCnt.Prd)),km.Prd)))
  })
  
  # fda introduction
  output$textIntro1.Prd <- renderText({ paste('If the values change, would',input$cnt,'be in another cluster?') })
  output$textIntro2.Prd <- renderText({ 'First will be able to select an year and the corresponding values will be displayed. So you can get an idea for reasonable values.' })
  output$textIntro3.Prd <- renderText({ 'Then you will have to set the values for your prediction.' })
  output$textIntro4.Prd <- renderText({ 'After that the predicted cluster will be shown.' })
  # show values from the previous year selected
  output$valY.Prd <- renderTable({ findValues(findYears(myInd.Prd,input$cnt)$dc,input$y.Prd) })
  # input for prediction
  vv.Prd <- reactive({ (c(input$v1.Prd,
                          input$v2.Prd,
                          input$v3.Prd,
                          input$v4.Prd,
                          input$v5.Prd)-mean.Prd)/var.Prd })
  # result of the prediction
  output$pred.Prd <- renderText({ paste('The predicted cluster is',
                                        fdaPred(fda.Prd$fComp,fda.Prd$center,vv.Prd()))})
  output$space1 <- renderText({ '. ' })
  output$space2 <- renderText({ '. ' })
  
  # technical details
  det.Prd <- reactive({input$showDet.Prd})
  output$textDet.Prd <- renderText({
    if(det.Prd()){ recap.Prd }
  })
  
  # ease to start a business  ----
  # cluster witha  brief description
  output$tab.Ets <- renderTable({ clu.Ets })
  # cluster of the selected country
  output$text.Ets <- renderText({ paste(input$cnt,'is in cluster',km.Ets$cluster[input$cnt]) })
  # map with cluters
  output$plotMap.Ets <- renderPlot({ 
    if(!input$macFlag) print(plotClusterMap(km.Ets$cluster, nClu.Ets))
    else               print(plotClusterMap(km.Ets$cluster, nClu.Ets, mac=T))
  })
  # radarchart with the cluster means
  output$plotRad.Ets <- renderPlot({ 
    if (input$showCntRad.Ets) print(radarTopic(DCs.Ets,km.Ets,cntRad=input$cnt,mac=input$macFlag))
    else                      print(radarTopic(DCs.Ets,km.Ets,                 mac=input$macFlag)) 
  })
  # how many similar countries do you want to show?
  simEts <- reactive({input$showSim.Ets})
  # similar countries
  output$tabSim.Ets <- renderTable({
    as.data.frame(t(getClu(names(simCnt(DCs.Ets,input$cnt,input$numSimCnt.Ets)),km.Ets)))
  })
  
  # fda introduction
  output$textIntro1.Ets <- renderText({ paste('If the values change, would',input$cnt,'be in another cluster?') })
  output$textIntro2.Ets <- renderText({ 'First will be able to select an year and the corresponding values will be displayed. So you can get an idea for reasonable values.' })
  output$textIntro3.Ets <- renderText({ 'Then you will have to set the values for your prediction.' })
  output$textIntro4.Ets <- renderText({ 'After that the predicted cluster will be shown.' })
  # show values from the previous year selected
  output$valY.Ets <- renderTable({ findValues(findYears(myInd.Ets,input$cnt)$dc,input$y.Ets) })
  # input for prediction
  vv.Ets <- reactive({ (c(input$v1.Ets,
                          input$v2.Ets,
                          input$v3.Ets,
                          input$v4.Ets,
                          input$v5.Ets)-mean.Ets)/var.Ets })
  # result of the prediction
  output$pred.Ets <- renderText({ paste('The predicted cluster is',
                                        fdaPred(fda.Ets$fComp,fda.Ets$center,vv.Ets()))})
  output$space1 <- renderText({ '. ' })
  output$space2 <- renderText({ '. ' })
  
  # technical details
  det.Ets <- reactive({input$showDet.Ets})
  output$textDet.Ets <- renderText({
    if(det.Ets()){ recap.Ets }
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
      
      tabPanel("GDP Growth",               # ----
               plotOutput("plotG"),
               checkboxInput('showPred','Show the predictions',value = FALSE),
               textOutput("textG")
               ),
      
      tabPanel("By topic",                 # ----
               selectInput('top', 'Topic', topics, selected = 'Agriculture'),
               textOutput("textByTopicTemp")
               ),
      
      tabPanel("Agriculture",              # ----
               tableOutput('tab.Agr'),
               textOutput("text.Agr"), 
               plotOutput("plotMap.Agr"),
               checkboxInput('showCntRad.Agr','Show the selected country on the radarplot',value = FALSE),
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
      
      tabPanel("Natural Resources",        # ----
               tableOutput('tab.Nat'),
               textOutput("text.Nat"), 
               plotOutput("plotMap.Nat"),
               checkboxInput('showCntRad.Nat','Show the selected country on the radarplot',value = FALSE),
               plotOutput("plotRad.Nat"),
               numericInput('numSimCnt.Nat', 'Show similar countries with their cluster', 3,
                            min = 1, max = 9),
               tableOutput("tabSim.Nat"),
               textOutput("textIntro1.Nat"),
               textOutput("textIntro2.Nat"),
               textOutput("textIntro3.Nat"),
               textOutput("textIntro4.Nat"),
               selectInput('y.Nat', 'Year', 1960:2015, 2010),
               tableOutput('valY.Nat'),
               numericInput('v1.Nat',sort(myInd.Nat)[1],0,step = 0.1),
               numericInput('v2.Nat',sort(myInd.Nat)[2],0,step = 0.1),
               numericInput('v3.Nat',sort(myInd.Nat)[3],0,step = 0.1),
               numericInput('v4.Nat',sort(myInd.Nat)[4],0,step = 0.1),
               numericInput('v5.Nat',sort(myInd.Nat)[5],0,step = 0.1),
               textOutput('pred.Nat'),
               checkboxInput('showDet.Nat','Show technical details',value = FALSE),
               textOutput("textDet.Nat")
               ),
      tabPanel("Telecommunications",       # ----
               tableOutput('tab.Tel'),
               textOutput("text.Tel"), 
               plotOutput("plotMap.Tel"),
               checkboxInput('showCntRad.Tel','Show the selected country on the radarplot',value = FALSE),
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
               textOutput('pred.Tel'),
               checkboxInput('showDet.Tel','Show technical details',value = FALSE),
               textOutput("textDet.Tel")
      ),
      tabPanel("Trade",                    # ----
               tableOutput('tab.Trd'),
               textOutput("text.Trd"), 
               plotOutput("plotMap.Trd"),
               checkboxInput('showCntRad.Trd','Show the selected country on the radarplot',value = FALSE),
               plotOutput("plotRad.Trd"),
               numericInput('numSimCnt.Trd', 'Show similar countries with their cluster', 3,
                            min = 1, max = 9),
               tableOutput("tabSim.Trd"),
               textOutput("textIntro1.Trd"),
               textOutput("textIntro2.Trd"),
               textOutput("textIntro3.Trd"),
               textOutput("textIntro4.Trd"),
               selectInput('y.Trd', 'Year', 1960:2015, 2010),
               tableOutput('valY.Trd'),
               numericInput('v1.Trd',sort(myInd.Trd)[1],0,step = 0.1),
               numericInput('v2.Trd',sort(myInd.Trd)[2],0,step = 0.1),
               numericInput('v3.Trd',sort(myInd.Trd)[3],0,step = 0.1),
               numericInput('v4.Trd',sort(myInd.Trd)[4],0,step = 0.1),
               numericInput('v5.Trd',sort(myInd.Trd)[5],0,step = 0.1),
               numericInput('v6.Trd',sort(myInd.Trd)[6],0,step = 0.1),
               numericInput('v7.Trd',sort(myInd.Trd)[6],0,step = 0.1),
               numericInput('v8.Trd',sort(myInd.Trd)[6],0,step = 0.1),
               numericInput('v9.Trd',sort(myInd.Trd)[6],0,step = 0.1),
               numericInput('v10.Trd',sort(myInd.Trd)[6],0,step = 0.1),
               textOutput('pred.Trd'),
               checkboxInput('showDet.Trd','Show technical details',value = FALSE),
               textOutput("textDet.Trd")
               ),
      tabPanel("Economic Indicators",      # ----
               tableOutput('tab.Ein'),
               textOutput("text.Ein"), 
               plotOutput("plotMap.Ein"),
               checkboxInput('showCntRad.Ein','Show the selected country on the radarplot',value = FALSE),
               plotOutput("plotRad.Ein"),
               numericInput('numSimCnt.Ein', 'Show similar countries with their cluster', 3,
                            min = 1, max = 9),
               tableOutput("tabSim.Ein"),
               textOutput("textIntro1.Ein"),
               textOutput("textIntro2.Ein"),
               textOutput("textIntro3.Ein"),
               textOutput("textIntro4.Ein"),
               selectInput('y.Ein', 'Year', 1960:2015, 2010),
               tableOutput('valY.Ein'),
               numericInput('v1.Ein',sort(myInd.Ein)[1],0,step = 0.1),
               numericInput('v2.Ein',sort(myInd.Ein)[2],0,step = 0.1),
               numericInput('v3.Ein',sort(myInd.Ein)[3],0,step = 0.1),
               numericInput('v4.Ein',sort(myInd.Ein)[4],0,step = 0.1),
               numericInput('v5.Ein',sort(myInd.Ein)[5],0,step = 0.1),
               numericInput('v6.Ein',sort(myInd.Ein)[6],0,step = 0.1),
               textOutput('pred.Ein'),
               checkboxInput('showDet.Ein','Show technical details',value = FALSE),
               textOutput("textDet.Ein")
      ),      
      tabPanel("Production",               # ----
               tableOutput('tab.Prd'),
               textOutput("text.Prd"), 
               plotOutput("plotMap.Prd"),
               checkboxInput('showCntRad.Prd','Show the selected country on the radarplot',value = FALSE),
               plotOutput("plotRad.Prd"),
               numericInput('numSimCnt.Prd', 'Show similar countries with their cluster', 3,
                            min = 1, max = 9),
               tableOutput("tabSim.Prd"),
               textOutput("textIntro1.Prd"),
               textOutput("textIntro2.Prd"),
               textOutput("textIntro3.Prd"),
               textOutput("textIntro4.Prd"),
               selectInput('y.Prd', 'Year', 1960:2015, 2010),
               tableOutput('valY.Prd'),
               numericInput('v1.Prd',sort(myInd.Prd)[1],0,step = 0.1),
               numericInput('v2.Prd',sort(myInd.Prd)[2],0,step = 0.1),
               numericInput('v3.Prd',sort(myInd.Prd)[3],0,step = 0.1),
               numericInput('v4.Prd',sort(myInd.Prd)[4],0,step = 0.1),
               numericInput('v5.Prd',sort(myInd.Prd)[5],0,step = 0.1),
               textOutput('pred.Prd'),
               checkboxInput('showDet.Prd','Show technical details',value = FALSE),
               textOutput("textDet.Prd")
               ),
  
      tabPanel("Ease to start a business", # ----
               tableOutput('tab.Ets'),
               textOutput("text.Ets"), 
               plotOutput("plotMap.Ets"),
               checkboxInput('showCntRad.Ets','Show the selected country on the radarplot',value = FALSE),
               plotOutput("plotRad.Ets"),
               numericInput('numSimCnt.Ets', 'Show similar countries with their cluster', 3,
                            min = 1, max = 9),
               tableOutput("tabSim.Ets"),
               textOutput("textIntro1.Ets"),
               textOutput("textIntro2.Ets"),
               textOutput("textIntro3.Ets"),
               textOutput("textIntro4.Ets"),
               selectInput('y.Ets', 'Year', 1960:2015, 2010),
               tableOutput('valY.Ets'),
               numericInput('v1.Ets',sort(myInd.Ets)[1],0,step = 0.1),
               numericInput('v2.Ets',sort(myInd.Ets)[2],0,step = 0.1),
               numericInput('v3.Ets',sort(myInd.Ets)[3],0,step = 0.1),
               numericInput('v4.Ets',sort(myInd.Ets)[4],0,step = 0.1),
               numericInput('v5.Ets',sort(myInd.Ets)[5],0,step = 0.1),
               textOutput('pred.Ets'),
               checkboxInput('showDet.Ets','Show technical details',value = FALSE),
               textOutput("textDet.Ets")
      )          
      # end ----
    )
  )
)

shinyApp(ui = ui, server = server)