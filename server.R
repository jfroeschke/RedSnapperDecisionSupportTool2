## server.R

library(shinydashboard)

server <- function(input, output, session) {  
  
################# Landings chart ############################
  selectYears <- reactive({
                    
     if(input$Alt2Radio=="Option 2a: 1986 - 2015"){
       allRec <- Private
       allRec <- filter(allRec, YEAR >= 1986 & YEAR <=2015)}
    if(input$Alt2Radio=="Option 2b: 1996 - 2015"){
      allRec <- Private
      allRec <- filter(allRec, YEAR >= 1996 & YEAR <=2015)}
    if(input$Alt2Radio=="Option 2c 2006 - 2015"){
      allRec <- Private
      allRec <- filter(allRec, YEAR >= 2006 & YEAR <=2015)}
    if(input$Alt2Radio=="Option 2d: 50% of average historical landings for the years 1986-2015 and 50% of average historical landings for the years 2006-2015"){
      allRec <- Private
      allRec <- filter(allRec, YEAR >= 1986 & YEAR <=2015)}
    
    allRec
    })
  
  
  LD <- reactive({
    allRec <- selectYears()
    x <-data.frame(YEAR=1986:2015)
    allRec <- merge(x, allRec, by='YEAR', all=TRUE) #used to keep xlims constant
    recLandingsPlot <- highchart() %>% 
      hc_title(text="Private recreational component") %>% 
      hc_xAxis(categories =allRec$YEAR) %>%
      hc_add_series(name = "Florida", data = allRec$FLW, type="line", marker = list(enabled = FALSE), color="#fb9a99") %>% 
      hc_add_series(name = "Alabama", data = allRec$AL, type="line", marker = list(enabled = FALSE), color="#33a02c") %>% 
      hc_add_series(name = "Mississippi", data = allRec$MS, type="line", marker = list(enabled = FALSE), color="#b2df8a") %>% 
      hc_add_series(name = "Louisiana", data = allRec$LA, type="line", marker = list(enabled = FALSE), color="#1f78b4") %>% 
      hc_add_series(name = "Texas", data = allRec$TX, type="line", marker = list(enabled = FALSE), color="#a6cee3") %>% 
      hc_add_series(name = "DWH, 2010 not included", data = allRec$star, type="scatter", color="black", marker=list(symbol="cross")) 
    recLandingsPlot
  })
  
  output$landingsChart <- renderHighchart({LD()})
################# Landings chart ############################
  landingsSummary <- reactive({
    
    if(input$Alt2Radio=="Option 2a: 1986 - 2015"){
      allRec <- Private
      allRec <- filter(allRec, YEAR >= 1986 & YEAR <=2015 & YEAR!=2010)
      x <- select(allRec, -star) %>% 
        melt(id="YEAR")
      colnames(x) <- c("Year", "State", "Landings")
      x2 <- group_by(x, State) %>% 
        summarise(Landings=mean(Landings) )
      x2$Allocation <- (x2$Landings/sum(x2$Landings))*100
      x2$State <- c("FL", "AL", "MS", "LA", "TX")
      x2
      }
    if(input$Alt2Radio=="Option 2b: 1996 - 2015"){
      allRec <- Private
      allRec <- filter(allRec, YEAR >= 1996 & YEAR <=2015 & YEAR!=2010)
      x <- select(allRec, -star) %>% 
        melt(id="YEAR")
      colnames(x) <- c("Year", "State", "Landings")
      x2 <- group_by(x, State) %>% 
        summarise(Landings=mean(Landings) )
      x2$Allocation <- (x2$Landings/sum(x2$Landings))*100
      x2}
    if(input$Alt2Radio=="Option 2c 2006 - 2015"){
      allRec <- Private
      allRec <- filter(allRec, YEAR >= 2006 & YEAR <=2015 & YEAR!=2010)
      x <- select(allRec, -star) %>% 
        melt(id="YEAR")
      colnames(x) <- c("Year", "State", "Landings")
      x2 <- group_by(x, State) %>% 
        summarise(Landings=mean(Landings) )
      x2$Allocation <- (x2$Landings/sum(x2$Landings))*100
      x2$State <- c("FL", "AL", "MS", "LA", "TX")
      x2}
    
    ####Separate section to calculate weighted mean
    if(input$Alt2Radio=="Option 2d: 50% of average historical landings for the years 1986-2015 and 50% of average historical landings for the years 2006-2015"){
      allRec <- Private
      allRec <- filter(allRec, YEAR >= 1986 & YEAR <=2015 & YEAR!=2010)#}
    x <- select(allRec, -star) %>% 
      melt(id="YEAR")
    colnames(x) <- c("Year", "State", "Landings")
    x2 <- group_by(x, State) %>% 
      summarise(Landings=mean(Landings) )
    x2$Allocation <- (x2$Landings/sum(x2$Landings))*100
    x3 <- filter(x, Year>=2006)
    x4 <- group_by(x3, State) %>% 
      summarise(Landings=mean(Landings) )
    x4$Allocation <- (x4$Landings/sum(x4$Landings))*100
    xout <- (x2$Allocation*.5 + x4$Allocation*.5)
    x2$Allocation <- xout}
    x2$State <- c("FL", "AL", "MS", "LA", "TX")
    x2$Allocation <- sprintf("%1.1f%%", x2$Allocation)
    x2})
  
  
################# Summary table
  output$summaryTable <- renderTable({landingsSummary()[,c(1,3)]},caption = "Private recreational component",
                                     caption.placement = getOption("xtable.caption.placement", "top"),
                                     caption.width = getOption("xtable.caption.width", NULL))
  ############################  
  ################# Landings chart For Hire ############################
  selectYearsForHire <- reactive({
    
    if(input$Alt2Radio=="Option 2a: 1986 - 2015"){
      allRec <- ForHire
      allRec <- filter(allRec, YEAR >= 1986 & YEAR <=2015)}
    if(input$Alt2Radio=="Option 2b: 1996 - 2015"){
      allRec <- ForHire
      allRec <- filter(allRec, YEAR >= 1996)}
    if(input$Alt2Radio=="Option 2c 2006 - 2015"){
      allRec <- ForHire
      allRec <- filter(allRec, YEAR >= 2006)}
    if(input$Alt2Radio=="Option 2d: 50% of average historical landings for the years 1986-2015 and 50% of average historical landings for the years 2006-2015"){
      allRec <- ForHire
      allRec <- filter(allRec, YEAR >= 1986)}
    
    allRec
  })
  
  
  LDForHire <- reactive({
    
    allRec <- selectYearsForHire()
    x <-data.frame(YEAR=1986:2015)
    allRec <- merge(x, allRec, by='YEAR', all=TRUE)
    recLandingsPlot <- highchart() %>% 
      hc_title(text="For hire recreational component") %>% 
      hc_xAxis(categories =allRec$YEAR) %>%
      # hc_xAxis(categories =allRec$YEAR,
      #          plotBands=list(
      #            list(color= "rgba(100, 0, 0, 0.1)",
      #                 from=allRec$YEAR[29],
      #                 to=allRec$YEAR[30]))) %>%
      hc_add_series(name = "Florida", data = allRec$FLW, type="line", marker = list(enabled = FALSE), color="#fb9a99") %>% 
      hc_add_series(name = "Alabama", data = allRec$AL, type="line", marker = list(enabled = FALSE), color="#33a02c") %>% 
      hc_add_series(name = "Mississippi", data = allRec$MS, type="line", marker = list(enabled = FALSE), color="#b2df8a") %>% 
      hc_add_series(name = "Louisiana", data = allRec$LA, type="line", marker = list(enabled = FALSE), color="#1f78b4") %>% 
      hc_add_series(name = "Texas", data = allRec$TX, type="line", marker = list(enabled = FALSE), color="#a6cee3") %>% 
      hc_add_series(name = "DWH, 2010 not included", data = allRec$star, type="scatter", color="black", marker=list(symbol="cross")) 
    recLandingsPlot
  })
  
  output$landingsChartForHire <- renderHighchart({LDForHire()})
  ################# Landings chart ############################
  landingsSummaryForHire <- reactive({
    
    if(input$Alt2Radio=="Option 2a: 1986 - 2015"){
      allRec <- ForHire
      allRec <- filter(allRec, YEAR >= 1986 & YEAR <=2015 & YEAR!=2010)
      x <- select(allRec, -star) %>% 
        melt(id="YEAR")
      colnames(x) <- c("Year", "State", "Landings")
      x2 <- group_by(x, State) %>% 
        summarise(Landings=mean(Landings) )
      x2$Allocation <- (x2$Landings/sum(x2$Landings))*100
      x2$State <- c("FL", "AL", "MS", "LA", "TX")
      x2
    }
    if(input$Alt2Radio=="Option 2b: 1996 - 2015"){
      allRec <- ForHire
      allRec <- filter(allRec, YEAR >= 1996 & YEAR <=2015 & YEAR!=2010)
      x <- select(allRec, -star) %>% 
        melt(id="YEAR")
      colnames(x) <- c("Year", "State", "Landings")
      x2 <- group_by(x, State) %>% 
        summarise(Landings=mean(Landings) )
      x2$Allocation <- (x2$Landings/sum(x2$Landings))*100
      x2$State <- c("FL", "AL", "MS", "LA", "TX")
      x2}
    if(input$Alt2Radio=="Option 2c 2006 - 2015"){
      allRec <- ForHire
      allRec <- filter(allRec, YEAR >= 2006 & YEAR <=2015 & YEAR!=2010)
      x <- select(allRec, -star) %>% 
        melt(id="YEAR")
      colnames(x) <- c("Year", "State", "Landings")
      x2 <- group_by(x, State) %>% 
        summarise(Landings=mean(Landings) )
      x2$Allocation <- (x2$Landings/sum(x2$Landings))*100
      x2$State <- c("FL", "AL", "MS", "LA", "TX")
            x2}
    
    ####Separate section to calculate weighted mean
    if(input$Alt2Radio=="Option 2d: 50% of average historical landings for the years 1986-2015 and 50% of average historical landings for the years 2006-2015"){
      allRec <- ForHire
      allRec <- filter(allRec, YEAR >= 1986 & YEAR <=2015 & YEAR!=2010)#}
      x <- select(allRec, -star) %>% 
        melt(id="YEAR")
      colnames(x) <- c("Year", "State", "Landings")
      x2 <- group_by(x, State) %>% 
        summarise(Landings=mean(Landings) )
      x2$Allocation <- (x2$Landings/sum(x2$Landings))*100
      x3 <- filter(x, Year>=2006)
      x4 <- group_by(x3, State) %>% 
        summarise(Landings=mean(Landings) )
      x4$Allocation <- (x4$Landings/sum(x4$Landings))*100
      xout <- (x2$Allocation*.5 + x4$Allocation*.5)
      x2$Allocation <- xout}
    x2$State <- c("FL", "AL", "MS", "LA", "TX")
    x2$Allocation <- sprintf("%1.1f%%", x2$Allocation)
    x2 <- x2[,c(1,3)]
    x2})
  
  # landingsSummaryChartForHire <- reactive({
  #   x2 <-  landingsSummary()
  #   hc <- highchart() %>% 
  #     hc_chart(type = "column") %>% 
  #     hc_title(text = "Percent Allocation") %>% 
  #     hc_xAxis(categories = x2$State) %>% 
  #     hc_add_series(data = x2$Allocation,
  #                   name = "Allocation")
  #   hc
  # })
  
  #output$summaryChartForHire <- renderHighchart({landingsSummaryChartForHire()})
  
  ################# Summary chart ############################
  
  ################# Summary table
  output$summaryTableForHire <- renderTable({landingsSummaryForHire()},
                                            caption = "For hire component",
                                            caption.placement = getOption("xtable.caption.placement", "top"),
                                            caption.width = getOption("xtable.caption.width", NULL))
 
###################################### Alt 3 ###########################
  ################# Landings chart ############################
  selectYearsAlt3 <- reactive({
    
    if(input$Alt3Radio=="Option 3a: 1986 - 2009"){
      allRec <- Private
      allRec <- filter(allRec, YEAR >= 1986 & YEAR <=2009)}
    if(input$Alt3Radio=="Option 3b: 1996 - 2009"){
      allRec <- Private
      allRec <- filter(allRec, YEAR >= 1996 & YEAR <=2009)}
    if(input$Alt3Radio=="Option 3c: 2006 - 2009"){
      allRec <- Private
      allRec <- filter(allRec, YEAR >= 2006 & YEAR <=2009)}
    if(input$Alt3Radio=="Option 3d: 50% of average historical landings for the years 1986-2009 and 50% of average historical landings for the years 2006-2009"){
      allRec <- Private
      allRec <- filter(allRec, YEAR >= 1986 & YEAR <=2009)}
    
    allRec
  })
  
  
  LDAlt3 <- reactive({
    
    allRec <- selectYearsAlt3()
    x <-data.frame(YEAR=1986:2015)
    allRec <- merge(x, allRec, by='YEAR', all=TRUE) #used to keep xlims constant
    
    recLandingsPlot <- highchart() %>% 
      hc_xAxis(categories =allRec$YEAR) %>%
      hc_title(text="Private recreational component") %>% 
      hc_add_series(name = "Florida", data = allRec$FLW, type="line", marker = list(enabled = FALSE), color="#fb9a99") %>% 
      hc_add_series(name = "Alabama", data = allRec$AL, type="line", marker = list(enabled = FALSE), color="#33a02c") %>% 
      hc_add_series(name = "Mississippi", data = allRec$MS, type="line", marker = list(enabled = FALSE), color="#b2df8a") %>% 
      hc_add_series(name = "Louisiana", data = allRec$LA, type="line", marker = list(enabled = FALSE), color="#1f78b4") %>% 
      hc_add_series(name = "Texas", data = allRec$TX, type="line", marker = list(enabled = FALSE), color="#a6cee3") #%>% 
    #hc_add_series(name = "DWH, 2010 not included", data = Private$star, type="scatter", color="black", marker = list(enabled = TRUE)) %>% 

    recLandingsPlot
  })
  
  output$landingsChartAlt3 <- renderHighchart({LDAlt3()})
  ################# Landings chart ############################
  landingsSummaryAlt3 <- reactive({
    
    if(input$Alt3Radio=="Option 3a: 1986 - 2009"){
      allRec <- Private
      allRec <- filter(allRec, YEAR >= 1986 & YEAR <=2009)
      x <- select(allRec, -star) %>% 
        melt(id="YEAR")
      colnames(x) <- c("Year", "State", "Landings")
      x2 <- group_by(x, State) %>% 
        summarise(Landings=mean(Landings) )
      x2$Allocation <- (x2$Landings/sum(x2$Landings))*100
      x2
    }
    if(input$Alt3Radio=="Option 3b: 1996 - 2009"){
      allRec <- Private
      allRec <- filter(allRec, YEAR >= 1996 & YEAR <=2009)
      x <- select(allRec, -star) %>% 
        melt(id="YEAR")
      colnames(x) <- c("Year", "State", "Landings")
      x2 <- group_by(x, State) %>% 
        summarise(Landings=mean(Landings) )
      x2$Allocation <- (x2$Landings/sum(x2$Landings))*100
      x2}
    if(input$Alt3Radio=="Option 3c: 2006 - 2009"){
      allRec <- Private
      allRec <- filter(allRec, YEAR >= 2006 & YEAR <=2009)
      x <- select(allRec, -star) %>% 
        melt(id="YEAR")
      colnames(x) <- c("Year", "State", "Landings")
      x2 <- group_by(x, State) %>% 
        summarise(Landings=mean(Landings) )
      x2$Allocation <- (x2$Landings/sum(x2$Landings))*100
      x2$State <- c("FL", "AL", "MS", "LA", "TX")
      x2} 
    
    ####Separate section to calculate weighted mean
    if(input$Alt3Radio=="Option 3d: 50% of average historical landings for the years 1986-2009 and 50% of average historical landings for the years 2006-2009"){
      allRec <- Private
      allRec <- filter(allRec, YEAR >= 1986 & YEAR <=2009)#}
      x <- select(allRec, -star) %>% 
        melt(id="YEAR")
      colnames(x) <- c("Year", "State", "Landings")
      x2 <- group_by(x, State) %>% 
        summarise(Landings=mean(Landings) )
      x2$Allocation <- (x2$Landings/sum(x2$Landings))*100
      x3 <- filter(x, Year>=2006)
      x4 <- group_by(x3, State) %>% 
        summarise(Landings=mean(Landings) )
      x4$Allocation <- (x4$Landings/sum(x4$Landings))*100
      xout <- (x2$Allocation*.5 + x4$Allocation*.5)
      x2$Allocation <- xout}
    x2$State <- c("FL", "AL", "MS", "LA", "TX")
    x2$Allocation <- sprintf("%1.1f%%", x2$Allocation)
    x2 <- x2[,c(1,3)]
    x2})
  
  # landingsSummaryChartAlt3 <- reactive({
  #   x2 <-  landingsSummaryAlt3()
  #   hc <- highchart() %>% 
  #     hc_chart(type = "column") %>% 
  #     hc_title(text = "Percent Allocation") %>% 
  #     hc_xAxis(categories = x2$State) %>% 
  #     hc_add_series(data = x2$Allocation,
  #                   name = "Allocation")
  #   hc
  # })
  # 
  # output$summaryChartAlt3 <- renderHighchart({landingsSummaryChartAlt3()})
  
  ################# Summary chart ############################
  
  ################# Summary table
  output$summaryTableAlt3 <- renderTable({landingsSummaryAlt3()},caption = "Private recreational component",
                                         caption.placement = getOption("xtable.caption.placement", "top"),
                                         caption.width = getOption("xtable.caption.width", NULL))
  ############################  
  ################# Landings chart For Hire ############################
  selectYearsForHireAlt3 <- reactive({
    
    if(input$Alt3Radio=="Option 3a: 1986 - 2009"){
      allRec <- ForHire
      allRec <- filter(allRec, YEAR >= 1986 & YEAR <=2009)} else
    if(input$Alt3Radio=="Option 3b: 1996 - 2009"){
      allRec <- ForHire
      allRec <- filter(allRec, YEAR >= 1996 & YEAR <=2009)} else
    if(input$Alt3Radio=="Option 3c: 2006 - 2009"){
      allRec <- ForHire
      allRec <- filter(allRec, YEAR >= 2006 & YEAR <=2009)} else
    if(input$Alt3Radio=="Option 3d: 50% of average historical landings for the years 1986-2009 and 50% of average historical landings for the years 2006-2009"){
      allRec <- ForHire
      allRec <- filter(allRec, YEAR >= 1986 & YEAR <=2009)} 
    
    allRec
  })
  
  
  LDForHireAlt3 <- reactive({
    
    allRec <- selectYearsForHireAlt3()
    x <-data.frame(YEAR=1986:2015)
    allRec <- merge(x, allRec, by='YEAR', all=TRUE)
    
    recLandingsPlot <- highchart() %>% 
      hc_xAxis(categories =allRec$YEAR) %>%
      hc_title(text="For hire component") %>% 
      # hc_xAxis(categories =allRec$YEAR,
      #          plotBands=list(
      #            list(color= "rgba(100, 0, 0, 0.1)",
      #                 from=allRec$YEAR[29],
      #                 to=allRec$YEAR[30]))) %>%
      hc_add_series(name = "Florida", data = allRec$FLW, type="line", marker = list(enabled = FALSE), color="#fb9a99") %>% 
      hc_add_series(name = "Alabama", data = allRec$AL, type="line", marker = list(enabled = FALSE), color="#33a02c") %>% 
      hc_add_series(name = "Mississippi", data = allRec$MS, type="line", marker = list(enabled = FALSE), color="#b2df8a") %>% 
      hc_add_series(name = "Louisiana", data = allRec$LA, type="line", marker = list(enabled = FALSE), color="#1f78b4") %>% 
      hc_add_series(name = "Texas", data = allRec$TX, type="line", marker = list(enabled = FALSE), color="#a6cee3") #%>% 
    #hc_add_series(name = "DWH, 2010 not included", data = allRec$star, type="scatter",color="black") 
    recLandingsPlot
  })
  
  output$landingsChartForHireAlt3 <- renderHighchart({LDForHireAlt3()})
  ################# Landings chart ############################
  landingsSummaryForHireAlt3 <- reactive({
    
    if(input$Alt3Radio=="Option 3a: 1986 - 2009"){
      allRec <- ForHire
      allRec <- filter(allRec, YEAR >= 1986 & YEAR <=2009)
      x <- select(allRec, -star) %>% 
        melt(id="YEAR")
      colnames(x) <- c("Year", "State", "Landings")
      x2 <- group_by(x, State) %>% 
        summarise(Landings=mean(Landings) )
      x2$Allocation <- (x2$Landings/sum(x2$Landings))*100
      x2$State <- c("FL", "AL", "MS", "LA", "TX")
      x2
    }
    if(input$Alt3Radio=="Option 3b: 1996 - 2009"){
      allRec <- ForHire
      allRec <- filter(allRec, YEAR >= 1996 & YEAR <=2009)
      x <- select(allRec, -star) %>% 
        melt(id="YEAR")
      colnames(x) <- c("Year", "State", "Landings")
      x2 <- group_by(x, State) %>% 
        summarise(Landings=mean(Landings) )
      x2$Allocation <- (x2$Landings/sum(x2$Landings))*100
      x2}
    if(input$Alt3Radio=="Option 3c: 2006 - 2009"){
      allRec <- ForHire
      allRec <- filter(allRec, YEAR >= 2006 & YEAR <=2009)
      x <- select(allRec, -star) %>% 
        melt(id="YEAR")
      colnames(x) <- c("Year", "State", "Landings")
      x2 <- group_by(x, State) %>% 
        summarise(Landings=mean(Landings) )
      x2$Allocation <- (x2$Landings/sum(x2$Landings))*100
      x2}
    
    ####Separate section to calculate weighted mean
    if(input$Alt3Radio=="Option 3d: 50% of average historical landings for the years 1986-2009 and 50% of average historical landings for the years 2006-2009"){
      allRec <- ForHire
      allRec <- filter(allRec, YEAR >= 1986 & YEAR <=2009)#}
      x <- select(allRec, -star) %>% 
        melt(id="YEAR")
      colnames(x) <- c("Year", "State", "Landings")
      x2 <- group_by(x, State) %>% 
        summarise(Landings=mean(Landings) )
      x2$Allocation <- (x2$Landings/sum(x2$Landings))*100
      x3 <- filter(x, Year>=2006)
      x4 <- group_by(x3, State) %>% 
        summarise(Landings=mean(Landings) )
      x4$Allocation <- (x4$Landings/sum(x4$Landings))*100
      xout <- (x2$Allocation*.5 + x4$Allocation*.5)
      x2$Allocation <- xout}
    x2$State <- c("FL", "AL", "MS", "LA", "TX")
    x2$Allocation <- sprintf("%1.1f%%", x2$Allocation)
    x2 <- x2[,c(1,3)]
    x2})
  
  # landingsSummaryChartForHireAlt3 <- reactive({
  #   x2 <-  landingsSummaryForHireAlt3()
  #   hc <- highchart() %>% 
  #     hc_chart(type = "column") %>% 
  #     hc_title(text = "Percent Allocation") %>% 
  #     hc_xAxis(categories = x2$State) %>% 
  #     hc_add_series(data = x2$Allocation,
  #                   name = "Allocation")
  #   hc
  # })
  
  # output$summaryChartForHireAlt3 <- renderHighchart({landingsSummaryChartForHireAlt3()})
  
  ################# Summary table
  output$summaryTableForHireAlt3 <- renderTable({landingsSummaryForHireAlt3()},caption = "For hire component",
                                         caption.placement = getOption("xtable.caption.placement", "top"),
                                         caption.width = getOption("xtable.caption.width", NULL))
  ###################################### End Alt 3 ###########################  
  ###################################### Alt 4 ###########################
  selectYearsAlt4 <- reactive({
    allRec <- Private
    allRec <- filter(allRec, YEAR>= 1986 & YEAR <=2015)
    
    ##Select start year (i.e., options for Alternatives 2 and 3
    ## OptionD will require additional work in the allocation calculatin
    ## based on the checkbox selectet
    if(input$selectOption == "OptionA"){
      allRec <- filter(allRec, YEAR>= 1986)}
    if(input$selectOption == "OptionB"){
      allRec <- filter(allRec, YEAR>= 1996)}
    if(input$selectOption == "OptionC"){
      allRec <- filter(allRec, YEAR>= 2006)}
    if(input$selectOption == "OptionD"){
      allRec <- filter(allRec, YEAR>= 1986)}
    
    ## Select End Year: Applies to either Alternative 2 (2015) or 
    ## Alternative 3 (2009)
    if(input$selectAlternative == "ALT2"){
      allRec <- filter(allRec, !(YEAR %in% input$ALT2))
      #ifelse(allRec$YEAR %in% input$ALT2, NA, allRec$YEAR)
    }
    if(input$selectAlternative == "ALT3"){
      allRec <- filter(allRec, YEAR <= 2009)
      allRec <- filter(allRec, !(YEAR %in% input$ALT3)) 
    }
    #     if(input$ALT2
    #     allRec <- filter(allRec, YEAR %in% 2015)
    # }
    allRec
  })
  
  #### Produce chart of landings for appropriate time series selecte
  LDAlt4 <- reactive({

    allRec <- selectYearsAlt4()
    ## This added to allow plotting of modified time series
    x <-data.frame(YEAR=1986:2015)
    allRec <- merge(x, allRec, by='YEAR', all=TRUE)
    ## see comment above
    recLandingsPlot <- highchart() %>%
      hc_xAxis(categories =allRec$YEAR) %>%
      hc_title(text="Private recreational component") %>% 
      hc_add_series(name = "Florida", data = allRec$FLW, type="line", marker = list(enabled = FALSE), color="#fb9a99") %>%
      hc_add_series(name = "Alabama", data = allRec$AL, type="line", marker = list(enabled = FALSE), color="#33a02c") %>%
      hc_add_series(name = "Mississippi", data = allRec$MS, type="line", marker = list(enabled = FALSE), color="#b2df8a") %>%
      hc_add_series(name = "Louisiana", data = allRec$LA, type="line", marker = list(enabled = FALSE), color="#1f78b4") %>%
      hc_add_series(name = "Texas", data = allRec$TX, type="line", marker = list(enabled = FALSE), color="#a6cee3") %>%
      hc_add_series(name = "DWH, 2010 not included", data = allRec$star, type="scatter", type="scatter", color="black", marker=list(symbol="cross"))
    recLandingsPlot
  })
  output$landingsChartAlt4 <- renderHighchart({LDAlt4()})
      output$test <- renderTable({selectYearsAlt4()})
      
      alt4summaryTablePrivate <- reactive({
        df <- selectYearsAlt4()
     
        if(input$selectOption == "OptionA"){
          x <- select(df, -star) %>%
            melt(id="YEAR")
          colnames(x) <- c("Year", "State", "Landings")
          x2 <- group_by(x, State) %>%
            summarise(Landings=mean(Landings) )
          x2$Allocation <- (x2$Landings/sum(x2$Landings))*100
          x2$State <- c("FL", "AL", "MS", "LA", "TX")
          x2$Allocation <- sprintf("%1.1f%%", x2$Allocation)
          x2} else 
              
              if(input$selectOption == "OptionB"){
                x <- select(df, -star) %>%
                  melt(id="YEAR")
                colnames(x) <- c("Year", "State", "Landings")
                x2 <- group_by(x, State) %>%
                  summarise(Landings=mean(Landings) )
                x2$Allocation <- (x2$Landings/sum(x2$Landings))*100
                x2$State <- c("FL", "AL", "MS", "LA", "TX")
                x2$Allocation <- sprintf("%1.1f%%", x2$Allocation)
                x2} else 
                  
                  if(input$selectOption == "OptionC"){
                    x <- select(df, -star) %>%
                      melt(id="YEAR")
                    colnames(x) <- c("Year", "State", "Landings")
                    x2 <- group_by(x, State) %>%
                      summarise(Landings=mean(Landings) )
                    x2$Allocation <- (x2$Landings/sum(x2$Landings))*100
                    x2$State <- c("FL", "AL", "MS", "LA", "TX")
                    x2$Allocation <- sprintf("%1.1f%%", x2$Allocation)
                    x2}  else
                      if(input$selectOption == "OptionD"){
                        df <- selectYearsAlt4()
                        x <- select(df, -star) %>% 
                          melt(id="YEAR")
                        colnames(x) <- c("Year", "State", "Landings")
                        x2 <- group_by(x, State) %>% 
                          summarise(Landings=mean(Landings) )
                        x2$Allocation <- (x2$Landings/sum(x2$Landings))*100
                        x3 <- filter(x, Year>=2006)
                        x4 <- group_by(x3, State) %>% 
                          summarise(Landings=mean(Landings) )
                        x4$Allocation <- (x4$Landings/sum(x4$Landings))*100
                        xout <- (x2$Allocation*.5 + x4$Allocation*.5)
                        x2$Allocation <- xout
                               # x2$State <- c("Florida", "Alabama", "Mississippi", "Louisiana", "Texas")
                        x2$Allocation <- sprintf("%1.1f%%", x2$Allocation)
                          
                        x2}
     
        })

      # ################# Summary table
      output$summaryTableAlt4Private <- renderTable({alt4summaryTablePrivate()[,c(1,3)]},
                                                    striped=TRUE,digits=1,
                                                    caption = "Private recreational component",
                                                    caption.placement = getOption("xtable.caption.placement", "top"),
                                                    caption.width = getOption("xtable.caption.width", NULL))
      #}
      ############################ Alt 4 For hire
      
      ###################################### Alt 4 ###########################
      selectYearsAlt4ForHire <- reactive({
        allRec <- ForHire
        allRec <- filter(allRec, YEAR>= 1986 & YEAR <=2015)
        
        ##Select start year (i.e., options for Alternatives 2 and 3
        ## OptionD will require additional work in the allocation calculatin
        ## based on the checkbox selectet
        if(input$selectOption == "OptionA"){
          allRec <- filter(allRec, YEAR>= 1986)}
        if(input$selectOption == "OptionB"){
          allRec <- filter(allRec, YEAR>= 1996)}
        if(input$selectOption == "OptionC"){
          allRec <- filter(allRec, YEAR>= 2006)}
        if(input$selectOption == "OptionD"){
          allRec <- filter(allRec, YEAR>= 1986)}
        
        ## Select End Year: Applies to either Alternative 2 (2015) or 
        ## Alternative 3 (2009)
        if(input$selectAlternative == "ALT2"){
          allRec <- filter(allRec, !(YEAR %in% input$ALT2))
          #ifelse(allRec$YEAR %in% input$ALT2, NA, allRec$YEAR)
        }
        if(input$selectAlternative == "ALT3"){
          allRec <- filter(allRec, YEAR <= 2009)
          allRec <- filter(allRec, !(YEAR %in% input$ALT3)) 
        }
 
        allRec
      })
      
      #### Produce chart of landings for appropriate time series selecte
      LDAlt4ForHire <- reactive({
        
        allRec <- selectYearsAlt4ForHire()
        ## This added to allow plotting of modified time series
        x <-data.frame(YEAR=1986:2015)
        allRec <- merge(x, allRec, by='YEAR', all=TRUE)
        ## see comment above
        recLandingsPlot <- highchart() %>%
          hc_xAxis(categories =allRec$YEAR) %>%
          hc_title(text="For hire recreational component") %>% 
          hc_add_series(name = "Florida", data = allRec$FLW, type="line", marker = list(enabled = FALSE), color="#fb9a99") %>%
          hc_add_series(name = "Alabama", data = allRec$AL, type="line", marker = list(enabled = FALSE), color="#33a02c") %>%
          hc_add_series(name = "Mississippi", data = allRec$MS, type="line", marker = list(enabled = FALSE), color="#b2df8a") %>%
          hc_add_series(name = "Louisiana", data = allRec$LA, type="line", marker = list(enabled = FALSE), color="#1f78b4") %>%
          hc_add_series(name = "Texas", data = allRec$TX, type="line", marker = list(enabled = FALSE), color="#a6cee3") %>%
          hc_add_series(name = "DWH, 2010 not included", data = allRec$star, type="scatter", color="black", marker=list(symbol="cross"))
        recLandingsPlot
      })
      output$landingsChartAlt4ForHire <- renderHighchart({LDAlt4ForHire()})
      output$testForHire <- renderTable({selectYearsAlt4ForHire()})
      
      alt4summaryTableForHire <- reactive({
        df <- selectYearsAlt4ForHire()
        
        if(input$selectOption == "OptionA"){
          x <- select(df, -star) %>%
            melt(id="YEAR")
          colnames(x) <- c("Year", "State", "Landings")
          x2 <- group_by(x, State) %>%
            summarise(Landings=mean(Landings) )
          x2$Allocation <- (x2$Landings/sum(x2$Landings))*100
          x2$State <- c("FL", "AL", "MS", "LA", "TX")
          x2$Allocation <- sprintf("%1.1f%%", x2$Allocation)
          x2} else 
            
            if(input$selectOption == "OptionB"){
              x <- select(df, -star) %>%
                melt(id="YEAR")
              colnames(x) <- c("Year", "State", "Landings")
              x2 <- group_by(x, State) %>%
                summarise(Landings=mean(Landings) )
              x2$Allocation <- (x2$Landings/sum(x2$Landings))*100
              x2$State <- c("FL", "AL", "MS", "LA", "TX")
              x2$Allocation <- sprintf("%1.1f%%", x2$Allocation)
              x2} else 
                
                if(input$selectOption == "OptionC"){
                  x <- select(df, -star) %>%
                    melt(id="YEAR")
                  colnames(x) <- c("Year", "State", "Landings")
                  x2 <- group_by(x, State) %>%
                    summarise(Landings=mean(Landings) )
                  x2$Allocation <- (x2$Landings/sum(x2$Landings))*100
                  x2$State <- c("FL", "AL", "MS", "LA", "TX")
                  x2$Allocation <- sprintf("%1.1f%%", x2$Allocation)
                  x2}  else
                    if(input$selectOption == "OptionD"){
                      df <- selectYearsAlt4()
                      x <- select(df, -star) %>% 
                        melt(id="YEAR")
                      colnames(x) <- c("Year", "State", "Landings")
                      x2 <- group_by(x, State) %>% 
                        summarise(Landings=mean(Landings) )
                      x2$Allocation <- (x2$Landings/sum(x2$Landings))*100
                      x3 <- filter(x, Year>=2006)
                      x4 <- group_by(x3, State) %>% 
                        summarise(Landings=mean(Landings) )
                      x4$Allocation <- (x4$Landings/sum(x4$Landings))*100
                      xout <- (x2$Allocation*.5 + x4$Allocation*.5)
                      x2$Allocation <- xout
                      x2$State <- c("FL", "AL", "MS", "LA", "TX")
                      x2$Allocation <- sprintf("%1.1f%%", x2$Allocation)
                      x2}
        
      })
      
      # ################# Summary table
      output$summaryTableAlt4ForHire <- renderTable({alt4summaryTableForHire()},caption = "For hire recreational component",
                                                    caption.placement = getOption("xtable.caption.placement", "top"),
                                                    caption.width = getOption("xtable.caption.width", NULL))
      ############################ Alt 4 For hire 
  ###################################### End Alt 4 ###########################  
######################### Alternative 5
      topN <- reactive({
        allRec <- Private2
        allRec <- filter(allRec, YEAR>=1986 & YEAR <=2015)
        ## define input from sliders
        ## this reactive allows user to select the range of years 
        ## and the number of top years to be included
        N <- input$topNumber
        
        allRec <-   allRec %>% filter(YEAR <= input$Year[2] &  YEAR >= input$Year[1] & YEAR !=2010)
        
        allFLW <- allRec %>% 
          #filter(YEAR <= 2015 & YEAR >=1986 & YEAR !=2010) %>% #subset years
          dplyr::select(YEAR, FLW) %>% #select variables 
          arrange(desc(FLW)) %>% 
          slice(1:N) %>% ##top in will be here
          arrange(YEAR) %>% 
          summarise(sum=sum(FLW))    
        allAL <- allRec %>% 
          #filter(YEAR <= 2015 & YEAR >=1986 & YEAR !=2010) %>% #subset years
          dplyr::select(YEAR, AL) %>% #select variables 
          arrange(desc(AL)) %>% 
          slice(1:N) %>% ##top in will be here
          summarise(sum=sum(AL))  
        
        allMS <- allRec %>% 
          #filter(YEAR <= 2015 & YEAR >=1986 & YEAR !=2010) %>% #subset years
          dplyr::select(YEAR, MS) %>% #select variables 
          arrange(desc(MS)) %>% 
          slice(1:N) %>% ##top in will be here
          summarise(sum=sum(MS)) 
        
        allLA <- allRec %>% 
          #filter(YEAR <= 2015 & YEAR >=1986 & YEAR !=2010) %>% #subset years
          dplyr::select(YEAR, LA) %>% #select variables 
          arrange(desc(LA)) %>% 
          slice(1:N) %>% ##top in will be here
          summarise(sum=sum(LA)) 
        allTX <- allRec %>% 
          #filter(YEAR <= 2015 & YEAR >=1986 & YEAR !=2010) %>% #subset years
          dplyr::select(YEAR, TX) %>% #select variables 
          arrange(desc(TX)) %>% 
          slice(1:N) %>% ##top in will be here
          summarise(sum=sum(TX)) 
        totalSum <- sum(allFLW, allAL, allMS, allLA, allTX)
        topNout <- data.frame(FLW=(allFLW/totalSum), AL=(allAL/totalSum),
                              MS=(allMS/totalSum), LA=(allLA/totalSum),TX=(allTX/totalSum))
        colnames(topNout) <- c("FL","AL", "MS", "LA", "TX")
        x <- colnames(topNout)
        #topNout <- rbind(topNout,rev(Biomass$Biomass) )
        #rownames(topNout) <- c("Landings")
        topNout <- t(topNout)
        x2 <- data.frame(State=x, Percent=topNout[,1])
        rownames(x2) <- NULL
        x2$PercentACL <- x2$Percent*.577
        x2$Percent <- sprintf("%1.1f%%", x2$Percent*100)
        x2$PercentACL <- sprintf("%1.1f%%", x2$PercentACL*100)
        colnames(x2) <- c("State", "% Rec. ACL", "% Total ACL")
       x2
      })
      
      output$out32 <- renderTable({
        topN()
      },
      striped=TRUE,digits=1,
      caption = "Private recreational component",
      caption.placement = getOption("xtable.caption.placement", "top"),
      caption.width = getOption("xtable.caption.width", NULL))
      
######## Reactive for time series table of top N
      topNLandings <- reactive({
        df1<- data.frame(YEAR=1986:2015)
        
        df2 <- data.frame(YEAR=6:9, z=1)
        df3 <- merge(df1,df2, by='YEAR', all=TRUE)
        
        allRec <- Private2
        allRec <- filter(allRec, YEAR>=1986 & YEAR <=2015)
        N <- input$topNumber
        allRec <-   allRec %>% filter(YEAR <= input$Year[2] &  YEAR >= input$Year[1] & YEAR !=2010)
        
        allFLW <- allRec %>% 
          #filter(YEAR <= 2015 & YEAR >=1986 & YEAR !=2010) %>% #subset years
          dplyr::select(YEAR, FLW) %>% #select variables 
          arrange(desc(FLW)) %>% 
          slice(1:N) %>% ##top in will be here
          arrange(YEAR) 
        
        df3 <- merge(df1,allFLW, by='YEAR', all=TRUE)
        
        allAL <- allRec %>% 
          #filter(YEAR <= 2015 & YEAR >=1986 & YEAR !=2010) %>% #subset years
          dplyr::select(YEAR, AL) %>% #select variables 
          arrange(desc(AL)) %>% 
          slice(1:N)
        
        df3 <- merge(df3,allAL, by='YEAR', all=TRUE)
        
        allMS <- allRec %>% 
          #filter(YEAR <= 2015 & YEAR >=1986 & YEAR !=2010) %>% #subset years
          dplyr::select(YEAR, MS) %>% #select variables 
          arrange(desc(MS)) %>% 
          slice(1:N)
        
        df3 <- merge(df3,allMS, by='YEAR', all=TRUE)
        
        allLA <- allRec %>% 
          #filter(YEAR <= 2015 & YEAR >=1986 & YEAR !=2010) %>% #subset years
          dplyr::select(YEAR, LA) %>% #select variables 
          arrange(desc(LA)) %>% 
          slice(1:N)
        
        df3 <- merge(df3,allLA, by='YEAR', all=TRUE)
        
        allTX <- allRec %>% 
          #filter(YEAR <= 2015 & YEAR >=1986 & YEAR !=2010) %>% #subset years
          dplyr::select(YEAR, TX) %>% #select variables 
          arrange(desc(TX)) %>% 
          slice(1:N)
        
        df3 <- merge(df3,allTX, by='YEAR', all=TRUE)
        df3
      })
      
      output$topNdata <- renderTable({
        topNLandings()
      }, 
      striped=TRUE,digits=1,
      caption = "Private recreational component",
      caption.placement = getOption("xtable.caption.placement", "top"),
      caption.width = getOption("xtable.caption.width", NULL))
######################## End Reactive for time series table of top N
### time series chart of top N      
      topNLandings <- reactive({
        
        df1<- data.frame(YEAR=1986:2015)
        
        allRec <- Private2
        allRec <- filter(allRec, YEAR>=1986 & YEAR <=2015)
        allRec <- merge(df1,allRec, by='YEAR', all=TRUE)
        
        
        N <- input$topNumber
        allRec <-   allRec %>% filter(YEAR <= input$Year[2] &  YEAR >= input$Year[1] & YEAR !=2010)
        
        allFLW <- allRec %>% 
          #filter(YEAR <= 2015 & YEAR >=1986 & YEAR !=2010) %>% #subset years
          dplyr::select(YEAR, FLW) %>% #select variables 
          arrange(desc(FLW)) %>% 
          slice(1:N) %>% ##top in will be here
          arrange(YEAR) 
        
        df3 <- merge(df1,allFLW, by='YEAR', all=TRUE)
        
        allAL <- allRec %>% 
          #filter(YEAR <= 2015 & YEAR >=1986 & YEAR !=2010) %>% #subset years
          dplyr::select(YEAR, AL) %>% #select variables 
          arrange(desc(AL)) %>% 
          slice(1:N)
        
        df3 <- merge(df3,allAL, by='YEAR', all=TRUE)
        
        allMS <- allRec %>% 
          #filter(YEAR <= 2015 & YEAR >=1986 & YEAR !=2010) %>% #subset years
          dplyr::select(YEAR, MS) %>% #select variables 
          arrange(desc(MS)) %>% 
          slice(1:N)
        
        df3 <- merge(df3,allMS, by='YEAR', all=TRUE)
        
        allLA <- allRec %>% 
          #filter(YEAR <= 2015 & YEAR >=1986 & YEAR !=2010) %>% #subset years
          dplyr::select(YEAR, LA) %>% #select variables 
          arrange(desc(LA)) %>% 
          slice(1:N)
        
        df3 <- merge(df3,allLA, by='YEAR', all=TRUE)
        
        allTX <- allRec %>% 
          #filter(YEAR <= 2015 & YEAR >=1986 & YEAR !=2010) %>% #subset years
          dplyr::select(YEAR, TX) %>% #select variables 
          arrange(desc(TX)) %>% 
          slice(1:N)
        
        df3 <- merge(df3,allTX, by='YEAR', all=TRUE)
        df3
      })
      
      
      ########## test high chart of all landings
      topNLandingsPlot <- reactive({
        allRec <- Private2
        allRec <- filter(allRec, YEAR >=1986 & YEAR <=2015)
        hc <- highchart() %>% 
        hc_xAxis(categories =allRec$YEAR) %>%
        hc_title(text="Private recreational component") %>% 
        hc_add_series(name = "Florida", data = allRec$FLW, type="line", marker = list(enabled = FALSE), color="#fb9a99") %>% 
        hc_add_series(name = "Alabama", data = allRec$AL, type="line", marker = list(enabled = FALSE), color="#33a02c") %>%
        hc_add_series(name = "Mississippi", data = allRec$MS, type="line", marker = list(enabled = FALSE), color="#b2df8a") %>%
        hc_add_series(name = "Louisiana", data = allRec$LA, type="line", marker = list(enabled = FALSE), color="#1f78b4") %>%
        hc_add_series(name = "Texas", data = allRec$TX, type="line", marker = list(enabled = FALSE), color="#a6cee3") %>%
        hc_add_series(name = "Florida Selected", data = topNLandings()$FLW, type="scatter", color="#fb9a99",showInLegend = FALSE) %>%
        hc_add_series(name = "Alabama Selected", data = topNLandings()$AL, type="scatter", color="#33a02c",showInLegend = FALSE) %>%
        hc_add_series(name = "Mississippi Selected", data = topNLandings()$MS, type="scatter", color="#b2df8a",showInLegend = FALSE) %>%
        hc_add_series(name = "Louisiana Selected", data = topNLandings()$LA, type="scatter", color="#1f78b4",showInLegend = FALSE) %>%
        hc_add_series(name = "Texas Selected", data = topNLandings()$TX, type="scatter", color="#a6cee3",showInLegend = FALSE) %>% 
        hc_add_series(name = "DWH, 2010 not included", data = allRec$star, type="scatter", color="black", marker=list(symbol="cross")) 
      hc
      })
      output$topNlandingsOut <- renderHighchart({topNLandingsPlot()})
### time series chart of top N  
### For hire Alternative 5topNForHire <- reactive({
    topNForHire <- reactive({
      allRec <- ForHire2 #calculated on landing rather than proportions
      allRec <- filter(allRec, YEAR>=1986 & YEAR <=2015)
      ## define input from sliders
      ## this reactive allows user to select the range of years 
      ## and the number of top years to be included
      N <- input$topNumber
      
      allRec <-   allRec %>% filter(YEAR <= input$Year[2] &  YEAR >= input$Year[1] & YEAR !=2010)
      
      allFLW <- allRec %>% 
        #filter(YEAR <= 2015 & YEAR >=1986 & YEAR !=2010) %>% #subset years
        dplyr::select(YEAR, FLW) %>% #select variables 
        arrange(desc(FLW)) %>% 
        slice(1:N) %>% ##top in will be here
        arrange(YEAR) %>% 
        summarise(sum=sum(FLW))    
      allAL <- allRec %>% 
        #filter(YEAR <= 2015 & YEAR >=1986 & YEAR !=2010) %>% #subset years
        dplyr::select(YEAR, AL) %>% #select variables 
        arrange(desc(AL)) %>% 
        slice(1:N) %>% ##top in will be here
        summarise(sum=sum(AL))  
      
      allMS <- allRec %>% 
        #filter(YEAR <= 2015 & YEAR >=1986 & YEAR !=2010) %>% #subset years
        dplyr::select(YEAR, MS) %>% #select variables 
        arrange(desc(MS)) %>% 
        slice(1:N) %>% ##top in will be here
        summarise(sum=sum(MS)) 
      
      allLA <- allRec %>% 
        #filter(YEAR <= 2015 & YEAR >=1986 & YEAR !=2010) %>% #subset years
        dplyr::select(YEAR, LA) %>% #select variables 
        arrange(desc(LA)) %>% 
        slice(1:N) %>% ##top in will be here
        summarise(sum=sum(LA)) 
      allTX <- allRec %>% 
        #filter(YEAR <= 2015 & YEAR >=1986 & YEAR !=2010) %>% #subset years
        dplyr::select(YEAR, TX) %>% #select variables 
        arrange(desc(TX)) %>% 
        slice(1:N) %>% ##top in will be here
        summarise(sum=sum(TX)) 
      totalSum <- sum(allFLW, allAL, allMS, allLA, allTX)
      topNout <- data.frame(FLW=(allFLW/totalSum), AL=(allAL/totalSum),
                            MS=(allMS/totalSum), LA=(allLA/totalSum),TX=(allTX/totalSum))
      colnames(topNout) <- c("FL","AL", "MS", "LA", "TX")
      x <- colnames(topNout)
      #topNout <- rbind(topNout,rev(Biomass$Biomass) )
      #rownames(topNout) <- c("Landings")
      topNout <- t(topNout)
      x2 <- data.frame(State=x, Percent=topNout[,1])
      rownames(x2) <- NULL
      x2$PercentACL <- x2$Percent*.423
      x2$Percent <- sprintf("%1.1f%%", x2$Percent*100)
      x2$PercentACL <- sprintf("%1.1f%%", x2$PercentACL*100)
      colnames(x2) <- c("State", "% Rec. ACL", "% Total ACL")
      x2
})

output$out32ForHire <- renderTable({
  topNForHire()
},
striped=TRUE,digits=1,
caption = "For hire recreational component",
caption.placement = getOption("xtable.caption.placement", "top"),
caption.width = getOption("xtable.caption.width", NULL))

      topNLandingsForHire <- reactive({
        df1<- data.frame(YEAR=1986:2015)
        
        df2 <- data.frame(YEAR=6:9, z=1)
        df3 <- merge(df1,df2, by='YEAR', all=TRUE)
        
        allRec <- ForHire2
        allRec <- filter(allRec, YEAR>=1986 & YEAR <=2015)
        N <- input$topNumber
        allRec <-   allRec %>% filter(YEAR <= input$Year[2] &  YEAR >= input$Year[1] & YEAR !=2010)
        
        allFLW <- allRec %>% 
          #filter(YEAR <= 2015 & YEAR >=1986 & YEAR !=2010) %>% #subset years
          dplyr::select(YEAR, FLW) %>% #select variables 
          arrange(desc(FLW)) %>% 
          slice(1:N) %>% ##top in will be here
          arrange(YEAR) 
        
        df3 <- merge(df1,allFLW, by='YEAR', all=TRUE)
        
        allAL <- allRec %>% 
          #filter(YEAR <= 2015 & YEAR >=1986 & YEAR !=2010) %>% #subset years
          dplyr::select(YEAR, AL) %>% #select variables 
          arrange(desc(AL)) %>% 
          slice(1:N)
        
        df3 <- merge(df3,allAL, by='YEAR', all=TRUE)
        
        allMS <- allRec %>% 
          #filter(YEAR <= 2015 & YEAR >=1986 & YEAR !=2010) %>% #subset years
          dplyr::select(YEAR, MS) %>% #select variables 
          arrange(desc(MS)) %>% 
          slice(1:N)
        
        df3 <- merge(df3,allMS, by='YEAR', all=TRUE)
        
        allLA <- allRec %>% 
          #filter(YEAR <= 2015 & YEAR >=1986 & YEAR !=2010) %>% #subset years
          dplyr::select(YEAR, LA) %>% #select variables 
          arrange(desc(LA)) %>% 
          slice(1:N)
        
        df3 <- merge(df3,allLA, by='YEAR', all=TRUE)
        
        allTX <- allRec %>% 
          #filter(YEAR <= 2015 & YEAR >=1986 & YEAR !=2010) %>% #subset years
          dplyr::select(YEAR, TX) %>% #select variables 
          arrange(desc(TX)) %>% 
          slice(1:N)
        
        df3 <- merge(df3,allTX, by='YEAR', all=TRUE)
    
        df3
      })
      
      
      ########## test high chart of all landings
      topNLandingsPlotForHire <- reactive({
        allRec <- ForHire2
        allRec <- filter(allRec, YEAR >=1986 & YEAR <=2015)
        hc <- highchart() %>% 
          hc_xAxis(categories =allRec$YEAR) %>%
          hc_title(text="For hire component") %>% 
          # hc_xAxis(categories =allRec$YEAR,
          #          plotBands=list(
          #            list(color= "rgba(100, 0, 0, 0.1)",
          #                 from=allRec$YEAR[29],
          #                 to=allRec$YEAR[30]))) %>%
          hc_add_series(name = "Florida", data = allRec$FLW, type="line", marker = list(enabled = FALSE), color="#fb9a99") %>% 
          hc_add_series(name = "Alabama", data = allRec$AL, type="line", marker = list(enabled = FALSE), color="#33a02c") %>%
          hc_add_series(name = "Mississippi", data = allRec$MS, type="line", marker = list(enabled = FALSE), color="#b2df8a") %>%
          hc_add_series(name = "Louisiana", data = allRec$LA, type="line", marker = list(enabled = FALSE), color="#1f78b4") %>%
          hc_add_series(name = "Texas", data = allRec$TX, type="line", marker = list(enabled = FALSE), color="#a6cee3") %>%
          hc_add_series(name = "Florida Selected", data = topNLandingsForHire()$FLW, type="scatter", color="#fb9a99",showInLegend = FALSE) %>%
          hc_add_series(name = "Alabama Selected", data = topNLandingsForHire()$AL, type="scatter", color="#33a02c",showInLegend = FALSE) %>%
          hc_add_series(name = "Mississippi Selected", data = topNLandingsForHire()$MS, type="scatter", color="#b2df8a",showInLegend = FALSE) %>%
          hc_add_series(name = "Louisiana Selected", data = topNLandingsForHire()$LA, type="scatter", color="#1f78b4",showInLegend = FALSE) %>%
          hc_add_series(name = "Texas Selected", data = topNLandingsForHire()$TX, type="scatter", color="#a6cee3",showInLegend = FALSE) %>% 
        hc_add_series(name = "DWH, 2010 not included", data = allRec$star, type="scatter", color="black", marker=list(symbol="cross")) 
        hc
      })
      output$topNlandingsOutForHire <- renderHighchart({topNLandingsPlotForHire()})
### End For hire Alternative 5
      
##################### End Alternative 5 #################################
###Alternative 6: Leaflet map of biomass
      output$map <- renderLeaflet({
        map
      })
      
######################## Alternative 6
      dfTool <- reactive({
        # df <- data.frame(Source=c("Biomass", "Landings","Trips"),
        #                  FL=c(.2994,.35,.40),
        #                  AL=c(.0630,.4,.3),
        #                  MS=c(0.0134, .01,.02),
        #                  LA=c(.2028,.2,.2),
        #                  TX=c(.4213,.04,.08))
        
        if(input$Id073=="All"){
          tmp <- filter(Total2, YEAR>= 1986 & YEAR <=2015 & YEAR !=2010)
          if(input$TimeSeriesSelect=="1986 - 2015"){
            tmp <- filter(tmp, YEAR>= 1986 & YEAR <=2015 & YEAR !=2010)
              } else
                if(input$TimeSeriesSelect=="1996 - 2015"){
                  tmp <- filter(tmp, YEAR>= 1996 & YEAR <=2015 & YEAR !=2010)
                } else
                  if(input$TimeSeriesSelect=="2006 - 2015"){
                    tmp <- filter(tmp, YEAR>= 2006 & YEAR <=2015 & YEAR !=2010)
                  } else
                    if(input$TimeSeriesSelect=="1986 - 2009"){
                      tmp <- filter(tmp, YEAR>= 1986 & YEAR <=2009 & YEAR !=2010)
                    } else
                      if(input$TimeSeriesSelect=="1996 - 2009"){
                        tmp <- filter(tmp, YEAR>= 1996 & YEAR <=2009 & YEAR !=2010)
                      } else
                        if(input$TimeSeriesSelect=="2006 - 2009"){
                          tmp <- filter(tmp, YEAR>= 2006 & YEAR <=2009 & YEAR !=2010)
                        } 
        
          x <- tmp %>% melt(id="YEAR")
          colnames(x) <- c("Year", "State", "Landings")
          x2 <- group_by(x, State) %>% 
            summarise(Landings=mean(Landings) )
          x2$Allocation <- (x2$Landings/sum(x2$Landings))*100
          x2$State <- c("FL", "AL", "MS", "LA", "TX")
          x2
          landOut <- x2$Landings
          
          
          df <- data.frame(Source=c("Biomass", "Landings","Trips"),
                           FL=c(.2994,landOut[1],1),
                           AL=c(.0630,landOut[2],1),
                           MS=c(0.0134,landOut[3],1),
                           LA=c(.2028,landOut[4],1),
                           TX=c(.4213,landOut[5],1))
        } else 
          if(input$Id073=="Private"){
            tmp <- filter(Private, YEAR>= 1986 & YEAR <=2015 & YEAR !=2010)
            if(input$TimeSeriesSelect=="1986 - 2015"){
              tmp <- filter(tmp, YEAR>= 1986 & YEAR <=2015 & YEAR !=2010)
            } else
              if(input$TimeSeriesSelect=="1996 - 2015"){
                tmp <- filter(tmp, YEAR>= 1996 & YEAR <=2015 & YEAR !=2010)
              } else
                if(input$TimeSeriesSelect=="2006 - 2015"){
                  tmp <- filter(tmp, YEAR>= 2006 & YEAR <=2015 & YEAR !=2010)
                } else
                  if(input$TimeSeriesSelect=="1986 - 2009"){
                    tmp <- filter(tmp, YEAR>= 1986 & YEAR <=2009 & YEAR !=2010)
                  } else
                    if(input$TimeSeriesSelect=="1996 - 2009"){
                      tmp <- filter(tmp, YEAR>= 1996 & YEAR <=2009 & YEAR !=2010)
                    } else
                      if(input$TimeSeriesSelect=="2006 - 2009"){
                        tmp <- filter(tmp, YEAR>= 2006 & YEAR <=2009 & YEAR !=2010)
                      } 
            x <- tmp %>% select(-star) %>%  melt(id="YEAR")
            colnames(x) <- c("Year", "State", "Landings")
            x2 <- group_by(x, State) %>% 
              summarise(Landings=mean(Landings) )
            x2$Allocation <- (x2$Landings/sum(x2$Landings))*100
            x2$State <- c("FL", "AL", "MS", "LA", "TX")
            x2
            landOut <- x2$Landings
            
            
            df <- data.frame(Source=c("Biomass", "Landings","Trips"),
                             FL=c(.2994,landOut[1],1),
                             AL=c(.0630,landOut[2],1),
                             MS=c(0.0134,landOut[3],1),
                             LA=c(.2028,landOut[4],1),
                             TX=c(.4213,landOut[5],1))
          } else
            if(input$Id073=="For-hire"){
              tmp <- filter(ForHire, YEAR>= 1986 & YEAR <=2015 & YEAR !=2010)
              if(input$TimeSeriesSelect=="1986 - 2015"){
                tmp <- filter(tmp, YEAR>= 1986 & YEAR <=2015 & YEAR !=2010)
              } else
                if(input$TimeSeriesSelect=="1996 - 2015"){
                  tmp <- filter(tmp, YEAR>= 1996 & YEAR <=2015 & YEAR !=2010)
                } else
                  if(input$TimeSeriesSelect=="2006 - 2015"){
                    tmp <- filter(tmp, YEAR>= 2006 & YEAR <=2015 & YEAR !=2010)
                  } else
                    if(input$TimeSeriesSelect=="1986 - 2009"){
                      tmp <- filter(tmp, YEAR>= 1986 & YEAR <=2009 & YEAR !=2010)
                    } else
                      if(input$TimeSeriesSelect=="1996 - 2009"){
                        tmp <- filter(tmp, YEAR>= 1996 & YEAR <=2009 & YEAR !=2010)
                      } else
                        if(input$TimeSeriesSelect=="2006 - 2009"){
                          tmp <- filter(tmp, YEAR>= 2006 & YEAR <=2009 & YEAR !=2010)
                        } 
              x <- tmp %>% select(-star) %>%  melt(id="YEAR")
              colnames(x) <- c("Year", "State", "Landings")
              x2 <- group_by(x, State) %>% 
                summarise(Landings=mean(Landings) )
              x2$Allocation <- (x2$Landings/sum(x2$Landings))*100
              x2$State <- c("FL", "AL", "MS", "LA", "TX")
              x2
              landOut <- x2$Landings
              
              
              df <- data.frame(Source=c("Biomass", "Landings","Trips"),
                               FL=c(.2994,landOut[1],1),
                               AL=c(.0630,landOut[2],1),
                               MS=c(0.0134,landOut[3],1),
                               LA=c(.2028,landOut[4],1),
                               TX=c(.4213,landOut[5],1))
            }
        
      }) #end Dftool
       
      dfTool2 <- reactive({
        df <- dfTool()
        for(i in 2:6){
          df[,i] <- sprintf("%1.1f%%", 100*df[,i])
        }
        df <- rbind(df[1,],df[3,], df[2,]) 
        #RN <- input$Id073
        df$Source <- c("Biomass","Trips", paste(input$Id073, "Landings", sep=" "))
        df
        
      })
      output$dfToolTable <- renderTable({dfTool2()},width='300px',colnames=TRUE)
      
      x <- reactive({
        # df <- data.frame(Source=c("Biomass", "Landings","Trips"),
        #                  FL=c(.2994,.35,.40),
        #                  AL=c(.0630,.4,.3),
        #                  MS=c(0.0134, .01,.02),
        #                  LA=c(.2028,.2,.2),
        #                  TX=c(.4213,.04,.08))
        x <- dfTool()
        ##Note: inputs c1 and b1 were switched as table order was reversed 
        ## for Effort and Landings
        FL <- (x[1,2] *input$a1) + (x[2,2] *input$c1) + (x[3,2] *input$b1)
        AL <- (x[1,3] *input$a1) + (x[2,3] *input$c1) + (x[3,3] *input$b1)
        MS <- (x[1,4] *input$a1) + (x[2,4] *input$c1) + (x[3,4] *input$b1)
        LA <- (x[1,5] *input$a1) + (x[2,5] *input$c1) + (x[3,5] *input$b1)
        TX <- (x[1,6] *input$a1) + (x[2,6] *input$c1) + (x[3,6] *input$b1)
        States <- data.frame(Allocation="Allocation",FL=FL, AL=AL,MS=MS, LA=LA, TX=TX)
        for(i in 2:6){
          States[,i] <- sprintf("%1.1f%%", 1*States[,i])
        }
        
        States
      })
      
      output$x2 <- renderTable({x()},width='300px',colnames=TRUE)
      
      checkOutput <- reactive({
        x <- data.frame(Total=input$a1 + input$b1 +input$c1)
        x
      })
      
      output$check <- renderTable({
        checkOutput()
        if(checkOutput()!=100){
          createAlert(session,"alert","exampleAlert",
                      title = "Oops",
                      content = "Weights should sum to 100",
                      append = FALSE)
          return(checkOutput())
        }  else {
          closeAlert(session, "exampleAlert")
          return(checkOutput())}
        
        
      },width='80px',colnames=TRUE)
      
  ####Pic for Alt 1.
      output$rsImage <- renderImage({
        return(list(
           src='www/IMG_3627_RS.JPG',
          #src='www/logo.png',
          filetype='image/png',
          width=650,
          alt='text'))
      }, deleteFile=FALSE)
################################### Alternative 6      

      
  observe({
    
    updateTabsetPanel(session, "tabP1", selected = input$tabP2)
    
  })
  
  observe({
    
    updateTabsetPanel(session, "tabP2", selected = input$tabP1)
    
  })
  ################# Links to the separate tabs##################
  
}
