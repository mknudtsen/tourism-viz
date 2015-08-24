library(shiny)
library(DT)
require(rCharts)
library(BH)
library(plyr)
library(shinyapps)

#options(RCHART_WIDTH = 900)
#options(RCHART_HEIGHT = 650)
#options(shiny.trace=TRUE)


latam <- read.csv("data/latamTotal.csv")
latam <- latam[!(latam$iso %in% c("CN", "BZ", "PY", "HN", "FR", "VE", "UY")),]
hofstede <- read.csv("data/hofstede.csv")
hofstede <- hofstede[!(hofstede$iso %in% c("CN", "FR", "UY", "VE")),]

shinyServer(function(input, output) {
    
    output$dataTable <- renderDataTable({
        datatable(latam[3:12], options = list(iDisplayLength = 25))
    })
    
    output$totalTitle <- renderText({
        view1 <- switch(input$summary,
            "arrivals" = "2012 international tourist arrivals, m",
            "receipts" = "2012 international tourism receipts, $bn",
            "domestic" = "2012 domestic tourism spending, $bn")
    })
    
    output$overview1 <- renderChart2({
        # return selected series
        latam$temp <- switch(input$summary,
            "arrivals" = round(latam$intlArrivals / 1000000, 2),
            "receipts" = round(latam$intlReceipts / 1000000000, 2),
            "domestic" = round(latam$domesticTotal, 2))
        
        # prepare data
        current <- subset(latam, year == 2012)
        current <- current[order(-current$temp),]
        current <- current[!(current$iso %in% "US"),]
        
        # enable one country as default
        #ids <- unique(current$iso)
        #top <- as.logical(ids != "BR")
        
        # set initial plot
        p0 <- nPlot(
            temp ~ country,
            data = current,
            group = "year",
            type = "multiBarHorizontalChart")
        
        # set plot labels and adjustments
        p0$yAxis(showMaxMin = FALSE)
        #p0$xAxis(axisLabel = "Year")
        p0$xAxis(showMaxMin = FALSE)
        p0$yAxis(tickFormat = 
            "#!function(d) { return d3.format('.')(d) }!#"
        )
        p0$chart(margin = list(left = 110))
        p0$chart(showLegend = FALSE)
        p0$chart(showControls = FALSE)
        p0$params$width <- 550
        #p0$params$height <- 425
        #p0$set(disabled = top)
        #p0$addParams(dom = "overview1")
        return(p0)
        
    })
    
    output$growthTitle <- renderText({
        view1 <- switch(input$summary,
            "arrivals" = "Arrivals, % change on a year earlier",
            "receipts" = "Receipts, % change on a year eariler",
            "domestic" = "Domestic, % change on a year earlier")
    })
    
    output$overview2 <- renderChart2({
        # return selected series
        latam$temp <- switch(input$summary,
            "arrivals" = latam$intlArrivals,
            "receipts" = latam$intlReceipts,
            "domestic" = latam$domesticTotal)
        
        # calculate new growth column based on selected series and add to new dataframe
        selected <- latam[c("iso", "country", "year", "temp")]
        grow <- ddply(selected, "country", transform,
                        growth=c(NA, exp(diff(log(temp))) -1))
        
        # subset and reorder for bar chart
        current <- subset(grow, year %in% c(2011, 2012))
        current <- current[!(current$iso %in% "US"),]
        currentOrder <- current[order(-current$year, -current$growth),]
        
        years <- unique(currentOrder$year)
        top <- as.logical(years != 2012)
        
        # create intial plot
        p <- nPlot(
            growth ~ country,
            data = currentOrder,
            group = "year",
            type = "multiBarHorizontalChart")
        
        p$yAxis(showMaxMin = FALSE)
        #p0$xAxis(axisLabel = "Year")
        p$xAxis(showMaxMin = FALSE)
        p$yAxis(tickFormat = 
            "#!function(d) { return d3.format('%')(d) }!#"
        )
        p$params$width <- 550
        #p$params$height <- 425
        p$chart(margin = list(left = 110))
        p$chart(showControls = FALSE)
        p$set(disabled = top)
        return(p)
    })
    
    output$title1 <- renderText({
        type1 <- switch(input$arrDept,
            "arrivals" = "International tourist arrivals, m",
            "departures" = "International resident departures, m")
    })
    
    output$timeSeries <- renderChart2({
        # prepare data
        # select series from ui input
        latam$temp1 <- switch(input$arrDept,
            "arrivals" = latam$intlArrivals,
            "departures" = latam$intlDepartures)
        latam$temp1 <- round((latam$temp1 / 1000000), 2)
        
        # enable one counry on default
        ids <- unique(latam$iso)
        top <- as.logical(ids != "BR")
        
        # set initial plot
        p1 <- nPlot(
            temp1 ~ year,
            data = latam,
            group = "country",
            type = "lineChart")
        
        # set plot labels and adjustments
        p1$yAxis(showMaxMin = FALSE)
        #p1$xAxis(axisLabel = "Year")
        p1$xAxis(showMaxMin = FALSE)
        p1$yAxis(tickFormat = 
            "#!function(d) { return d3.format('.1')(d) }!#"
        )
        p1$chart(margin = list(left = 80))
        p1$params$height <- 450
        p1$params$width <- 825
        p1$set(disabled = top)
        #p1$addParams(dom = "timeSeries")
        return(p1)
    })
    
    output$title2 <- renderText({
        type2 <- switch(input$revSpend,
            "receipts" = "Receipts per tourist (inbound), $",
            "expenditures" = "Expenditures per resident (outbound), $",
            "domestic" = "Domestic travel and tourism spending, $bn")
    })
    
    output$touristRevenues <- renderChart2({
        # prepare data
        # select series from ui input
        latam$temp2 <- switch(input$revSpend,
            "receipts" = round((latam$intlReceipts / latam$intlArrivals), 2),
            "expenditures" = round((latam$intlExpenditures / latam$intlDepartures), 2),
            "domestic" = round(latam$domesticTotal, 2))
        
        ids <- unique(latam$iso)
        top <- as.logical(ids != 'BR')
        
        # set initial plot
        p3 <- nPlot(
            temp2 ~ year, 
            data = latam,
            group = "country",
            type = "lineChart")
        
        # set plot labels and adjustments
        p3$yAxis(showMaxMin = FALSE)
        p3$yAxis(tickFormat = 
            "#!function(d) { return '$' + d }!#"
        )
        #p3$xAxis(axisLabel = "Year")
        p3$xAxis(showMaxMin = FALSE)
        p3$chart(margin = list(left = 80))
        p3$params$height <- 450
        p3$params$width <- 825
        p3$set(disabled = top)
        return(p3)
    })
    
    output$title3 <- renderText({
        type3 <- switch(input$tech,
            "internet" = "Internet users as a percentage of population")
    })
    
    output$internet <- renderChart2({
        # prepare data
        # select series from ui input
        latam$temp3 <- switch(input$tech,
            "internet" = round(latam$internetUsers, 0))
        
        ids <- unique(latam$iso)
        top <- as.logical(ids != 'BR')
        
        # set initial plot
        p4 <- nPlot(
            temp3 ~ year, 
            data = latam,
            group = "country",
            type = "lineChart")
        
        # set plot labels and adjustments
        p4$yAxis(showMaxMin = FALSE)
        #p4$xAxis(axisLabel = "Year")
        p4$xAxis(showMaxMin = FALSE)
        p4$yAxis(tickFormat = 
            "#!function(d) { return d + '%' }!#"
            )
        p4$chart(margin = list(left = 80))
        p4$params$height <- 450
        p4$params$width <- 825
        p4$set(disabled = top)
        return(p4)
    })
    
    output$hofstede <- renderChart2({
        p2 <- nPlot(
            uai ~ country,
            data = hofstede,
            type = "discreteBarChart")
        return(p2)
    })
})
