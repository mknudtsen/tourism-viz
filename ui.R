library(shiny)
library(DT)
library(shinythemes)
require(rCharts)


shinyUI(navbarPage(theme = shinytheme("cosmo"), "Tourism in Latin America",
                   
       
                   
            tabPanel("Market Analysis",
                tabsetPanel(type = "tabs",

                    tabPanel("Overview", 
                        fluidRow(
                            column(12, 
                                selectInput("summary", label = "",
                                    choices = list("Arrivals" = "arrivals", "Receipts" = "receipts", "Domestic" = "domestic"),
                                    selected = "arrivals"),
                                
                                fluidRow(
                                    column(6, align = "left",
                                       h3(textOutput("totalTitle"), align = "center"),
                                       showOutput("overview1", "nvd3")
                                    ),
                                
                                    column(6, align = "left",
                                       h3(textOutput("growthTitle"), align = "center"),
                                       showOutput("overview2", "nvd3")
                                    )
                                )
                            )
                                  
                        )
                    ),                                                       

                            
                    tabPanel("International Arrivals and Depatures", 
                        fluidRow(
                            column(3,
                                selectInput("arrDept", label = "", 
                                choices = list("Arrivals" = "arrivals", "Departures" = "departures"), 
                                selected = "arrivals")
                            ),
                                              
                            column(9,
                                h3(textOutput("title1"), align = "center"),
                                showOutput("timeSeries", "nvd3")
                            )        
                        )
                    ),
                                 
                    tabPanel("Tourism Receipts and Spending",
                         fluidRow(
                            column(3, 
                                selectInput("revSpend", label = "", 
                                choices = list("Receipts per tourist" = "receipts", "Expenditures per resident" = "expenditures", "Domestic tourism spending" = "domestic"), 
                                selected = "receipts")
                            ),
                            column(9,
                                h3(textOutput("title2"), align = "center"),
                                showOutput("touristRevenues", "nvd3")
                            )
                         )
                    ),
                    
                    tabPanel("Internet Penetration",
                        fluidRow(
                            column(3, 
                                selectInput("tech", label = "", 
                                choices = list("Internet users" = "internet"), 
                                selected = "internet")
                            ),
                            column(9,
                                h3(textOutput("title3"), align = "center"),
                                showOutput("internet", "nvd3")
                            )
                        )
                    )
                    
                    #tabPanel("Hofstede's Cultural Dimensions",
                    #    fluidRow(
                            
                    #         )
                    #) 
                    
              )
                
            ),
                   
            tabPanel("DataTable",
                dataTableOutput("dataTable")
            )
            
            
    )
)