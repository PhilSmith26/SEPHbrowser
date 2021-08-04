# Chart module #6 for SEPH FWI empl by ind and prov, NSA
# August 2, 2021
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(tidyverse)

source("Tabl_specs.R")

Geo1001 <- c(
  "Canada",
  "Newfoundland and Labrador",
  "Prince Edward Island",     
  "Nova Scotia",
  "New Brunswick",
  "Quebec",                   
  "Ontario",
  "Manitoba",
  "Saskatchewan",
  "Alberta",
  "British Columbia",
  "Yukon",                    
  "Northwest Territories",
  "Nunavut" 
)
naics1001 <- c(
  "Industrial aggregate excluding unclassified businesses [11-91N]",           
  "Goods producing industries [11-33N]",                                       
  "Forestry, logging and support [11N]",                                       
  "Mining, quarrying, and oil and gas extraction [21]",                        
  "Utilities [22]",                                                            
  "Construction [23]",                                                         
  "Manufacturing [31-33]",                                                     
  "Non-durable goods [311N]",                                                  
  "Durable goods [321N]",                                                      
  "Service producing industries [41-91N]",                                     
  "Trade [41-45N]",                                                            
  "Wholesale trade [41]",                                                      
  "Retail trade [44-45]",                                                      
  "Transportation and warehousing [48-49]",                                    
  "Information and cultural industries [51]",                                  
  "Finance and insurance [52]",                                                
  "Real estate and rental and leasing [53]",                                   
  "Professional, scientific and technical services [54]",                      
  "Management of companies and enterprises [55]",                              
  "Administrative and support, waste management and remediation services [56]",
  "Educational services [61]",                                                 
  "Education special [611N]",                                                  
  "Health care and social assistance [62]",                                    
  "Arts, entertainment and recreation [71]",                                   
  "Accommodation and food services [72]",                                      
  "Other services (except public administration) [81]",                        
  "Public administration [91]",                                                
  "Forestry and logging [113]"
)
trf1001 <- c(
  "Original data (no transformation)",
  "Including trend line",
  "Index, first month = 100",
  "One-month percentage change",
  "Twelve-month percentage change"
)
e <- seq.Date(TS[[6]]$Strt,TS[[6]]$Endt,by="month")
s <- character()
for (i in 1:length(e)) {
  s[i] <- format(e[i],"%b %Y")
}
r <- c(s[length(s)-5],s[length(s)])

mc06UI <- function(id) {
  tabPanel(tags$b(tags$span(style="color:blue", HTML("Charts"))),
    prettyRadioButtons(NS(id,"Geo"),
      tags$b(tags$span(style="color:blue;font-size:20px", 
      "Choose a geography:")),choices=Geo1001,bigger=TRUE,
      outline=TRUE,inline=TRUE,shape="round",animation="pulse"),
    tags$style(HTML(".selectize-input, .option {color:blue;font-size:20px}")),   
    selectInput(NS(id,"naics"),
      label=tags$b(tags$span(style="color:blue; font-size:20px",
      "Choose an industry:")),choices=naics1001,width = "100%"),
    fluidRow(column(5,
      prettyRadioButtons(NS(id,"trf"),
        tags$b(tags$span(style="color:blue;font-size:20px", 
        "Choose a transformation:")),choices=trf1001,bigger=TRUE,
        outline=TRUE,inline=TRUE,shape="round",animation="pulse")),
      column(5,textInput(NS(id,"altTitl"),
        tags$b(tags$span(style="color:blue;font-size:20px",
          "Choose your own chart title (optional):")),
          value="",width="90%")),
      column(2,downloadButton(NS(id,"downloadData1"),label="Download chart"))
    ),
    fluidRow(column(2, offset=10,
      switchInput(NS(id,"seasAdj"),label="Seasonally adjust",value=FALSE))
    ),
    chooseSliderSkin(skin="Round",color="blue"),
    sliderTextInput(NS(id,"Dates"),label= 
      "Choose starting and ending dates:",
      choices=s,
      selected=r,
      dragRange = TRUE,
      width="100%"),
    tags$script(HTML(
      "$('.shiny-input-container:has(input[id=\"idmc06-Dates\"])>label')
      .css({ffontWeight:900,fontSize:'20px',color:'blue'})")),
    plotOutput(NS(id,"chart")) 
  )
}
mc06Server <- function(id) {
  moduleServer(id,function(input,output,session) {
    q0 <- readRDS(paste0("rds/",TS[[6]]$STCno,".rds"))
    Geo2  <- reactive({input$Geo})
    Seas2  <- reactive({input$seasAdj})
    naics2  <- reactive({input$naics})
    type2 <- reactive(case_when(
      input$trf=="Original data (no transformation)"~1,
      input$trf=="Including trend line"~2,
      input$trf=="Index, first month = 100"~3,
      input$trf=="One-month percentage change"~4,
      input$trf=="Twelve-month percentage change"~5
    ))    
    month1 <- reactive({
      as.Date(paste0(substr(input$Dates[1],1,3)," 1, ",
        substr(input$Dates[1],5,8)),format("%b %d, %Y"))
    })
    month2 <- reactive({
      as.Date(paste0(substr(input$Dates[2],1,3)," 1, ",
        substr(input$Dates[2],5,8)),format("%b %d, %Y"))
    })
    altTitle <- reactive({input$altTitl})
    chartP <- reactive({
      if (Seas2()) {Make_chrtM06(naics2(),Geo2(),"TRUE",type2(),
        month1(),month2(),altTitle(),"")}
      else {(Make_chrtM06(naics2(),Geo2(),"FALSE",type2(),
        month1(),month2(),altTitle(),""))}
    })  
    output$chart <- renderPlot({chartP()},height=700)
    output$downloadData1 <- downloadHandler(
      filename=function() {
        paste0("Fixed_weighted_index.png")
      },
      content=function(file) {
        ggsave(file,chartP(),height=8,width=14,dpi=300)
      }
    )
    observe({
      r <- c(s[length(s)-25],s[length(s)])
      updateSliderTextInput(session,inputId="Dates",
        label="Choose starting and ending dates:",
        choices=s,
        selected=r)
      tags$script(HTML(
        "$('.shiny-input-container:has(input[id=\"idmc06-Dates\"])>label')
        .css({fontWeight:900,fontSize:'20px',color:'blue'})"))
    })
    observeEvent(input$Geo,
      updateSelectInput(session,"naics",label="Choose an industry:",
        choices=unique(filter(q0,GEO==input$Geo)$NAICS))
    )
  })
}

