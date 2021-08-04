# Chart module #8 for SEPH empl, AWE, AHE, AWH by ind, SA
# August 3, 2021
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(tidyverse)

source("Tabl_specs.R")

Est12001 <- c(
  "Employment",                                 
  "Average weekly earnings including overtime",
  "Average hourly earnings including overtime", 
  "Average weekly hours" 
)
Typ1001 <- c(
  "Employees paid by the hour",             
  "Salaried employees paid a fixed salary"
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
  "Health care and social assistance [62]",                                    
  "Arts, entertainment and recreation [71]",                                   
  "Accommodation and food services [72]",                                      
  "Other services (except public administration) [81]",                        
  "Public administration [91]" 
)
trf1001 <- c(
  "Original data (no transformation)",
  "Including trend line",
  "Index, first month = 100",
  "One-month percentage change",
  "Twelve-month percentage change"
)
e <- seq.Date(TS[[8]]$Strt,TS[[8]]$Endt,by="month")
s <- character()
for (i in 1:length(e)) {
  s[i] <- format(e[i],"%b %Y")
}
r <- c(s[length(s)-5],s[length(s)])

mc08UI <- function(id) {
  tabPanel(tags$b(tags$span(style="color:blue", HTML("Charts"))),
    prettyRadioButtons(NS(id,"Est"),
      tags$b(tags$span(style="color:blue;font-size:20px", 
        "Employment or AWE or AHE or AWH:")),choices=Est12001,bigger=TRUE,
        outline=TRUE,inline=TRUE,shape="round",animation="pulse"),
    prettyRadioButtons(NS(id,"Typ"),
      tags$b(tags$span(style="color:blue;font-size:20px", 
        "Type of employee:")),choices=Typ1001,bigger=TRUE,
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
    chooseSliderSkin(skin="Round",color="blue"),
    sliderTextInput(NS(id,"Dates"),label= 
      "Choose starting and ending dates:",
      choices=s,
      selected=r,
      dragRange = TRUE,
      width="100%"),
    tags$script(HTML(
      "$('.shiny-input-container:has(input[id=\"idmc08-Dates\"])>label')
      .css({ffontWeight:900,fontSize:'20px',color:'blue'})")),
    plotOutput(NS(id,"chart")) 
  )
}
mc08Server <- function(id) {
  moduleServer(id,function(input,output,session) {
    q0 <- readRDS(paste0("rds/",TS[[8]]$STCno,".rds"))
    Est2  <- reactive({input$Est})
    Typ2  <- reactive({input$Typ})
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
    chartP <- reactive({ Make_chrtM08(naics2(),Est2(),Typ2(),type2(),
        month1(),month2(),altTitle(),"") })
    output$chart <- renderPlot({chartP()},height=700)
    output$downloadData1 <- downloadHandler(
      filename=function() {
        paste0("Employment_or_AWE_or_AHE_or_AWH.png")
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
        "$('.shiny-input-container:has(input[id=\"idmc08-Dates\"])>label')
        .css({fontWeight:900,fontSize:'20px',color:'blue'})"))
    })
    #observeEvent(input$Est,
    #  updateSelectInput(session,"naics",label="Choose an industry:",
    #    choices=unique(filter(q0,EST==input$Est)$NAICS))
    #)
  })
}

