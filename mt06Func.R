# Table module #6 for SEPH FWI by ind and prov, NSA
# August 2, 2021

library(gt)
library(tidyverse)
library(lubridate)

source("Tabl_specs.R")

Geo1000 <- c(
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
naics1000 <- c(
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
trf1000 <- c(
  "Original data (no transformation)",
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

mt06UI <- function(id) {
  tabPanel(tags$b(tags$span(style="color:blue", HTML("Tables"))),
    prettyRadioButtons(NS(id,"Geo"),
      tags$b(tags$span(style="color:blue;font-size:20px", 
        "Choose a geography:")),choices=Geo1000,bigger=TRUE,
        outline=TRUE,inline=TRUE,shape="round",animation="pulse"),
    prettyRadioButtons(NS(id,"trf"),
      tags$b(tags$span(style="color:blue;font-size:20px", 
        "Choose a transformation:")),choices=trf1000,bigger=TRUE,
        outline=TRUE,inline=TRUE,shape="round",animation="pulse"),
    column(2,offset=10,downloadButton(NS(id,"downloadData1"),
      label="Download table")),
    chooseSliderSkin(skin="Round",color="blue"),
    sliderTextInput(NS(id,"Dates"),label="Choose starting and ending dates:",
      choices=s,
      selected=r,
      dragRange = TRUE,
      width="100%"),
    tags$script(HTML(
      "$('.shiny-input-container:has(input[id=\"idmt06-Dates\"])>label')
      .css({fontFamily:'helvetica',fontWeight:900,fontSize:'20px',color:'blue'})")),    
    htmlOutput(NS(id,"notabl")),
    gt_output(NS(id,"tabl")) 
  )
}
mt06Server <- function(id) {
  moduleServer(id,function(input,output,session) {
    Geo2  <- reactive({input$Geo})
    type1 <- reactive(case_when(
      input$trf=="Original data (no transformation)"~1,
      input$trf=="Index, first month = 100"~2,
      input$trf=="One-month percentage change"~3,
      input$trf=="Twelve-month percentage change"~4
    ))    
    month1 <- reactive({
      as.Date(paste0(substr(input$Dates[1],1,3)," 1, ",
        substr(input$Dates[1],5,8)),format("%b %d, %Y"))
    })
    month2 <- reactive({
      as.Date(paste0(substr(input$Dates[2],1,3)," 1, ",
        substr(input$Dates[2],5,8)),format("%b %d, %Y"))
    })
    expr <- reactive({
      Make_tablM06(Geo2(),type1(),month1(),month2())
    })
    output$tabl <- render_gt({
        expr()[[1]]
    })
    output$downloadData1 <- downloadHandler(
      filename=function() {
        paste0("Fixed_weighted_index.csv")
      },
      content=function(file) {
        write.csv(expr()[[2]],file)
      }
    )
    observe({ # monthly table update
      updateSliderTextInput(session,inputId="Dates",
        label="Choose starting and ending dates:",
        choices=s,
        selected=r)
      tags$script(HTML(
        "$('.shiny-input-container:has(input[id=\"idmt06-Dates\"])>label')
        .css({fontFamily:'helvetica',fontWeight:900,fontSize:'20px',color:'blue'})"))
    })
  })
}
