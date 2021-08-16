# Chart module #4 for SEPH AHE for salaried empl by ind and prov, NSA
# August 2, 2021; improved August 10, 2021

mc04UI <- function(id) {
  tabPanel(tags$b(tags$span(style="color:blue", HTML("Charts"))),
    tags$b(tags$span(style="color:blue",HTML(paste0("<h2>Average hourly ",
      "earnings for salaried employees by industry and province or ",
      "territory, seasonally adjusted</h2><br>")))),
    prettyRadioButtons(NS(id,"Geo"),
      tags$b(tags$span(style="color:blue;font-size:20px", 
      "Choose a geography:")),choices=geo01,bigger=TRUE,
      outline=TRUE,inline=TRUE,shape="round",animation="pulse"),
    tags$style(HTML(".selectize-input, .option {color:blue;font-size:20px}")),   
    selectInput(NS(id,"naics"),
      label=tags$b(tags$span(style="color:blue; font-size:20px",
      "Choose an industry:")),choices=naics06,width = "100%"),
    fluidRow(column(5,
      prettyRadioButtons(NS(id,"trf"),
        tags$b(tags$span(style="color:blue;font-size:20px", 
        "Choose a transformation:")),choices=trf02,bigger=TRUE,
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
      choices=getChoices(4)[[1]],
      selected=getChoices(4)[[3]],
      dragRange = TRUE,
      width="100%"),
    tags$script(HTML(
      "$('.shiny-input-container:has(input[id=\"idmc04-Dates\"])>label')
      .css({ffontWeight:900,fontSize:'20px',color:'blue'})")),
    plotOutput(NS(id,"chart")) 
  )
}
mc04Server <- function(id) {
  moduleServer(id,function(input,output,session) {
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
      if (Seas2()) {Make_chrtM04(naics2(),Geo2(),"TRUE",type2(),
        month1(),month2(),altTitle(),"")}
      else {(Make_chrtM04(naics2(),Geo2(),"FALSE",type2(),
        month1(),month2(),altTitle(),""))}
    })  
    output$chart <- renderPlot({chartP()},height=700)
    output$downloadData1 <- downloadHandler(
      filename=function() {
        paste0("SEPHdata.png")
      },
      content=function(file) {
        ggsave(file,chartP(),height=8,width=14,dpi=300)
      }
    )
    observe({
      updateSliderTextInput(session,inputId="Dates",
        label="Choose starting and ending dates:",
        choices=getChoices(4)[[1]],
        selected=getChoices(4)[[3]])
      tags$script(HTML(
        "$('.shiny-input-container:has(input[id=\"idmc04-Dates\"])>label')
        .css({fontWeight:900,fontSize:'20px',color:'blue'})"))
    })
  })
}

