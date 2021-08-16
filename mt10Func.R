# Table module #10 for SEPH empl and AWE by prov and ind, SA
# August 3, 2021; improved August 9, 2021

mt10UI <- function(id) {
  tabPanel(tags$b(tags$span(style="color:blue", HTML("Tables"))),
    tags$b(tags$span(style="color:blue",HTML(paste0("<h2>Employment ",
      "and average weekly earnings by ",
      "province or territory, seasonally adjusted</h2><br>")))),
    prettyRadioButtons(NS(id,"Geo"),
      tags$b(tags$span(style="color:blue;font-size:20px", 
        "Canada, province or territory:")),choices=geo01,bigger=TRUE,
        outline=TRUE,inline=TRUE,shape="round",animation="pulse"),
    prettyRadioButtons(NS(id,"Est"),
      tags$b(tags$span(style="color:blue;font-size:20px", 
        "Employment or AWE:")),choices=est03,bigger=TRUE,
        outline=TRUE,inline=TRUE,shape="round",animation="pulse"),
    prettyRadioButtons(NS(id,"trf"),
      tags$b(tags$span(style="color:blue;font-size:20px", 
        "Choose a transformation:")),choices=trf01,bigger=TRUE,
        outline=TRUE,inline=TRUE,shape="round",animation="pulse"),
    column(2,offset=10,downloadButton(NS(id,"downloadData1"),
      label="Download table")),
    chooseSliderSkin(skin="Round",color="blue"),
    sliderTextInput(NS(id,"Dates"),label="Choose starting and ending dates:",
      choices=getChoices(10)[[1]],
      selected=getChoices(10)[[2]],
      dragRange = TRUE,
      width="100%"),
    tags$script(HTML(
      "$('.shiny-input-container:has(input[id=\"idmt10-Dates\"])>label')
      .css({fontFamily:'helvetica',fontWeight:900,fontSize:'20px',color:'blue'})")),    
    htmlOutput(NS(id,"notabl")),
    gt_output(NS(id,"tabl")) 
  )
}
mt10Server <- function(id) {
  moduleServer(id,function(input,output,session) {
    Geo2  <- reactive({input$Geo})
    Est2  <- reactive({input$Est})
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
      Make_tablM10(Geo2(),Est2(),type1(),month1(),month2())
    })
    output$tabl <- render_gt({
        expr()[[1]]
    })
    output$downloadData1 <- downloadHandler(
      filename=function() {
        paste0("SEPHdata.csv")
      },
      content=function(file) {
        write.csv(expr()[[2]],file)
      }
    )
    observe({ # monthly table update
      updateSliderTextInput(session,inputId="Dates",
        label="Choose starting and ending dates:",
        choices=getChoices(10)[[1]],
        selected=getChoices(10)[[2]])
      tags$script(HTML(
        "$('.shiny-input-container:has(input[id=\"idmt10-Dates\"])>label')
        .css({fontFamily:'helvetica',fontWeight:900,fontSize:'20px',color:'blue'})"))
    })
  })
}
