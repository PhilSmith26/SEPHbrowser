# Table module #8 for SEPH empl, AHE, AWE and AWH by ind, SA
# August 3, 2021

mt08UI <- function(id) {
  tabPanel(tags$b(tags$span(style="color:blue", HTML("Tables"))),
    tags$b(tags$span(style="color:blue",HTML(paste0("<h2>Employment, ",
      "AWE, AHE and AWH by industry, seasonally adjusted</h2><br>")))),
    prettyRadioButtons(NS(id,"Est"),
      tags$b(tags$span(style="color:blue;font-size:20px", 
        "Employment or AWE or AHE or AWH:")),choices=est02,bigger=TRUE,
        outline=TRUE,inline=TRUE,shape="round",animation="pulse"),
    prettyRadioButtons(NS(id,"Typ"),
      tags$b(tags$span(style="color:blue;font-size:20px", 
        "Type of employee:")),choices=typ01,bigger=TRUE,
        outline=TRUE,inline=TRUE,shape="round",animation="pulse"),
    prettyRadioButtons(NS(id,"trf"),
      tags$b(tags$span(style="color:blue;font-size:20px", 
        "Choose a transformation:")),choices=trf01,bigger=TRUE,
        outline=TRUE,inline=TRUE,shape="round",animation="pulse"),
    column(2,offset=10,downloadButton(NS(id,"downloadData1"),
      label="Download table")),
    chooseSliderSkin(skin="Round",color="blue"),
    sliderTextInput(NS(id,"Dates"),label="Choose starting and ending dates:",
      choices=getChoices(8)[[1]],
      selected=getChoices(8)[[2]],
      dragRange = TRUE,
      width="100%"),
    tags$script(HTML(
      "$('.shiny-input-container:has(input[id=\"idmt08-Dates\"])>label')
      .css({fontFamily:'helvetica',fontWeight:900,fontSize:'20px',color:'blue'})")),    
    htmlOutput(NS(id,"notabl")),
    gt_output(NS(id,"tabl")) 
  )
}
mt08Server <- function(id) {
  moduleServer(id,function(input,output,session) {
    Est2  <- reactive({input$Est})
    Typ2  <- reactive({input$Typ})
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
      Make_tablM08(Est2(),Typ2(),type1(),month1(),month2())
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
    observe({
      updateSliderTextInput(session,inputId="Dates",
        label="Choose starting and ending dates:",
        choices=getChoices(8)[[1]],
        selected=getChoices(8)[[2]])
      tags$script(HTML(
        "$('.shiny-input-container:has(input[id=\"idmt08-Dates\"])>label')
        .css({fontFamily:'helvetica',fontWeight:900,fontSize:'20px',color:'blue'})"))
    })
  })
}
