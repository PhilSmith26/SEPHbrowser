# Shiny app for SEPH
# August 10, 2021
library(shiny)
library(shinyjs)
library(tidyverse)
library(rlist)
library(shinyWidgets)
library(lubridate)
library(gt)
library(scales)

source("Common_stuff.R")
source("SEPHbrowserChoices.R")
source("Tabl_specs.R") 
source("Make_tablM01.R") 
source("Make_tablM02.R") 
source("Make_tablM03.R") 
source("Make_tablM04.R") 
source("Make_tablM05.R") 
source("Make_tablM06.R") 
source("Make_tablM07.R") 
source("Make_tablM08.R") 
source("Make_tablM09.R") 
source("Make_tablM10.R") 
source("Make_tablM11.R") 
source("Make_chrtM01.R") 
source("Make_chrtM02.R") 
source("Make_chrtM03.R") 
source("Make_chrtM04.R") 
source("Make_chrtM05.R") 
source("Make_chrtM06.R") 
source("Make_chrtM07.R") 
source("Make_chrtM08.R") 
source("Make_chrtM09.R") 
source("Make_chrtM10.R") 
source("Make_chrtM11.R") 
source("mt01Func.R")
source("mt02Func.R")
source("mt03Func.R")
source("mt04Func.R")
source("mt05Func.R")
source("mt06Func.R")
source("mt07Func.R")
source("mt08Func.R")
source("mt09Func.R")
source("mt10Func.R")
source("mt11Func.R")
source("mc01Func.R")
source("mc02Func.R")
source("mc03Func.R")
source("mc04Func.R")
source("mc05Func.R")
source("mc06Func.R")
source("mc07Func.R")
source("mc08Func.R")
source("mc09Func.R")
source("mc10Func.R")
source("mc11Func.R")

ui <- navbarPage(
  tags$head(tags$style(HTML('* {font-family: "Optima"};'))),
  title = tags$b(tags$span(style="color:red", 
  "Survey of Employment, Payrolls and Hours (SEPH)")),
  windowTitle = "Canadian SEPH browser",
  selected = "tabPanel01",
  setBackgroundColor(
    color = c("#ffc125", "#ffe4c4"), # color = c("#d7ebfe", "#6aade7"),
    gradient = "linear",
    direction = "bottom"
  ),
  tabPanel(tags$b(tags$span(style="color:blue", 
    HTML("Introduction<br>and explanation"))),
    value = "tabPanel01",
    htmlOutput("textInfo")
  ),
  tabPanel(tags$b(tags$span(style="color:blue", 
    HTML("List of tables"))),
    value = "tabPanel02",
    htmlOutput("tblList")
  ),
  navbarMenu(tags$b(tags$span(style="color:blue", #9
    HTML("Table 1"))),
    tags$style(type='text/css', ".selectize-input { 
      font-size: 24px; line-height: 24px;} .selectize-dropdown 
      { font-size: 20px; line-height: 20px; }"),
    mt09UI(id="idmt09"),
    mc09UI(id="idmc09")
  ),
  navbarMenu(tags$b(tags$span(style="color:blue", #8
    HTML("Table 2"))),
    #HTML("Employment, AWE, AHE and AWH by industry, SA"))),
    tags$style(type='text/css', ".selectize-input { 
      font-size: 24px; line-height: 24px;} .selectize-dropdown 
      { font-size: 20px; line-height: 20px; }"),
    mt08UI(id="idmt08"),
    mc08UI(id="idmc08")
  ),
  navbarMenu(tags$b(tags$span(style="color:blue", #7
    HTML("Table 3"))),
    #HTML("Employment and average weekly earnings by industry, SA"))),
    tags$style(type='text/css', ".selectize-input { 
      font-size: 24px; line-height: 24px;} .selectize-dropdown 
      { font-size: 20px; line-height: 20px; }"),
    mt07UI(id="idmt07"),
    mc07UI(id="idmc07")
  ),
  navbarMenu(tags$b(tags$span(style="color:blue", #10
    HTML("Table 4"))),
    #HTML("Employment and average weekly earnings by province by industry, SA"))),
    tags$style(type='text/css', ".selectize-input { 
      font-size: 24px; line-height: 24px;} .selectize-dropdown 
      { font-size: 20px; line-height: 20px; }"),
    mt10UI(id="idmt10"),
    mc10UI(id="idmc10")
  ),
  navbarMenu(tags$b(tags$span(style="color:blue", #6
    HTML("Table 5"))),
    #HTML("Fixed weighted index of AHE by industry and province, NSA"))),
    tags$style(type='text/css', ".selectize-input { 
      font-size: 24px; line-height: 24px;} .selectize-dropdown 
      { font-size: 20px; line-height: 20px; }"),
    mt06UI(id="idmt06"),
    mc06UI(id="idmc06")
  ),
  navbarMenu(tags$b(tags$span(style="color:blue", #1
    HTML("Table 6"))),
    #HTML("Employment by industry and province, NSA"))),
    tags$style(type='text/css', ".selectize-input { 
      font-size: 24px; line-height: 24px;} .selectize-dropdown 
      { font-size: 20px; line-height: 20px; }"),
    mt01UI(id="idmt01"),
    mc01UI(id="idmc01")
  ),
  navbarMenu(tags$b(tags$span(style="color:blue", #2
    HTML("Table 7"))),
    #HTML("Average weekly earnings by industry and province, NSA"))),
    tags$style(type='text/css', ".selectize-input { 
      font-size: 24px; line-height: 24px;} .selectize-dropdown 
      { font-size: 20px; line-height: 20px; }"),
    mt02UI(id="idmt02"),
    mc02UI(id="idmc02")
  ),
  navbarMenu(tags$b(tags$span(style="color:blue", #3
    HTML("Table 8"))),
    #HTML("Average hourly earnings by industry and province, NSA"))),
    tags$style(type='text/css', ".selectize-input { 
      font-size: 24px; line-height: 24px;} .selectize-dropdown 
      { font-size: 20px; line-height: 20px; }"),
    mt03UI(id="idmt03"),
    mc03UI(id="idmc03")
  ),
  navbarMenu(tags$b(tags$span(style="color:blue", #4
    HTML("Table 9"))),
    #HTML("Average hourly earnings for salaried employees by industry and province, NSA"))),
    tags$style(type='text/css', ".selectize-input { 
      font-size: 24px; line-height: 24px;} .selectize-dropdown 
      { font-size: 20px; line-height: 20px; }"),
    mt04UI(id="idmt04"),
    mc04UI(id="idmc04")
  ),
  navbarMenu(tags$b(tags$span(style="color:blue", #5
    HTML("Table 10"))),
    #HTML("Standard work week for salaried employees by industry and province, NSA"))),
    tags$style(type='text/css', ".selectize-input { 
      font-size: 24px; line-height: 24px;} .selectize-dropdown 
      { font-size: 20px; line-height: 20px; }"),
    mt05UI(id="idmt05"),
    mc05UI(id="idmc05")
  ),
  navbarMenu(tags$b(tags$span(style="color:blue", #11
    HTML("Table 11"))),
    #HTML("Average weekly hours by province by industry, NSA"))),
    tags$style(type='text/css', ".selectize-input { 
      font-size: 24px; line-height: 24px;} .selectize-dropdown 
      { font-size: 20px; line-height: 20px; }"),
    mt11UI(id="idmt11"),
    mc11UI(id="idmc11")
  )
)
server <- function(input, output,session) {
  info <- "Info.html"
  output$textInfo <- renderUI(includeHTML(info))
  tbls <- "TblList.html"
  output$tblList <- renderUI(includeHTML(tbls))
  mt01Server(id="idmt01")
  mt02Server(id="idmt02")
  mt03Server(id="idmt03")
  mt04Server(id="idmt04")
  mt05Server(id="idmt05")
  mt06Server(id="idmt06")
  mt07Server(id="idmt07")
  mt08Server(id="idmt08")
  mt09Server(id="idmt09")
  mt10Server(id="idmt10")
  mt11Server(id="idmt11")
  mc01Server(id="idmc01")
  mc02Server(id="idmc02")
  mc03Server(id="idmc03")
  mc04Server(id="idmc04")
  mc05Server(id="idmc05")
  mc06Server(id="idmc06")
  mc07Server(id="idmc07")
  mc08Server(id="idmc08")
  mc09Server(id="idmc09")
  mc10Server(id="idmc10")
  mc11Server(id="idmc11")
}
shinyApp(ui, server)