.libPaths("C:/Data/R_Library")
source("Functions.R")
package_load(c("shiny","shinydashboard","leaflet", "shinyBS", "shinyjs","rhandsontable"))
package_load(c("odbc","DBI","rstudioapi", "DT"))
package_load(c("rstudioapi","rgdal","stringi","varhandle","odbc","DBI", "shinyWidgets"))
options(stringsAsFactors = F)

##################
##Interface
loginpage <- div(style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                 wellPanel(
                   tags$h2("Connecting to FPSP database", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                   br(),
                   textInput("userName", placeholder="Username", label = tagList(icon("user"), "Username"), value="fpspAdmin"),
                   passwordInput("passwd", placeholder="Password", label = tagList(icon("unlock-alt"), "Password"), value = "KhaiDat12345@"),
                   br(),
                   div(
                     style = "text-align: center;",
                     actionButton("login", "CONNECT", style = "color: white; background-color:#3c8dbc;
                                  padding: 10px 15px; width: 150px; cursor: pointer;
                                  font-size: 18px; font-weight: 600;"),
                     shinyjs::hidden(
                       div(id = "nomatch",
                           tags$p("Oops! Incorrect username or password!",
                                  style = "color: red; font-weight: 600; 
                                  padding-top: 5px;font-size:16px;", 
                                  class = "text-center"))),
                     br(),
                     br(),
                     tags$code("Note: You need to connect the corporate network first"),
                     br()
                   ))
)


tables = tabItem(tabName = "db",fluidRow(box(width = 12, dataTableOutput('tablelist'))))
dashboard = tabItem(tabName = "dashboard",h2("Dashboard tab content being developed"))
fun =tabItem(tabName = "funs",h2("Select a function to run"),
                absolutePanel(h2("Survey prioritisation", style= "font-size: 20px; font-weight: 600"), top=150,left="25%",
                              bsButton("pri_matrix","Create input table (csv file)",icon = icon("table")),
                              bsButton("pri_bg","Calculate budget input"),
                              bsButton("pri_run","Run prioritisation app"),
                              bsButton("pri_update","Update master outputs")),
                absolutePanel(h2("Survey scheduling", style= "font-size: 20px; font-weight: 600"), top=250,left="25%",
                              bsButton("sche_pg","Create costingform and data"),
                              bsButton("sche_update","Update costingform to schedule"),
                              bsButton("f1","Function",disabled=T),
                              bsButton("f2","Function",disabled=T)),
                absolutePanel(h2("Contract management", style= "font-size: 20px; font-weight: 600"), top=350,left="25%",
                              bsButton("cm_cost","Update costingform to ContractMaster"),
                              bsButton("cm_var","Update variationform to ContractMaster"),
                              bsButton("cm_invoi1","Create an invoice template"),
                              bsButton("cm_invoi1","Update invoicingform to ContractMaster",disabled=T)),
                absolutePanel(h2("Survey reporting", style= "font-size: 20px; font-weight: 600"), top=450,left="25%",
                              bsButton("re_fid","Update FID"),
                              bsButton("re_kpi","Calculate FPSP KPI"),
                              bsButton("f1","Function",disabled=T),
                              bsButton("f2","Function",disabled=T)),
                absolutePanel(h2("Other functions", style= "font-size: 20px; font-weight: 600"), top=550,left="25%",
                              bsButton("ot_tb","Write a table to database"),
                              bsButton("ot_ly","Write a layer to database"),
                              bsButton("f1","Function",disabled=T),
                              bsButton("f2","Function",disabled=T)),
                absolutePanel(h2("Output files", style= "font-size: 20px; font-weight: 600"), top=650,left="25%",
                              uiOutput("Pri_matrix_dwl"),
                              uiOutput("Pri_bg_dwl"))
                )


#######
sidebar <- dashboardSidebar(
  bsButton("con_db","Connecting database",disabled = F),
  sidebarMenu(id = "tabs", sidebarMenuOutput("menu"))
)

#######
body <- dashboardBody(
  shinyjs::useShinyjs(),
  tabItems(tables,dashboard,fun),
  bsModal(id = "loginpage", trigger="notrigger", title ="",loginpage),
  bsModal(id = "pri_matrix_page",title ="Select coupe from ROP for prioritisation",size="large",trigger="notrigger",
          dateRangeInput("PHD_range", "1. Select a range of PHD:", start = Sys.Date(),end=Sys.Date()+365, 
                         format = "dd/mm/yyyy",startview = "month", width='50%'),
          fluidRow(column(width=5,checkboxGroupInput(inputId = "C_status_selector",label = "Select coupe status:")),
                   column(width=3,checkboxGroupInput(inputId = "C_silv_selector", label = "Select SilvilType:")),
                   column(width=4,checkboxGroupInput(inputId = "pri_tn_selector", label = "Select TN included in the run:"))
          ),
          fluidRow(column(width=7,radioButtons(inputId = "C_com_selector",label = "Select commitment options:",
                                               choices = c("Both committed and non-committed coupes"="comm1",
                                                           "Only non-committed coupes (All techniques are accounted for)" ="comm2"), selected ="comm1")),
                   column(width=5,checkboxGroupInput(inputId = "C_TRP_selector",label = "Select TRP source:"))
                   ),
          tags$code(textOutput("pri_warning")),
          fluidRow(
            column(width=10,tags$code(textOutput("C_selected"))),
            column(width=2,bsButton("run_pri_matrix","Run",style="success", size = "large",disabled = F))
                  )
          #tags$style(type = "text/css", ".datepicker{z-index: 1100 !important;}")
          )
  
)

ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "FPSP Database functions", titleWidth = 350),
  sidebar,body
)




