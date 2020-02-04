server <- function(input, output,session) {

#0.connect to the database
  observeEvent(input$con_db, {
    toggleModal(session,"loginpage",toggle="open")
  })
  con=reactiveValues(cc=NULL)
  tables = reactiveValues()
  observeEvent(input$login, {
    UserName <- isolate(input$userName)
    Password <- isolate(input$passwd)
    conect.test <- dbCanConnect(odbc(),
                                Driver = "SQL Server",
                                Server = "devcrmsql01.ffm.vic.gov.au",
                                Database = "FPSP_App",
                                UID= UserName,
                                PWD= Password,
                                Port = 1433)
    if(conect.test) {
      con$cc = dbConnect(odbc(),
                         Driver = "SQL Server",
                         Server = "devcrmsql01.ffm.vic.gov.au",
                         Database = "FPSP_App",
                         UID= UserName,
                         PWD= Password,
                         Port = 1433)
      toggleModal(session,"loginpage",toggle="close")
      updateButton(session, "con_db","Database connected",disabled = T)
      output$menu <- renderMenu({
        sidebarMenu(
          menuItem("Functions", icon = icon("th"), tabName = "funs"),
          menuItem("Dashboard", icon = icon("dashboard"), tabName = "dashboard"),
          menuItem("Database tables", icon = icon("database"), tabName = "db")
        )
      })
      tableList = as.data.frame(sort(dbListTables(con$cc)))
      names(tableList) = "Table name"
      output$tablelist = DT::renderDataTable({
        datatable(tableList, options = list(autoWidth = TRUE,searching = T))
      })
      #Tables
      #chedule and rop tables
      tables$schedule = dbReadTable(con$cc, "SurveySchedule")
      tables$rop = dbReadTable(con$cc, "ROP")
      tables$trp = dbReadTable(con$cc, "trp")
      #lookups
      tables$lu_technique = dbReadTable(con$cc, "SurveyTech_LU")
      tables$lu_VFstatus = dbReadTable(con$cc, "VFCoupeStatus_LU")
      tables$lu_VFActivity = dbReadTable(con$cc, "VFActivity_LU")
      tables$lu_ScheduleStatus = dbReadTable(con$cc, "ScheduleStatus_LU")
      tables$lu_SurveyStatus = dbReadTable(con$cc, "FieldSurveyStatus_LU")
      tables$lu_Pricode = dbReadTable(con$cc, "PriCode_LU")
      tables$rop = merge(tables$rop, tables$lu_VFstatus[, c("CoupeStatusID","CoupeStatus")], by ="CoupeStatusID",all.x=T, all.y=F)
      tables$rop = merge(tables$rop, tables$lu_VFActivity[, c("ActivityID","AcvityAbbre")], by ="ActivityID",all.x=T, all.y=F)
      
    } else {
      shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
      shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
    }
  })
  
#1. PRIORITISATION FUNCTIONS
#1.1. Creating coupe table (survey matrix)
  #pop-up funtion window
  observeEvent(input$pri_matrix, {
    toggleModal(session,"pri_matrix_page",toggle="open")
  })
  
  #selecting coupes from ROP
  pri_filter <- reactiveValues()
  observe({
    #select coupes by PHD
    pri_filter$phd = input$PHD_range
    min_PHD =  isolate(pri_filter$phd[1])
    max_PHD =  isolate(pri_filter$phd[2])
    pri_filter$pri_rop = tables$rop[tables$rop$ProposedHarvestDate >= min_PHD & tables$rop$ProposedHarvestDate <= max_PHD, ]
    #pop-up checkbox
    CoupeStatus= sort(unique(pri_filter$pri_rop$CoupeStatus))
    updateCheckboxGroupInput(session, inputId ="C_status_selector", choices = CoupeStatus, selected = CoupeStatus)
    AcvityAbbre= sort(unique(pri_filter$pri_rop$AcvityAbbre))
    updateCheckboxGroupInput(session, inputId ="C_silv_selector", choices = AcvityAbbre, selected = AcvityAbbre)
    pri_trp = tables$trp[tables$trp$CoupeID %in% pri_filter$pri_rop$CoupeID, ]
    pri_filter$pri_trp =pri_trp
    trp_source = sort(unique(pri_trp$TRPSource))
    updateCheckboxGroupInput(session, inputId ="C_TRP_selector", choices = trp_source, selected = trp_source)
    pri_tn = sort(unique(tables$lu_technique$SurveyTechPri))
    pri_tn =pri_tn[!pri_tn =="N/A"]
    pri_filter$pri_tn=pri_tn
    updateCheckboxGroupInput(session, inputId ="pri_tn_selector", choices = pri_tn, selected = pri_tn)
  })
  
  observe({
    input$C_status_selector
    input$C_silv_selector
    input$C_TRP_selector
    input$C_com_selector
    input$pri_tn_selector
    pri_filter$tn_selected=input$pri_tn_selector
    #select coupes by status, SilviTyoe and TRP source
    pri_temp = pri_filter$pri_rop[pri_filter$pri_rop$CoupeStatus %in%  input$C_status_selector, ]
    pri_temp = pri_temp[pri_temp$AcvityAbbre %in% input$C_silv_selector, ]
    pri_coupe = pri_filter$pri_trp[pri_filter$pri_trp$TRPSource %in% input$C_TRP_selector, "CoupeID", drop =T]
    pri_temp = pri_temp[pri_temp$CoupeID %in% pri_coupe, ]
    #Select coupes by commitment options
    statusID = tables$lu_ScheduleStatus[tables$lu_ScheduleStatus$ScheduleStatus %in% c("Future", "Current"), "ScheduleStatusID", drop=T]
    pri_ss = tables$schedule[tables$schedule$ScheduleStatusID %in% statusID, ]
    pri_filter$pri_ss = pri_ss
    committed_status = tables$lu_SurveyStatus[tables$lu_SurveyStatus$FieldSurveyStatus %in% c("Planned","In Progress", "Completed"), "FieldSurveyStatusID", drop=T]
    committed_coupes = unique(pri_ss[pri_ss$FieldSurveyStatusID %in% committed_status, "CoupeID", drop=T])
    if(input$C_com_selector=="comm1"){
      pri_committed = length(pri_temp$CoupeID[pri_temp$CoupeID %in% committed_coupes])
      pri_filter$contract_status = c(pri_committed, (nrow(pri_temp) - pri_committed))
    } else{
      pri_temp = pri_temp[!pri_temp$CoupeID %in% committed_coupes, ]
      pri_filter$contract_status = c(0, nrow(pri_temp))
    }
    #remove dunlicates by CoupeID
    #dup.check = duplicate.df(pri_temp, "CoupeID")
    #pri_filter$pri_rop1 = pri_temp#dup.check[[1]]
    #if(dup.check[[2]] != "none"){
    #  output$pri_warning <- renderText({dup.check[[2]]})
    #}
    pri_filter$pri_rop1 = pri_temp
  })
  #output message - Display number of coupes selected
  output$C_selected <- renderText({
    paste0("No. coupes selected: ",nrow(pri_filter$pri_rop1),
          " (Committed: ", pri_filter$contract_status[1], 
          "; Non-committed: ",pri_filter$contract_status[2],")")
  })
  #creating the coupe table
  observeEvent(input$run_pri_matrix,{
    pri_filter$matrix = pri_matrix(pri_rop =pri_filter$pri_rop1,
                                   pri_ss = pri_filter$pri_ss,
                                   pri_tn = pri_filter$pri_tn,
                                   tn_selected = pri_filter$tn_selected,
                                   lu_Pricode = tables$lu_Pricode,
                                   lu_technique = tables$lu_technique)
    output$Pri_matrix_dwl<-renderUI({
      downloadButton('dwl_pri_matrix',tags$b("Prioritisation_input_table (csv)",style = "color: green; background-color: white"))
    })
    output$dwl_pri_matrix<-downloadHandler(
      filename=function(){paste0("Pri_Input_Table_(",nrow(pri_filter$matrix),
                                 "Coupes_Committed", pri_filter$contract_status[1], 
                                 "_Noncommitted",pri_filter$contract_status[2],")_",Sys.Date(),".csv")}
      ,content=function(file){write.csv(pri_filter$matrix,file, row.names = F)}
    )
    toggleModal(session,"pri_matrix_page",toggle="close")
  })
 
#1.2.Bufget calculation
#1.3.Run pri app
  observeEvent(input$pri_run, {
    message("running prioritisation app")
    rstudioapi::jobRunScript(path = 'G:/LFE_Group/FEM/Knowledge_Engagement/Monitor_Eval_Research/Projects/FPSP/Data/scripts/R/FPSP_Functions/pri_run.R')
  })
#2.SURVEY PACKAGING
  
}