
server <- function(input, output, session) {
  
  
  # Daten laden --------------------------------------------------------------
  
  
  
  observeEvent(input$randv,{
    
    if(req(input$randv)!= 0 & req(input$Versdata)!= 0){
      
      
      if( req(input$randv) !=0 & req(input$Versdata) !=0 ){
       updateButton(
           session, 
           inputId = "start_data", 
           label = "DATEN LADEN", 
           icon = icon("bar-chart-o"), 
           style = "primary_ok")
         
       }
    }})
    
    
  observeEvent(input$obsRandv,{
      
      workEnvir$DF <-  fread(input$randv$datapath, sep = input$sep)
      output$df <- renderDataTable({ datatable(workEnvir$DF, options = list(orderClasses = TRUE, pageLength = 10))})
      output$box_pat1 <-  box_create("box_pat1","df","Eingelesene Randverteilung")
      
  })
  
  
  observeEvent(input$Versdata,{
    if( req(input$randv) !=0 & req(input$Versdata) !=0 ){
      updateButton(
        session, 
        inputId = "start_data", 
        label = "DATEN LADEN", 
        icon = icon("bar-chart-o"), 
        style = "primary_ok")
      
      
    }
    

  })  
  
  Tab_Randverteilung <- function(){
    
    
    if(exists("DX", envir = workEnvir) & exists("DF", envir = workEnvir)){
      
      
      Namen <- workEnvir$DX@Dimnames[[2]]
      Match <- match(Namen, workEnvir$DF$Risikogruppe)
        
      Ziel <- workEnvir$DF$Wert
      Ziel[workEnvir$DF$Einheit=="Euro"] <- Ziel[workEnvir$DF$Einheit=="Euro"]/input$Standardkosten
      Ziel[workEnvir$DF$Einheit=="Diagnosen"] <- Ziel[workEnvir$DF$Einheit=="Diagnosen"]/input$Diagnosefaktor
      Ziel[workEnvir$DF$Einheit=="VT"] <- Ziel[workEnvir$DF$Einheit=="VT"]/365
      
      Ziel <- Ziel[Match]
      workEnvir$constraints <- Ziel
      Ist  <-  colSums(workEnvir$DX)
      
      workEnvir$GKV.Versicherte <- GKV.Versicherte <- input$GKV.Versicherte
      workEnvir$Versicherte <- Versicherte <- nrow(workEnvir$DX)  
      
      workEnvir$Hochrehnung <- workEnvir$GKV.Versicherte/workEnvir$Versicherte
      
      workEnvir$DR <- data.table(Name=Namen
                                 ,Typ=workEnvir$DF$Typ[Match]
                                 ,Ist=round(Ist,0)
                                 ,Ist_hochgerechnet=round(Ist*workEnvir$Hochrehnung,0)
                                 ,Ziel=round(Ziel,0) 
                                 ,Fehler=round(Ist*workEnvir$Hochrehnung-Ziel,2)
                                 ,Deckung=round((Ist/Ziel)*workEnvir$Hochrehnung,2))
      
      
      
      
      workEnvir$DR$Typ <- as.character(workEnvir$DR$Typ)
      parent <- workEnvir$DR[,list(Fehler=sum(abs(Fehler))),by=Typ]
      labels <- c(parent$Typ,workEnvir$DR$Name)
      parents <- c(rep("",nrow(parent)),workEnvir$DR$Typ)
      value <- c(parent$Fehler,workEnvir$DR$Fehler)
      
      workEnvir$fig <- plot_ly(
        labels = labels,
        parents = parents,
        values = abs(value),
        type = 'treemap'
        # branchvalues = 'total'
      )
      
      output$tab_fig <- renderPlotly({
        workEnvir$fig
      })
      
      
    }else{
      workEnvir$DR <- data.table(Name=NA,Typ=NA, Ziel=NA, Ist=NA)
    }
    
  }  
 
  
  Tab_Stichprobe <- function(){
    
    Stichprobenergebnis  <-  colSums(workEnvir$X*workEnvir$Stichprobe)
    
    workEnvir$SR <- data.table(Name=workEnvir$DR$Name
                               ,Typ=workEnvir$DR$Typ
                               ,Ist=workEnvir$DR$Ist
                               ,Ist_hochgerechnet=workEnvir$DR$Ist_hochgerechnet
                               ,Stichprobe = round(Stichprobenergebnis,2)
                               ,Ziel=round(workEnvir$DR$Ziel,0)
                               ,Fehler=round(Stichprobenergebnis-workEnvir$DR$Ziel,2)
                               ,Deckung=round((Stichprobenergebnis/workEnvir$DR$Ziel),2))
    
    
    output$ds <- renderDataTable({
      datatable(workEnvir$SR
                , options = list(orderClasses = TRUE, pageLength = 10))})
    
    
    workEnvir$SR$Typ <- as.character(workEnvir$SR$Typ)
    parent <- workEnvir$SR[,list(Fehler=sum(abs(Fehler))),by=Typ]
    labels <- c(parent$Typ,workEnvir$SR$Name)
    parents <- c(rep("",nrow(parent)),workEnvir$SR$Typ)
    value <- c(parent$Fehler,workEnvir$SR$Fehler)
    
    workEnvir$fig_erg <- plot_ly(
      labels = labels,
      parents = parents,
      values = abs(value),
      type = 'treemap'
      # branchvalues = 'total'
    )
    
    output$tab_fig_erg <- renderPlotly({
      workEnvir$fig_erg 
    })
    
    
    
  }
   
  observeEvent(input$start_data, {
    
    if(req(input$randv)!= 0 & req(input$Versdata)!= 0){
    
      
      workEnvir$DF <-  read.table(input$randv$datapath,
                             sep = input$sep,dec=",",header=TRUE)
      
      
      
      output$df <- renderDataTable({
        datatable(workEnvir$DF, options = list(orderClasses = TRUE, pageLength = 10))})
      
      
      load(input$Versdata$datapath)
      
      workEnvir$X <- X
      workEnvir$DX <- X
      dx <- colSums(X)
      dx <- data.frame(Typ=substring(names(dx),1,3)
                       ,Namen=names(dx)
                       ,Anzahl=dx)
      
      row.names(dx) <- NULL
      
      output$dx<- renderDataTable({
        datatable(dx, options = list(orderClasses = TRUE, pageLength = 10))})
      
     Tab_Randverteilung() 
      
      
    updateButton(
      session, 
      inputId = "start_data", 
      label = "DATEN GELADEN", 
      icon = icon("bar-chart-o"), 
      style = "primary")
    
    
    updateButton(
      session, 
      inputId = "confirm", 
      label = "START SAMPLE", 
      icon = icon("bar-chart-o"), 
      style = "primary_ok")
    
    
    sampling_ready <<- TRUE
    
    
    output$selectUI <- renderUI({ 
      selectInput("out_item", "Ausschluss von:", workEnvir$DR$Name,multiple = TRUE)
    })
    
    
  }else{
      output$oText <- renderText({ "Daten Quellen eintragen!" })
  }
  })
    
  
  # Stichprobe ziehen --------------------------------------------------------------
  
  observeEvent(input$confirm,{ 
  
    if(sampling_ready){
  
      updateButton(
        session, 
        inputId = "confirm", 
        label = "START SAMPLING", 
        icon = icon("bar-chart-o"), 
        style = "primary")
  
      
  progress <- shiny::Progress$new()
  on.exit(progress$close())
      
  progress$set(message = "Teste auf Multikollinearität", value = 0)  
      
  
  
  if(sum(is.na(workEnvir$constraints))>0){
    out <- which(is.na(workEnvir$constraints))
    workEnvir$DX <- workEnvir$DX[,-out]
    workEnvir$constraints <- workEnvir$constraints[-out]
  } 
  
  if(length(input$out_item)>0){
     out <- which(workEnvir$DX@Dimnames[[2]] %in% input$out_item)
     workEnvir$DX <- workEnvir$DX[,-out]
     workEnvir$constraints <- workEnvir$constraints[-out]
  }
  
  all_out <- c()
  repeat{
    out <- multi.col(workEnvir$DX,1/input$multicol)[1]
    if(length(out)>0){
      workEnvir$DX <- workEnvir$DX[,-out]
      workEnvir$constraints <- workEnvir$constraints[-out]
      all_out <- c(all_out,workEnvir$DX@Dimnames[[2]][out])
    }else{
      cat(paste0("Ausschluss: ",all_out,"   ","\n"))
      break}
  }
  
  
  # Entropy Balancing
  
  base.size <- nrow(workEnvir$DX)
  constrains.size <- ncol(workEnvir$DX)

  workEnvir$base.weight = rep(workEnvir$GKV.Versicherte/workEnvir$Versicherte, workEnvir$Versicherte)

  workEnvir$tr.total <- c(workEnvir$GKV.Versicherte,workEnvir$constraints)
  workEnvir$co.x <- cBind(rep(1,nrow(workEnvir$DX)),workEnvir$DX)

  workEnvir$coefs = c(log(workEnvir$GKV.Versicherte/workEnvir$Versicherte),rep(0,(ncol(workEnvir$co.x)-1)))
  
  ## run algo
  
  workEnvir$Status <- list(
    base.size = workEnvir$Versicherte
    ,Base.weight  = workEnvir$base.weight
    ,weights.ebal = workEnvir$base.weight
    ,X = workEnvir$co.x
    ,Tr.total   =  workEnvir$tr.total
    ,coefs   = workEnvir$coefs
    ,Newton   = 0
    ,gradient = 99999999
    ,weight.max   = input$maxSampleWeight * (workEnvir$GKV.Versicherte/workEnvir$Versicherte) 
    ,ss   = 0
  )
  
  progress$set(message = "Ziehe Stichprobe", value = 0)  
  
  for(i in 1:input$n_sample_try){
    if(mean(abs(workEnvir$Status[["gradient"]])>10)){
      
      progress$inc(1/input$n_sample_try, detail = paste("Fehler", round(mean(abs(workEnvir$Status[["gradient"]]) ),0 ) ) )

     # output$oText <- renderText({ paste0("Iteration: ", i, "   ") } )
      workEnvir$Status <- eb_step(workEnvir$Status,print.level=3)
      
    }
    
  }
  
  workEnvir$Stichprobe <- workEnvir$Status[["weights.ebal"]]
  Tab_Stichprobe()
  
  output$ds <- renderDataTable({
    datatable(workEnvir$DS
              , options = list(orderClasses = TRUE, pageLength = 10))})
  
  }})  
  
  # Daten Tabellen darstellen -------------------------------------------------------------- 
  
  box_create <- function(id,call_df,title){
    
    return( renderUI({
      div(
        style = "position: relative",
        tabBox(
          id = id,
          width = NULL,
          height = 400,
          tabPanel(
            title = title,
            #htmlOutput("patients_total"),
            withSpinner(
              DT::dataTableOutput(call_df),
              type = 4,
              color = "#d33724",
              size = 0.7
            )
          )
        )
      )
    })
    )
    
  }
  
  box_tab_create <- function(id,call_df,title,call_fig){
    
    return( renderUI({
      div(
        style = "position: relative",
        tabBox(
          id = id,
          width = NULL,
          height = 400,
          
          
          # tab 1
          
          
          tabPanel(
            title = title,
            #htmlOutput("patients_total"),
            withSpinner(
              DT::dataTableOutput(call_df),
              type = 4,
              color = "#d33724",
              size = 0.7
            )
          ),
          
          
          # tab 2
          
          tabPanel(
            title = title,
            #htmlOutput("patients_total"),
            withSpinner(
              plotlyOutput(call_fig, height = 300),
              type = 4,
              color = "#d33724",
              size = 0.7
            )
          )
          
        )
      )
    })
    )
    
  }


 
  
  
  # DYNAMIC RENDER RULES ----------------------------------------------------
  
  
   observeEvent("", {
    hide("tab")
  })
  
  observeEvent("", {
    show("patients_panel")
    hide("randverteilung_panel")
    hide("outcome_panel")
    
    Tab_Randverteilung()
    output$dr <- renderDataTable({
                  datatable(workEnvir$DR
                , options = list(orderClasses = TRUE, pageLength = 10))})
    
    
    
    output$box_pat1 <-  NULL#box_create("box_pat1","df","Eingelesene Randverteilung")
    output$box_pat2 <-  NULL#box_create("box_pat2","dx","Eingelesene Versichertendaten")
    
  }, once = TRUE)
  
  observeEvent(input$randverteilung, {
    hide("patients_panel")
    show("randverteilung_panel")
    hide("outcome_panel")
    
    output$box_pat1 <-   box_tab_create("box_pat1","dr","Abweichung der Grunddaten","tab_fig")
    output$box_pat2 <-  NULL
    
  })
  
  observeEvent(input$patients, {
    show("patients_panel")
    hide("randverteilung_panel")
    hide("outcome_panel")
    
    
    output$box_pat1 <-  box_create("box_pat1","df","Eingelesene Randverteilung")
    output$box_pat2 <-  box_create("box_pat2","dx","Eingelesene Versichertendaten")
    
    
    
  })
  
  observeEvent(input$outcome, {
    show("outcome_panel")
    hide("randverteilung_panel")
    hide("patients_panel")
    
    output$box_pat1 <-   box_tab_create("box_pat1","ds","Abweichung der Stichprobe","tab_fig_erg")
    output$box_pat2 <-  NULL
    
  })
  
  
  # show active button with color
  
  observeEvent(input$tab, {
    x <- input$tab
    updateButton(session, "patients", style = {
      if (x == "Daten") {
        paste("warning")
      } else {
        paste("success")
      }
    })
    updateButton(session, "randverteilung", style = {
      if (x == "Randverteilung") {
        paste("warning")
      } else {
        paste("success")
      }
    })
    updateButton(session, "outcome", style = {
      if (x == "Outcome") {
        paste("warning")
      } else {
        paste("success")
      }
    })
  })
  
  
  


  # DOWNLOAD ----------------------------------------------------------------
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$filename, Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data.frame(ID=workEnvir$X@Dimnames[[1]]
                          ,Gewicht=workEnvir$Stichprobe)
                , file
                ,sep=";",dec=","
                ,row.names=FALSE)
    }
  )
  
  output$downloadMicroData <- downloadHandler(
    filename = function() {
      paste(input$filename, Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(workEnvir$SR
                , file
                ,sep=";",dec=","
                ,row.names=FALSE)
    }
  )
  
  download_box <- function(exportname, plot) {
    downloadHandler(
      filename = function() {
        paste(exportname, Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        ggsave(file, plot = plot, device = "png", width = 8)
      }
    )
  }
  

  
  
}
