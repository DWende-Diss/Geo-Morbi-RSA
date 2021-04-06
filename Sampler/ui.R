

ui <- dashboardPage(
  skin = "black",
  title = "RSampler",
  
  # HEADER ------------------------------------------------------------------
  
  dashboardHeader(
    title = span(img(src = "radar.svg", height = 35), "RSampler"),
    titleWidth = 300,
    dropdownMenu(
      type = "notifications", 
      headerText = strong("HELP"), 
      icon = icon("question"), 
      badgeStatus = NULL,
      notificationItem(
        text = (steps$text[1]),
        icon = icon("spinner")
      ),
      notificationItem(
        text = steps$text[2],
        icon = icon("address-card")
      ),
      notificationItem(
        text = steps$text[3],
        icon = icon("calendar")
      ),
      notificationItem(
        text = steps$text[4],
        icon = icon("user-md")
      ),
      notificationItem(
        text = steps$text[5],
        icon = icon("ambulance")
      ),
      notificationItem(
        text = steps$text[6],
        icon = icon("flask")
      ),
      notificationItem(
        text = strong(steps$text[7]),
        icon = icon("exclamation")
      )
    ),
    tags$li(
      a(
        strong("ABOUT RSample"),
        height = 40,
        href = "",
        title = "",
        target = "_blank"
      ),
      class = "dropdown"
    )
  ),
  
  # SIDEBAR -----------------------------------------------------------------
  
  dashboardSidebar(
    width = 300,
    introBox(#data.step = 3, data.intro = intro$text[3], #  intro tour
             #div(class = "inlay", style = "height:15px;width:100%;background-color:  #1aa0e0;"),
                        
                        
              # ),
              

# Menue Randverteilung ----------------------------------------------------


               menuItem(
                 "Randverteilung",
                 tabName = "Randv",
                 icon = icon("user-md"),
                 div( 
                 fileInput("randv", "Randverteilung (CSV)",
                           accept = c(
                             "text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")
                 )
                 ),
                 div(
                 radioButtons("sep", "Separator",
                              choices = c(Comma = ",",
                                          Semicolon = ";",
                                          Tab = "\t"),
                              selected = ","),
                    actionButton("obsRandv", "Teste Daten")
                 )
               ),
 

# Menue Versichertendaten -------------------------------------------------


              
               br(),
              menuItem(
                "Versichertendaten",
                icon = icon("calendar"),
                tabName = "versdata",

                fileInput("Versdata", "Versichertendaten (Rdata)",
                          accept = c(".Rdata"))
                #actionButton("obsVers", "Lade Versichertendaten"),
                #br()

              ),
  

# Button Datenladen -------------------------------------------------

br(),
bsButton(inputId = "start_data",
         label = "Lade Daten",
         icon = icon("play-circle"),
         style = "primary"),


# Menue Einstellungen -----------------------------------------------------


             
              br(), 
              menuItem(
                "Einstellungen",
                icon = icon("address-card"),
                tabName = "options",
                
                numericInput(inputId = "GKV.Versicherte",
                             label = "GKV Versichertenzahl",
                             value = 72485407),
                
                numericInput(inputId = "Standardkosten",
                             label = "Standardkosten (jährlich)",
                             value = 2500),
                
                
                numericInput(inputId = "Diagnosefaktor",
                             label = "Anzahl Diagnosen je Versicherten",
                             value = 29),
                
                sliderInput("maxSampleWeight", "Maximales relatives Versichertengewicht:",
                            min = 0, max = 50,
                            value = 5, step = 0.5),
                
                
                sliderInput("multicol", "Sensitivität der Mulikollinearitaetskontrolle",
                            min = 0, max = 100,
                            value = 10, step = 1),
                
                sliderInput("n_sample_try", "Maximale Anzahl an Stichproben",
                            min = 1, max = 100,
                            value = 10, step = 1)
                
                
              ), 

# Ausschlussgruppen -----------------------------------------------------
br(),
menuItem(
  "Ausschluss",
  icon = icon("address-card"),
  tabName = "out_item_tab",

 # selectInput("out_item", "Ausschluss von:",            choices=workEnvir$DR$Name,multiple = TRUE)
 htmlOutput("selectUI")
     
),  
            
# Button Start -------------------------------------------------
br(),
sidebarMenu(
  #introBox(data.step = 1, data.intro = intro$text[1], # intro tour
  div(id = "sidebar_button",
      bsButton(inputId = "confirm",
               label = "START SAMPLE",
               icon = icon("play-circle"),
               style = "primary")
      #style = "not_ready")
  ),
  
# Menue Download ----------------------------------------------------------


              br(), 
               menuItem(
                 "DOWNLOAD Result",
                 tabName = "download",
                 icon = icon("download"),
                 textInput(
                   inputId = "filename",
                   placeholder = "Name download file",
                   label = ""
                 ),
                 div(
                   downloadButton(
                     outputId = "downloadData",
                     label = "Speichern der Stichprobe",
                     icon = icon("download"),
                     style = "color: black; margin-left: 15px; margin-bottom: 5px;"
                   )
                 ),
                 div(
                   downloadButton(
                     outputId = "downloadMicroData",
                     label = "Speichern der QS-Datei",
                     icon = icon("download"),
                     style = "color: black; margin-left: 15px; margin-bottom: 5px;"
                   )
                 )
               ),
               br()

             )
    )),
  
  

  
  
  
  # BODY --------------------------------------------------------------------
  
  dashboardBody(
    tags$head(
      tags$link(
        rel = "stylesheet", 
        type = "text/css", 
        href = "wig_style.css")
    ),
    
    useShinyjs(),
    introjsUI(),
    
    # MAIN BODY ---------------------------------------------------------------
    
    fluidRow(
      column(
        width = 12,
        introBox(
          bsButton("patients", 
                   label = "Daten", 
                   icon = icon("user"), 
                   style = "success"),
          bsButton("randverteilung", 
                   label = "Randverteilung", 
                   icon = icon("spinner", class = "spinner-box"), 
                   style = "success"),
          bsButton("outcome", 
                   label = "OUTCOME", 
                   icon = icon("thumbs-o-up"), 
                   style = "success"),
          data.step = 2, data.intro = intro$text[2])
      )
    ),
    
    verbatimTextOutput("oText"),
    #box(DT::dataTableOutput("df")),
    
  #  tableOutput("rawData"),
  
  
  #  fluid_design("randverteilung_panel", "box_test"),
  #  fluid_design("diagnostics_panel", "box5", "box6", "box7", "box8"),
  #  fluid_design("outcome_panel", "box_los1", "box_los2", "box_los3", NULL),
    
   # fluidRow(
    #    id="patients_panel",
          uiOutput("box_pat1"),
          uiOutput("box_pat2")
    #)
  
#  fluidRow(
#    div(
#     id = "randverteilung_panel", 
#      uiOutput("box_pat1"),
#      uiOutput("box_pat2"),
#    )
#  )
  
  
  
  )
)
