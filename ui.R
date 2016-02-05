library(shinydashboard)
source("helpers.R")

####rakenduse interface
dashboardPage(
  dashboardHeader(title = "Riigiteenused",  titleWidth = 200),
  dashboardSidebar(
    width = 200,
    ####külgmenüü
    sidebarMenu(
      menuItem("Üldine", icon = icon("institution"),tabName = "uldine"),
      menuItem("Ministeeriumite lõikes", icon = icon("building"),
               tabName = "minloikes"),
      menuItem("Asutuste lõikes", icon = icon("home"),tabName = "asutloikes"),
      menuItem("Info", icon = icon("info"),tabName = "info")
      ######KUSTUTAMISEKS
#       downloadButton('downloadData', 'Download'),
#       downloadButton('downloadData2', 'Download')
      #################
    )
  ),
  dashboardBody(
    tabItems(
      ###üldine body
      tabItem(tabName = "uldine",
              fluidRow(width=4, valueBoxOutput("MinArv"),
                       width=4, valueBoxOutput("AsutusteArv"),
                       width=4, valueBoxOutput("TeenusteArv")),
              fluidRow(
                box(width=6, plotOutput("TeenuseidKanalis", height = 250)),
                box(width=6, plotOutput("Moodikuid", height = 250))),
              fluidRow(
                width=4, valueBoxOutput("Kasutuskordi"),
                width=4, valueBoxOutput("Rahulolu"),
                width=4, valueBoxOutput("Maksumus"),
                width=4, valueBoxOutput("Ajakulu")
              )
      ),
      ####ministeeriumite lõikes body
      tabItem(tabName = "minloikes",
              fluidRow(box(width=4,title = "Haldusala",solidHeader = TRUE,
                           background = "light-blue", collapsible = F,
                           "Vali haldusala",
                           uiOutput("ministeerium"), height=150),
                       width=4, valueBoxOutput("MinTeenusteArv"),
                       width=4, valueBoxOutput("MinAsutusteArv")
              ),
              fluidRow(
                box(width=6, plotOutput("TeenuseidKanalisMin", height = 250)),
                box(width=6, plotOutput("MoodikuidMin", height = 250))),
              fluidRow(
                width=4, valueBoxOutput("MinKasutuskordi"),
                width=4, valueBoxOutput("MinRahulolu"),
                width=4, valueBoxOutput("MinMaksumus"),
                width=4, valueBoxOutput("MinAjakulu")
              )
      ),
      ####asutuste lõikes body
      tabItem(tabName = "asutloikes",
              fluidRow(box(width=4,title = "Allasutus",background = "light-blue",
                           solidHeader = TRUE,  "Vali haldusala",
                           uiOutput("ministeerium2"),
                           "Vali allasutus", uiOutput("allasutus"), height=250),
                       box(width=8, plotOutput("TeenuseidKanalisAsut", height = 250)),
                       box(width=4, plotOutput("MoodikuidAsut", height = 250)),
                       width=4, valueBoxOutput("AsutTeenusteArv"),
                       valueBoxOutput("AsutKasutuskordi"),
                       valueBoxOutput("AsutRahulolu"),
                       valueBoxOutput("AsutMaksumus"),
                       valueBoxOutput("AsutAjakulu")
              )
      ),
      tabItem(tabName = "info",
            fluidRow(box(width=12,
                         tags$h4("Tegemist on riigiteenuste andmete 
visualiseeringuga. Projekti kohta loe", 
                                 a("siit.",target="_blank",href="https://github.com/MKM-ITAO/riigiteenused"),
HTML(paste("Kuna tegemist on pilootprojektiga ning mõõtmismetoodika 
           on kujunemisjärgus, on andmetest ", tags$span(style="color:red", "järelduste tegemine 
omal vastutusel!"), sep = "")), "Rakenduse kood on", 
                    a("siin.",target="_blank",href="https://github.com/RRisto/Riigiteenused_shinyApp"),
"Andmed on seisuga 05.02.2016, kell 08:39."
)))
    ))
  )
)