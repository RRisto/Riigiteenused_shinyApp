library(shinydashboard)
source("helpers.R")

andmed=read.table("./andmed/2015-11-24_andmedPikk.txt", sep=";", header=T)
#ministeeriumite nimed dropdowni
andmed$ministeerium=gsub("i haldusala", "", andmed$ministeerium)
minnid=as.character(unique(andmed$ministeerium))
#asutused dropdowni
asutused=as.character(unique(andmed$allasutus))

####rakenduse interface
dashboardPage(
  dashboardHeader(title = "",  titleWidth = 200),
  dashboardSidebar(
    width = 200,
    ####külgmenüü
    sidebarMenu(
      menuItem("Üldine", icon = icon("institution"),tabName = "uldine"),
      menuItem("Ministeeriumite lõikes", icon = icon("building"),
               tabName = "minloikes"),
      menuItem("Asutuste lõikes", icon = icon("home"),tabName = "asutloikes")
    )
  ),
  dashboardBody(
    tabItems(
      ###üldine body
      tabItem(tabName = "uldine",
              fluidRow(box(h2("siia sisu"))
              )
      ),
      ####ministeeriumite lõikes body
      tabItem(tabName = "minloikes",
              fluidRow(box(width=4,title = "Haldusala",solidHeader = TRUE,
                           background = "light-blue", collapsible = TRUE,
                           selectInput("ministeerium", label = h6(""),
                                       minnid), height=100),
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
              fluidRow(box(width=4,title = "Sisendid",background = "light-blue", 
                           selectInput("asutused", 
                                       label = h5("Asutus"), asutused))
              )
      )
    )
  )
)