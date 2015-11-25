
library(shinydashboard)

dashboardPage(
  dashboardHeader(title = "",  titleWidth = 200),
  dashboardSidebar(
    width = 200,
        sidebarMenu(
          menuItem("Üldine", icon = icon("th"),tabName = "uldine"),
          menuItem("Ministeeriumite lõikes", icon = icon("th"),tabName = "minloikes"),
          menuItem("Asutuste lõikes", icon = icon("th"),tabName = "asutloikes")
          
    )
  ),
  dashboardBody(
    tabItems(
    ###üldine body
      tabItem(tabName = "uldine",
              fluidRow(box(h2("siia sisu"))
              )
      ),
      ####min lõikes body
      tabItem(tabName = "minloikes",
    fluidRow(box(width=4,title = "Sisendid",background = "light-blue", selectInput("ministeerium", 
                                                                              label = h5("Ministeerium"), 
                         choices = list("Haridus- ja Teadusministeerium" = "Haridus- ja Teadusministeeriumi haldusala",
                                        "Justiitsministeerium" = "Justiitsministeeriumi haldusala",
                                        "Keskkonnaministeerium" = "Keskkonnaministeeriumi haldusala",
                                        "Kultuuriministeerium"="Kultuuriministeeriumi haldusala",
                                        "Majandus- ja Kommunikatsiooniministeerium"="Majandus- ja Kommunikatsiooniministeeriumi haldusala",
                                        "Rahandusministeerium"="Rahandusministeeriumi haldusala",
                                        "Siseministeerium"="Siseministeeriumi haldusala",
                                        "Sotsiaalministeerium"="Sotsiaalministeeriumi haldusala",
                                        "Kaitseministeerium"="Kaitseministeeriumi haldusala",
                                        "Maaeluministeerium"="Maaeluministeeriumi haldusala",
                                        "Välisministeerium"="Välisministeeriumi haldusala"), 
                         selected = 1),
             selectInput("naitaja", label = h5("Näitaja"), 
                         choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3), 
                         selected = 1)),
             fluidRow(box(textOutput("text1")))
    )
  ),
  ####asutuste lõikes body
  tabItem(tabName = "asutloikes",
          fluidRow(box(h2("siia sisu"))
          )
  )
  
)
)
)