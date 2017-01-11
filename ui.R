library(shinydashboard)
source("helpers.R")
library(shinyjs)#vajalik laeb teksti kuvamiseks
#CSSi klass, mis kuvab "laeb" teksti, kui andmeid laetase
appCSS <- "
#loading-content {
position: absolute;
background: #A9D0F5;
opacity: 0.8;
z-index: 100;
left: 0;
right: 0;
height: 100%;
text-align: center;
color: #FFFFFF;
}
"
####rakenduse interface
dashboardPage(
  dashboardHeader(title = "Riigiteenused",  titleWidth = 200),
  dashboardSidebar(
    width = 200,
    #see on customiseeritud cssi jaoks
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    ####külgmenüü
    #sidebarMenu(
    h4(htmlOutput("valiVaade")),
    sidebarMenuOutput("kylgmenuu"),
    #keelevalik
    p(""),
    radioButtons(inputId = "keel", label = NULL,
                 choices = c("Eesti keeles" = "et", "In English" = "en"),
                 selected = "et")
    
    #####KUSTUTAMISEKS vaja ainult andmete rakendusest alla laadimiseks
                # downloadButton('downloadData', 'Download'),
                #  downloadButton('downloadData2', 'Download')
    ################
    
  ),
  dashboardBody(
    ##see osa vajalik "laeb" teate kuvamiseks andmete sisselaadimisel  
    useShinyjs(),
    inlineCSS(appCSS),
    # Loading message
    div(
      id = "loading-content",
      h2("Laen ja töötlen andmeid, palun oota ..."),
      h2("Loading and cleaning data, please wait ...")
    ),
    tabItems(
      ###üldine body
      tabItem(tabName = "uldine",
              fluidRow(width=4, valueBoxOutput("MinistArv"),
                       width=4, valueBoxOutput("AsutusteArv"),
                       width=4, valueBoxOutput("TeenusteArv")),
              fluidRow(
                box(width=6, plotOutput("TeenuseidKanalis", height = 250)),
                box(width=6, plotOutput("Moodikuid", height = 250))),
              fluidRow(
                width=4, valueBoxOutput("Kasutuskordi"),
                width=4, valueBoxOutput("Rahulolu"),
                width=4, valueBoxOutput("Maksumus"),
                width=4, valueBoxOutput("Ajakulu"),
                width=4, valueBoxOutput("AjakuluBruto")
              )
      ),
      ####ministeeriumite lõikes body
      tabItem(tabName = "minloikes",
              fluidRow(box(width=4,title=textOutput("haldusalavalik2"),
                           solidHeader = TRUE,
                           background = "light-blue", collapsible = F,
                           textOutput("haldusalavalikinfo2"),
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
                width=4, valueBoxOutput("MinAjakulu"),
                width=4, valueBoxOutput("MinAjakuluBruto")
              )
      ),
      ####asutuste lõikes body
      tabItem(tabName = "asutloikes",
              fluidRow(box(width=4,
                           title=textOutput("haldusalavalik"),
                           background = "light-blue",
                           solidHeader = TRUE,  
                           textOutput("haldusalavalikinfo"),
                           uiOutput("ministeerium2"),
                           textOutput("asutusevalik"), 
                           uiOutput("allasutus"), 
                           height=250),
                       box(width=8, plotOutput("TeenuseidKanalisAsut", 
                                               height = 250)),
                       box(width=4, plotOutput("MoodikuidAsut", height = 250)),
                       width=4, valueBoxOutput("AsutTeenusteArv"),
                       valueBoxOutput("AsutKasutuskordi"),
                       valueBoxOutput("AsutRahulolu"),
                       valueBoxOutput("AsutMaksumus"),
                       valueBoxOutput("AsutAjakulu"),
                       valueBoxOutput("AsutAjakuluBruto")
              )
      ),
      
      #############info tab
      tabItem(tabName = "info",
              fluidRow(box(width=12,
                           HTML(paste(textOutput("info1"),
                                      a("https://github.com/MKM-ITAO/riigiteenused",
                                        target="_blank",
                                        href="https://github.com/MKM-ITAO/riigiteenused"),
                                      strong(textOutput("info2")), 
                                      textOutput("info3"), 
                                      a("https://github.com/RRisto/Riigiteenused_shinyApp",
                                        target="_blank",
                                        href="https://github.com/RRisto/Riigiteenused_shinyApp"),
                                      p(""),
                                      textOutput( "time")))
              )))
    )))
