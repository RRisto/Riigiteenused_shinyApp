library(shinydashboard)
source("helpers.R")
andmed=read.table("./andmed/2015-11-24_andmedPikk.txt", sep=";", header=T)
andmed$ministeerium=gsub("i haldusala", "", andmed$ministeerium)


server <- function(input, output) {
#ministeeriumi teenuste arv
  output$MinTeenusteArv <- renderValueBox({
    valueBox(
      paste(length(unique(andmed[andmed$ministeerium==input$ministeerium,]$identifikaator))), 
      "kaardistatud teenust",icon = icon("list-ol"),color = "purple"
    )
  })
  #asutuste arv ministeeriumi haldusalas
  output$MinAsutusteArv <- renderValueBox({
    valueBox(
      paste(length(unique(andmed[andmed$ministeerium==input$ministeerium,]$allasutus))), 
      "allasutust",icon = icon("home"),color = "purple"
    )
  })
  #teenuseid kanali kohta
  output$TeenuseidKanalisMin <- renderPlot({
    data <- summeerija2(andmed[andmed$ministeerium==input$ministeerium&andmed$naitaja=="rahulolu",], c("kanal", "identifikaator", "naitaja"))
    visualiseerija2(data, aes(x=kanal, y=arv),"")+ ggtitle("Teenuste arv kanalite lõikes")
  })
  #ministeeriumi moodikuid kanali kohta
  output$MoodikuidMin <- renderPlot({
    data <- summeerija(andmed[andmed$ministeerium==input$ministeerium,], c("naitaja"))
    visualiseerija(data, aes(x=naitaja, y=stat_olemas_pr), "")+ 
      ggtitle("Järgmiste mõõdikuga teenuste osakaal:")
  })
  #ministeeriumi kasutuskordade arv kokku
  output$MinKasutuskordi <- renderValueBox({
    valueBox(
      paste(sum(andmed[andmed$ministeerium==input$ministeerium&andmed$naitaja=="osutamistearv",]$value, na.rm = T)), 
      "korda kasutati teenuseid",icon = icon("hand-o-left"),color = "purple")
  })
  #minsteeriumi rahulolu
  output$MinRahulolu <- renderValueBox({
    valueBox(
      paste(round(
        mean(
          andmed[andmed$ministeerium==input$ministeerium&andmed$naitaja=="rahulolu",]$value, na.rm = T), 1)), 
      "% keskmine rahulolu",icon = icon("smile-o"),color = "purple")
  })
#ministeeriumi maksumus
    output$MinMaksumus <- renderValueBox({
    valueBox(
      paste(
        sum(
          andmed[andmed$ministeerium==input$ministeerium&andmed$naitaja=="maksumus",]$value, na.rm = T)), 
      "teenuste kulu riigile",icon = icon("euro"),color = "purple")
  })
    #ministeeriumi ajakulu
    output$MinAjakulu <- renderValueBox({
      valueBox(
        paste(
          round(
          mean(
            andmed[andmed$ministeerium==input$ministeerium&andmed$naitaja=="ajakulu",]$value, na.rm = T)*
            mean(andmed[andmed$ministeerium==input$ministeerium&andmed$naitaja=="osutamistearv",]$value, na.rm = T),1)), 
        "tundi kogu ajakulu klientidele",icon = icon("clock-o"),color = "purple")
    })
  }
