library(shinydashboard)
source("helpers.R")
andmed=readRDS("./andmed/2015-11-25_andmedPikk.rds")
andmed$ministeerium=gsub("i haldusala", "", andmed$ministeerium)


server <- function(input, output) {
  ##########ministeeriumi asjad
  #ministeeriumi teenuste arv
  output$MinTeenusteArv <- renderValueBox({
    TeenusteSum(andmed=andmed, minist=input$ministeerium, minJah=1)
  })
  
  #asutuste arv ministeeriumi haldusalas
  output$MinAsutusteArv <- renderValueBox({
    valueBox(
      paste(length(unique(andmed[andmed$ministeerium==input$ministeerium,]$allasutus))), 
      "allasutust",icon = icon("home"),color = "purple"
    )
  })
  
  #plot ministeeriumi teenuseid kanali kohta
  output$TeenuseidKanalisMin <- renderPlot({
    data <- summeerija2(andmed[andmed$ministeerium==input$ministeerium&andmed$naitaja=="rahulolu",], c("kanal", "identifikaator", "naitaja"))
    visualiseerija2(data, aes(x=kanal, y=arv),"")+ ggtitle("Teenuste arv kanalite lõikes")
  })
  
  # plotministeeriumi moodikuid kanali kohta
  output$MoodikuidMin <- renderPlot({
    data <- summeerija(andmed[andmed$ministeerium==input$ministeerium,], c("naitaja"))
    visualiseerija(data, aes(x=naitaja, y=stat_olemas_pr), "")+ 
      ggtitle("Järgmiste mõõdikuga teenuste osakaal:")
  })
  
  #ministeeriumi kasutuskordade arv kokku
  output$MinKasutuskordi <- renderValueBox({
    KasutuskordadeSum(andmed=andmed, minist=input$ministeerium, minJah=1)
  })
  #minsteeriumi keskmine rahulolu
   output$MinRahulolu <- renderValueBox({
  KeskmineRahulolu(andmed=andmed, minist=input$ministeerium, minJah=1)
  })
#ministeeriumi teenuste maksumus
    output$MinMaksumus <- renderValueBox({
      HalduskuluSum(andmed=andmed, minist=input$ministeerium, minJah=1)
  })
    #ministeeriumi teenuste ajakulu
    output$MinAjakulu <- renderValueBox({
      KliendiAjakuluSum(andmed=andmed, minist=input$ministeerium, minJah=1)
    })
    
    ############################allasutuste asjad
    ##interaktiivselt kuvab asutused, mis valitud minni al on
    output$allasutus <- renderUI({
      selectInput("asutus", "", as.character(unique(andmed[andmed$ministeerium==input$ministeerium2,]$allasutus)))
    })
    
    #allasutuse teenuste arv
    output$AsutTeenusteArv <- renderValueBox({
      TeenusteSum(andmed=andmed, allasutus =input$asutus, minJah=0)
    })
    #plot teenuseid kanalis 
    output$TeenuseidKanalisAsut <- renderPlot({
      data <- summeerija2(andmed[andmed$allasutus==input$asutus&andmed$naitaja=="rahulolu",], c("kanal", "identifikaator", "naitaja"))
      visualiseerija2(data, aes(x=kanal, y=arv),"")+ ggtitle("Teenuste arv kanalite lõikes")
    })
    
    # plot asutuse moodikuid kanali kohta
    output$MoodikuidAsut <- renderPlot({
      data <- summeerija(andmed[andmed$allasutus==input$asutus,], c("naitaja"))
      visualiseerija(data, aes(x=naitaja, y=stat_olemas_pr), "")+ 
        ggtitle("Järgmiste mõõdikuga teenuste osakaal:")
    })
    #asutuse kasutuskordade arv kokku
    output$AsutKasutuskordi <- renderValueBox({
      KasutuskordadeSum(andmed=andmed, allasutus=input$asutus, minJah=0)
    })
    #asutuse keskmine rahulolu
    output$AsutRahulolu <- renderValueBox({
      KeskmineRahulolu(andmed=andmed, allasutus=input$asutus, minJah=0)
    })
    #asutuse teenuste maksumus
    output$AsutMaksumus <- renderValueBox({
      HalduskuluSum(andmed=andmed, allasutus=input$asutus, minJah=0)
    })
    #ministeeriumi teenuste ajakulu
    output$AsutAjakulu <- renderValueBox({
      KliendiAjakuluSum(andmed=andmed, allasutus=input$asutus, minJah=0)
    })
}


