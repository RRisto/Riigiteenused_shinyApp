library(shinydashboard)
source("helpers.R")
library(ggplot2)
library(rjson)
library(riigiteenused)
library(curl) #shiny server nõuab seda

#andmed=readRDS("./andmed/2016-02-05_andmedPikk.rds")
# andmed sisse, pikaks ja minni nimedest haldusala maha
andmedLai=riigiteenused::andmedSisse("https://www.riigiteenused.ee/api/et/all")
andmed=andmedPikaks(andmedLai)
andmed$ministeerium=gsub("i haldusala", "", andmed$ministeerium)
andmed$kanal=gsub("Kliendijuures", "Kliendi juures", andmed$kanal)
andmed$kanal=gsub("Eiseteenindus", "E-iseteenindus", andmed$kanal)
andmed$kanal=gsub("Epost", "E-post", andmed$kanal)
andmed$naitaja=gsub("osutamistearv", "osutamiste arv", andmed$naitaja)
#kuupäeva ja kellaaja kuvamiseks
values <- reactiveValues()

server <- function(input, output, session) {
  #kustutab "laeb" teksti, kui andmed on sisse loetud
  hide(id = "loading-content", anim = TRUE, animType = "fade")
  
  #kuupäeva ja kellaaja kuvamiseks
  isolate(values$time <- Sys.time())
    output$time <- renderText({
      paste("Andmed on seisuga", as.character(values$time+7200), "EET") 
      #liidan, et selleks et shiny server kuvaks Eesti aja järgi
  })
  
  #######KUSTUTAMISEKS vaja ainult andmete rakendusest alla laadimiseks
#     output$downloadData <- downloadHandler(
#       filename = "andmed.csv",
#       content = function(file) {
#         write.table(andmed, file, sep=";")
#       }
#     )
#     output$downloadData2 <- downloadHandler(
#       filename = "andmedLai.csv",
#       content = function(file) {
#         write.table(andmedLai[, 1:9], file, sep=";")
#       }
#     )
  #############
    
  ########üldise vaate asjad
  #ministeeriumite arv üldine
  output$MinArv <- renderValueBox({
    ########kui joonistab graafikuid, siis kuvab selle teate
    progress <- shiny::Progress$new(session, min=0, max=1)
        on.exit(progress$close())
        progress$set(message = 'Joonistan graafikuid',
                     detail = 'Palun oota ...')
        #################################
        valueBox(
          paste(paste(length(unique(andmed$ministeerium)))), 
          "ministeeriumi",icon = icon("institution"),color = "purple")
  })
  #allasutuste arv üldine
  output$AsutusteArv <- renderValueBox({
    valueBox(paste(length(unique(andmed$allasutus))), 
             "allasutust",icon = icon("home"),color = "purple" )
  })
  #teenuste arv üldine
  output$TeenusteArv <- renderValueBox({
    TeenusteSum(andmed=andmed, minJah=2)
  })
  # plot teenuseid kanalis üldine
  output$TeenuseidKanalis <- renderPlot({
    data <- summeerija2(andmed[andmed$naitaja=="rahulolu",], c("kanal", "identifikaator", "naitaja"))
    visualiseerija2(data, aes(x=kanal, y=arv),"")
  })
  # plot moodikuid kanali kohta üldine
  output$Moodikuid <- renderPlot({
    #     ########kui joonistab graafikuid, siis kuvab selle teate
    progress <- shiny::Progress$new(session, min=0, max=1)
    on.exit(progress$close())
    
    progress$set(message = 'Joonistan graafikuid',
                 detail = 'Palun oota ...')
    #################################
    data <- summeerija(andmed, c("naitaja"))
    visualiseerija(data, aes(x=naitaja, y=stat_olemas_pr, label=stat_olemas_tk), "")
    #visualiseerija(data, aes(x=naitaja, y=stat_olemas_tk), "")
    
    })
  #üldine kasutuskordade arv kokku
  output$Kasutuskordi <- renderValueBox({
    KasutuskordadeSum(andmed=andmed, minist=input$ministeerium, minJah=2)
  })
  #üldine keskmine rahulolu
  output$Rahulolu <- renderValueBox({
    KeskmineRahulolu(andmed=andmed, minist=input$ministeerium, minJah=2)
  })
  #üldine teenuste maksumus
  output$Maksumus <- renderValueBox({
    HalduskuluSum(andmed=andmed, minist=input$ministeerium, minJah=2)
  })
  #üldine teenuste ajakulu
  output$Ajakulu <- renderValueBox({
    KliendiAjakuluSum(andmed=andmed, minist=input$ministeerium, minJah=2)
  })
  ##########ministeeriumi vaate asjad
  #ministeeriumite nimekiri dropdowni
  output$ministeerium <- renderUI({
    selectInput("ministeerium", "", as.character(unique(andmed$ministeerium)))
  })
  #ministeeriumi teenuste arv
  output$MinTeenusteArv <- renderValueBox({
      TeenusteSum(andmed=andmed, minist=input$ministeerium, minJah=1)
  })
  #asutuste arv ministeeriumi haldusalas
  output$MinAsutusteArv <- renderValueBox({
    valueBox(
      paste(length(unique(andmed[andmed$ministeerium==input$ministeerium,]$allasutus))), 
      "allasutust",icon = icon("home"),color = "purple")
  })
  #plot ministeeriumi teenuseid kanali kohta
  output$TeenuseidKanalisMin <- renderPlot({
    data <- summeerija2(andmed[andmed$ministeerium==input$ministeerium&andmed$naitaja=="rahulolu",], c("kanal", "identifikaator", "naitaja"))
    visualiseerija2(data, aes(x=kanal, y=arv),"")
  })
  # plot ministeeriumi moodikuid kanali kohta
  output$MoodikuidMin <- renderPlot({
    #     ########kui joonistab graafikuid, siis kuvab selle teate
    progress <- shiny::Progress$new(session, min=0, max=1)
    on.exit(progress$close())
    
    progress$set(message = 'Joonistan graafikuid',
                 detail = 'Palun oota ...')
    #################################
    data <- summeerija(andmed[andmed$ministeerium==input$ministeerium,], c("naitaja"))
    #visualiseerija(data, aes(x=naitaja, y=stat_olemas_pr), "")
    visualiseerija(data, aes(x=naitaja, y=stat_olemas_pr, label=stat_olemas_tk), "")
    
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
    ########kui joonistab graafikuid, siis kuvab selle teate
    progress <- shiny::Progress$new(session, min=0, max=1)
    on.exit(progress$close())
    
    progress$set(message = 'Joonistan graafikuid',
                 detail = 'Palun oota ...')
    #################################
    KliendiAjakuluSum(andmed=andmed, minist=input$ministeerium, minJah=1)
  })
  
  ############################allasutuste asjad
  ##interaktiivselt kuvab minnide nimed dropdownis
  output$ministeerium2 <- renderUI({
    selectInput("ministeerium2", "", as.character(unique(andmed$ministeerium)))
  })
  ##interaktiivselt kuvab asutused, mis valitud minni al on
  output$allasutus <- renderUI({
    selectInput("asutus", "", as.character(unique(andmed[andmed$ministeerium==input$ministeerium2,]$allasutus)))
  })
  #allasutuse teenuste arv
  output$AsutTeenusteArv <- renderValueBox({
    #plot teenuseid kanalis 
    TeenusteSum(andmed=andmed, allasutus =input$asutus, minJah=0)
  })
  output$TeenuseidKanalisAsut <- renderPlot({
    data <- summeerija2(andmed[andmed$allasutus==input$asutus&andmed$naitaja=="rahulolu",], c("kanal", "identifikaator", "naitaja"))
    visualiseerija2(data, aes(x=kanal, y=arv),"")
  })
  # plot asutuse moodikuid kanali kohta
  output$MoodikuidAsut <- renderPlot({
    data <- summeerija(andmed[andmed$allasutus==input$asutus,], c("naitaja"))
    visualiseerija(data, aes(x=naitaja, y=stat_olemas_pr, label=stat_olemas_tk), "")
    #visualiseerija(data, aes(x=naitaja, y=stat_olemas_pr), "")
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
  #asutuse teenuste ajakulu
  output$AsutAjakulu <- renderValueBox({
    #     ########kui joonistab graafikuid, siis kuvab selle teate
    progress <- shiny::Progress$new(session, min=0, max=1)
    on.exit(progress$close())
    
    progress$set(message = 'Joonistan graafikuid',
                 detail = 'Palun oota ...')
    #################################
    KliendiAjakuluSum(andmed=andmed, allasutus=input$asutus, minJah=0)
  })
}


