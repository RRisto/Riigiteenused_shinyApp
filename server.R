library(shinydashboard)
source("helpers.R")
library(ggplot2)
library(rjson)
library(riigiteenused)
library(curl)
library(data.table)
library(rsconnect)#vaja üleslaadimiseks

#andmete sisselaadimine
andmedLai=riigiteenused::andmedSisse()
andmed=andmedPikaks(andmedLai)
andmed=andmeMudija(andmed)
andmed=tolked(andmed)
#UI tolked
translation=read.csv("./translations/dictionary_ui.csv", sep=";",stringsAsFactors = F)#tõlked interface raami jaoks

#ui muutujad:
ikoonivarv="purple"
ikoontransaktsioon="hand-o-left"
ikoonrahulolu="smile-o"
ikoonhalduskulu="euro"
ikoonajakulu="clock-o"
#ikoonajakuluBruto=NA
#kuupäeva ja kellaaja kuvamiseks
values <- reactiveValues() 

server <- function(input, output, session) {
  #kustutab "laeb" teksti, kui andmed on sisse loetud
  hide(id = "loading-content", anim = TRUE, animType = "fade")
  
  #kuupäeva ja kellaaja kuvamiseks
  isolate(values$time <- Sys.time())
  output$time <- renderText({
    paste(tr("Andmed on seisuga"), as.character(values$time+7200), "EET") 
    #liidan, et selleks et shiny server kuvaks Eesti aja järgi
  })
  
  #######KUSTUTAMISEKS vaja andmete rakendusest alla laadimiseks arendamisel
      # output$downloadData <- downloadHandler(
      #   filename = "andmed.csv",
      #   content = function(file) {
      #     write.table(andmed, file, sep=";")
      #   }
      # )
      # output$downloadData2 <- downloadHandler(
      #   filename = "andmedLai.csv",
      #   content = function(file) {
      #     write.table(andmedLai[, 1:9], file, sep=";")
      #   }
      # )
  #############
  #funktsioon interface'i tõlkimiseks
  tr <- function(text){ # translates text into current language
    translation[translation$key==text,input$keel]
  }
  #funktsioon ooteteksti kuvamiseks
  ooteTekst=function() {
    progress <- shiny::Progress$new(session, min=0, max=0)
    on.exit(progress$close())
    progress$set(message = 'Palun oota / Please wait ...')
  }
  
  ######sidebarmenu
  #tekst tabi valimiseks
  output$valiVaade=renderText({
    #as.character(tr("Vali vaade"))
    HTML(paste("<b>",as.character(tr("Vali vaade")),"</b>"))
  })
  output$kylgmenuu <- renderMenu({
    sidebarMenu(
      menuItem(tr("uldine"), icon = icon("institution"),tabName = "uldine"),
      menuItem(tr("minloikes"), icon = icon("building"),
               tabName = "minloikes"),
      menuItem(tr("asutloikes"), icon = icon("home"),tabName = "asutloikes"),
      menuItem("Info", icon = icon("info"),tabName = "info")
    )
  })
  
  ########üldise vaate asjad
  #ministeeriumite arv üldine
  output$MinistArv <- renderValueBox({
    valueBox(
      paste(paste(length(unique(andmed$ministeerium)))), 
      tr("ministeeriumi"),icon = icon("institution"),color = ikoonivarv)
  })
  #allasutuste arv üldine
  output$AsutusteArv <- renderValueBox({
    valueBox(paste(length(unique(andmed$allasutus))), 
             tr("allasutust"),icon = icon("home"),color = ikoonivarv )
  })
  #teenuste arv üldine
  output$TeenusteArv <- renderValueBox({
    ikoonija(andmed=andmed, minJah=2,
             text=tr("kaardistatud teenust"), keel=input$keel, 
             ikoon="list-ol", varv=ikoonivarv, teenusteSum=T)
  })
  # plot teenuseid kanalis üldine
  output$TeenuseidKanalis <- renderPlot({
    if (input$keel=="et") {
      data <- summeerija2(andmed[andmed$naitaja=="rahulolu",], 
                          c("kanal", "identifikaator", "naitaja"))
      visualiseerija2(data, aes(x=reorder(kanal, -arv), y=arv),
                      title=as.character(tr("Teenuste arv kanalite lõikes")),"")
    } else {
      data <- summeerija2(andmed[andmed$naitaja=="rahulolu",], 
                          c("kanal_en", "identifikaator", "naitaja"))
      visualiseerija2(data, aes(x=reorder(kanal_en, -arv), y=arv),
                      title=as.character(tr("Teenuste arv kanalite lõikes")),"")
    }
  })
  # plot moodikuid kanali kohta üldine
  output$Moodikuid <- renderPlot({
    ########kui joonistab graafikuid, siis kuvab teate
    ooteTekst()
    #################################
    if (input$keel=="et") {
      data <- summeerija(andmed, c("naitaja"))
      visualiseerija(data, aes(x=reorder(naitaja, -stat_olemas_pr),
                               y=stat_olemas_pr, label=stat_olemas_tk),
                     title=as.character(tr("Mõõdikutega kanalite osakaal ja arv:")), "")
    } else {
      data <- summeerija(andmed, c("naitaja_en"))
      visualiseerija(data, aes(x=reorder(naitaja_en, -stat_olemas_pr),
                               y=stat_olemas_pr, label=stat_olemas_tk),
                     title=as.character(tr("Mõõdikutega kanalite osakaal ja arv:")), "")
    }
  })
  #üldine kasutuskordade arv kokku
  output$Kasutuskordi <- renderValueBox({
    ikoonija(andmed=andmed, minist=input$ministeerium, minJah=2,
             text=tr("korda kasutati teenuseid"), keel=input$keel, 
             naitajaNimi="osutamiste arv", ikoon=ikoontransaktsioon, 
             varv=ikoonivarv)
  })
  #üldine keskmine rahulolu
  output$Rahulolu <- renderValueBox({
    ikoonija(andmed=andmed, minist=input$ministeerium, minJah=2,
             text=tr("keskmine rahulolu"), keel=input$keel, 
             naitajaNimi="rahulolu", ikoon=ikoonrahulolu, varv=ikoonivarv, 
             arvutaKeskmine=T)
  })
  #üldine teenuste maksumus
  output$Maksumus <- renderValueBox({
    ikoonija(andmed=andmed, minist=input$ministeerium, minJah=2,
             text=tr("teenuste kulu riigile"), keel=input$keel, 
             naitajaNimi="halduskulu", ikoon=ikoonhalduskulu, 
             varv=ikoonivarv)
  })
  #üldine teenuste ajakulu
  output$Ajakulu <- renderValueBox({
    ikoonija(andmed=andmed, minist=input$ministeerium, minJah=2,
             text=tr("tundi kulutasid kliendid"), keel=input$keel, 
             naitajaNimi="ajakulu", ikoon=ikoonajakulu, 
             varv=ikoonivarv, arvutaKeskmine = T)
  })#üldine teenuste bruto ajakulu
  output$AjakuluBruto <- renderValueBox({
    ikoonija(andmed=andmed, minist=input$ministeerium, minJah=2,
             text=tr("aegBruto"), keel=input$keel, 
             naitajaNimi="ajakulu (bruto)", ikoon=ikoonajakulu, varv=ikoonivarv, 
             arvutaKeskmine=T)
  })
  
  ##########ministeeriumi vaate asjad
  #ministeeriumite nimekiri dropdowni
  output$ministeerium <- renderUI({
    if (input$keel=="et") {
      selectInput("ministeerium", "", as.character(unique(andmed$ministeerium)))
    } else if (input$keel=="en") {
      selectInput("ministeerium", "", as.character(unique(andmed$ministeerium_en)))
    }
  })
  #ministeeriumi teenuste arv
  output$MinTeenusteArv <- renderValueBox({
    ikoonija(andmed=andmed, minJah=1,minist=input$ministeerium,
             text=tr("kaardistatud teenust"), keel=input$keel, 
             ikoon="list-ol", varv=ikoonivarv, teenusteSum=T)
  })
  #asutuste arv ministeeriumi haldusalas
  output$MinAsutusteArv <- renderValueBox({
    if (input$keel=="et") {
      valueBox(
        paste(length(unique(andmed[andmed$ministeerium==input$ministeerium,]$allasutus))), 
        tr("allasutust"),icon = icon("home"),color = ikoonivarv)
    } else {
      valueBox(
        paste(length(unique(andmed[andmed$ministeerium_en==input$ministeerium,]$allasutus))), 
        tr("allasutust"),icon = icon("home"),color = ikoonivarv)
    }
  })
  #plot ministeeriumi teenuseid kanali kohta
  output$TeenuseidKanalisMin <- renderPlot({
    if (input$keel=="et") {
      data <- summeerija2(andmed[andmed$ministeerium==input$ministeerium&andmed$naitaja=="rahulolu",], c("kanal", "identifikaator", "naitaja"))
      visualiseerija2(data, aes(x=reorder(kanal, -arv), y=arv),
                      title=as.character(tr("Teenuste arv kanalite lõikes")),"")
    } else {
      data <- summeerija2(andmed[andmed$ministeerium_en==input$ministeerium&andmed$naitaja=="rahulolu",], c("kanal_en", "identifikaator", "naitaja_en"))
      visualiseerija2(data, aes(x=reorder(kanal_en, -arv), y=arv),
                      title=as.character(tr("Teenuste arv kanalite lõikes")),"")
    }
  })
  # plot ministeeriumi moodikuid kanali kohta
  output$MoodikuidMin <- renderPlot({
    #     ########kui joonistab graafikuid, siis kuvab teate
    ooteTekst()
    #################################
    if (input$keel=="et") {
      data <- summeerija(andmed[andmed$ministeerium==input$ministeerium,], c("naitaja"))
      visualiseerija(data, aes(x=reorder(naitaja, -stat_olemas_pr), 
                                    y=stat_olemas_pr, label=stat_olemas_tk),
                     title=as.character(tr("Mõõdikutega kanalite osakaal ja arv:")), "")
    } else {
      data <- summeerija(andmed[andmed$ministeerium_en==input$ministeerium,], c("naitaja_en"))
      visualiseerija(data, aes(x=reorder(naitaja_en, -stat_olemas_pr), 
                               y=stat_olemas_pr, label=stat_olemas_tk),
                     title=as.character(tr("Mõõdikutega kanalite osakaal ja arv:")), "")
    }
  })
  #ministeeriumi kasutuskordade arv kokku
  output$MinKasutuskordi <- renderValueBox({
    ikoonija(andmed=andmed, minist=input$ministeerium, minJah=1,
             text=tr("korda kasutati teenuseid"), keel=input$keel, 
             naitajaNimi="osutamiste arv", ikoon=ikoontransaktsioon, 
             varv=ikoonivarv)
  })
  #minsteeriumi keskmine rahulolu
  output$MinRahulolu <- renderValueBox({
    ikoonija(andmed=andmed, minist=input$ministeerium, minJah=1,
             text=tr("keskmine rahulolu"), keel=input$keel, 
             naitajaNimi="rahulolu", ikoon=ikoonrahulolu, 
             varv=ikoonivarv, arvutaKeskmine = T)
  })
  #ministeeriumi teenuste maksumus
  output$MinMaksumus <- renderValueBox({
    ikoonija(andmed=andmed, minist=input$ministeerium, minJah=1,
             text=tr("teenuste kulu riigile"), keel=input$keel, 
             naitajaNimi="halduskulu", ikoon=ikoonhalduskulu, 
             varv=ikoonivarv)
  })
  #ministeeriumi teenuste ajakulu
  output$MinAjakulu <- renderValueBox({
    ikoonija(andmed=andmed, minist=input$ministeerium, minJah=1,
             text=tr("tundi kulutasid kliendid"), keel=input$keel, 
             naitajaNimi="ajakulu", ikoon=ikoonajakulu, 
             varv=ikoonivarv, arvutaKeskmine = T)
  })
  #ministeeriumi teenuste ajakulu bruto
  output$MinAjakuluBruto <- renderValueBox({
    ikoonija(andmed=andmed, minist=input$ministeerium, minJah=1,
             text=tr("aegBruto"), keel=input$keel, 
             naitajaNimi="ajakulu (bruto)", ikoon=ikoonajakulu, varv=ikoonivarv, 
             arvutaKeskmine=T)
  })
  
  ############################allasutuste asjad
  ##interaktiivselt kuvab minnide nimed dropdownis
  output$ministeerium2 <- renderUI({
    if (input$keel=="et") {
      selectInput("ministeerium2", "", as.character(unique(andmed$ministeerium)))
    } else if (input$keel=="en") {
      selectInput("ministeerium2", "", as.character(unique(andmed$ministeerium_en)))
    }
  })
  ##interaktiivselt kuvab asutuste nimekirja, mis valitud minni all on
  output$allasutus <- renderUI({
    if (input$keel=="et") {
      selectInput("asutus", "", as.character(unique(andmed[andmed$ministeerium==input$ministeerium2,]$allasutus)))
    } else {
      selectInput("asutus", "", as.character(unique(andmed[andmed$ministeerium_en==input$ministeerium2,]$allasutus_en)))
    }
  })
  #allasutuse teenuste arv
  output$AsutTeenusteArv <- renderValueBox({
    #plot teenuseid kanalis 
    ikoonija(andmed=andmed, minJah=0,allasutusnimi =input$asutus,
             text=tr("kaardistatud teenust"), keel=input$keel, 
             ikoon="list-ol", varv=ikoonivarv, teenusteSum=T)
  })
  #plot teenuseid kanalite lõikes
  output$TeenuseidKanalisAsut <- renderPlot({
    if (input$keel=="et") {
      data <- summeerija2(andmed[andmed$allasutus==input$asutus&andmed$naitaja=="rahulolu",], c("kanal", "identifikaator", "naitaja"))
      visualiseerija2(data, aes(x=reorder(kanal, -arv), y=arv),
                      title=as.character(tr("Teenuste arv kanalite lõikes")),"")
    } else {
      data <- summeerija2(andmed[andmed$allasutus_en==input$asutus&andmed$naitaja=="rahulolu",], c("kanal_en", "identifikaator", "naitaja"))
      visualiseerija2(data, aes(x=reorder(kanal_en, -arv), y=arv),
                      title=as.character(tr("Teenuste arv kanalite lõikes")),"")
    }
  })
  # plot asutuse moodikuid kanali kohta
  output$MoodikuidAsut <- renderPlot({
    if (input$keel=="et") {
      data <- summeerija(andmed[andmed$allasutus==input$asutus,], c("naitaja"))
      visualiseerija(data, aes(x=reorder(naitaja, -stat_olemas_pr), 
                               y=stat_olemas_pr, label=stat_olemas_tk),
                     title=as.character(tr("Mõõdikutega kanalite osakaal ja arv:")), "")
    } else {
      data <- summeerija(andmed[andmed$allasutus_en==input$asutus,], c("naitaja_en"))
      visualiseerija(data, aes(x=reorder(naitaja_en,-stat_olemas_pr),
                               y=stat_olemas_pr, label=stat_olemas_tk),
                     title=as.character(tr("Mõõdikutega kanalite osakaal ja arv:")), "")
    }
  })
  #asutuse kasutuskordade arv kokku
  output$AsutKasutuskordi <- renderValueBox({
    ikoonija(andmed=andmed, allasutusnimi=input$asutus, minJah=0,
             text=tr("korda kasutati teenuseid"), keel=input$keel, 
             naitajaNimi="osutamiste arv", ikoon=ikoontransaktsioon, varv=ikoonivarv)
  })
  #asutuse keskmine rahulolu
  output$AsutRahulolu <- renderValueBox({
    ikoonija(andmed=andmed, allasutusnimi=input$asutus, minJah=0,
             text=tr("keskmine rahulolu"), keel=input$keel, 
             naitajaNimi="rahulolu", ikoon=ikoonrahulolu, varv=ikoonivarv, 
             arvutaKeskmine=T)
  })
  #asutuse teenuste maksumus
  output$AsutMaksumus <- renderValueBox({
    ikoonija(andmed=andmed, allasutusnimi=input$asutus, minJah=0,
             text=tr("teenuste kulu riigile"), keel=input$keel, 
             naitajaNimi="halduskulu", ikoon=ikoonhalduskulu, varv=ikoonivarv)
  })
  #asutuse teenuste ajakulu
  output$AsutAjakulu <- renderValueBox({
    #     ########kui joonistab graafikuid, siis kuvab teate
    ooteTekst()
    #################################
    ikoonija(andmed=andmed, allasutusnimi=input$asutus, minJah=0,
              text=tr("tundi kulutasid kliendid"), keel=input$keel, 
              naitajaNimi="ajakulu", ikoon=ikoonajakulu, varv=ikoonivarv, 
              arvutaKeskmine=T)
  })#brutoajakulu 
  output$AsutAjakuluBruto <- renderValueBox({
     ikoonija(andmed=andmed, allasutusnimi=input$asutus, minJah=0,
             text=tr("aegBruto"), keel=input$keel, 
             naitajaNimi="ajakulu (bruto)", ikoon=ikoonajakulu, varv=ikoonivarv, 
             arvutaKeskmine=T)
  })
  ###########info tabi tõlked
  output$info1=renderText({
    as.character(tr("info_key1")) #kuvab ui-s facotrina kui pole as char
  })
  output$info2=renderText({
    as.character(tr("info_key2")) #kuvab ui-s facotrina kui pole as char
  })
  output$info3=renderText({
    as.character(tr("info_key3")) #kuvab ui-s facotrina kui pole as char
  })
  ##############
  
  #####Tabide min/asutus valikute frame tõlked
  output$haldusalavalik <- renderText({
    as.character(tr("Haldusala"))
  })
  output$haldusalavalikinfo <- renderText({
    as.character(tr("Vali haldusala"))
  })
  output$asutusevalik <- renderText({
    as.character(tr("Vali allasutus"))
  })
  output$haldusalavalik2 <- renderText({
    as.character(tr("Haldusala"))
  })
  output$haldusalavalikinfo2 <- renderText({
    as.character(tr("Vali haldusala"))
  })
}


