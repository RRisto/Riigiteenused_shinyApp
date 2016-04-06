library(shinydashboard)
source("helpers.R")
library(ggplot2)
library(rjson)
library(riigiteenused)
library(curl)
library(data.table)

#andmete sisselaadimine
andmedLai=riigiteenused::andmedSisse("https://www.riigiteenused.ee/api/et/all")
andmed=andmedPikaksDT(andmedLai)
andmed[, ministeerium:=gsub("i haldusala", "", andmed[,ministeerium])]
andmed[, ministeerium:=gsub("Riigikantsele", "Riigikantselei", andmed[,ministeerium])]
andmed[, kanal:=gsub("Kliendijuures", "Kliendi juures", andmed[,kanal])]
andmed[, kanal:=gsub("Eiseteenindus", "E-iseteenindus", andmed[,kanal])]
andmed[, kanal:=gsub("Eesti", "Eesti.ee", andmed[,kanal])]
andmed[, kanal:=gsub("Epost", "E-post", andmed[,kanal])]
andmed[, kanal:=gsub("Letiteenus", "Teeninduslett", andmed[,kanal])]
andmed[, naitaja:=gsub("osutamistearv", "osutamiste arv", andmed[,naitaja])]

#loen sisse tõlkefailid (fread aitab encodingu probleeme ennetada)
kanali_tolked=fread("./translations/dictionary_channels.csv", encoding = "UTF-8")
naitaja_tolked=fread("./translations/dictionary_moodik.csv", encoding = "UTF-8")
ministeerium_tolked=fread("./translations/dictionary_ministeeriumid.csv", encoding = "UTF-8")
asutused_tolked=fread("./translations/dictionary_asutused.csv", encoding = "UTF-8")
translation=read.csv("./translations/dictionary_ui.csv", sep=";",stringsAsFactors = F)#tõlked interface raami jaoks

#keevitan kanalite, moodikute tõlked juurde, kui siin "andmed" ridade
#arv väheneb, on kuskil keys bugi sees
andmed=merge(andmed, kanali_tolked, by.x="kanal", by.y="key")
andmed=merge(andmed, naitaja_tolked, by.x="naitaja", by.y="key")
andmed=merge(andmed, ministeerium_tolked, by.x="ministeerium", by.y="key")
andmed=merge(andmed, asutused_tolked, by.x="allasutus", by.y="key")

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
#       output$downloadData <- downloadHandler(
#         filename = "andmed.csv",
#         content = function(file) {
#           write.table(andmed, file, sep=";")
#         }
#       )
#       output$downloadData2 <- downloadHandler(
#         filename = "andmedLai.csv",
#         content = function(file) {
#           write.table(andmedLai[, 1:9], file, sep=";")
#         }
#       )
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
      tr("ministeeriumi"),icon = icon("institution"),color = "purple")
  })
  #allasutuste arv üldine
  output$AsutusteArv <- renderValueBox({
    valueBox(paste(length(unique(andmed$allasutus))), 
             tr("allasutust"),icon = icon("home"),color = "purple" )
  })
  #teenuste arv üldine
  output$TeenusteArv <- renderValueBox({
    TeenusteSum(andmed=andmed, minJah=2, text=tr("kaardistatud teenust"),
                keel=input$keel)
  })
  # plot teenuseid kanalis üldine
  output$TeenuseidKanalis <- renderPlot({
    if (input$keel=="et") {
      data <- summeerija2(andmed[andmed$naitaja=="rahulolu",], 
                          c("kanal", "identifikaator", "naitaja"))
      visualiseerija2(data, aes(x=kanal, y=arv),
                      title=as.character(tr("Teenuste arv kanalite lõikes")),"")
    } else {
      data <- summeerija2(andmed[andmed$naitaja=="rahulolu",], 
                          c("kanal_en", "identifikaator", "naitaja"))
      visualiseerija2(data, aes(x=kanal_en, y=arv),
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
      visualiseerija(data, aes(x=naitaja, y=stat_olemas_pr, label=stat_olemas_tk),
                     title=as.character(tr("Mõõdikutega kanalite osakaal ja arv:")), "")
    } else {
      data <- summeerija(andmed, c("naitaja_en"))
      visualiseerija(data, aes(x=naitaja_en, y=stat_olemas_pr, label=stat_olemas_tk),
                     title=as.character(tr("Mõõdikutega kanalite osakaal ja arv:")), "")
    }
  })
  #üldine kasutuskordade arv kokku
  output$Kasutuskordi <- renderValueBox({
    KasutuskordadeSum(andmed=andmed, minist=input$ministeerium, minJah=2,
                      text=tr("korda kasutati teenuseid"),keel=input$keel)
  })
  #üldine keskmine rahulolu
  output$Rahulolu <- renderValueBox({
    KeskmineRahulolu(andmed=andmed, minist=input$ministeerium, minJah=2, 
                     text=tr("keskmine rahulolu"), keel=input$keel)
  })
  #üldine teenuste maksumus
  output$Maksumus <- renderValueBox({
    HalduskuluSum(andmed=andmed, minist=input$ministeerium, minJah=2,
                  text=tr("teenuste kulu riigile"), keel=input$keel)
  })
  #üldine teenuste ajakulu
  output$Ajakulu <- renderValueBox({
    KliendiAjakuluSum(andmed=andmed, minist=input$ministeerium, minJah=2,
                      text=tr("tundi kulutasid kliendid"), keel=input$keel)
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
    TeenusteSum(andmed=andmed, minist=input$ministeerium, minJah=1,
                text=tr("kaardistatud teenust"), keel=input$keel)
  })
  #asutuste arv ministeeriumi haldusalas
  output$MinAsutusteArv <- renderValueBox({
    if (input$keel=="et") {
      valueBox(
        paste(length(unique(andmed[andmed$ministeerium==input$ministeerium,]$allasutus))), 
        tr("allasutust"),icon = icon("home"),color = "purple")
    } else {
      valueBox(
        paste(length(unique(andmed[andmed$ministeerium_en==input$ministeerium,]$allasutus))), 
        tr("allasutust"),icon = icon("home"),color = "purple")
    }
  })
  #plot ministeeriumi teenuseid kanali kohta
  output$TeenuseidKanalisMin <- renderPlot({
    if (input$keel=="et") {
      data <- summeerija2(andmed[andmed$ministeerium==input$ministeerium&andmed$naitaja=="rahulolu",], c("kanal", "identifikaator", "naitaja"))
      visualiseerija2(data, aes(x=kanal, y=arv),
                      title=as.character(tr("Teenuste arv kanalite lõikes")),"")
    } else {
      data <- summeerija2(andmed[andmed$ministeerium_en==input$ministeerium&andmed$naitaja=="rahulolu",], c("kanal_en", "identifikaator", "naitaja_en"))
      visualiseerija2(data, aes(x=kanal_en, y=arv),
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
      visualiseerija(data, aes(x=naitaja, y=stat_olemas_pr, label=stat_olemas_tk),
                     title=as.character(tr("Mõõdikutega kanalite osakaal ja arv:")), "")
    } else {
      data <- summeerija(andmed[andmed$ministeerium_en==input$ministeerium,], c("naitaja_en"))
      visualiseerija(data, aes(x=naitaja_en, y=stat_olemas_pr, label=stat_olemas_tk),
                     title=as.character(tr("Mõõdikutega kanalite osakaal ja arv:")), "")
    }
  })
  #ministeeriumi kasutuskordade arv kokku
  output$MinKasutuskordi <- renderValueBox({
    KasutuskordadeSum(andmed=andmed, minist=input$ministeerium, minJah=1,
                      text=tr("korda kasutati teenuseid"), keel=input$keel)
  })
  #minsteeriumi keskmine rahulolu
  output$MinRahulolu <- renderValueBox({
    KeskmineRahulolu(andmed=andmed, minist=input$ministeerium, minJah=1,
                     text=tr("keskmine rahulolu"), keel=input$keel)
  })
  #ministeeriumi teenuste maksumus
  output$MinMaksumus <- renderValueBox({
    HalduskuluSum(andmed=andmed, minist=input$ministeerium, minJah=1,
                  text=tr("teenuste kulu riigile"), keel=input$keel)
  })
  #ministeeriumi teenuste ajakulu
  output$MinAjakulu <- renderValueBox({
    KliendiAjakuluSum(andmed=andmed, minist=input$ministeerium, minJah=1,
                      text=tr("tundi kulutasid kliendid"), keel=input$keel)
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
    TeenusteSum(andmed=andmed, allasutusnimi =input$asutus, minJah=0,
                text=tr("kaardistatud teenust"), keel=input$keel)
  })
  #plot teenuseid kanalite lõikes
  output$TeenuseidKanalisAsut <- renderPlot({
    if (input$keel=="et") {
      data <- summeerija2(andmed[andmed$allasutus==input$asutus&andmed$naitaja=="rahulolu",], c("kanal", "identifikaator", "naitaja"))
      visualiseerija2(data, aes(x=kanal, y=arv),
                      title=as.character(tr("Teenuste arv kanalite lõikes")),"")
    } else {
      data <- summeerija2(andmed[andmed$allasutus_en==input$asutus&andmed$naitaja=="rahulolu",], c("kanal_en", "identifikaator", "naitaja"))
      visualiseerija2(data, aes(x=kanal_en, y=arv),
                      title=as.character(tr("Teenuste arv kanalite lõikes")),"")
    }
  })
  # plot asutuse moodikuid kanali kohta
  output$MoodikuidAsut <- renderPlot({
    if (input$keel=="et") {
      data <- summeerija(andmed[andmed$allasutus==input$asutus,], c("naitaja"))
      visualiseerija(data, aes(x=naitaja, y=stat_olemas_pr, label=stat_olemas_tk),
                     title=as.character(tr("Mõõdikutega kanalite osakaal ja arv:")), "")
    } else {
      data <- summeerija(andmed[andmed$allasutus_en==input$asutus,], c("naitaja_en"))
      visualiseerija(data, aes(x=naitaja_en, y=stat_olemas_pr, label=stat_olemas_tk),
                     title=as.character(tr("Mõõdikutega kanalite osakaal ja arv:")), "")
    }
  })
  #asutuse kasutuskordade arv kokku
  output$AsutKasutuskordi <- renderValueBox({
    KasutuskordadeSum(andmed=andmed, allasutusnimi=input$asutus, minJah=0,
                      text=tr("korda kasutati teenuseid"), keel=input$keel)
  })
  #asutuse keskmine rahulolu
  output$AsutRahulolu <- renderValueBox({
    KeskmineRahulolu(andmed=andmed, allasutusnimi=input$asutus, minJah=0,
                     text=tr("keskmine rahulolu"), keel=input$keel)
  })
  #asutuse teenuste maksumus
  output$AsutMaksumus <- renderValueBox({
    HalduskuluSum(andmed=andmed, allasutusnimi=input$asutus, minJah=0,
                  text=tr("teenuste kulu riigile"), keel=input$keel)
  })
  #asutuse teenuste ajakulu
  output$AsutAjakulu <- renderValueBox({
    #     ########kui joonistab graafikuid, siis kuvab teate
    ooteTekst()
    #################################
    KliendiAjakuluSum(andmed=andmed, allasutusnimi=input$asutus, minJah=0,
                      text=tr("tundi kulutasid kliendid"), keel=input$keel)
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


