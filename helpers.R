#########Igasugu abifunktsioone
##meltimine data.tabeliga, teeb laiast pika vormingu, abifunk korrastajale
meltimineDT=function(kanal, data) {
  sub=data[,grepl(paste(kanal, "|identifikaator|ministeerium|allasutus|tegevusvaldkond|teenusetyyp|makse", sep=""), 
                  names(data)), with=F]

  id=grep(c("identifikaator|ministeerium|allasutus|tegevusvaldkond|teenusetyyp|makse|link"), names(sub), value=T)
  if(length(id)<7) {
    tulem=NULL
  } else {
    #meldime andmed kitsaks
    tulem=melt(sub, id=id)
    #muudan variable nime ära, mis on kanalispets, muidu ei saa rbindida
    lingiNimi=names(tulem)[7]
    setnames(tulem, old=lingiNimi, new=c("link"))
  }
  tulem
}
##teeb laiast pika
korrastajaDT=function(andmed, eemalda, mootmiseAasta) {
  library(data.table)
  #eemalda - mis osa columnite nimedest tuleb eemdalda
  setnames(andmed, names(andmed), gsub(eemalda, "", names(andmed)))
  #kanalite lõikes meldime
  veeb=meltimineDT("Veebileht / portaal.", data=andmed)
  iseteen=meltimineDT("E-iseteenindus.", data=andmed)
  eesti=meltimineDT("Eesti.ee.", data=andmed)
  nuti=meltimineDT("Nutirakendus.", data=andmed)
  digitv=meltimineDT("Digitelevisioon.", data=andmed)
  epost=meltimineDT("E-post.", data=andmed)
  sms=meltimineDT("Tekstisõnum.", data=andmed)
  telefon=meltimineDT("Telefon.", data=andmed)
  faks=meltimineDT("Faks.", data=andmed)
  post=meltimineDT("Post.", data=andmed)
  lett=meltimineDT("Letiteenus.", data=andmed) 
  kodus=meltimineDT("Kliendi juures.", data=andmed)
  
  #rbindime
  koos=rbindlist(list(veeb, iseteen, eesti, nuti, digitv, epost, sms, telefon, faks, 
                      post, lett, kodus))

  #eemaldame kanali ja näitaja ning paneme eraldi veergu
  if (length(koos)==0) {
    return(NULL)
  } else {
    koos[,variable:=gsub(".ee.", ".", as.character(koos[,variable]), fixed=T)]
    koos[,variable:=gsub("Letiteenus büroos", "Letiteenus", as.character(koos[,variable]), fixed=T)]
    koos[,variable:=gsub("E-iseteenindus", "Eiseteenindus", as.character(koos[,variable]), fixed=T)]
    koos[,variable:=gsub("E-post", "Epost", as.character(koos[,variable]), fixed=T)]
    koos[,variable:=gsub("Veebileht / portaal", "Veebileht", as.character(koos[,variable]), fixed=T)]
    koos[,variable:=gsub("Kliendi juures", "Kliendijuures", as.character(koos[,variable]), fixed=T)]
    
    koos[, c("kanal", "naitaja") := tstrsplit(as.character(koos[["variable"]]), "\\.(?=[^\\.]+$)", perl=T)]
    koos[,kanal:=gsub("^.*\\.", "", koos[, kanal])]
    #viskame välja tühjad read, kus pole linki
    koos=koos[link!="NA"]
    koos[,MootmiseAasta:=mootmiseAasta]
    koos
  }
}
##kogu eelneva seob üheks funktsiooniks, vaja muuta, kui aastaid tuleb rohkem
andmedPikaksDT=function(andmedLai) {
  library(data.table)
  andmed=data.table(andmedLai)
  andmedLai2015=andmed[, !grepl("empty.|2011.|2013.|2012.|2014.", 
                                names(andmed)), with=F]
  andmedLai2014=andmed[, !grepl("empty.|2011.|2013.|2012.|2015.",
                                names(andmed)), with=F]
  andmedLai2013=andmed[, !grepl("empty.|2011.|2012.|2014.|2015.",
                                names(andmed)), with=F]
  andmedLai2012=andmed[, !grepl("empty.|2011.|2013.|2014.|2015.",
                                names(andmed)), with=F]
  andmedLai2011=andmed[, !grepl("empty.|2014.|2013.|2012.|2015.",
                                names(andmed)), with=F]
  andmedLaiEmpty=andmed[, !grepl("2014.|2011.|2013.|2012.|2015.",
                                names(andmed)), with=F]
  
  puhas2015=korrastajaDT(andmedLai2015, "2015.", "2015")
  puhas2014=korrastajaDT(andmedLai2014, "2014.", "2014")
  puhas2013=korrastajaDT(andmedLai2013, "2013.", "2013")
  puhas2012=korrastajaDT(andmedLai2012, "2012.", "2012")
  puhas2011=korrastajaDT(andmedLai2011, "2011.", "2011")
  puhasEmpty=korrastajaDT(andmedLaiEmpty, "empty.", "pole moodetud")
  andmedPikk=rbind(puhas2015, puhas2014, puhas2013, puhas2012,puhas2011,
                   puhasEmpty)
  andmedPikk[, value:=as.numeric(as.character(value))]
}

#############Statistika, graafika funktsioonid

#abifunktsioon andmete summeerimiseks
summeerija=function(data, ...) { #... paned jutumärkidesse variabled mille järgi grupeerida
  library(dplyr)
  tulem=data %>%
    group_by_(...) %>%
    summarize(stat_olemas_tk=sum(!is.na(value)),
              max_stat=length(value), #ehk kui palju oleks kanali näitaja hulk
              stat_olemas_pr=sum(!is.na(value))/length(value)) 
  tulem
}

#ja eelneva funktsiooni andmete visualiseerimiseks (skaala %)
visualiseerija=function(data, mapping, ylab, ymax, title) {
  #teljepikkus=max(data$max_stat)#y-telje kõrgus
  #localenv <- environment()
  library(ggplot2)
  library(scales)
  ggplot(data, mapping)+
    geom_bar(stat = "identity", fill="lightblue")+
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 45, hjust=1, size=13))+
    xlab("")+
    ylab(ylab)+
    coord_cartesian(ylim=c(0,1))+
    #coord_cartesian(ylim=c(0,teljepikkus), expand=F)+
    geom_text(nudge_y=0.05)+
    #scale_y_discrete(labels = percent)+
    #ggtitle("Mõõdikutega kanalite osakaal ja arv:")
    ggtitle(title)
}


#summeerija, ei tooda protsente
summeerija2=function(data, ...) { #... paned jutumärkidesse variabled mille järgi grupeerida
  library(dplyr)
  tulem=data %>%
    group_by_(...) %>%
    summarize(arv=n()) 
  tulem
}
#ja eelneva andmete alusel graafiku tegemiseks
visualiseerija2=function(data, mapping, ylab, title) {
  library(ggplot2)
  ggplot(data, mapping)+
    geom_bar(stat = "identity", fill="lightblue")+
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 45, hjust=1, size=13))+
    xlab("")+
    ylab(ylab)+
    #ggtitle(enc2native("Teenuste arv kanalite lõikes"))
    ggtitle(enc2native(title))
}
#teenuste arv minni/asutuse haldusalas, teeb valueboxi interface
TeenusteSum=function(andmed, minist, allasutusnimi, minJah, text, keel) {
  if (keel=="et") {
    if (minJah==1) { #kui muu, siis on allasutus
      andmed=andmed[andmed$ministeerium==minist,]
    } else if (minJah==2) {
      andmed=andmed
    } else    {
     # andmed=andmed[andmed$allasutus==allasutus,]
      andmed=andmed[allasutus==allasutusnimi]
    }
    valueBox(
      paste(length(unique(andmed$identifikaator))), 
      text,icon = icon("list-ol"),color = "purple")
  } else {
    if (minJah==1) { #kui muu, siis on allasutus
      andmed=andmed[andmed$ministeerium_en==minist,]
    } else if (minJah==2) {
      andmed=andmed
    } else    {
      #andmed=andmed[andmed$allasutus_en==allasutus,]
      andmed=andmed[allasutus_en==allasutusnimi]
    }
    valueBox(
      paste(length(unique(andmed$identifikaator))), 
      text,icon = icon("list-ol"),color = "purple")
  }
}

#kasutuskordade summa arvutamiseks, teeb vale boxi interface
KasutuskordadeSum=function(andmed, minist, allasutusnimi, minJah, text, keel) {
  if (keel=="et") {
    if (minJah==1) { #kui muu, siis on allasutus
      andmed=andmed[andmed$ministeerium==minist,]
    } else if (minJah==2) {
      andmed=andmed
    }else if (minJah==0) {
      #andmed=andmed[andmed$allasutus==allasutus,]
      andmed=andmed[allasutus==allasutusnimi]
    }
    valueBox(
      paste(format(sum(andmed[andmed$naitaja=="osutamiste arv",]$value, na.rm = T), big.mark=" ")), 
      text,icon = icon("hand-o-left"),color = "purple")
  } else if (keel=="en") {
    if (minJah==1) { #kui muu, siis on allasutus
      andmed=andmed[andmed$ministeerium_en==minist,]
    } else if (minJah==2) {
      andmed=andmed
    }else {
      #andmed=andmed[andmed$allasutus_en==allasutus,]
      andmed=andmed[allasutus_en==allasutusnimi]
    }
    valueBox(
      paste(format(sum(andmed[andmed$naitaja=="osutamiste arv",]$value, na.rm = T), big.mark=" ")), 
      text,icon = icon("hand-o-left"),color = "purple")
  }
}

#keskmise rahulolu arvutamiseks, teeb value boxi kohe interfaces
KeskmineRahulolu=function(andmed, minist, allasutusnimi, minJah, text, keel) {
  if (keel=="et") {
    if (minJah==1) { #kui muu, siis on allasutus
      andmed=andmed[andmed$ministeerium==minist,]
    } else if (minJah==2) {
      andmed=andmed
    }else {
      #andmed=andmed[andmed$allasutus==allasutus,]
      andmed=andmed[allasutus==allasutusnimi]
    }
    valueBox(
      paste(round(
        mean(
          andmed[andmed$naitaja=="rahulolu",]$value, na.rm = T), 1)) 
      ,paste("%", text),icon = icon("smile-o"),color = "purple")
  } else {#kui on inglise keel
    if (minJah==1) { #kui muu, siis on allasutus
      andmed=andmed[andmed$ministeerium_en==minist,]
    } else if (minJah==2) {
      andmed=andmed
    }else {
      #andmed=andmed[andmed$allasutus_en==allasutus,]
      andmed=andmed[allasutus_en==allasutusnimi]
    }
    valueBox(
      paste(round(
        mean(
          andmed[andmed$naitaja=="rahulolu",]$value, na.rm = T), 1)) 
      ,paste("%", text),icon = icon("smile-o"),color = "purple")
  }
}

##asutuste/minni teenuse kogukulu arvutamiseks, teeb valueboxi interface
HalduskuluSum=function(andmed, minist, allasutusnimi, minJah, text, keel) {
  if (keel=="et") {
    if (minJah==1) { #kui muu, siis on allasutus
      andmed=andmed[andmed$ministeerium==minist,]
    } else if (minJah==2) {
      andmed=andmed
    }else {
      #andmed=andmed[andmed$allasutus==allasutus,]
      andmed=andmed[allasutus==allasutusnimi]
    }
    valueBox(
      paste(
        format(round(
          sum(andmed[andmed$naitaja=="halduskulu",]$value, na.rm = T)), big.mark=" ")), 
      text,icon = icon("euro"),color = "purple")
  } else {
    if (minJah==1) { #kui muu, siis on allasutus
      andmed=andmed[andmed$ministeerium_en==minist,]
    } else if (minJah==2) {
      andmed=andmed
    }else {
      #andmed=andmed[andmed$allasutus_en==allasutus,]
      andmed=andmed[allasutus_en==allasutusnimi]
    }
    valueBox(
      paste(
        format(round(
          sum(andmed[andmed$naitaja=="halduskulu",]$value, na.rm = T)), big.mark=" ")), 
      text,icon = icon("euro"),color = "purple")
  }
}

#asutuste/minni klientide ajakulu kokku arvutamiseks, teeb kohe
#valueboxi interface
KliendiAjakuluSum=function(andmed, minist, allasutusnimi, minJah, text, keel) {
  if (keel=="et") {
    if (minJah==1) { #kui muu, siis on allasutus
      andmed=andmed[andmed$ministeerium==minist,]
    } else if (minJah==2) {
      andmed=andmed
    }else {
      #andmed=andmed[andmed$allasutus==allasutus,]
      andmed=andmed[allasutus==allasutusnimi]
    }
    osutamistearv=andmed[andmed$naitaja=="osutamiste arv",]$value
    ajakulu=andmed[andmed$naitaja=="ajakulu",]$value
    valueBox(
      paste(paste(format(round(sum(ajakulu*osutamistearv ,na.rm=T)), big.mark=" "))), 
      text,icon = icon("clock-o"),color = "purple")
  } else {
    if (minJah==1) { #kui muu, siis on allasutus
      andmed=andmed[andmed$ministeerium_en==minist,]
    } else if (minJah==2) {
      andmed=andmed
    }else {
      #andmed=andmed[andmed$allasutus==allasutus,]
      andmed=andmed[allasutus_en==allasutusnimi]
    }
    osutamistearv=andmed[andmed$naitaja=="osutamiste arv",]$value
    ajakulu=andmed[andmed$naitaja=="ajakulu",]$value
    valueBox(
      paste(paste(format(round(sum(ajakulu*osutamistearv ,na.rm=T)), big.mark=" "))), 
      text,icon = icon("clock-o"),color = "purple")
  }
  
}
