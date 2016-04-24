#########Igasugu abifunktsioone
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
