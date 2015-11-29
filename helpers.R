#########Igasugu abifunktsioone

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

#ja eelenav funktsiooni andmete visualiseerimiseks (skaala %)
visualiseerija=function(data, mapping, ylab) {
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
    scale_y_discrete(labels = percent)+
    ggtitle("Järgmiste mõõdikuga teenuste osakaal:")
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
visualiseerija2=function(data, mapping, ylab) {
  library(ggplot2)
  ggplot(data, mapping)+
    geom_bar(stat = "identity", fill="lightblue")+
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 45, hjust=1, size=13))+
    xlab("")+
    ylab(ylab)+
    ggtitle(enc2native("Teenuste arv kanalite lõikes"))
}
#teenuste arv minni/asutuse haldusalas, teeb valueboxi interface
TeenusteSum=function(andmed, minist, allasutus, minJah) {
  if (minJah==1) { #kui muu, siis on allasutus
    andmed=andmed[andmed$ministeerium==minist,]
  } else if (minJah==2) {
    andmed=andmed
  } else    {
    andmed=andmed[andmed$allasutus==allasutus,]
  }
  valueBox(
    paste(length(unique(andmed$identifikaator))), 
    "kaardistatud teenust",icon = icon("list-ol"),color = "purple")
}

#kasutuskordade summa arvutamiseks, teeb vale boxi interface
KasutuskordadeSum=function(andmed, minist, allasutus, minJah) {
  if (minJah==1) { #kui muu, siis on allasutus
    andmed=andmed[andmed$ministeerium==minist,]
  } else if (minJah==2) {
    andmed=andmed
  }else {
    andmed=andmed[andmed$allasutus==allasutus,]
  }
  valueBox(
    paste(format(sum(andmed[andmed$naitaja=="osutamistearv",]$value, na.rm = T), big.mark=" ")), 
    "korda kasutati teenuseid",icon = icon("hand-o-left"),color = "purple")
}

#keskmise rahulolu arvutamiseks, teeb value boxi kohe interfaces
KeskmineRahulolu=function(andmed, minist, allasutus, minJah) {
  if (minJah==1) { #kui muu, siis on allasutus
    andmed=andmed[andmed$ministeerium==minist,]
  } else if (minJah==2) {
    andmed=andmed
  }else {
    andmed=andmed[andmed$allasutus==allasutus,]
  }
  valueBox(
    paste(round(
      mean(
        andmed[andmed$naitaja=="rahulolu",]$value, na.rm = T), 1)), 
    "% keskmine rahulolu",icon = icon("smile-o"),color = "purple")
}

##asutuste/minni teenuse kogukulu arvutamiseks, teeb valueboxi interface
HalduskuluSum=function(andmed, minist, allasutus, minJah) {
  if (minJah==1) { #kui muu, siis on allasutus
    andmed=andmed[andmed$ministeerium==minist,]
  } else if (minJah==2) {
    andmed=andmed
  }else {
    andmed=andmed[andmed$allasutus==allasutus,]
  }
  valueBox(
    paste(
      format(
        sum(andmed[andmed$naitaja=="halduskulu",]$value, na.rm = T), big.mark=" ")), 
    "teenuste kulu riigile",icon = icon("euro"),color = "purple")
}

#asutuste/minni klientide ajakulu kokku arvutamiseks, teeb kohe
#valueboxi interface
KliendiAjakuluSum=function(andmed, minist, allasutus, minJah) {
  if (minJah==1) { #kui muu, siis on allasutus
    andmed=andmed[andmed$ministeerium==minist,]
  } else if (minJah==2) {
    andmed=andmed
  }else {
    andmed=andmed[andmed$allasutus==allasutus,]
  }
  osutamistearv=andmed[andmed$naitaja=="osutamistearv",]$value
  ajakulu=andmed[andmed$naitaja=="ajakulu",]$value
  valueBox(
    paste(paste(format(round(sum(ajakulu*osutamistearv ,na.rm=T)), big.mark=" "))), 
    "tundi kulutasid kliendid teenuste kasutamisele",icon = icon("clock-o"),color = "purple")
}

