#########Igasugu abifunktsioone
#############Statistika, graafika funktsioonid
#algne andmete mudimine
andmeMudija=function(andmed){
  andmed=data.table(andmed)
  andmed$value=as.numeric(andmed$value)
  #ümbernimetamine
  andmed[, ministeerium:=gsub("i haldusala", "", andmed[,ministeerium])]
  andmed[, ministeerium:=gsub("Riigikantsele", "Riigikantselei", andmed[,ministeerium])]
  andmed[, kanal:=gsub("Epost", "E-post", andmed[,kanal])]
  andmed[, kanal:=gsub("Veebileht / portaal", "Veebileht", andmed[,kanal])]
  andmed[, naitaja:=gsub("osutamistearv", "osutamiste arv", andmed[,naitaja])]
  andmed[, naitaja:=gsub("ajakuluBruto", "ajakulu (bruto)", andmed[,naitaja])]
  andmed
}
#tõlgete külge liitmine
tolked=function(andmed) {
  #loen sisse tõlkefailid (fread aitab encodingu probleeme ennetada)
  kanali_tolked=fread("./translations/dictionary_channels.csv", encoding = "UTF-8")
  naitaja_tolked=fread("./translations/dictionary_moodik.csv", encoding = "UTF-8")
  ministeerium_tolked=fread("./translations/dictionary_ministeeriumid.csv", encoding = "UTF-8")
  asutused_tolked=fread("./translations/dictionary_asutused.csv", encoding = "UTF-8")

  #keevitan kanalite, moodikute tõlked juurde, kui siin "andmed" ridade
  #ridade arv väheneb, on kuskil keys bugi sees/puudu
  andmed=merge(andmed, kanali_tolked, by.x="kanal", by.y="key")
  andmed=merge(andmed, naitaja_tolked, by.x="naitaja", by.y="key")
  andmed=merge(andmed, ministeerium_tolked, by.x="ministeerium", by.y="key")
  andmed=merge(andmed, asutused_tolked, by.x="allasutus", by.y="key")
  andmed
}
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

#funktsioon, mis loob appile ikoonid
ikoonija=function(andmed, minist, allasutusnimi, minJah, text, keel, 
                   naitajaNimi=NA, ikoon, varv, arvutaKeskmine=F, 
                  teenusteSum=F) {
  if (keel=="et") {
    if (minJah==1) { #kui muu, siis on allasutus
      andmed=andmed[andmed$ministeerium==minist,]
    } else if (minJah==2) {
      andmed=andmed
    }else {
      andmed=andmed[allasutus==allasutusnimi]
    }
  } else {#keel=="en"
    if (minJah==1) { #kui muu, siis on allasutus
      andmed=andmed[andmed$ministeerium_en==minist,]
    } else if (minJah==2) {
      andmed=andmed
    }else {
      andmed=andmed[allasutus_en==allasutusnimi]
    }
  }
  if (teenusteSum==T) {#teenuste summa
    arvud=length(unique(andmed$identifikaator))
  } else if (teenusteSum==F) {
    arvud=andmed[naitaja==naitajaNimi, value]
    if (arvutaKeskmine==T) {#vaja muutuja keskmine
      arvud=mean(arvud, na.rm=T)
    } else {#siis kui vaja võtta näitaja summa
      arvud=sum(arvud ,na.rm=T)
    }
  } 
  valueBox(
    paste(paste(format(round(arvud), big.mark=" "))), 
    text,icon = icon(ikoon),color = varv)
}
