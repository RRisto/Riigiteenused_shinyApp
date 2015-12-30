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

# visualiseerija=function(data, mapping, ylab) {
#   #localenv <- environment()
#   #library(ggplot2)
#   library(scales)
#   #library(plotly)
#   ggplot(data, mapping)+
#     geom_bar(stat = "identity", fill="lightblue")+
#     theme_minimal()+
#     theme(axis.text.x = element_text(angle = 45, hjust=1, size=13))+
#     xlab("")+
#     ylab(ylab)+
#     coord_cartesian(ylim=c(0,1))+
#     scale_y_discrete(labels = percent)+
#     ggtitle("Järgmiste mõõdikuga teenuste osakaal:")
#   
# }

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

###############funktsioonid andmete sisse lugemiseks ja pikaks tegemiseks
meltimine=function(kanal, data) {
  library(reshape2)
  #leiame ainult seda kanalit puudutavad muutujad
  sub=data[, grepl(paste(kanal, "|identifikaator|ministeerium|allasutus|tegevusvaldkond|teenusetyyp|makse", sep=""), names(data))]
  #määran id-d, mis meltimisel meltimata jäävad
  id=grep(c("identifikaator|ministeerium|allasutus|tegevusvaldkond|teenusetyyp|makse|link"), names(sub), value=T)
  #kui selle kanali kohta stati pole, anna vastuseks null
  if(length(id)<7) {
    tulem=NULL
  } else {
    #meldime andmed kitsaks
    tulem=melt(sub, id=id)
    #muudan variable nime ära, mis on kanalispets, muidu ei saa rbindida
    names(tulem)=c("identifikaator", "tegevusvaldkond", "tyyp", "ministeerium", "allasutus",  
                   "makse", "link",  "variable",           
                   "value")
  }
  tulem
}

#abifunktsioon andmete sisse lugemiseks
korrastaja=function(andmed, eemalda) {
  library(reshape2)
  #eemalda - mis osa columnite nimedest tuleb eemdalda
  names(andmed)=gsub(pattern=eemalda,"" ,names(andmed))
  #kanalite lõikes meldime
  veeb=meltimine("Veebileht / portaal.", data=andmed)
  iseteen=meltimine("E-iseteenindus.", data=andmed)
  eesti=meltimine("Eesti.ee.", data=andmed)
  nuti=meltimine("Nutirakendus.", data=andmed)
  digitv=meltimine("Digitelevisioon.", data=andmed)
  epost=meltimine("E-post.", data=andmed)
  #sms=meltimine("Tekstisõnum.", data=andmed)
  sms=meltimine("Tekstis\u00F5num.", data=andmed) #shiny jaoks vaja
  telefon=meltimine("Telefon.", data=andmed)
  faks=meltimine("Faks.", data=andmed)
  post=meltimine("Post.", data=andmed)
  #lett=meltimine("Letiteenus büroos.", data=andmed) #võib muutuda! vaja ka gsubi siis lisada
  lett=meltimine("Letiteenus b\u00FCroos.", data=andmed) #shiny jaoks vaja
  kodus=meltimine("Kliendi juures.", data=andmed)
  #rbindime
  koos=rbind(veeb, iseteen, eesti, nuti, digitv, epost, sms, telefon, faks, 
             post, lett, kodus)
  #leiame kanali ja näitaja
  #kanal <- strsplit(as.character(koos$variable), split ="\\.\\w{1,}$")
  #stati saamiseks eemaldame punktid kanali nimedest
  koos$variable=gsub(".ee.", ".", as.character(koos$variable), fixed=T)
  koos$variable=gsub("Letiteenus b\u00FCroos", "Letiteenus", as.character(koos$variable), fixed=T)
  koos$variable=gsub("E-iseteenindus", "Eiseteenindus", as.character(koos$variable), fixed=T)
  koos$variable=gsub("E-post", "Epost", as.character(koos$variable), fixed=T)
  koos$variable=gsub("Veebileht / portaal", "Veebileht", as.character(koos$variable), fixed=T)
  stat=gsub("Kliendi juures", "Kliendijuures", as.character(koos$variable), fixed=T)
  
  #lõikame punktini asja maha
  stat <- strsplit(stat, split ="\\.")
  #teeme df-ks
  df=as.data.frame(stat)
  #transponeerime
  df=as.data.frame(t(df))
  #lisame algsesse andmestikku
  koos$kanal=df[,2]
  koos$naitaja=df[,3]
  #viskame välja tühjad read, kus pole linki
 # koos=koos[!is.na(koos$link),]
  koos$value=as.numeric(as.character(koos$value))
  koos
} 
#andmete download
andmedSisse=function() {
  #loeme andmed sisse
  library(jsonlite)
  library(data.table)
  andmed=fromJSON(readLines("https://www.riigiteenused.ee/api/et/all"), flatten=T)
  andmed=andmed["teenuste_kanalid_ja_moodikud"!="list()"]
  andmedMoodik <- rbindlist(lapply(andmed[["teenuste_kanalid_ja_moodikud"]], function(x) {
    as.list(unlist(x))
  }), fill=TRUE)
  
  andmed
}

#andmed pikaks
DataLong2=function(andmedLai) {
  vars=names(andmedLai) %in% c("regulatsioon", "objectId", "eluarisyndmus", "kirjeldus", 
                               "eeltingimus", "jareltingimus", "createdAt", 
                               "updatedAt", "keel", "osakondyksus", "omanikunimi", 
                               "omanikutelefon","omanikuemail", 
                               "konfinfo", "seotuddokumendid", "seisund", 
                               "muudatustvajav", "aegumisekpv", "funktsioon", 
                               "veebiaadress")
  #eemaldame muutujad
  andmedLai=andmedLai[,!vars]
  #aastate põhjal teeme andmed 2-ks (kui aastaid rohkem siis vastavalt sellele
  #arv muutub)
  andmedLai2014=andmedLai[, !grepl("empty.|2011.", names(andmedLai))]
  andmedLai2011=andmedLai[, !grepl("empty.|2014.", names(andmedLai))]
  andmedLaiEmpty=andmedLai[, !grepl("2014.|2011.", names(andmedLai))]
  puhas2014=korrastaja(andmedLai2014, "2014.")
  puhasEmpty=korrastaja(andmedLaiEmpty, "empty.")
  puhas2011=korrastaja(andmedLai2011, "2011.")
  #paneme andme kokku
  andmedPikk=rbind(puhas2014, puhas2011,puhasEmpty)
  andmedPikk
}
 