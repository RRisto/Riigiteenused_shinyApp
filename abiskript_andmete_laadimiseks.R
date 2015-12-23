#abiskript andmete käsitsi allalaadimiseks
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
  sms=meltimine("Tekstisõnum.", data=andmed)
  telefon=meltimine("Telefon.", data=andmed)
  faks=meltimine("Faks.", data=andmed)
  post=meltimine("Post.", data=andmed)
  lett=meltimine("Letiteenus büroos.", data=andmed) #võib muutuda! vaja ka gsubi siis lisada
  kodus=meltimine("Kliendi juures.", data=andmed)
  #rbindime
  koos=rbind(veeb, iseteen, eesti, nuti, digitv, epost, sms, telefon, faks, 
             post, lett, kodus)
  #leiame kanali ja näitaja
  #kanal <- strsplit(as.character(koos$variable), split ="\\.\\w{1,}$")
  #stati saamiseks eemaldame punktid kanali nimedest
  koos$variable=gsub(".ee.", ".", as.character(koos$variable), fixed=T)
  koos$variable=gsub("Letiteenus büroos", "Letiteenus", as.character(koos$variable), fixed=T)
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
  koos=koos[!is.na(koos$link),]
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
  andmedLaiEmpty=andmedLai[, !grepl(".2014.|2011.", names(andmedLai))]
  puhas2014=korrastaja(andmedLai2014, "2014.")
  puhasEmpty=korrastaja(andmedLaiEmpty, "empty.")
  puhas2011=korrastaja(andmedLai2011, "2011.")
  #paneme andme kokku
  andmedPikk=rbind(puhas2014, puhas2011,puhasEmpty)
  return(andmedPikk)
}
andmedLai=andmedSisse()
andmedPikk=DataLong2(andmedLai)
#teeme valued numericuks
andmedPikk$value=as.numeric(as.character(andmedPikk$value))
#kirjutame faili
failinimirds=paste0(Sys.Date(), "_andmedPikk.rds")
saveRDS(andmedPikk, failinimirds)
