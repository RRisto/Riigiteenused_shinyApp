########Abiskript tõlkefailide õiesse kodeeringusse viimiseks
#csv-st teeme rds, kuna muidu serverisse laadimisel tekib koadega jama
kanali_tolked=read.csv("./translations/dictionary_channels.csv", sep=";", stringsAsFactors = F)
naitaja_tolked=read.csv("./translations/dictionary_moodik.csv", sep=";", stringsAsFactors = F)
ministeerium_tolked=read.csv("./translations/dictionary_ministeeriumid.csv", sep=";", stringsAsFactors = F)
asutused_tolked=read.csv("./translations/dictionary_asutused.csv", sep=";", stringsAsFactors = F)
translation=read.csv("./translations/dictionary_ui.csv", sep=";")#tõlked interface raami jaoks

#csv
write.table(translation,"./translations/dictionary_ui.csv", sep=";",row.names = F, fileEncoding ="utf-8")
write.table(asutused_tolked,"./translations/dictionary_asutused.csv", sep=";",row.names = F, fileEncoding ="utf-8")
write.table(ministeerium_tolked,"./translations/dictionary_ministeeriumid.csv", sep=";",row.names = F, fileEncoding ="utf-8")
write.table(naitaja_tolked,"./translations/dictionary_moodik.csv", sep=";",row.names = F, fileEncoding ="utf-8")
write.table(kanali_tolked,"./translations/dictionary_channels.csv", sep=";",row.names = F, fileEncoding ="utf-8")

#rds
# saveRDS(kanali_tolked, "./translations/dictionary_channels.RDS")
# saveRDS(naitaja_tolked, "./translations/dictionary_moodik.RDS")
# saveRDS(ministeerium_tolked, "./translations/dictionary_ministeeriumid.RDS")
# saveRDS(asutused_tolked, "./translations/dictionary_asutused.RDS")
# saveRDS(translation, "./translations/dictionary_ui.RDS")

############abiskript leidmaks tõlgetes mismache tegelikest andmetest
#kanalid
kanalOlemas=c()
for (i in 1:nrow(andmed)) {
  if(andmed$kanal[i]%in%kanali_tolked$key) {
    kanalOlemas[i]=FALSE
  } else {
    kanalOlemas[i]=TRUE
  }
}
#väärtused, mis tõlke failis ei klapi
unique(andmed$kanal[kanalOlemas])

############allasutused
asutusOlemas=c()
for (i in 1:nrow(andmed)) {
  if(andmed$allasutus[i]%in%asutused_tolked$key) {
    asutusOlemas[i]=FALSE
  } else {
    asutusOlemas[i]=TRUE
  }
}

#väärtused, mis tõlke failis ei klapi
unique(andmed$allasutus[asutusOlemas])

#mitmel teenusel on statisika rohkem kui ühe aasta kohta
library(dplyr)

mootmiseAastaUnik=andmed%>%
  group_by(identifikaator)%>%
  summarise(n=length(unique(MootmiseAasta)))

mootmiseAastaUnik[mootmiseAastaUnik$n>1,]

#asutuste musta nimekrija kuvamine
#nimekirja kopisin riigiteenuste protalaist excelisse ja salvestasin csv-na
asutused=read.csv("asutused_must.csv", sep=";")
asutused$Nimi=as.character(asutused$Nimi)
asutused_eestikeel=asutused[grepl("Eesti keel",asutused$Nimi)&!grepl("haldusala",asutused$Nimi),]
asutused_eestikeel=gsub("\\(Eesti keel)", "", asutused_eestikeel)

asutused_inglisekeel=asutused[grepl("Inglise keel",asutused$Nimi)&!grepl("administrative area",asutused$Nimi),]
asutused_inglisekeel=gsub("\\(Inglise keel)", "", asutused_inglisekeel)
#remove whitespace
library(stringr)
asutused_eestikeel=str_trim(asutused_eestikeel, side="right")
asutused_inglisekeel=str_trim(asutused_inglisekeel, side="right")

#duplikaadid välja
asutused_eestikeel=asutused_eestikeel[!duplicated(asutused_eestikeel)]
asutused_inglisekeel=asutused_inglisekeel[!duplicated(asutused_inglisekeel)]
#salvestame
write.table(asutused_eestikeel, "asutused_est.csv", row.names = F, sep=";")
write.table(asutused_inglisekeel, "asutused_eng.csv", row.names = F, sep=";")


