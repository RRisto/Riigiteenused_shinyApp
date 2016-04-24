########Abiskript tõlkefailide õiesse kodeeringusse viimiseks
#csv-st teeme rds, kuna muidu serverisse laadimisel tekib koadega jama
# kanali_tolked=read.csv("./translations/algfailid/dictionary_channels.csv", sep=";", stringsAsFactors = F)
# naitaja_tolked=read.csv("./translations/algfailid/dictionary_moodik.csv", sep=";", stringsAsFactors = F)
# ministeerium_tolked=read.csv("./translations/algfailid/dictionary_ministeeriumid.csv", sep=";", stringsAsFactors = F)
# asutused_tolked=read.csv("./translations/algfailid/dictionary_asutused.csv", sep=";", stringsAsFactors = F)
# translation=read.csv("./translations/algfailid/dictionary_ui.csv", sep=";")#tõlked interface raami jaoks
# 
# #csv
# write.table(translation,"./translations/dictionary_ui.csv", sep=";",row.names = F, fileEncoding ="utf-8")
# write.table(asutused_tolked,"./translations/dictionary_asutused.csv", sep=";",row.names = F, fileEncoding ="utf-8")
# write.table(ministeerium_tolked,"./translations/dictionary_ministeeriumid.csv", sep=";",row.names = F, fileEncoding ="utf-8")
# write.table(naitaja_tolked,"./translations/dictionary_moodik.csv", sep=";",row.names = F, fileEncoding ="utf-8")
# write.table(kanali_tolked,"./translations/dictionary_channels.csv", sep=";",row.names = F, fileEncoding ="utf-8")
# 
# #rds
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