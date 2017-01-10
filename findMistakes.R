library(riigiteenused)
andmedLai=andmedSisse()
andmedPikk=andmedPikaks(andmedLai)
#toob ära id-d, mis ei ole pikas tabelis kuid on laias (arvatavsti mõõtmise aasta
#/kanal puudu)
andmedLai[!andmedLai$identifikaator%in%andmedPikk$identifikaator,
          "identifikaator"]
lai=jsonlite::fromJSON("https://www.riigiteenused.ee/api/et/all")
lai$identifikaator[lai$teenuste_kanalid_ja_moodikud=="list()"]