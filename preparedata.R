library(sf)
library(tidyverse)

baseurl <- "https://kartta.hel.fi/ws/geoserver/avoindata/wfs?"
wfs_request <- "request=GetFeature&service=WFS&version=2.0.0&typeName=Halke_aanestysalue&outputFormat=json"
hki_alue_wfs <- paste0(baseurl,wfs_request)
alueet <- st_read(hki_alue_wfs)

alueet <- alueet %>% 
  st_transform(4326) %>% 
  rename(atunnus = tunnus)

# https://tulospalvelu.vaalit.fi/EPV-2024/fi/ladattavat_tiedostot.html
# Ehdokasasettajakohtaiset tulokset
# Otsikkorivi ja data yhdistetty ensin LibreOfficessa
ehdtul <- read.csv("epv-2024_apa_maa.csv", sep = ",", fileEncoding = "ISO-8859-1", stringsAsFactors = FALSE)

ehdtul <- ehdtul %>% 
  rename(atunnus = Äänestysaluetunnus)

tulos <- left_join(ehdtul, alueet)

hki <- tulos %>% 
  filter(Kuntanro == 91) %>% 
  select(atunnus, Kunnan.vaalipiirin.hv.alueen..äänestysalueen.nimi.suomeksi,Puolueen.ryhmän.nimilyhenne.suomeksi, Osuus.kaikista.äänistä....) %>% 
  rename(puolue = Puolueen.ryhmän.nimilyhenne.suomeksi,
         alue = Kunnan.vaalipiirin.hv.alueen..äänestysalueen.nimi.suomeksi,
         osuus = Osuus.kaikista.äänistä....) %>% 
  mutate(osuus = osuus/10)

top <- hki %>% 
  group_by(atunnus) %>% 
  summarise(max(osuus))

top2 <- left_join(top, hki, by = "atunnus") %>% 
  filter(`max(osuus)` == osuus,
         atunnus != "****") %>% 
  select(-`max(osuus)`)

# 090A Ä-alue 090A VAS 269 (Oodi?)
# 090B Ä-alue 090B VIHR 252 (ulkomaat?)

alue_top <- left_join(alueet, top2) %>% 
  select(atunnus, nimi_fi, puolue, osuus) %>% 
  mutate(across(where(is.character), str_trim),
         col = ifelse(puolue == "KOK", "blue",
                      ifelse(puolue == "VAS", "red",
                             ifelse(puolue == "SDP", "magenta",
                                    ifelse(puolue == "VIHR", "green", "black")))))

write_rds(alue_top, "alue_top.RDS")
