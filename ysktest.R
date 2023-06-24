library(httr)
library(progress)
library(tidyverse)
library(jsonlite)
setwd(WD_HERE)
secim_id_liste <- c("2023 2.Tur" = 60822,
                    "2023 1.Tur" = 60792,
                    "2019 İstanbul Yenileme" = 50031,
                    "2019 Yerel Seçimler" = 49302,
                    "2018 Secimleri" = 49002,
                    "2017 Referandum" = 46827,
                    "2015 - 2 Seçimleri" = 44706,
                    "2015 - 1 Seçimleri" = 41754,
                    "2014 Cumhurbaşkanlığı Seçimi" = 40122,
                    "2014 Yerel Seçimler" = 36039,
                    "2011 Genel Seçimleri" = 22506)

secim_turu <- c("Belediye Başkanlığı" = 2,
                "Belediye Meclisi" = 3,
                "İl Genel Meclisi" = 4,
                "Büyükşehir Belediye Başkanlığı" = 6,
                "Referandum" = 7,
                "Milletvekili" = 8,
                "Cumhurbaskani" = 9)

secimler <- data.frame(
    cb_2023_tur_2 = c(60822,9,1),
    cb_2023_tur_1 = c(60792,9,1),
    tbmm_2023_tur_1 = c(60792,8,1),
    belediye_2019_ist_yenileme = c(50031,6,1),
    yerel_belediye_baskan_2019 = c(49302,2,1),
    yerel_belediye_meclis_2019 = c(49302,3,1),
    yerel_il_genel_meclis_2019 = c(49302,4,1),
    yerel_buyukseh_baskan_2019 = c(49302,6,1),
    tbmm_2018 = c(49002,8,1),
    referandum_2017 = c(46827,7,1),
    tbmm_2015_tur_2 = c(44706,8,1),
    tbmm_2015_tur_1 = c(41754,8,1),
    yerel_belediye_baskan_2014 = c(36039,2,1),
    yerel_belediye_meclis_2014 = c(36039,3,1),
    yerel_il_genel_meclis_2014 = c(36039,4,1),
    yerel_buyukseh_baskan_2014 = c(36039,6,1),
    tbmm_2011 = c(22506,8,1)
)

yurt_disi_secim_liste <- data.frame(
  yurt_disi_cb_2023_tur_2 = c(60822,9),
  yurt_disi_cb_2023_tur_1 = c(60792,9),
  yurt_disi_tbmm_2023_tur_1 = c(60792,8),
  yurt_disi_tbmm_2018 = c(49002,8),
  yurt_disi_referandum_2017 = c(46827,7),
  yurt_disi_tbmm_2015_tur_2 = c(44706,8),
  yurt_disi_tbmm_2015_tur_1 = c(41754,8),
  yurt_disi_tbmm_2011 = c(22506,8)

)

yurt_ici_tutanak_getter <- function(secim_id,secim_turu,yurt_ici_disi = 1){

  secim_cevresi_getter <- function(secim_id,secim_turu,yurt_ici_disi){
    res <- VERB("GET", url = paste0("https://sonuc.ysk.gov.tr/api/getIlList?secimId=",secim_id,"&secimTuru=",secim_turu,"&sandikTuru=-1&yurtIciDisi=",yurt_ici_disi))
    return(jsonlite::fromJSON(content(res, 'text')))
  }

  ilce_getter <- function(secim_id,secim_turu,il_id,secim_cevresi_id,yurt_ici_disi){
    res <- VERB("GET", url = paste0("https://sonuc.ysk.gov.tr/api/getIlceList?secimId=",secim_id,"&secimTuru=",secim_turu,"&ilId=",il_id,"&secimCevresiId=",secim_cevresi_id,"&sandikTuru=-1&yurtIciDisi=",yurt_ici_disi))
    return(jsonlite::fromJSON(content(res, 'text')))
  }

  sonuc_getter <- function(secim_id,secim_turu,il_id,ilce_id,yurt_ici_disi,secim_cevresi_id){
    res <- VERB("GET", url = paste0("https://sonuc.ysk.gov.tr/api/getSecimSandikSonucList?secimId=",secim_id,
                                    "&secimTuru=",secim_turu,"&ilId=",il_id,
                                    "&ilceId=",ilce_id,"&beldeId=0&birimId=0&muhtarlikId=&cezaeviId=&sandikTuru=&sandikNoIlk=&sandikNoSon=",
                                    "&ulkeId=&disTemsilcilikId=&gumrukId=&",
                                    "yurtIciDisi=",yurt_ici_disi,"&sandikRumuzIlk=&sandikRumuzSon=",
                                    "&secimCevresiId=",secim_cevresi_id,"&sandikId=&sorguTuru=2"))
    return(jsonlite::fromJSON(content(res, 'text')))
  }

  sandik_liste <- data.frame()
  secim_cevresi <- secim_cevresi_getter(secim_id,secim_turu,yurt_ici_disi)
  pb <- progress_bar$new(total = nrow(secim_cevresi))
  for (i in 1:nrow(secim_cevresi)){
    pb$tick()
    ilce <- ilce_getter(secim_id,secim_turu,secim_cevresi$il_ID[i],secim_cevresi$secim_CEVRESI_ID[i],yurt_ici_disi)
    for (j in ilce$ilce_ID){
      sonuc <- sonuc_getter(secim_id,secim_turu,secim_cevresi$il_ID[i],j,yurt_ici_disi,secim_cevresi$secim_CEVRESI_ID[i])
      sandik_liste <- rbind(sandik_liste, sonuc)
    }
  }
  return(sandik_liste)
}

yurt_disi_tutanak_getter <- function(secim_id,secim_turu){

  ulke_getter <- function(secim_id){
    res <- VERB("GET", url = paste0("https://sonuc.ysk.gov.tr/api/getUlkeList?secimId=",secim_id))
    return(fromJSON(content(res, 'text')))
  }

  ulke_bolge_getter <- function(secim_id,secim_turu,ulke_id){
    res <- VERB("GET", url = paste0("https://sonuc.ysk.gov.tr/api/getSecimSandikSonucList?secimId=",secim_id,
                                    "&secimTuru=",secim_turu,"&ilId=&ilceId=&beldeId=&birimId=&muhtarlikId=&cezaeviId=",
                                    "&sandikTuru=&sandikNoIlk=&sandikNoSon=&ulkeId=",ulke_id,"&disTemsilcilikId=",
                                    "&gumrukId=&yurtIciDisi=2&sandikRumuzIlk=&sandikRumuzSon=&secimCevresiId=&sandikId=&sorguTuru=2"))
    return(fromJSON(content(res, 'text')))
  }

  dis_temsilcilik_sandik_getter <- function(secim_id,secim_turu,ulke_id,dis_temsilcilik_id){
    res <- VERB("GET", url = paste0("https://sonuc.ysk.gov.tr/api/getSecimSandikSonucList?secimId=",secim_id,
                                    "&secimTuru=",secim_turu,"&ilId=&ilceId=&beldeId=&birimId=&muhtarlikId=&cezaeviId=",
                                    "&sandikTuru=&sandikNoIlk=&sandikNoSon=&ulkeId=",ulke_id,
                                    "&disTemsilcilikId=",dis_temsilcilik_id,
                                    "&gumrukId=&yurtIciDisi=2&sandikRumuzIlk=&sandikRumuzSon=&secimCevresiId=&sandikId=&sorguTuru=2"))
    return(fromJSON(content(res, 'text')))
  }


  gumruk_getter <- function(secim_id){
    res <- VERB("GET", url = paste0("https://sonuc.ysk.gov.tr/api/getGumrukList?secimId=",secim_id))
    return(fromJSON(content(res, 'text')))
  }

  gumruk_sandik_getter <- function(secim_id,secim_turu,ilce_id,gumruk_id){
    res <- VERB("GET", url = paste0("https://sonuc.ysk.gov.tr/api/getSecimSandikSonucList?secimId=",secim_id,
                                    "&secimTuru=",secim_turu,
                                    "&ilId=&ilceId=",ilce_id,"&beldeId=&birimId=&muhtarlikId=&cezaeviId=&sandikTuru=",
                                    "&sandikNoIlk=&sandikNoSon=&ulkeId=&disTemsilcilikId=&gumrukId=",gumruk_id,
                                    "&yurtIciDisi=2&sandikRumuzIlk=&sandikRumuzSon=&secimCevresiId=&sandikId=&sorguTuru=2"))
    return(fromJSON(content(res, 'text')))
  }

  gumruk_sandik_liste <- data.frame()
  dis_temsilcilik_sandik_liste <- data.frame()

  gumruk_liste <- gumruk_getter(secim_id)


  for (i in 1:nrow(gumruk_liste)){
    sonuc1 <- gumruk_sandik_getter(secim_id,secim_turu,gumruk_liste$ilce_ID[i],gumruk_liste$gumruk_ID[i])
    gumruk_sandik_liste <- rbind(gumruk_sandik_liste, sonuc1)
  }

  #ulke_liste <- ulke_getter(secim_id)
#
  #for (i in 1:nrow(ulke_liste)){
  #  ulke_id <- ulke_liste$ulke_ID[i]
  #  ulke_bolgeler <- ulke_bolge_getter(secim_id,secim_turu,ulke_id)
  #  for (j in 1:nrow(ulke_bolgeler)){
  #    dis_temsilcilik_id <- ulke_bolgeler$dis_TEMSILCILIK_ID[j]
  #    sonuc2 <- dis_temsilcilik_sandik_getter(secim_id,secim_turu,ulke_id,dis_temsilcilik_id)
  #    dis_temsilcilik_sandik_liste <- rbind(dis_temsilcilik_sandik_liste, sonuc2)
  #  }
  #}
#
  #return(list(gumruk_sandik_liste,dis_temsilcilik_sandik_liste))
  return(gumruk_sandik_liste)
}


for(l in 1:7){
  print(l)
  a <- yurt_disi_tutanak_getter(yurt_disi_secim_liste[,l][1],yurt_disi_secim_liste[,l][2])
  write.csv(a[[1]],paste0(names(yurt_disi_secim_liste)[l],"_gumruk",".csv"))
  write.csv(a[[2]],paste0(names(yurt_disi_secim_liste)[l],"_dis_temsilcilik",".csv"))
}
l <- 8
a <- yurt_disi_tutanak_getter(yurt_disi_secim_liste[,l][1],yurt_disi_secim_liste[,l][2])
write.csv(a,paste0(names(yurt_disi_secim_liste)[l],"_gumruk",".csv"))
