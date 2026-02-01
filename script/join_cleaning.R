saveRDS(fp_final, "fp_final.rds") 


## basqa dataya menimsetmek ####

fp_yekun = fp_final
saveRDS(fp_yekun, "yekun.rds") 

## profit elave olunmasi ####

library(dplyr)

fp_final = fp_final %>% 
  mutate(profit = round(revenue * profit_margin, 2))


## cografi movqey sutunu ####

library(dplyr)
library(stringr)

fp_final = fp_final %>%
  mutate(cografim = case_when(
    #1. ZONALAR 
    
    # Şimal Zonası
    str_detect(pos, "QUBA|QUSAR|XACMAZ|XUDAT|SIYEZEN|SABRAN") ~ "ŞİMAL REGİONU",
    
    # Cənub Zonası
    str_detect(pos, "LENKERAN|MASALLI|CELILABAD|ASTARA|LERIK|YARDIMLI") ~ "CƏNUB REGİONU",
    
    # Qərb Zonası
    str_detect(pos, "GENCE|GANJA|SEMKIR|TOVUZ|QAZAX|AGSTAFA|GEDEBEY|NAFTALAN") ~ "QƏRB REGİONU",
    
    # Aran və Mərkəzi Zonalar (Göyçay, Aran, Qarabağ daxil)
    str_detect(pos, "GOYCAY|GOYCHAY|AGDAS|UCAR|KURDEMIR|ISMAYILLI|
               SAMAXI|QEBELE|AGSU|BERDE|YEVLAX|AGDAM|AGCABEDI|SABIRABAD|
               SAATLI|IMISLI|BEYLEQAN|TERTER|NEFTCALA|SALYAN|SIRVAN|
               ZARDA|QEDIROGLU|MINGECEVIR") 
    ~ "ARAN/MƏRKƏZİ REGİON",
    
    # 2. SUMQAYIT 
    str_detect(pos, "SUMQAYIT|SUMGAYIT") ~ "SUMQAYIT",
    
    # 3. BAKI DAXİLİ (Spesifik Rayonlar)
    str_detect(pos, "BINEQEDI|6 MKR|7 MKR|8 MKR|9 MKR|BILECERI|
               SULUTEPE|XOCASEN|RESULZADE") ~ "BAKI (BİNƏQƏDİ)",
    
    str_detect(pos, "YASAMAL|SERIFZADE|ELMLER|INSAATCILAR|METBUAT|
               GELENQAYA|YENI YASAMAL") ~ "BAKI (YASAMAL R-NU)",
    
    str_detect(pos, "KHETAI|XETAI|EHMEDLI|HEZI ASLANOV|GUNESLI|
               AG SEHER|NOBEL|8 NOYABR|BABEK|NZS") ~ "BAKI (XƏTAİ/GÜNƏŞLİ)",
    
    str_detect(pos, "28 MAY|28 MALL|NESIMI|ECEMI|20 YANVAR|3 MKR|
               4 MKR|5 MKR|TBILISI|TIFLIS|AZADLIQ|OAZIS|KUBINKA") ~ "BAKI (NƏSİMİ R-NU)",
    
    str_detect(pos, "NERIMANOV|GENCLIK|INQILAB|HEYDER ELIYEV|METROPARK|
               TEBRIZ|ATATURK|MONTIN") ~ "BAKI (NƏRİMANOV R-NU)",
    
    str_detect(pos, "QARA QARAYEV|NEFTCHILER|XALQLAR|8 KM|KESLE") ~ "BAKI (NİZAMİ R-NU)",
    
    str_detect(pos, "LOKBATAN|SEBAIL|BAYIL|BADAMDAR|PORT BAKU|
               SAHIL|TARGOVI|ICERISEHER|AZNEFT") ~ "BAKI (SƏBAİL/QARADAĞ)",
    
    #  4. BAKI KƏNAR / ABŞERON 
    str_detect(pos, "SUVELAN|SHUVALAN|MERDEKAN|BUZOVNA|BILGEH|
               PIRSAGI|NOVXANI|MARDAKAN") ~ "BAĞLAR (XƏZƏR R-NU)",
    
    str_detect(pos, "KOROGLU|SABUNCU|BAKIXANOV|ZABRAT|MASHTAGA|MASTAGA") ~ "BAKI (SABUNÇU R-NU)",
    
    str_detect(pos, "HOVSAN|BINE|QALA|ZIRE") ~ "BAKI (KƏNAR QƏSƏBƏLƏR)",
    
    str_detect(pos, "XIRDALAN|MASAZIR|SARAY|QOBU|JEYRANBATAN|
               ABSHERON|ABSERON") ~ "ABŞERON (XIRDALAN/MASAZIR)",
    
    # 5. ÜMUMİ VƏ DİGƏR
    str_detect(pos, "ARAZ MARKET|OBA MARKET|
               AL MARKET|BRAVO|BAZARSTORE|RAHAT|
               SPAR|NEPTUN|BOLMART|GRANDMART|MEGASTORE") ~ "BAKI (ÜMUMİ ŞƏBƏKƏ)",
    
    str_detect(pos, "MARKET|MAGAZA|APTEK|ERZAQ|TICARET") ~ "BAKI (ÜMUMİ - TƏSNİFATSIZ)",
    
    TRUE ~ "BAKI (DİGƏR)"))
