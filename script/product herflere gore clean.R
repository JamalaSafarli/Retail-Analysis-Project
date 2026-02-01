library(stringr)
fp_final = fp_final %>%
  mutate(product_name = toupper(product_name)) %>% 
  mutate(product_name = str_replace_all(product_name, c(
    "Ə" = "E",
    "İ" = "I",
    "Ş" = "S",
    "Ç" = "C",
    "Ğ" = "G",
    "Ö" = "O",
    "Ü" = "U"
  ))) %>%
  mutate(product_name = str_squish(product_name))


## herflere gore cleaning prod ####

library(dplyr)
library(stringr)

## z herfi ####

fp_final = fp_final %>%
  mutate(product_name = case_match(product_name,
                            
                                   "ZOLOTOE SOLNISKO GUNEBAXAN YAGI 1 L"    ~ "ZOLOTOE SOLNISHKO GUNEBAXAN YAGI 1 L",
                                   "ZOLOTOE SOLNISKO GUNEBAXAN 1 L"         ~ "ZOLOTOE SOLNISHKO GUNEBAXAN YAGI 1 L",
                                   "ZOLOTOE SOLNISHKO GUNEBAXAN YAG 1 L"    ~ "ZOLOTOE SOLNISHKO GUNEBAXAN YAGI 1 L",
                                   "ZOLOTOE SOLNISKA GUNEBAXAN YAG 1 L"     ~ "ZOLOTOE SOLNISHKO GUNEBAXAN YAGI 1 L",
                                   "ZOLOTOE SOLNISNKO 1 L GUNEBAXAN"        ~ "ZOLOTOE SOLNISHKO GUNEBAXAN YAGI 1 L",
                                   "ZOLOTAYA SEMECKA GUNEBAXAN YAGI 1 L"    ~ "ZOLOTAYA SEMECHKA GUNEBAXAN YAGI 1 L",
                                   "ZOLOTOE SOLNISKO QARGIDALI YAGI 1 L"    ~ "ZOLOTOE SOLNISHKO QARGIDALI YAGI 1 L",
                                   "ZOLOTOE SOLNISHKO QARGIDALI YAG 1 L"    ~ "ZOLOTOE SOLNISHKO QARGIDALI YAGI 1 L",
                              
                                   "ZEYTUN BAGLARI ZEYTUN YAGI SUSE 1 L"    ~ "ZEYTUN BAGLARI ZEYTUN YAGI 1 L SHUSHE",
                                   "ZEYTUN BAGLARI 1 L SUSE"                ~ "ZEYTUN BAGLARI ZEYTUN YAGI 1 L SHUSHE",
                                   "ZEYTUN BAGLARI 1 L 9 SUSE"              ~ "ZEYTUN BAGLARI ZEYTUN YAGI 1 L SHUSHE",
                                   "Z BAGLARI ZEYTUN YAGI 1 L"              ~ "ZEYTUN BAGLARI ZEYTUN YAGI 1 L",
                                   "ZEYTUN BAGLARI EKSTRA ZEYTUN YAGI 500 ML" ~ "ZEYTUN BAGLARI EXTRA ZEYTUN YAGI 500 ML",
                                   "ZEYTUN BAGLARI EXTRA SUSE 250 ML"       ~ "ZEYTUN BAGLARI EXTRA ZEYTUN YAGI 250 ML SHUSHE",
                                
                                   "ZAVOD COREYI"                           ~ "ZAVOD CHOREYI",
                                   "ZAVOD COREYI 600 Q"                     ~ "ZAVOD CHOREYI 600 Q",
                                   "COREK TENDIR 440 Q"                     ~ "CHOREK TENDIR 440 Q",
                                   "ZAVOD COREK XIRDALAN (BOYUK)"           ~ "ZAVOD CHOREYI XIRDALAN BOYUK",
                                   "TENDIR COREYI ED"                       ~ "CHOREK TENDIR ED",
                                
                                   "ROSEN KONFETI YUMMIS SO VKUSOM FRUKTOV KQ" ~ "ROSHEN KONFETI YUMMIS FRUKTOV KQ",
                                   "ROSHEN KONFETI YUMMIS SO VKUSOM FRUKTOV KQ" ~ "ROSHEN KONFETI YUMMIS FRUKTOV KQ",
                                   "KOTEKS ULTRA NOCHNIE"                   ~ "KOTEX ULTRA NOCHNIE",
                                   "PROKLADKI KOTEKS ULTRA NOCHNIE"         ~ "KOTEX ULTRA NOCHNIE",
                                   "MAYONEZ MAXEEV 210 Q ZEYTUN P"          ~ "MAYONEZ MAXEEV ZEYTUN 210 Q",
                                
                                   "1KQ"                                    ~ "1 KQ",
                                   "2KQ"                                    ~ "2 KQ",
                                   "500GR"                                  ~ "500 Q",
                                   "250GR"                                  ~ "250 Q",
                                   "250GR16"                                ~ "250 Q",
                                   "ML"                                     ~ "ML",
                                   "SUSE"                                   ~ "SHUSHE",
                                   .default = product_name)) %>%

  mutate(product_name = str_squish(product_name))


## y herfi ####

library(dplyr)
library(stringr)

fp_final = fp_final %>%
  mutate(product_name = product_name %>%
           str_to_upper() %>%                 
           str_replace_all("  +", " ") %>%    
           str_trim()) %>%                    
  mutate(product_name = product_name %>%
      
           str_replace_all("\\b(CHOREK|CHOREYI|COREYI|CHOREYİ|COREYİ)\\b", "COREK") %>%
           str_replace_all("\\b(YAGI|YA)\\b", "YAG") %>%
           str_replace_all("\\b(YOQURT|YOGHURT)\\b", "YOGURT") %>%
           str_replace_all("\\b(YASKINO|YAWKINO|YASKINA)\\b", "YASHKINO") %>%
           str_replace_all("\\bSAVUSKIN\\b", "SAVUSHKIN") %>%
           str_replace_all("\\bRASTISKA\\b", "RASTISHKA") %>%
           str_replace_all("(\\d+)(Q|KQ|ML|L|ED|GR|MQ)\\b", "\\1 \\2")) %>%

  mutate(product_name = case_match(product_name,
               
                                   "ZOLOTOE SOLNISHKO GUNEBAXAN YAG 1 L" ~ "ZOLOTOE SOLNISHKO GUNEBAXAN YAG 1 L",
                                   "YASHKINO VAFLI SOKOLADLI 200 Q"      ~ "YASHKINO SOKOLADLI VAFLI 200 Q",
                                   "YASHKINO PRYANIK ALBALI 350 Q"       ~ "YASHKINO ALBALILI PRYANIK 350 Q",
                                   "YUMURTA HACIQABUL ZX ED"             ~ "YUMURTA HACIQABUL ED",
                                   "YUMURTA HACIQABUL BROYLER TR EDEDLI" ~ "YUMURTA HACIQABUL ED",
                                   "YUMURTA YARDIMLI KEND 10 ED"          ~ "YUMURTA YARDIMLI 10 ED",
                                   "YUMURTA SADE GENCE"                  ~ "YUMURTA SADE",
                                   "YURDUM XAMA 25 3 KQ"                 ~ "YURDUM XAMA 25% 3 KQ",
                                   "YANTAR MIR YANTAR PENDIR 80 Q"        ~ "YANTAR PENDIR 80 Q",
                                   "YEDOY SAC LAVAS"                      ~ "YEDOY SAC LAVASI",
                                   "YE DOY DOYUMLUQ TENDIR LAVASI"        ~ "YEDOY DOYUMLUQ TENDIR LAVASI",
                           
                                   .default = product_name
  )) %>%
  mutate(product_name = str_squish(product_name))


# x herfi ####

library(dplyr)
library(stringr)

fp_final = fp_final %>%
  mutate(product_name = product_name %>%
           str_to_upper() %>%
           str_replace_all("\\b(CHOREK|CHOREYI|COREYI|CHOREYİ|COREYİ)\\b", "COREK") %>%
           str_replace_all("  +", " ") %>%
           str_trim()) %>%
  
  mutate(product_name = case_match(product_name,
                                   
                                   "ZOLOTOE SOLNISKO GUNEBAXAN YAGI 1 L"    ~ "ZOLOTOE SOLNISHKO GUNEBAXAN YAGI 1 L",
                                   "ZOLOTOE SOLNISKO GUNEBAXAN 1 L"         ~ "ZOLOTOE SOLNISHKO GUNEBAXAN YAGI 1 L",
                                   "ZOLOTOE SOLNISHKO GUNEBAXAN YAG 1 L"    ~ "ZOLOTOE SOLNISHKO GUNEBAXAN YAGI 1 L",
                                   "ZOLOTOE SOLNISKA GUNEBAXAN YAG 1 L"     ~ "ZOLOTOE SOLNISHKO GUNEBAXAN YAGI 1 L",
                                   "ZOLOTOE SOLNISNKO 1 L GUNEBAXAN"        ~ "ZOLOTOE SOLNISHKO GUNEBAXAN YAGI 1 L",
                                   "ZOLOTAYA SEMECKA GUNEBAXAN YAGI 1 L"    ~ "ZOLOTAYA SEMECHKA GUNEBAXAN YAGI 1 L",
                                   "ZOLOTOE SOLNISKO QARGIDALI YAGI 1 L"    ~ "ZOLOTOE SOLNISHKO QARGIDALI YAGI 1 L",
                                   "ZOLOTOE SOLNISHKO QARGIDALI YAG 1 L"    ~ "ZOLOTOE SOLNISHKO QARGIDALI YAGI 1 L",
                                   
                                   "ZEYTUN BAGLARI ZEYTUN YAGI SUSE 1 L"    ~ "ZEYTUN BAGLARI ZEYTUN YAGI 1 L SHUSHE",
                                   "ZEYTUN BAGLARI 1 L SUSE"                ~ "ZEYTUN BAGLARI ZEYTUN YAGI 1 L SHUSHE",
                                   "ZEYTUN BAGLARI 1 L 9 SUSE"              ~ "ZEYTUN BAGLARI ZEYTUN YAGI 1 L SHUSHE",
                                   "Z BAGLARI ZEYTUN YAGI 1 L"              ~ "ZEYTUN BAGLARI ZEYTUN YAGI 1 L",
                                   "ZEYTUN BAGLARI EKSTRA ZEYTUN YAGI 500 ML" ~ "ZEYTUN BAGLARI EXTRA ZEYTUN YAGI 500 ML",
                                   "ZEYTUN BAGLARI EXTRA SUSE 250 ML"       ~ "ZEYTUN BAGLARI EXTRA ZEYTUN YAGI 250 ML SHUSHE",
                                  
                                   "ZAVOD COREK"                            ~ "ZAVOD COREK",
                                   "ZAVOD COREK 600 Q"                      ~ "ZAVOD COREK 600 Q",
                                   "COREK TENDIR 440 Q"                     ~ "TENDIR COREK 440 Q",
                                   "ZAVOD COREK XIRDALAN (BOYUK)"           ~ "ZAVOD COREK XIRDALAN BOYUK",
                                   "TENDIR COREK ED"                        ~ "TENDIR COREK ED",
                                  
                                   "ROSEN KONFETI YUMMIS SO VKUSOM FRUKTOV KQ" ~ "ROSHEN KONFETI YUMMIS FRUKTOV KQ",
                                   "ROSHEN KONFETI YUMMIS SO VKUSOM FRUKTOV KQ" ~ "ROSHEN KONFETI YUMMIS FRUKTOV KQ",
                                   "KOTEKS ULTRA NOCHNIE"                   ~ "KOTEX ULTRA NOCHNIE",
                                   "PROKLADKI KOTEKS ULTRA NOCHNIE"         ~ "KOTEX ULTRA NOCHNIE",
                                   "MAYONEZ MAXEEV 210 Q ZEYTUN P"          ~ "MAYONEZ MAXEEV ZEYTUN 210 Q",
                                  
                                   "1KQ"                                    ~ "1 KQ",
                                   "2KQ"                                    ~ "2 KQ",
                                   "500GR"                                  ~ "500 Q",
                                   "250GR"                                  ~ "250 Q",
                                   "250GR16"                                ~ "250 Q",
                                   "ML"                                     ~ "ML",
                                   "SUSE"                                   ~ "SHUSHE",
                                   .default = product_name)) %>%
  mutate(product_name = str_squish(product_name))

## w herfi ####

library(dplyr)
library(stringr)

fp_final = fp_final %>%
  mutate(product_name = product_name %>%
           str_to_upper() %>%
           str_replace_all("\\b(WINCESTER|WINCHESTR|WINCESHTER|WINCH)\\b", "WINCHESTER") %>%
           str_replace_all("\\b(WINSTONE|WINSTONN)\\b", "WINSTON") %>%
           str_replace_all("\\b(WEST GOLD|WESTGOLDD)\\b", "WESTGOLD") %>%
           str_replace_all("\\b(YASKINO|YAWKINO|YASKINA)\\b", "YASHKINO") %>%
           str_replace_all("\\b(SAVUSKIN|SAVUSKINN)\\b", "SAVUSHKIN") %>%
           str_replace_all("\\b(WHISKASS|WISKAS)\\b", "WHISKAS") %>%
           str_replace_all("\\b(SIGARET|SIQARETİ)\\b", "SIQARET") %>%
           str_replace_all("\\b(YAGI|YA)\\b", "YAG") %>%
           str_replace_all("\\b(CHOREK|CHOREYI|COREYI)\\b", "COREK") %>%
           str_replace_all("\\b(SUSE|SHUSHE)\\b", "SHUSHE") %>%
           str_replace_all("(\\d+)(Q|KQ|ML|L|ED|GR|MQ)\\b", "\\1 \\2") %>%
        
           str_replace_all("  +", " ") %>%
           str_trim()) %>%

  mutate(product_name = case_match(product_name,
                                
                                   "WINCHESTER KSSS SILVER"                 ~ "WINCHESTER KSSS SILVER",
                                   "WINCHESTER KSS SILVER SIQARET NAZIK"    ~ "WINCHESTER KSS SILVER SIQARET",
                                   "WINCHESTER COMPACT SILVER SIQARET"      ~ "WINCHESTER COMPACT SILVER",
                                   "WINCHESTER COMPACT BLUE SIQARET"        ~ "WINCHESTER COMPACT BLUE",
                        
                                   "WINSTON XS STYLE SILVER"                ~ "WINSTON XS STYLE SILVER",
                                   "WINSTON XSTYLE COMPACT SILVER SIQARET"  ~ "WINSTON XSTYLE COMPACT SILVER",
                                   "WINSTON XSTYLE SILVER 7702"             ~ "WINSTON XSTYLE SILVER",
                       
                                   "WESTGOLD YAG 1 KQ"                      ~ "WESTGOLD KERE YAG 1 KQ",
                                   "WESTGOLD YAG 1000 Q"                    ~ "WESTGOLD KERE YAG 1 KQ",
                                   "WESTGOLD KERE YAG 82 8 400 Q"           ~ "WESTGOLD KERE YAG 82.8% 400 Q",
                                
                                   "WHISKAS PISIK YEMI KIT LAMB CIG 28X75 Q" ~ "WHISKAS PISIK YEMI KIT LAMB 75 Q",
                                   "WHISKAS PISIK YEMI JELE KUR INDEYK 28X75 Q" ~ "WHISKAS PISIK YEMI JELE 75 Q",
                           
                                   "XLORGEKSIDIN"                           ~ "XLORQEKSIDIN",
                                   "XLORQEKSIDINA"                          ~ "XLORQEKSIDIN",
                                   "YOD 5F 10 ML"                           ~ "YOD %5 10 ML",
                                   "WR ESS T SOS L 400 Q"                   ~ "WAITROSE ESSENTIAL TOMAT SOUSU 400 Q",
                                   "WR KREM QAYMA 150 Q"                    ~ "WAITROSE KREM QAYMAQ 150 Q",
                                   
                                   .default = product_name)) %>%

  mutate(product_name = str_squish(product_name))

## v herfi ####

library(dplyr)
library(stringr)

fp_final = fp_final %>%
  mutate(product_name = case_match(product_name,
                                  
                                   "VITA 1000 MULTIVITAMIN SIRESI 1 L T P" ~ "VITA 1000 MULTIVITAMIN SIRESI 1 L",
                                   "VITA 1000 SAFTALI SIRESI 1 L T P"      ~ "VITA 1000 SAFTALI SIRESI 1 L",
                                   "VITA 1000 ALBALI SIRESI 1 L T P"       ~ "VITA 1000 ALBALI SIRESI 1 L",
                                 
                                   "VICEROY N SERIES SILVER"               ~ "VICEROY N SERIES SILVER",
                                   "VICEROY N SERIES SI"                   ~ "VICEROY N SERIES SILVER",
                                   "VICEROY N SERIES BLUE SIQARET"         ~ "VICEROY N SERIES BLUE",
                                   "VICEROY N SERIES SIL"                  ~ "VICEROY N SERIES SILVER",
                                 
                                   "VAFLI ETI HOSHBESH 40 Q KAKAOLU"        ~ "ETI HOSHBESH KAKAOLU VAFLI 40 Q",
                                   "VAFLI SLADOK 200 Q KAKAO FINDIQ"        ~ "VAFLI SLADOK KAKAO FINDIQ 200 Q",
                               
                                   "VAZELEN KOSMETICHESKIY"                ~ "VAZELIN KOSMETIK",
                                   "VAZELIN KOSMETICHESKIY"                ~ "VAZELIN KOSMETIK",
                                   "XLORGEKSIDIN"                          ~ "XLORQEKSIDIN",
                                   "YOD 5F 10 ML"                          ~ "YOD %5 10 ML",
                                 
                                   "WINCHESTER KSSS SILVER"                ~ "WINCHESTER KSSS SILVER",
                                   "WINCHESTER KSS SILVER SIQARET NAZIK"   ~ "WINCHESTER KSSS SILVER",
                                   "WINCHESTER COMPACT SILVER SIQARET"     ~ "WINCHESTER COMPACT SILVER",
                              
                                   "1KQ"                                   ~ "1 KQ",
                                   "2KQ"                                   ~ "2 KQ",
                                   "500GR"                                 ~ "500 Q",
                                   "250GR16"                               ~ "250 Q",
                                   
                                   .default = product_name)) %>%
  mutate(product_name = str_squish(product_name))

## u herfi ####

library(dplyr)
library(stringr)

fp_final = fp_final %>%
  mutate(product_name = str_squish(toupper(product_name))) %>%
  mutate(product_name = case_match(product_name,
                  
                                   "UL ALBENI 31 Q"                         ~ "ULKER ALBENI 31 Q",
                                   "ULKER ALBENI 31Q"                       ~ "ULKER ALBENI 31 Q",
                                   "ULKER ALBENI CIKOLATA 31 Q"             ~ "ULKER ALBENI 31 Q",
                                   "ULKER ALBENI CIKOLATA 52 Q"             ~ "ULKER ALBENI 52 Q",
                                   "ULKER BISKREM KAKAOLU 100Q18"           ~ "ULKER BISKREM KAKAOLU 100 Q",
                                   "ULKER BISKREM 100 Q ED"                 ~ "ULKER BISKREM 100 Q",
                                   "ULKER HALLEY SHOK BISKIVIT 240 Q"       ~ "ULKER HALLEY 240 Q",
                                   "ULKER HALLEY 300 Q ED"                  ~ "ULKER HALLEY 300 Q",
                                   "ULKER CIZI KRAKER SADE 70 Q"            ~ "ULKER CIZI 70 Q",
                                   "UL CIZI KREKER 70 Q"                    ~ "ULKER CIZI 70 Q",
                                   "ULKER 9KAT TAT INCE INCE IN CVZ 114 Q"  ~ "ULKER 9 KAT TAT INCE 114 Q",
                                   "ULKER POTIBOR 175Q164"                  ~ "ULKER POTIBOR 175 Q",
                            
                                   "UN DOYMAK 1 KQ"                         ~ "DOYMAK UN 1 KQ",
                                   "UN DOYMAK 2 KQ"                         ~ "DOYMAK UN 2 KQ",
                                   "UN DOYMAK 5 KQ"                         ~ "DOYMAK UN 5 KQ",
                                   "UN KARMEN 50KQ"                         ~ "KARMEN UN 50 KQ",
                                   "UN KARMEN 1 KQ 0086"                    ~ "KARMEN UN 1 KQ",
                                   "UN MAKFA 1 KQ 2572"                     ~ "MAKFA UN 1 KQ",
                                   "UN MAKFA 2 KQ 2565"                     ~ "MAKFA UN 2 KQ",
                                   "UN MEHRIBAN KQ 11475"                   ~ "MEHRIBAN UN KQ",
                                  
                                   "USASTIY NYANA USAQ YUYUCU TOZ 400 Q"    ~ "USHASTIY NYAN USAQ YUYUCU TOZ 400 Q",
                                   "USHASTIY NYAN POROSHOK 400 Q ED"        ~ "USHASTIY NYAN USAQ YUYUCU TOZ 400 Q",
                                   "USASTIY NYAN SAMPUN 200 ML"             ~ "USHASTIY NYAN SHAMPUN 200 ML",
                                   "USHASTIY NYAN SAMPUN USAQ 200 ML"       ~ "USHASTIY NYAN SHAMPUN 200 ML",
                                   "USASTIY NYAN NEM SALFETKA 120 ED"       ~ "USHASTIY NYAN NEM SALFET 120 ED",
                             
                                   "ULUDAG EFS PORT GAZ PET1 L"             ~ "ULUDAG EFSANE PORTAQAL GAZOZ 1 L",
                                   "ULUDAG EFSANE PORTAGAL GAZOZ 250 ML SUSE" ~ "ULUDAG EFSANE PORTAQAL GAZOZ 250 ML SHUSHE",
                                   "ULUDAG FRUTTI C MAX LIMON 0 2 L"        ~ "ULUDAG FRUTTI C MAX LIMON 200 ML",
                                
                                   "UZUN OMUR IVANOVKA XAMA 25 3KQ"         ~ "UZUN OMUR IVANOVKA XAMA 25% 3 KQ",
                                   "UZ OM IVANOVKA SHOR 400 Q"              ~ "UZUN OMUR IVANOVKA SHOR 400 Q",
                                   "UZUN OMUR CAMIS QATIQI 1400 Q"          ~ "UZUN OMUR CAMISH QATIGI 1400 Q",
                    
                                   "UQOL AKTIVIROVANNIY 250 MQ 10 TAB"      ~ "UQOL AKTIVIROVANNIY 250 MQ N10",
                                   "UKSUSNAYA KISLOTA 180 ML 70"            ~ "UKSUSNAYA KISLOTA 70% 180 ML",
                                   "UTYONOK TUALET UCUN 500 ML DENIZ TERAVET" ~ "UTYONOK TUALET TEMIZLEYICI 500 ML",
                                   
                                   .default = product_name
  ))

## t herfi ####

fp_final = fp_final %>%
  mutate(product_name = str_squish(toupper(product_name))) %>%
  mutate(product_name = case_match(product_name,
                            
                                   "TENDIR COREYI ED"                       ~ "TENDIR COREYI",
                                   "TENDIR COREK GENCE"                     ~ "GENCE TENDIR COREYI",
                                   "TENDIR COREYI ORTA"                     ~ "TENDIR COREYI ORTA",
                                   "TENDIR COREYI BOYUK KUNCUTLU"           ~ "TENDIR COREYI BOYUK KUNCUTLU",
                                   "TENDIR COREYI KEPEKLI ED"               ~ "TENDIR COREYI KEPEKLI",
                                   "TENDIR COREYI QARA"                     ~ "TENDIR COREYI QARA",
                                
                                   "TWIX SOKOLAD 55Q"                       ~ "TWIX 55 Q",
                                   "TWIX 55 Q"                              ~ "TWIX 55 Q",
                                   "TWIX CIKOLATA BAR 55 Q"                 ~ "TWIX 55 Q",
                                   "TWIX EXTRA 82 Q"                        ~ "TWIX EXTRA 82 Q",
                                   "TWIX XTRA 82 Q"                         ~ "TWIX EXTRA 82 Q",
                                   "TWIX MINIS SHOK KQ"                     ~ "TWIX MINIS KQ",
                                   "TWIX SALTED CARAMEL BAR 55 Q"           ~ "TWIX SALTED CARAMEL 55 Q",
                                 
                                   "TESS PLEASURE MEYVELI QARA CAY DEMLEME 100 Q" ~ "TESS PLEASURE TEA 100 Q",
                                   "TESS PLEASURE QARA CAY 400 Q"           ~ "TESS PLEASURE TEA 400 Q",
                                   "TESS SUNRISE QARA CAY 300 Q"            ~ "TESS SUNRISE TEA 300 Q",
                                   "TESS EARL GREY QARA CAY 100 Q"          ~ "TESS EARL GREY TEA 100 Q",
                                   "TESS FLIRT MEYVELI YASIL CAY PAKET 25 EDEDLI" ~ "TESS FLIRT GREEN TEA N25",
                                
                                   "TIDE YUYUCU TOZ AVTOMAT COLOR 400 Q"    ~ "TIDE COLOR AVTOMAT 400 Q",
                                   "TIDE YUYUCU TOZ LS RENGLI 400 Q"        ~ "TIDE COLOR 400 Q",
                                   "TIDE YUYUCU TOZ LIMON LS 400 Q"         ~ "TIDE LIMON 400 Q",
                                   "TIDE AVTOMAT RENGLI YUYUCU TOZ 5 KQ"    ~ "TIDE COLOR AVTOMAT 5 KQ",
                              
                                   "TURK CHEF DOYPAK PROVANSAL MAYONEZ 56 LI 400 ML" ~ "TURK CHEF MAYONEZ PROVANSAL 400 ML",
                                   "TURK CHEF DOYPAK KETCUP CILI 200 ML"    ~ "TURK CHEF KETCHUP CHILI 200 ML",
                                   "TURK CHEF DOYPAK KETCUP KABABLI 200 ML" ~ "TURK CHEF KETCHUP KABABLI 200 ML",
                                   "TURK CHEF YASIL NOXUD SUSE 720 ML"      ~ "TURK CHEF YASHIL NOXUD 720 ML",
                                   "TURK CHEF QARGIDALI DENELERI TENEKE 425 ML" ~ "TURK CHEF QARGIDALI 425 ML",
                                
                                   "TOYBOX OYUNCAQLI SAQQIZ 5 Q"             ~ "TOYBOX OYUNCAQLI SAQQIZ 5 Q",
                                   "TOYBOX OYUNCAQLI YUMURTA 22 Q"          ~ "TOYBOX OYUNCAQLI YUMURTA 22 Q",
                                   "TOYBOX JELLY AYICIK 40 Q"               ~ "TOYBOX JELLY AYICIK 40 Q",
                                   "TOYBOX METRE SAQQIZ 35 Q"               ~ "TOYBOX METRE SAQQIZ 35 Q",
                               
                                   "TERAFLYU LIMON 325 MQ N 10 SASHE NOVARTIS PHARMA" ~ "TERAFLU LIMON 325 MG N10",
                                   "TERAFLU N10 LIMON (ED)"                 ~ "TERAFLU LIMON N10",
                                   "TAYLOL XOT LIMON N 12 DIGERLER NOBEL FARMA" ~ "TYLOL HOT LIMON N12",
                                   "TETRACHIKLIN 100 MQ N 20 HEBLER BIOSINTEZ" ~ "TETRACIKLIN 100 MG N20",
                                   "TEMPALQIN N 10 HEBLER SOPHARMA"          ~ "TEMPALGIN N10",
                                   "TROKSEVAZIN 2 40 Q GEL ACTAVIS"         ~ "TROKSEVAZIN GEL 40 Q",
                             
                                   "TAC BUGDA UNU KQ"                       ~ "TAC BUGDA UNU KQ",
                                   "TAC SEKER TOZU 700 Q"                   ~ "TAC SEKER TOZU 700 Q",
                                   "TAC QARABASAQ 700 Q"                    ~ "TAC QARABASAQ 700 Q",
                                   "TAC YUMRU DUYU 700 Q"                   ~ "TAC YUMRU DUYU 700 Q",
                                   "TAC BASMATI DUYU KQ"                    ~ "TAC BASMATI DUYU KQ",
                               
                                   "TOMAT FINAL 720 Q"                      ~ "FINAL TOMAT PASTASI 720 Q",
                                   "TOMAT FINAL 275 Q 9244"                 ~ "FINAL TOMAT PASTASI 275 Q",
                                   "TUTKU KAKAO 6X36X40 Q ETI"               ~ "ETI TUTKU KAKAO 40 Q",
                                   "TUC KREKER PAPRIKA 100 Q"               ~ "TUC KREKER PAPRIKA 100 Q",
                                   "TIC TAC STRAWBERYY FIELDS 16 Q"         ~ "TIC TAC STRAWBERRY 16 Q",
                                   "TORABIKA CAPPUCINO2"                    ~ "TORABIKA CAPPUCCINO 25 Q",
                                   "TAMASA MIAD DUYU KQ"                    ~ "TAMASHA MIAD DUYU KQ",
                             
                                   "TEST DLYA OPRED BEREM S PIPETKOY..."    ~ "TEST HAMILELIK (PIPETKALI)",
                                   "TEST LADY MOON (PIPETKA) 4 MM N1"       ~ "TEST HAMILELIK LADY MOON N1",
                               
                                   "TUALET KAGIZI ED"                       ~ "TUALET KAGIZI",
                                   "TIBBI MASKA SENSE 3 QAT"                ~ "TIBBI MASKA 3 QAT",
                                   "TIKINTI MATERIALARI"                    ~ "TIKINTI MATERIALLARI",
                                   "TELEFON VE TABLET"                      ~ "TELEFON VE TABLET",
                                   "TEMIZLIK AVADANLIGI"                    ~ "TEMIZLIK AVADANLIGI",
                                   
                                   .default = product_name))

## 0 herfi ####

fp_final = fp_final %>%
  mutate(product_name = str_squish(toupper(product_name))) %>%
  mutate(product_name = case_match(product_name,
                                
                                   "OZMO OGOPOGO CIK KAK SUT KEK 4 24 30 Q AMADA" ~ "OZMO OGOPOGO 30 Q",
                                   "OZMO OGOPOGO CILEKLI KEK 30 Q"                ~ "OZMO OGOPOGO CIYELEKLI 30 Q",
                                   "OZMO EGG FACES SOLEN 20 Q"                    ~ "OZMO EGG 20 Q",
                                   "OZMO HAPPY AMADA SOLEN 44 Q"                  ~ "OZMO HAPPY 44 Q",
                                   "OZMO FUN 4X24X23 Q SOLEN"                     ~ "OZMO FUN 23 Q",
                                   "OZMO POPSY SUTLU CIKOLATA DRAJE 27 Q"         ~ "OZMO POPSY 27 Q",
                                   "OZMO CORNET RENGLI SOLEN 25 Q"                ~ "OZMO CORNET 25 Q",
                                 
                                   "ORBIT STRAWBERRY BANAN 444114 45371"          ~ "ORBIT CIYELEK BANAN 13.6 Q",
                                   "ORBIT WHITE NEJNAYA MYATA 444107 39598"       ~ "ORBIT NEJNAYA MYATA 13.6 Q",
                                   "ORBIT SAQQIZ QARPIZ 14 Q"                     ~ "ORBIT WATERMELON 14 Q",
                                   "ORBIT SPEARMINT 444324 1596"                  ~ "ORBIT SPEARMINT 13.6 Q",
                                   "ORBIT EUCALYPTUS 444345 36026"                ~ "ORBIT EUCALYPTUS 13.6 Q",
                                   "ORBIT WHITE XXL BUBLE 20 4 Q"                 ~ "ORBIT XXL BUBBLEMINT 20.4 Q",
                            
                                   "OREO KAKAO VANIL KREMALI PEC 95 Q"            ~ "OREO KAKAO VANIL 95 Q",
                                   "OREO KAKAO VANIL KREMALI PEC 228 Q"           ~ "OREO KAKAO VANIL 228 Q",
                                   "OREO SUDLU PECENYE 38 Q"                      ~ "OREO 38 Q",
                                   "OREO PEC 1+2CI 50 ENDIRIM 95 Q"               ~ "OREO 95 Q (1+1)",
                                   "OREO PIROJNOYE 30 Q (1X24)"                   ~ "OREO FRESH MILK SNACK 30 Q",
                    
                                   "OVCULAR KULLEME SOSISKA KQ"                   ~ "OVCULAR KULLEME SOSISKA KQ",
                                   "OVCULAR SEDEF SERVELAT KQ"                    ~ "OVCULAR SEDEF SERVELAT KQ",
                                   "OVCULAR KRAKOV 350 400 Q"                     ~ "OVCULAR KRAKOV KQ",
                                   "OVQAT SOBA SOSISKA VAKUM KQ"                  ~ "OVQAT SOBA SOSISKA KQ",
                                   "OVCU SEDEF DOKTUR KOLBASA KQ"                 ~ "OVCULAR SEDEF DOKTORSKAYA KQ",
                            
                                   "OMAN UN 1 KQ"                                 ~ "OMAN UN 1 KQ",
                                   "OMAN UN 2 KQ"                                 ~ "OMAN UN 2 KQ",
                                   "OMAN BUKME QEND KQ"                           ~ "OMAN BUKME QEND KQ",
                                   "OMAN KELLE QEND 5 KQ"                         ~ "OMAN KELLE QEND 5 KQ",
                            
                                   "OLD SPICE DEO KISI S"                         ~ "OLD SPICE DEODORANT 150 ML",
                                   "OLD SPICE DEO STICK"                          ~ "OLD SPICE DEO STICK 50 ML",
                                   "OLD SPICE DUS GELI CAPTAIN 400 ML"            ~ "OLD SPICE DUS GELI 400 ML",
                             
                                   "OLITALIA QAR YAG 1 L"                         ~ "OLITALIA QARGIDALI YAGI 1 L",
                                   "OLITALIA DI OLIVA ZEYTUN YAGI 1 L"            ~ "OLITALIA ZEYTUN YAGI 1 L",
                                   "OLITALIA POMACE ZEYT YAGI 1 L"                ~ "OLITALIA ZEYTUN YAGI POMACE 1 L",
                               
                                   "OMEPRAZOL 20 MQ N 30 KAPSULLAR BORISOVSKIJ"   ~ "OMEPRAZOL 20 MG N30",
                                   "OMEZ (AL OMEZ) 20 MQ 10 (KAPSULI) (VIVIMED)"   ~ "OMEZ 20 MG N10",
                                   "OTRIVIN MENTOL 0 1 10 ML SPREY"               ~ "OTRIVIN MENTOL SPREY 10 ML",
                                   "OTRIVIN 0 05 10 ML DAMCI NOVARTIS PHARMA"     ~ "OTRIVIN DAMCI 0.05% 10 ML",
                                   "OTIPAKS 16 Q 15 ML QULAQ DAMCISI BIOCODEX"    ~ "OTIPAKS QULAQ DAMCISI 15 ML",
                             
                                   "OLA BEZ CLASSIC NORMAL 10 ED ROMASHKA"        ~ "OLA GUNDELIK BEZ N10",
                                   "OLA DISKLER 120 ED"                           ~ "OLA PAMBIQ DISK 120 ED",
                                   "OLA SILK PAMBIQ DISK"                         ~ "OLA PAMBIQ DISK 120 ED",
                                   "OVA TUALET KAGIZI 3 QAT 4 EDEDLI"              ~ "OVA TUALET KAGIZI N4",
                                   "OKEY TUZ RUHU 600 ML"                         ~ "OKEY TUZ RUHU 600 ML",
                               
                                   "ORION CHOCO PIE 12 EDEDLI 360 Q"              ~ "ORION CHOCO PIE 360 Q",
                                   "ORION CHOCO PIE 6 EDEDLI 180 Q"               ~ "ORION CHOCO PIE 180 Q",
                                   "ORION CHOCO BOY 30 50 Q"                      ~ "ORION CHOCO BOY 50 Q",
                                   
                                   "ORTA FRENC FRAYZ"                             ~ "FRENCH FRIES ORTA",
                                   "OPTIMAL SIRKE TURSUSU 70 UKSUS 150 ML"        ~ "OPTIMAL SIRKE 70% 150 ML",
                                   "OGUZ ZAVOD COREYI 400 Q"                      ~ "OGUZ COREYI 400 Q",
                                   "ONCU QARA ZEYTUN KQ"                          ~ "ONCU QARA ZEYTUN KQ",
                                     .default = product_name))

## r herfi ####

fp_final = fp_final %>%
  mutate(product_name = str_squish(toupper(product_name))) %>%
  mutate(product_name = case_match(product_name,
                           
                                   "ROTHMANS SILVER YENI SIQARET"           ~ "ROTHMANS SILVER",
                                   "ROTHMANS SILVER YENI"                  ~ "ROTHMANS SILVER",
                                   "ROTHMANS BLUE YENI SIQARET"             ~ "ROTHMANS BLUE",
                                   "ROTHMANS BLUE YENI"                    ~ "ROTHMANS BLUE",
                                   "ROTHMANS CLICK AMBER SIQARET"           ~ "ROTHMANS CLICK AMBER",
                                   "ROTHMANS DEMI SILVER SIQARET"           ~ "ROTHMANS DEMI SILVER",
                                   "ROTHMANS DEMI CLICK SENSATION"         ~ "ROTHMANS DEMI CLICK",
                          
                                   "ROSHEN MOLOCNAYA KAPLYA KQ"             ~ "ROSHEN MILKY SPLASH KQ",
                                   "ROSHEN STAKAN MOLOKO KONFET KQ"         ~ "ROSHEN MILKY SPLASH KQ",
                                   "ROSHEN BESHENAYA PCELKA KONFET KQ"      ~ "ROSHEN BESHENAYA PCELKA KQ",
                                   "ROSHEN BESEN PCYOLKA FRUTTI KQ"         ~ "ROSHEN BESHENAYA PCELKA KQ",
                                   "ROSHEN LOLLI POPS KOKTEYL KAR KQ"       ~ "ROSHEN LOLLI POPS KQ",
                                   "ROSHEN MINKY BINKY ASORT KAR KQ"        ~ "ROSHEN MINKY BINKY KQ",
                                   "ROSHEN JOHNNY KROCKER SOKOLADLI KQ"     ~ "ROSHEN JOHNNY KROCKER KQ",
                                   "ROSHEN ESMERALDA LOVITA BISK CHOCO 150 Q" ~ "ROSHEN LOVITA 150 Q",
                                   "ROSHEN PORISTIY SUDLU SOKOLAD 80 Q"     ~ "ROSHEN SOKOLAD PORISTIY 80 Q",
                             
                                   "ROLLTON VERMISEL TOYUQLU 60 Q"          ~ "ROLLTON VERMISEL TOYUQ 60 Q",
                                   "ROLLTON BULYON VERMISEL TOYUQ 60 Q"     ~ "ROLLTON VERMISEL TOYUQ 60 Q",
                                   "ROLLTON VEMESIL MAL ETLI 60 Q"          ~ "ROLLTON VERMISEL MAL 60 Q",
                                   "ROLLTON LAPSA PO DOMASHNEMU TOYUQ 85 Q" ~ "ROLLTON LAPSHA 85 Q",
                                   "ROLLTON KARTOF PURESI TOYUQLU 40 Q"      ~ "ROLLTON KARTOF PURE 40 Q",
                                   "ROLLTON MAKARON ROJKI 400 Q"            ~ "ROLLTON MAKARON 400 Q",
                                
                                   "RED BULL ENER0 25 L"                    ~ "RED BULL 250 ML",
                                   "RED BULL ENERGY DRINK 250 ML"           ~ "RED BULL 250 ML",
                                   "RED BULL EN 0 355 L"                    ~ "RED BULL 355 ML",
                                   "RED BULL ENERGY DRINK 355 ML"           ~ "RED BULL 355 ML",
                                   "RED BULL SUMMER EDITION TROPIK 250 ML"  ~ "RED BULL SUMMER EDITION 250 ML",
                              
                                   "RICHMOND QARABASAQ 800 Q"               ~ "RICHMOND QARABASAQ 800 Q",
                                   "RICHMOND QARABASHAQ 800 Q"              ~ "RICHMOND QARABASAQ 800 Q",
                                   "RICHMOND SEKER TOZU 800 Q"              ~ "RICHMOND SEKER TOZU 800 Q",
                                   "RICHMOND TOZ SEKER 0 8 KQ"              ~ "RICHMOND SEKER TOZU 800 Q",
                                   "RICHMOND YUMRU DUYU 800 Q"              ~ "RICHMOND DUYU YUMRU 800 Q",
                                   "RICHMOND UZUN DUYU 800 Q"               ~ "RICHMOND DUYU UZUN 800 Q",
                           
                                   "RASTISK YGRT CYLK70"                    ~ "RASTISHKA YOGURT CIYELEK 70 Q",
                                   "RASTISKA KESMIK CIYELEK ERIK 6X45 Q"     ~ "RASTISHKA KESMIK 6X45 Q",
                                   "RASTISHKA MILKSEYK SOKOLAD BANAN 210 Q" ~ "RASTISHKA MILKSEYK 210 Q",
                                   "RASTISKA ICMELI YER CIYELEYI 90 Q"      ~ "RASTISHKA YOGURT ICMELI 90 Q",
                                
                                   "RIBOKSIN 2 10 ML N 10 AMPULA GALICHFARM" ~ "RIBOKSIN 10 ML N10",
                                   "RIBOKSIN 2% 10X10 (ED)"                 ~ "RIBOKSIN 10 ML N10",
                                   "RINQER 400 ML STEKLO MEHLUL JSC INFUZIA" ~ "RINGER 400 ML",
                                   "RINGER 500 ML (POLIFLEKS)"              ~ "RINGER 500 ML",
                                   "RELIF QEMORROIDALNIE N 12 SHAM"         ~ "RELIF N12",
                                   "REQIDRON BIO 6 4 Q N 5 SASHE ORION"     ~ "REQIDRON BIO N5",
                                   "RENNI APELSIN N 24 JEVAT TAB BAYER"      ~ "RENNI APELSIN N24",
                       
                                   "REXONA DEO 150 ML INVISIBLE DIAMOND"    ~ "REXONA DEODORANT 150 ML",
                                   "REXONA ANTIPERSPIRANT 150 ML MEN GORUNMEZ" ~ "REXONA MEN DEODORANT 150 ML",
                                   "REXONA DEO ANTIPERSPIRANT KISI 150 ML"   ~ "REXONA MEN DEODORANT 150 ML",
                                   "REXONA ROLL ON KOTTON QADIN 50 ML"      ~ "REXONA ROLL-ON 50 ML",
                                  
                                   "ROMASKA XAMA 25 350 Q"                  ~ "ROMASHKA XAMA 25% 350 Q",
                                   "ROMASHKA SMETANA 25 200 Q"              ~ "ROMASHKA XAMA 25% 200 Q",
                                   "REVEL TUALET KAGIZI 4 LU 2 QAT"         ~ "REVEL TUALET KAGIZI N4",
                                   "RUFFLES PEYNIR SOGAN 104 Q"             ~ "RUFFLES PEYNIR SOGAN 104 Q",
                                   "RUSSKIY BISKVIT RULET KLUBNICNIY 150 Q" ~ "RUSSKIY BISKVIT RULET 150 Q",
                                   "ROSSIYSKIY KAKAO 100 Q"                 ~ "ROSSIYSKIY KAKAO 100 Q",
                                   "RYABA MAYONEZ ZEYTUNLU 67 390 Q"        ~ "RYABA MAYONEZ 390 Q",
                                   
                                   .default = product_name))

## q herfi ####

library(dplyr)
library(stringr)

fp_final = fp_final %>%
  mutate(product_name = str_squish(toupper(product_name))) %>% 
  mutate(product_name = str_replace_all(product_name, "(\\d+)(KQ|Q|ML|L|MQ|MG)", "\\1 \\2")) %>%
  mutate(product_name = case_match(product_name,
                             
                                   "QIZIL QUYU LIMONAD ARMUD 1.5 L PET"     ~ "QIZIL QUYU LIMONAD ARMUD 1.5 L",
                                   "QIZIL QUYU TERXUN 1.5 L PET"            ~ "QIZIL QUYU LIMONAD TERXUN 1.5 L",
                                   "QIZIL QUYU QIZIL GUL 1.5 L PET"         ~ "QIZIL QUYU LIMONAD QIZIL GUL 1.5 L",
                                   "QIZIL QUYU ARMUD LIMONAD 1 L"           ~ "QIZIL QUYU LIMONAD ARMUD 1 L",
                                   "QIZIL QUYU TERXUN LIMONAD 1 L"          ~ "QIZIL QUYU LIMONAD TERXUN 1 L",
                                   "QIZIL QUYU UZUM LIMONAD 1 L"            ~ "QIZIL QUYU LIMONAD UZUM 1 L",
                                   "QIZIL QUYU FEYXOA 1.5 L PET"            ~ "QIZIL QUYU LIMONAD FEYXOA 1.5 L",
                                   "QIZIL QUYU MOHITO 250 ML"               ~ "QIZIL QUYU MOXITO 250 ML",
                           
                                   "QARABASAQ 25 KQ"                        ~ "QARABASHAQ 25 KQ",
                                   "QRECKA KQ"                              ~ "QRECHKA KQ",
                                   "QRECHKA KQ"                             ~ "QRECHKA KQ",
                                   "QIRMIZI MERCI KQ"                       ~ "MERCIMEK QIRMIZI KQ",
                                   "QIRMIZI MERCI CEKI"                     ~ "MERCIMEK QIRMIZI CEKI",
                                   "QEND AZERSHEKER KELLE 800 Q"            ~ "AZERSHEKER QEND KELLE 800 Q",
                                   "QEND AZERSHEKER KELLE 600 Q"            ~ "AZERSHEKER QEND KELLE 600 Q",
                                   "QEND DISHLEME 500 Q"                    ~ "QEND DISHLEME 500 Q",
                             
                                   "QATIQ AZERSUD 3.2 1 KQ KEND"            ~ "AZERSUD KEND QATIGI 3.2% 1 KQ",
                                   "QAYMAQ AZERSUD 200 Q T PAK"             ~ "AZERSUD QAYMAQ 200 Q",
                                   "QATIQ IVANOVKA KQ"                      ~ "IVANOVKA QATIQ KQ",
                                   "QAYMAQ IVANOVKA KQ"                     ~ "IVANOVKA QAYMAQ KQ",
                                   "QAYMAQ SUTASH 200 ML"                   ~ "SUTAS QAYMAQ 200 ML",
                                
                                   "Q B BULYON KUR 10 Q"                    ~ "GALLINA BLANCA BULYON TOYUQ 10 Q",
                                   "Q B BULYON MAL 10 Q"                    ~ "GALLINA BLANCA BULYON MAL ETI 10 Q",
                                   "QALINA BLANKA TOYUQ 100 Q"              ~ "GALLINA BLANCA TOYUQ 100 Q",
                                   "QALINA BLANKA MAL 100 Q"                ~ "GALLINA BLANCA MAL ETI 100 Q",
                                 
                                   "QEPARIN 25 Q MELHEM NIJPHARM"           ~ "HEPARIN MELHEM 25 Q",
                                   "QUTTALAKS 15 ML DAMCI"                  ~ "GUTTALAX DAMCI 15 ML",
                                   "QENFERON 500 000 ED 10"                 ~ "GENFERON 500,000 ED N10",
                                   "QENOPRIL 10 MQ N 28"                    ~ "GENOPRIL 10 MG N28",
                                   "QIALUROFT 5 ML GOZ DAMCISI"             ~ "HIALUROFT GOZ DAMCISI 5 ML",
                                 
                                   "Q S FAIRY 450 ML SOCNIY LIMON"          ~ "FAIRY QAB YUYUCU LIMON 450 ML",
                                   "Q S FAIRY 450 ML YASHIL ALMA"           ~ "FAIRY QAB YUYUCU YASHIL ALMA 450 ML",
                                   "QAB YUYUCU TOZU RAMIZ 500 Q"            ~ "RAMIZ QAB YUYUCU TOZ 500 Q",
                                
                                   "Q E TOYUQ DOS SUMUKSUZ KQ"              ~ "TOYUQ DOSH SUMUKSUZ KQ",
                                   "Q E DANA SUMUKSUZ KQ"                   ~ "DANA SUMUKSUZ KQ",
                                   "QUBA TOYUGU KQ"                         ~ "QUBA TOYUQ KQ",
                                   "QEDIM DAD SOSISKA KQ"                   ~ "QEDIM DAD SOSISKA KQ",
                                  
                                   "QADIN BEZI MOLPED PURE SOFT NORMAL"     ~ "MOLPED QADIN BEZI NORMAL",
                                   "QULAQ COPU 200 LU NILUSI"               ~ "NILUSI QULAQ COPU N200",
                                 
                                   "QUBERNATOR ARAQ 500 ML"                 ~ "QUBERNATOR ARAQ 500 ML",
                                   "QLEYS SOKOLAD KQ"                       ~ "LAYS SOKOLAD KQ",
                                   "QELEM I 6095 AROMA"                     ~ "QELEM AROMA I-6095",
                                   
                                   .default = product_name))

## p herfi ####

fp_final = fp_final %>%
  mutate(product_name = str_squish(toupper(product_name))) %>%
  mutate(product_name = case_match(product_name,
                               
                                   "PALL MALL NANO SILVER SIQARET"          ~ "PALL MALL NANO SILVER",
                                   "PALL MALL NANO WHITE SIQARET"           ~ "PALL MALL NANO WHITE",
                                   "PALL MALL AM NIGHTS QARAGAT"            ~ "PALL MALL AMERICAN NIGHTS",
                                   "PALL MALL MIAMI SUNSET"                 ~ "PALL MALL MIAMI SUNSET",
                                   "PALL MALL XS BLUE"                      ~ "PALL MALL XS BLUE",
                             
                                   "PEPSI COLA 1 L"                         ~ "PEPSI 1 L",
                                   "PEPSI COLA 2 L"                         ~ "PEPSI 2 L",
                                   "PEPSI COLA 0 25 L"                      ~ "PEPSI 250 ML",
                                   "PEPSI COLA 500 ML"                      ~ "PEPSI 500 ML",
                                   "PEPSI TWIST 2 L"                        ~ "PEPSI TWIST 2 L",
                                   "PEPSI MAX YENI 1 L"                     ~ "PEPSI MAX 1 L",
                                  
                                   "PAKMAYA QURU XEMIR MAYASI 60 Q"         ~ "PAKMAYA MAYA 60 Q",
                                   "PAKMAYA KURU HAMUR MAYASI 80 Q"         ~ "PAKMAYA MAYA 80 Q",
                                   "PAKMAYA MAYA 80 Q"                      ~ "PAKMAYA MAYA 80 Q",
                                   "PAKMAYA SEKERLI VANIL 5 5 Q"            ~ "PAKMAYA VANILIN 5 Q",
                                   "PAKMAYA SHOKOLADLI SOS 125 Q"           ~ "PAKMAYA SHOKOLADLI SOS 125 Q",
                                   "PAKMAYA KAKAO 50 Q"                     ~ "PAKMAYA KAKAO 50 Q",
                             
                                   "PERSIL YUYUCU TOZ LS COLOR 6 KQ"        ~ "PERSIL COLOR 6 KQ",
                                   "PERSIL MATIK GULUN BUYUSU 6 KQ"         ~ "PERSIL GULUN BUYUSU 6 KQ",
                                   "PERWOLL QARA 1 L"                       ~ "PERWOLL BLACK 1 L",
                                   "PERWOLL 1 L BLACK MAGIC"                ~ "PERWOLL BLACK 1 L",
                                   "PERWOLL SIHIR 1 L RENGLI"               ~ "PERWOLL COLOR 1 L",
                                
                                   "PRESIDENT GAYMAG 200 Q 1X21"            ~ "PRESIDENT QAYMAQ 200 Q",
                                   "PREZIDENT PENDIR YAGLI 50 Q"            ~ "PRESIDENT PENDIR 50 Q",
                                   "PRESIDENT KERE YAGI 82 400 Q"           ~ "PRESIDENT KERE YAGI 400 Q",
                                   "PRESIDENT CEDDAR PENDIR 150 Q"          ~ "PRESIDENT PENDIR CHEDDAR 150 Q",
                                   "PRESIDENT XAMA 20 180 Q"                ~ "PRESIDENT XAMA 20% 180 Q",
                                  
                                   "PATRON TUM YAQUAR 60 Q"                 ~ "PATRON TUM 60 Q",
                                   "PATRON 4D 80 Q 1+1"                     ~ "PATRON TUM 4D 80 Q",
                                   "PATRON GUNEBAXAN TUMU QARA 4D BIG 110 Q" ~ "PATRON TUM 4D 110 Q",
                                   "PATRON YAQUAR ALA TUM 100 Q"            ~ "PATRON TUM 100 Q",
                                   
                                   "PAPIA TUALET KAGIZI 4 QAT 4 RULON"      ~ "PAPIA TUALET KAGIZI N4",
                                   "PAPIA TK 12 RULON"                      ~ "PAPIA TUALET KAGIZI N12",
                                   "PAPI SALFET 23 25 SM 180 EDEDLI"        ~ "PAPI SALFET 180 ED",
                                   "PAPI YAS DESMALI PLS KAPAK 72X36"       ~ "PAPI NEM SALFET N72",
                                   "PAPILION QULAQ COPU 200 EDEDLI"         ~ "PAPILION QULAQ COPU N200",
                               
                                   "PORTAGAL KQ"                            ~ "PORTAQAL KQ",
                                   "PORTAGAL MISIR KQ"                      ~ "PORTAQAL MISIR KQ",
                                   "POMIDOR ZIRE KQ"                        ~ "POMIDOR ZIRE KQ",
                                   "POMIDOR SALXIM KQ"                      ~ "POMIDOR SALXIM KQ",
                                   "PEC YASHKINO 137 Q PORTAGAL"            ~ "PECENYE YASHKINO PORTAQAL 137 Q",
                                   "PECENYE OREO CEVIR TAT BATIR 95 Q"      ~ "OREO 95 Q",
                                   "PENDIR SACAQ AZFOOD KQ"                 ~ "SACAQ PENDIRI KQ",
                         
                                   "PANTAP 40 MQ N 28 HEBLER NOBEL ILAC"    ~ "PANTAP 40 MG N28",
                                   "PARACHETAMOL 0 5 Q N 20 HEBLER SOPHARMA" ~ "PARACETAMOL 500 MG N20",
                                   "PEREKIS VODORODA 3 100 ML S PIPETKOY"   ~ "PEREKIS VODORODA 3% 100 ML",
                                 
                                   "PROSTOKVASHINO XAMA 25 315 Q"           ~ "PROSTOKVASHINO XAMA 25% 315 Q",
                                   "PROSTOKVASHINO SUD 3 2 1 L"             ~ "PROSTOKVASHINO SUD 3.2% 1 L",
                .default = product_name))

## n herfi ####

fp_final = fp_final %>%
  mutate(product_name = str_squish(toupper(product_name))) %>%
  mutate(product_name = case_match(product_name,
                                   "NUR ZAVOD COREYI"                       ~ "NUR ZAVOD COREK",
                                   "NUR Z COREYI PK 650Q"                   ~ "NUR ZAVOD COREK 650 Q",
                                   "NUR ZAVOD COREYI BOYUK"                 ~ "NUR ZAVOD COREK BOYUK",
                                   "N1 ZAVOD COREK 600 Q"                   ~ "N1 ZAVOD COREK 600 Q",
                                   "N1 ZAVOD COREK 600"                     ~ "N1 ZAVOD COREK 600 Q",
                                   "N1 BORADIN COREYI 400 Q"                ~ "N1 BORODINO COREK 400 Q",
                                   "N1 BORADIN COREYI 40"                   ~ "N1 BORODINO COREK 400 Q",
                                   "N1 BORODIN COREYI 400 Q ED"             ~ "N1 BORODINO COREK 400 Q",
                                   "N1 XUSUSI BATON COREYI 400 Q"           ~ "N1 BATON COREK 400 Q",
                                   "N1 KEPEKLI COREK 400 Q"                 ~ "N1 KEPEKLI COREK 400 Q",
                                   "N1 PEHRIZ COREYI 300 Q"                 ~ "N1 PEHRIZ COREK 300 Q",
                                   "N1 ALMAN COREYI 400"                    ~ "N1 ALMAN COREK 400 Q",
                                   "N1 HOTDOG COREYI 80"                    ~ "N1 HOTDOG COREK 80 Q",
                                   "N1 BURGER COREK 100"                    ~ "N1 BURGER COREK 100 Q",
                                   "NZS KURE COREYI 550 Q"                  ~ "NZS KURE COREK 550 Q",
                                   "NAXCIVAN LAVASI 5 ED"                   ~ "NAXCIVAN LAVASI N5",
                         
                                   "NATURA MULTIVITAMIN SIRESI 200 ML T P"  ~ "NATURA SIRE MULTIVITAMIN 200 ML",
                                   "NATURA SAFTALI SIRESI 200 ML T P"       ~ "NATURA SIRE SHAFTALI 200 ML",
                                   "NATURA PORTAGAL SIRESI 200 ML T P"      ~ "NATURA SIRE PORTAQAL 200 ML",
                                   "NATURA ALBALI SIRESI 200 ML T P"        ~ "NATURA SIRE ALBALI 200 ML",
                                   "NATURA ALMA SIRESI 200 ML T P"          ~ "NATURA SIRE ALMA 200 ML",
                                   "NATURA BASE MULTIMEYVE SIRESI 1 L T P"  ~ "NATURA SIRE MULTIMEYVE 1 L",
                                   "NATURA PORTAGAL 200 ML"                 ~ "NATURA SIRE PORTAQAL 200 ML",
                                   "NATURA SAFTALI 200 ML"                  ~ "NATURA SIRE SHAFTALI 200 ML",
                                   "NATURA ALMA 200 ML"                     ~ "NATURA SIRE ALMA 200 ML",
                                   "NATURA ALBALI 200 ML"                   ~ "NATURA SIRE ALBALI 200 ML",
                                   "NATURA SHAFTALI SIRESI 1 L"             ~ "NATURA SIRE SHAFTALI 1 L",
                          
                                   "NESCAFE 3IN1 16 Q"                      ~ "NESCAFE 3IN1 16 Q",
                                   "NESCAFE 3IN1 CLASSIC 14 5 Q"            ~ "NESCAFE 3IN1 CLASSIC 14.5 Q",
                                   "NESCAFE 3IN1 MILD KOFE 14 5 Q"          ~ "NESCAFE 3IN1 MILD 14.5 Q",
                                   "NESCAFE 3IN1 STRONG 14 5 Q"             ~ "NESCAFE 3IN1 STRONG 14.5 Q",
                                   "NESCAFE GOLD ERGOS 2 Q"                 ~ "NESCAFE GOLD 2 Q",
                                   "NESCAFE CLASSIC SUSE 47 5 Q"            ~ "NESCAFE CLASSIC 47.5 Q",
                                   "NESCAFE CLASSIC 47 5 Q (SUSE)"          ~ "NESCAFE CLASSIC 47.5 Q",
                                   "NESCAFE CLASSIC KOFE SUSE 95 Q"         ~ "NESCAFE CLASSIC 95 Q",
                                   "NESCAFE GOLD KOFE 47 5 Q"               ~ "NESCAFE GOLD 47.5 Q",
                                   "NESCAFE CLASSIC CAPP 18 Q"              ~ "NESCAFE CAPPUCCINO 18 Q",
                 
                                   "NESTLE KIT KAT SUDLU SHOK KQ"           ~ "NESTLE KITKAT SUDLU KQ",
                                   "NESTLE KITKAT 4 FINGER 41 5 Q"          ~ "NESTLE KITKAT 4 FINGER 41.5 Q",
                                   "NESTLE KITKAT WAFER SWEETS SOKOLAD KQ" ~ "NESTLE KITKAT WAFER KQ",
                                   "NESTLE KITKAT KING SIZE 58 Q"           ~ "NESTLE KITKAT KING SIZE 58 Q",
                                   "NESTLE NESQUIK SUTLU SOKOLAD 100 Q"     ~ "NESTLE NESQUIK SUDLU SOKOLAD 100 Q",
                                   "NESTLE NESQUIK SEHER YEMEYI 250 Q"      ~ "NESTLE NESQUIK SEHER YEMEYI 250 Q",
                                
                                   "NUTELLA 350 Q"                          ~ "NUTELLA 350 Q",
                                   "NUTELLA 630 Q"                          ~ "NUTELLA 630 Q",
                                   "NUTELLA AND GO T1X24"                   ~ "NUTELLA & GO 52 Q",
                                   "NUTELLA GO 52Q"                         ~ "NUTELLA & GO 52 Q",
                                   "NUTELLA GO 52 Q"                        ~ "NUTELLA & GO 52 Q",
                             
                                   "NIVEA DEO SPREY 150M"                   ~ "NIVEA DEODORANT 150 ML",
                                   "NIVEA DEO ROLL ON QA"                   ~ "NIVEA ROLL-ON 50 ML",
                                   "NIVEA DUS GELI 250 ML"                  ~ "NIVEA DUSH GELI 250 ML",
                                   "NIVEA EL KREMI 75 ML"                   ~ "NIVEA EL KREMI 75 ML",
                                   "NIVEA KREM 30ML GOY D Q"                ~ "NIVEA KREM 30 Q",
                          
                                   "NIMESIL 100 MQ N 30 SASHE BERLIN CHEMIE" ~ "NIMESIL 100 MG N30",
                                   "NIMESIL 100 MQ 30 (SASE) (BERLIN CHEMIE)" ~ "NIMESIL 100 MG N30",
                                   "NIMESIL N30 PAKET"                      ~ "NIMESIL N30",
                                   "NO SHPA 40 MQ N 24 HEBLER SANOFI"        ~ "NO-SPA 40 MG N24",
                                   "NO SPA 40 MQ 24 (TABLETKI) (SANOFI AVENTIS)" ~ "NO-SPA 40 MG N24",
                                   "NUROFEN 200 MQ N 10 HEBLER RECKITT BENCKISER LTD" ~ "NUROFEN 200 MG N10",
                                   "NUROFEN FORTE 400 MQ N 12 HEBLER RECKITT BENCKISER LTD" ~ "NUROFEN FORTE 400 MG N12",
                         
                                   "NUTRILAK 1 (0 6 AY) 350 Q (8042)"       ~ "NUTRILAK 1 (0-6 AY) 350 Q",
                                   "NUTRILAK PREMIUM 1 300 Q"                ~ "NUTRILAK PREMIUM 1 300 Q",
                                   "NUTRILAK PREMIUM 2 600 Q"                ~ "NUTRILAK PREMIUM 2 600 Q",
                                
                                   "NZS PIVE SADE 4 7 SUSE 0 5 L"            ~ "NZS PIVE SADE 0.5 L",
                                   "NZS PREMIUM PIVE 500 ML"                ~ "NZS PIVE PREMIUM 0.5 L",
                                   "NZS STRONG PIVE 6 500 ML"               ~ "NZS PIVE STRONG 0.5 L",
                                   "NABEGHLAVI 1 L PET"                     ~ "NABEGHLAVI 1 L",
                                   "NABEGHLAVI 0 5 L SUSE"                  ~ "NABEGHLAVI 0.5 L",
                                   "NAPITOK BIZON 0 25 L BANKA"              ~ "BIZON ENERGY 250 ML",
                                   "NAPITOK RED BIZON 0 25 L"               ~ "BIZON RED 250 ML",
                                 
                                   "NID SARIKOK PAK 40Q"                    ~ "NID SARIKOK 40 Q",
                                   "NID ISTIOT Q UY 10Q"                    ~ "NID ISTIOT 10 Q",
                                   "NID MIXEK 10 Q"                         ~ "NID MIXEK 10 Q",
                                   "NID LIMON DUZU 15 Q"                    ~ "NID LIMON DUZU 15 Q",
                                
                                   "NET TUZ RUHU 600 Q"                     ~ "NET TUZ RUHU 600 Q",
                                   "NOXES SABUN LIMON 150 Q"                ~ "NOXES SABUN LIMON 150 Q",
                                   "NEM SALFET PAPI 15 ED CIB"              ~ "PAPI NEM SALFET N15",
                                   
                                   .default = product_name))


