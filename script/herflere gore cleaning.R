fp_final = fp_final %>%
  mutate(pos = toupper(pos)) %>% 
  mutate(pos = str_replace_all(pos, c(
    "Ə" = "E",
    "İ" = "I",
    "Ş" = "S",
    "Ç" = "C",
    "Ğ" = "G",
    "Ö" = "O",
    "Ü" = "U"
  ))) %>%
  mutate(pos = str_squish(pos))
library(dplyr)

## z herfi ####

fp_final = fp_final %>%
  mutate(pos = case_match(pos,
                          "ZAUR MARTKET"                               ~ "ZAUR MARKET",
                          "ZEFIR MOLL MG"                              ~ "ZEFIR MALL MG",
                          "ZERGERLIK VE DIGER MEISET MEMULAMATLARININ HAZIRLA" ~ "ZERGERLIK VE DIGER MEISET MEMULATLARININ HAZIRLANMASI",
                          "ZIMMI ERZAQ MALLARINI SATIS"                ~ "ZIMMI ERZAQ MALLARININ SATISI",
                          "ZOOMAGAZASI"                                ~ "ZOO MAGAZA",
                          "ANBAR VE PARAKENDE SATISI"                  ~ "ANBAR VE PERAKENDE SATISI", 
                          "ZABRAT MAGAZA"                              ~ "ZABRAT MAGAZA", 
                          "ZIPPY GEYIM MAGAZA"                         ~ "ZIPPY GEYIM MAGAZA",
                          .default = pos ))

## y herfi ####

fp_final = fp_final %>%
  mutate(pos = case_match(pos,
              
                          "YDM 9 SAIR MALLARIN SATIS"                  ~ "YDM 9 SAIR MALLARIN SATISI",
                          "YEM MEHSUL SATIS"                           ~ "YEM MEHSULLARI SATISI",
                          "YEM SATIS"                                  ~ "YEM SATISI",
                          "YEM SATIS MAGAZA"                           ~ "YEM SATISI MAGAZA",
                          "YENI GENCE TIBB MERKEZ"                     ~ "YENI GENCE TIBBI MERKEZI",
                          "YENI MARKET TICARET MERKEZ"                 ~ "YENI MARKET TICARET MERKEZI",
                          "YUMSAQ MEBEL VE DOSEK ISTEHSALI SATIS"      ~ "YUMSAQ MEBEL VE DOSEK ISTEHSALI VE SATISI",
                          "YUYUCU VASITELERIN ISTEHSAL SEXI"           ~ "YUYUCU VASITELERININ ISTEHSALI SEXI",
                          "YUYUCU MALLARI PARFUMERIYA"                 ~ "YUYUCU MALLAR VE PARFUMERIYA",
                          "YATSAN YUMSAQ MEBEL AKSESSUAR MAGAZA"       ~ "YATSAN YUMSAQ MEBEL VE AKSESSUAR MAGAZASI", 
  ))

## v herfi ####

fp_final = fp_final %>%
  mutate(pos = case_match(pos,
                        
                          "VILLEROY BOCH"                              ~ "VILLEROY & BOCH",
                          "VIMAY MED TIBB MERKEZ"                      ~ "VIMAY MED TIBBI MERKEZI",
                          "VIPLIFE SIRKETI PERAKENDE TICARET"          ~ "VIPLIFE SIRKETI PERAKENDE TICARETI",
                          "VOLKSGIM"                                   ~ "VOLKSGYM",
                          "VITA APTEK"                                 ~ "VITA APTEKI",
                          .default = pos ))

## u herfi ####

fp_final = fp_final %>%
  mutate(pos = case_match(pos,
                          
                          "UN MEMULATLARINININ ISTEHSALI VE SATIS"     ~ "UN MEMULATLARININ ISTEHSALI VE SATISI",
                          "UN VE UN MEHSUL SATIS"                      ~ "UN VE UN MEHSULLARININ SATISI",
                          "UNIFORMALARIN SATIS MAGAZA"                 ~ "UNIFORMALARIN SATISI MAGAZASI",
                          "ULVI SADLIQ EV"                             ~ "ULVI SADLIQ EVI",
                          "UROLOJI XESTEXANA"                          ~ "UROLOJI XESTEXANASI",
                          "USAQ GEYIMLERI MAGAZA"                      ~ "USAQ GEYIMLERI MAGAZASI", 
                          "U S POLO ASSN"                              ~ "US POLO ASSN", 
                          .default = pos ))

## q herfi ####

fp_final = fp_final %>%
  mutate(pos = case_match(pos,
                        
                          "QAB QACAQ MAGAZA"                           ~ "QAB-QACAQ MAGAZASI",
                          "QADIN SAGLAMLIQ MERKEZ"                     ~ "QADIN SAGLAMLIQ MERKEZI",
                          "QAFQAZ EHTIYAT HISSE"                       ~ "QAFQAZ EHTIYAT HISSELERI",
                          "QALA TICARET MERKEZ"                        ~ "QALA TICARET MERKEZI",
                          "QARISIQ MALLAR SATIS"                       ~ "QARISIQ MALLARIN SATISI",
                          "QARISIQ MALLLAR MAGAZA"                     ~ "QARISIQ MALLAR MAGAZASI",
                          "QAZ VE SU AVADANLIQLARININ PERAKENDE SATIS" ~ "QAZ VE SU AVADANLIQLARININ PERAKENDE SATISI",
                          "QEYRI YACAYIS BINASI"                       ~ "QEYRI YASAYIS BINASI",
                          "QEYRI YASAYIS SAHESI SAMSUNG MAGAZA"        ~ "QEYRI YASAYIS SAHESI SAMSUNG MAGAZASI",
                          "QISMET KLINIKA"                             ~ "QISMET KLINIKASI",
                          "QOLFF AKSESUARLARININ SATIS"                ~ "QOLFF AKSESUARLARININ SATISI",
                          "QULLE RESTORAN"                             ~ "QULLE RESTORANI",
                          "QUM VE INSAAT MALLARI SATIS"                ~ "QUM VE INSAAT MALLARININ SATISI",
                          "QURU TEMIZLEME MERKEZ"                      ~ "QURU TEMIZLEME MERKEZI",
                          "QADIN GEYIM MAGAZA"                         ~ "QADIN GEYIMI MAGAZASI",
                          .default = pos ))

## r herfi ####

fp_final = fp_final %>%
  mutate(pos = case_match(pos,
                          
                          "RAHAT YENI GUNESLI PERAKENDE TICARET"       ~ "RAHAT YENI GUNESLI PERAKENDE TICARETI",
                          "RAUF INSAAT"                                ~ "RAUF INSAAT",
                          "REAL M MAGAZA"                              ~ "REAL M MAGAZASI",
                          "REAL TIBB MERKEZ"                           ~ "REAL TIBBI MERKEZI",
                          "REFERANS KLINIK LABARATORIYA"               ~ "REFERANS KLINIK LABORATORIYASI",
                          "REKLAM SERGI SALON"                         ~ "REKLAM SERGI SALONU",
                          "REMZI INSAAT TESERRUFAT MALLARI SATIS MAGAZA" ~ "REMZI INSAAT TESERRUFAT MALLARI SATISI MAGAZASI",
                          "RESAD MAHIROGLU MAGAZA"                     ~ "RESAD MAHIROGLU MAGAZASI",
                          "RESAD TICARET MAGAZA"                       ~ "RESAD TICARET MAGAZASI",
                          "RESPUBLIKA MUALICE DIAQNOSTIKA MERKEZ"      ~ "RESPUBLIKA MUALICE DIAQNOSTIKA MERKEZI",
                          "RESPUBLIKA IDMAN TIBB DIAQNOSTIKA VE REABILITASI" ~ "RESPUBLIKA IDMAN TIBBI DIAQNOSTIKA VE REABILITASIYASI",
                          "RETRO ISTIRAHET MERKEZ"                     ~ "RETRO ISTIRAHET MERKEZI",
                          "RETRO SAM ISTIRAHET MERKEZ"                 ~ "RETRO SAM ISTIRAHET MERKEZI",
                          "ROYAL AESHETIC CENTRE"                      ~ "ROYAL AESTHETIC CENTRE",
                          "RTR INSAAT TIKINTI MATERIALLAR MAGAZA"      ~ "RTR INSAAT TIKINTI MATERIALLARI MAGAZASI",
                          "RVR SAUNA"                                  ~ "RVR SAUNAS",
                          .default = pos ))

## p herfi ####

fp_final = fp_final %>%
  mutate(pos = case_match(pos,
                         
                          "PAK 46 ERZAQ MAGAZA"                        ~ "PAK 46 ERZAQ MAGAZASI",
                          "PALTAR MAGAZA"                              ~ "PALTAR MAGAZASI",
                          "PANDA BABY MAGAZA"                          ~ "PANDA BABY MAGAZASI",
                          "PANDA KIDS MAGAZA"                          ~ "PANDA KIDS MAGAZASI",
                          "PARAMED TIBB KLINIKA"                       ~ "PARAMED TIBBI KLINIKASI",
                          "PARCA MAGAZA"                               ~ "PARCA MAGAZASI",
                          "PARCA SATIS"                                ~ "PARCA SATISI",
                          "PARFUMERIYA MAGAZA"                         ~ "PARFUMERIYA MAGAZASI",
                          "PARIS RESTORAN"                             ~ "PARIS RESTORANI",
                          "PARK BULVAR TICARET MERKEZ"                 ~ "PARK BULVAR TICARET MERKEZI",
                          "PARKET EKZOTIK MAGAZA"                      ~ "PARKET EKZOTIK MAGAZASI",
                          "PASHABAHCE CAPAYEV"                         ~ "PASABAHCE CAPAYEV",
                          "PAUL AND SHARK MAGAZA"                      ~ "PAUL & SHARK MAGAZASI",
                          "PERAKENDE SATICS NOQTESI"                   ~ "PERAKENDE SATIS NOQTESI",
                          "PERAKENDE SATIS MAGAZA"                     ~ "PERAKENDE SATISI MAGAZASI",
                          "PERAKENDE TICARET TIKINTI MATERIALLAR"      ~ "PERAKENDE TICARET TIKINTI MATERIALLARI",
                          "PETIT BATEAU USAQ PALTARLARI MAGAZA"        ~ "PETIT BATEAU USAQ PALTARLARI MAGAZASI",
                          "PIERO PAUL GEYIM MAGAZA"                    ~ "PIERO PAUL GEYIM MAGAZASI",
                          "PLASTIK BORULARIN SATIS"                    ~ "PLASTIK BORULARIN SATISI",
                          "PLASTIK MEMULATLARININ SATIS"               ~ "PLASTIK MEMULATLARININ SATISI",
                          "PLASTIK PROFEL MAGAZA"                      ~ "PLASTIK PROFIL MAGAZASI",
                          "PLASTIK QAPI PENCERE MATERIALAR SATIS"      ~ "PLASTIK QAPI PENCERE MATERIALLARI SATISI",
                          "POLIQAFIYA MALLARI MAGAZA"                  ~ "POLIQRAFIYA MALLARI MAGAZASI",
                          "PROFIL MAGAZA"                              ~ "PROFIL MAGAZASI",
                          "PULL AND BEAR GEYIM MAGAZA"                 ~ "PULL & BEAR GEYIM MAGAZASI",
                          "PURPLE QENNADI MEMULATLARI VE GULLER MAGAZA" ~ "PURPLE QENNADI MEMULATLARI VE GULLER MAGAZASI",
                          "PVC PLASTIK MATERIALLARIN SATIS MAGAZA"     ~ "PVC PLASTIK MATERIALLARININ SATISI MAGAZASI",
                          .default = pos ))

## n herfi ####

fp_final = fp_final %>%
  mutate(pos = case_match(pos,
                         
                          "NAHID INSAAT"                               ~ "NAHID INSAATI",
                          "NARGILE MAGAZA"                             ~ "NARGILE MAGAZASI",
                          "NATIONAL PRIMEHOSPITAL"                     ~ "NATIONAL PRIME HOSPITAL",
                          "NATURAL MEAT MAGAZA"                        ~ "NATURAL MEAT MAGAZASI",
                          "NAVES MAGAZA"                               ~ "NAVES MAGAZASI",
                          "NB LAZUR TIBB MERKEZ"                       ~ "NB LAZUR TIBBI MERKEZI",
                          "NEBZ TIBB MERKEZ"                           ~ "NEBZ TIBBI MERKEZI",
                          "NEFTCALA 7 APTEK"                           ~ "NEFTCALA 7 APTEKI",
                          "NEHREM ERZAQ MAGAZA"                        ~ "NEHREM ERZAQ MAGAZASI",
                          "NEPTUN S ADLI MAGAZA"                       ~ "NEPTUN S ADLI MAGAZASI",
                          "NERGIZ KLINIKA"                             ~ "NERGIZ KLINIKASI",
                          "NERGIZ MED KLINIKA"                         ~ "NERGIZ MED KLINIKASI",
                          "NERIMANOV MAGAZA"                           ~ "NERIMANOV MAGAZASI",
                          "NET ELEKTRIK MAGAZA"                        ~ "NET ELEKTRIK MAGAZASI",
                          "NEW YORKER GEYIM MAGAZA"                    ~ "NEW YORKER GEYIM MAGAZASI",
                          "NIKE MAGAZA"                                ~ "NIKE MAGAZASI",
                          "NOMEX MAGAZA"                               ~ "NOMEX MAGAZASI",
                          "NORM MAGAZA"                                ~ "NORM MAGAZASI",
                          "NURMARKEY SAHIBKAR MAGAZA"                  ~ "NURMARKET SAHIBKAR MAGAZASI",
                          "NURSEL TICARET"                             ~ "NURSEL TICARETI",
                          "NYUKOND MAGAZA"                             ~ "NYUKOND MAGAZASI",
                          .default = pos 
  ))

## l herfi ####

fp_final = fp_final %>%
  mutate(pos = case_match(pos,
                         
                          "LA SENZA ALT GEYIM MAGAZA"                  ~ "LA SENZA ALT GEYIMI MAGAZASI",
                          "LA VIE EN ROSE GEYIM MAGAZA"                ~ "LA VIE EN ROSE GEYIM MAGAZASI",
                          "LABARATORIYA"                               ~ "LABORATORIYA",
                          "LABARATORIYA SUNLAB"                        ~ "LABORATORIYA SUNLAB",
                          "LABSTYLE TIBB MERKEZ"                       ~ "LABSTYLE TIBBI MERKEZI",
                          "LADY LIFE QADIN GEYIM"                      ~ "LADY LIFE QADIN GEYIMI",
                          "LAMINAT SATIS UZRE MAGAZA"                  ~ "LAMINAT SATISI UZRE MAGAZA",
                          "LANSET CERRAHLIQ KLINIKA"                   ~ "LANSET CERRAHLIQ KLINIKASI",
                          "LAVAZZA COFE STORE"                         ~ "LAVAZZA COFFEE STORE",
                          "LC WAIKIKI DREAM MAGAZA"                    ~ "LC WAIKIKI DREAM MAGAZASI",
                          "LC WAIKIKI GEYIM MAGAZA"                    ~ "LC WAIKIKI GEYIM MAGAZASI",
                          "LC WAIKIKI GEYIMLERIN SATIS MAGAZA"         ~ "LC WAIKIKI GEYIMLERIN SATISI MAGAZASI",
                          "LENKERAN 2 SAYLI MAGAZA"                    ~ "LENKERAN 2 SAYLI MAGAZASI",
                          "LENKERAN MAGAZA"                            ~ "LENKERAN MAGAZASI",
                          "LENOVA TEXNIKA MAGAZA"                      ~ "LENOVO TEXNIKA MAGAZASI",
                          "LESYA QARISIQ MALLAR MAGAZA"                ~ "LESYA QARISIQ MALLAR MAGAZASI",
                          "LIDER TIKINTI MATERIALLAR"                  ~ "LIDER TIKINTI MATERIALLARI",
                          "LIMAN KLINIKA"                              ~ "LIMAN KLINIKASI",
                          "LIMONAD TOPDAN SATIS"                       ~ "LIMONAD TOPDAN SATISI",
                          "LLADRO MAGAZA"                              ~ "LLADRO MAGAZASI",
                          "LOFT GEYIM MAGAZA"                          ~ "LOFT GEYIM MAGAZASI",
                          "LOGMAN KLINIKA"                             ~ "LOGMAN KLINIKASI",
                          "LONGINES MAGAZA"                            ~ "LONGINES MAGAZASI",
                          "LOR HOSPITAL KLINIKA"                       ~ "LOR HOSPITAL KLINIKASI",
                          "LOTOS GEYIM MAGAZA"                         ~ "LOTOS GEYIM MAGAZASI",
                          "LUFIAN GEYIM MAGAZA"                        ~ "LUFIAN GEYIM MAGAZASI",
                          "LUMEGRAND MAGAZA"                           ~ "LUMEGRAND MAGAZASI",
                          "LUXURY WINE ELEQANCE"                       ~ "LUXURY WINE ELEGANCE",
                          .default = pos ))

## k herfi ####

fp_final = fp_final %>%
  mutate(pos = case_match(pos,
                          
                          "KAFEL METLAG MAGAZA"                        ~ "KAFEL METLAX MAGAZASI",
                          "KAFEL METLAX MAGAZA"                        ~ "KAFEL METLAX MAGAZASI",
                          "KAFEL VE METLAX SATIS"                      ~ "KAFEL VE METLAX SATISI",
                          "KAGIZ DESMAL ISTHSALI SEXI"                 ~ "KAGIZ DESMAL ISTEHSALI SEXI",
                          "KAMRAN APTEK"                               ~ "KAMRAN APTEKI",
                          "KAPSULA APTEK"                              ~ "KAPSULA APTEKI",
                          "KARACA HOME MAGAZA"                         ~ "KARACA HOME MAGAZASI",
                          "KARACA QAB QACAQ MAGAZA AZADLIQ"            ~ "KARACA QAB-QACAQ MAGAZASI AZADLIQ",
                          "KARVAN MAGAZA"                              ~ "KARVAN MAGAZASI",
                          "KAVANNI MEBEL MAGAZA"                       ~ "KAVANNI MEBEL MAGAZASI",
                          "KEHRIZ RESTORAN"                            ~ "KEHRIZ RESTORANI",
                          "KEND TESERRUFATI MAGAZA"                    ~ "KEND TESERRUFATI MAGAZASI",
                          "KEND TESERUFATLARI MAGAZA"                  ~ "KEND TESERRUFAT MALLARI MAGAZASI",
                          "KEPRO ESTETIK KLINIKA"                      ~ "KEPRO ESTETIK KLINIKASI",
                          "KERALUX TLD"                                ~ "KERALUX LTD",
                          "KERPIC SATIS MAGAZA"                        ~ "KERPIC SATISI MAGAZASI",
                          "KEYTIDAG ERZAQ MAGAZA"                      ~ "KEYTIDAG ERZAQ MAGAZASI",
                          "KIA HUNDAI EHT"                             ~ "KIA HYUNDAI EHTIYAT HISSELERI",
                          "KIDS PLANET USAQ EYLENCE MERKEZ"            ~ "KIDS PLANET USAQ EYLENCE MERKEZI",
                          "KING SMART SUPERMARKET"                     ~ "KINGSMART SUPERMARKET",
                          "KIOSK 203 ERZAQ MALLAR SATIS"               ~ "KIOSK 203 ERZAQ MALLARI SATISI",
                          "KIREMIT KERPIC ISTEHSALI VE SATIS"          ~ "KIREMIT KERPIC ISTEHSALI VE SATISI",
                          "KISI KOYNEKLERI MAGAZA"                     ~ "KISI KOYNEKLERI MAGAZASI",
                          "KITAB MAGAZA"                               ~ "KITAB MAGAZASI",
                          "KITABXANA VE KITAB SATIS"                   ~ "KITABXANA VE KITAB SATISI",
                          "KM KLINIKA"                                 ~ "KM KLINIKASI",
                          "KOHNE PALTAR MAGAZA"                        ~ "KOHNE PALTAR MAGAZASI",
                          "KOMFOR MEBEL MAGAZA"                        ~ "KOMFOR MEBEL MAGAZASI",
                          "KOMPUTER AKSESSUARLARI MAGAZA"              ~ "KOMPUTER AKSESSUARLARI MAGAZASI",
                          "KOMPUTER AVADANLIQLARI MAGAZA"              ~ "KOMPUTER AVADANLIQLARI MAGAZASI",
                          "KOMPUTERLERIN SATIS"                        ~ "KOMPUTERLERIN SATISI",
                          "KOMPYUTER MAGAZA"                           ~ "KOMPYUTER MAGAZASI",
                          "KONTAKT HOME MAGAZA"                        ~ "KONTAKT HOME MAGAZASI",
                          "KONTAKT TELEFON MAGAZA"                     ~ "KONTAKT TELEFON MAGAZASI",
                          "KOSK"                                       ~ "KIOSK",
                          "KOSMETIKA MAGAZA"                           ~ "KOSMETIKA MAGAZASI",
                          "KOSMETIKA MALLARI MAGAZA"                   ~ "KOSMETIKA MALLARI MAGAZASI",
                          "KOSMETIKA SATIS MAGAZA"                     ~ "KOSMETIKA SATISI MAGAZASI",
                          "KOSMETOLOJI MERKEZ"                         ~ "KOSMETOLOJI MERKEZI",
                          "KRAL INSAAT MAGAZA"                         ~ "KRAL INSAAT MAGAZASI",
                          "KURDEMIR OPTIMAL ELEKTRONIKA MAGAZA"        ~ "KURDEMIR OPTIMAL ELEKTRONIKA MAGAZASI",
                          .default = pos ))

## j herfi ####

fp_final = fp_final %>%
  mutate(pos = case_match(pos,
                          
                          "JAC AVTO EHTIYAT HISSE"                     ~ "JAC AVTO EHTIYAT HISSELERI",
                          "JACADI 2 MAGAZA"                            ~ "JACADI 2 MAGAZASI",
                          "JACADI MAGAZA"                              ~ "JACADI MAGAZASI",
                          "JUNIOR CLUB MAGAZA"                         ~ "JUNIOR CLUB MAGAZASI",
                          "JYSK MAGAZA"                                ~ "JYSK MAGAZASI",
                          .default = pos ))

## i herfi ####

fp_final = fp_final %>%
  mutate(pos = case_match(pos,
                          
                          "ICARE VE TEKER SATIS"                       ~ "ICARE VE TEKER SATISI",
                          "ICE PORT MAGAZA"                            ~ "ICE PORT MAGAZASI",
                          "ICKI VE TUTUN MEMULATLARI DADAXIL OLMAQLA ERZAQ MA" ~ "ICKI VE TUTUN MEMULATLARI DAXIL OLMAQLA ERZAQ MAGAZASI",
                          "ICKILERIN SATIS"                            ~ "ICKILERIN SATISI",
                          "IDEAL ETRIYYAT MAGAZA"                      ~ "IDEAL ETRIYYAT MAGAZASI",
                          "IDEAL ETRIYYAT VE KOSMETIKA MAGAZA"         ~ "IDEAL ETRIYYAT VE KOSMETIKA MAGAZASI",
                          "IDEAL INSAAT MATERIALLAR"                   ~ "IDEAL INSAAT MATERIALLARI",
                          "IDEAL MAGAZA"                               ~ "IDEAL MAGAZASI",
                          "IDEAL MARKET ERZAQ SATIS MAGAZA"            ~ "IDEAL MARKET ERZAQ SATISI MAGAZASI",
                          "IDEAL TIBB KLINIKA"                         ~ "IDEAL TIBBI KLINIKASI",
                          "IDMAN MALLAR SATIS"                         ~ "IDMAN MALLARI SATISI",
                          "IDMAN MALLARI MAGAZA"                       ~ "IDMAN MALLARI MAGAZASI",
                          "IDO USAQ GEYIM SATIS"                       ~ "IDO USAQ GEYIMI SATISI",
                          "IDO USAQ GEYIMLERININ SATIS"                ~ "IDO USAQ GEYIMLERININ SATISI",
                          "ILGAROGLU MAGAZA 32 33 MEHELLE"             ~ "ILGAROGLU MAGAZASI 32 33 MEHELLE",
                          "IMPLANTECH MAGAZA"                          ~ "IMPLANTECH MAGAZASI",
                          "INDESIT EV ESYALARININ SATIS MERKEZ"        ~ "INDESIT EV ESYALARININ SATISI MERKEZI",
                          "INDESIT MAGAZA"                             ~ "INDESIT MAGAZASI",
                          "INGILAB MG"                                 ~ "INGILAB MAGAZASI",
                          "INGLOT KOSMETIK KIOSKU"                     ~ "INGLOT KOSMETIKA KIOSKU",
                          "INGLOT KOSMETIKA MAGAZA"                    ~ "INGLOT KOSMETIKA MAGAZASI",
                          "INSAAT MAGAZA"                              ~ "INSAAT MAGAZASI",
                          "INSAAT MALLAR MAGAZA"                       ~ "INSAAT MALLARI MAGAZASI",
                          "INSAAT MATERIALAR MAGAZA"                   ~ "INSAAT MATERIALLARI MAGAZASI",
                          "INSTYLE GEYIM MAGAZA"                       ~ "INSTYLE GEYIM MAGAZASI",
                          "INTERKABEL MAGAZA"                          ~ "INTERKABEL MAGAZASI",
                          "IPEK YOLU MAGAZA"                           ~ "IPEK YOLU MAGAZASI",
                          "IRFU ISTILIK SISTEMLERI MAGAZA"             ~ "IRFU ISTILIK SISTEMLERI MAGAZASI",
                          "IRSAD ELECTRONCS MAGAZA"                    ~ "IRSAD ELECTRONICS MAGAZASI",
                          "IRSAD ELECTRONICS 28 MALL MAGAZA"           ~ "IRSAD ELECTRONICS 28 MALL MAGAZASI",
                          "IRSAD ELECTRONIKS 8 KM MAGAZA"              ~ "IRSAD ELECTRONICS 8 KM MAGAZASI",
                          "ISLENMIS AVTO EHTIYAT HISSE SATIS"          ~ "ISLENMIS AVTO EHTIYAT HISSELERI SATISI",
                          "ISMAYILLI MAGAZA"                           ~ "ISMAYILLI MAGAZASI",
                          "ISTANBUL KLINIKA"                           ~ "ISTANBUL KLINIKASI",
                          "ISTEHSALAT OBYEKTII"                        ~ "ISTEHSALAT OBYEKTI",
                          "ISTILIK SISTEMLERI MAGAZA"                  ~ "ISTILIK SISTEMLERI MAGAZASI",
                          "ISTILIK SISTEMLERININ SATIS"                ~ "ISTILIK SISTEMLERININ SATISI",
                          "ISTIQBAL MEBEL MAGAZA"                      ~ "ISTIQBAL MEBEL MAGAZASI",
                          "ISTIRAHET VE SAGLAMLIQ MERKEZ"              ~ "ISTIRAHET VE SAGLAMLIQ MERKEZI",
                          .default = pos ))

## h herfi ####

fp_final = fp_final %>%
  mutate(pos = case_match(pos,
                         
                          "HAPPY BABY MAGAZA"                          ~ "HAPPY BABY MAGAZASI",
                          "HARIBO MAGAZA"                              ~ "HARIBO MAGAZASI",
                          "HAVALANDIRMA AVADANLIQLARI MAGAZA"          ~ "HAVALANDIRMA AVADANLIQLARI MAGAZASI",
                          "HAYAT KLINIK TIBB MERKEZ"                   ~ "HAYAT KLINIK TIBBI MERKEZI",
                          "HAYAT KLINIKA"                              ~ "HAYAT KLINIKASI",
                          "HAZIR MEHSULLARIN PERAKENDE SATIS"          ~ "HAZIR MEHSULLARIN PERAKENDE SATISI",
                          "HB GUVEN KLINIKA"                           ~ "HB GUVEN KLINIKASI",
                          "HEDIYYELER MAGAZA"                          ~ "HEDIYYELER MAGAZASI",
                          "HEDIYYELER VE GULLERIN SATIS MAGAZA"        ~ "HEDIYYELER VE GULLERIN SATISI MAGAZASI",
                          "HEMID MARKET ERZAQ MAGAZA"                  ~ "HEMID MARKET ERZAQ MAGAZASI",
                          "HESTERXAN IS MERKEZ OFIS VE ICARE OBYEKTI"  ~ "HESTERXAN IS MERKEZI OFIS VE ICARE OBYEKTI",
                          "HIND KLINIKA"                               ~ "HIND KLINIKASI",
                          "HIPPO GEYIM MAGAZA"                         ~ "HIPPO GEYIM MAGAZASI",
                          .default = pos ))

## f herfi ####

fp_final = fp_final %>%
  mutate(pos = case_match(pos,
                          
                          "FAB TIKINTI MATERIALLAR"                    ~ "FAB TIKINTI MATERIALLARI",
                          "FAIQ TIKINTI MATERIALLAR MAGAZA"            ~ "FAIQ TIKINTI MATERIALLARI MAGAZASI",
                          "FANTAZIYA SIRNIYYAT MAGAZA"                 ~ "FANTAZIYA SIRNIYYAT MAGAZASI",
                          "FARMASY APTEK"                              ~ "FARMASY APTEKI",
                          "FENDI GEYIM MAGAZA"                         ~ "FENDI GEYIM MAGAZASI",
                          "FERAH ECZACILIQ MEHSULLARIN SATIS"          ~ "FERAH ECZACILIQ MEHSULLARININ SATISI",
                          "FERAH TIBB LEVAZIMATLARIN SATIS"            ~ "FERAH TIBBI LEVAZIMATLARININ SATISI",
                          "FEXRI 82 MAGAZA"                            ~ "FEXRI 82 MAGAZASI",
                          "FIRMA MAGAZA"                               ~ "FIRMA MAGAZASI",
                          "FIRMA MAGAZA N1"                            ~ "FIRMA MAGAZASI N1",
                          "FLOORELLA MAGAZA"                           ~ "FLOORELLA MAGAZASI",
                          "FMF MAGAZA"                                 ~ "FMF MAGAZASI",
                          "FOVARIT MARKET"                             ~ "FAVORIT MARKET",
                          "FUNDA TIBB MERKEZ"                          ~ "FUNDA TIBBI MERKEZI",
                          .default = pos ))

## d herfi ####

fp_final = fp_final %>%
  mutate(pos = case_match(pos,
                          
                          "DAM ORTUKLERININ SATIS"                     ~ "DAM ORTUKLERININ SATISI",
                          "DAM ORTUKLERININ SATIS MERKEZ"              ~ "DAM ORTUKLERININ SATIS MERKEZI",
                          "DAM ORTUYLERIN SATIS"                       ~ "DAM ORTUKLERININ SATISI",
                          "DAS KORPU INSAAT MATERIALLAR MAGAZA"        ~ "DAS KORPU INSAAT MATERIALLARI MAGAZASI",
                          "DEDEM AQRO MAGAZA"                          ~ "DEDEM AQRO MAGAZASI",
                          "DEFACTO GEYIM MAGAZA"                       ~ "DEFACTO GEYIM MAGAZASI",
                          "DEFETERXANA LEVAZIMATLARI MAGAZA"           ~ "DEFTERXANA LEVAZIMATLARI MAGAZASI",
                          "DEFTERXANA LEVAZIMATLARININ SATIS MAGAZA"   ~ "DEFTERXANA LEVAZIMATLARININ SATISI MAGAZASI",
                          "DEFTERXANA MAGAZA"                          ~ "DEFTERXANA MAGAZASI",
                          "DEFTERXANA MALLARI MAGAZA"                  ~ "DEFTERXANA MALLARI MAGAZASI",
                          "DEKNA AZERBAYCAN PARCA MAGAZA"              ~ "DEKNA AZERBAYCAN PARCA MAGAZASI",
                          "DEMIR MEMULATLARI SATIS"                    ~ "DEMIR MEMULATLARI SATISI",
                          "DEMIR MEMULATLARININ SATIS MAGAZA"          ~ "DEMIR MEMULATLARININ SATISI MAGAZASI",
                          "DENIZ MEBEL MAGAZA"                         ~ "DENIZ MEBEL MAGAZASI",
                          "DENT STAR STOMATOLOJI KLINIKA"              ~ "DENT STAR STOMATOLOJI KLINIKASI",
                          "DERIMOD GEYIM MAGAZA"                       ~ "DERIMOD GEYIM MAGAZASI",
                          "DERNEGUL MAGAZA 2"                          ~ "DERNEGUL MAGAZASI 2",
                          "DERNEGUL SATIS MERKEZ"                      ~ "DERNEGUL SATIS MERKEZI",
                          "DEVTERXANA LEVAZIMATLARININ SATIS"          ~ "DEFTERXANA LEVAZIMATLARININ SATISI",
                          "DIAQNOZ TIBB MERKEZ"                        ~ "DIAQNOZ TIBBI MERKEZI",
                          "DIET STORE MERKEZ"                          ~ "DIET STORE MERKEZI",
                          "DIVAR KAGIZI MAGAZA"                        ~ "DIVAR KAGIZI MAGAZASI",
                          "DIVAR KAGIZLARI MAGAZA"                     ~ "DIVAR KAGIZLARI MAGAZASI",
                          "DNK KLINIK"                                 ~ "DNK KLINIKASI",
                          "DOGTAS KELEBEK MEBEL MAGAZA"                ~ "DOGTAS KELEBEK MEBEL MAGAZASI",
                          "DOGTAS MEBEL MAGAZA"                        ~ "DOGTAS MEBEL MAGAZASI",
                          "DOCTA APTEK"                                ~ "DOKTA APTEKI",
                          "DOKTOR AZ KLINIKA"                          ~ "DOKTOR AZ KLINIKASI",
                          "DOLCE GABBANA GEYIM MAGAZA"                 ~ "DOLCE & GABBANA GEYIM MAGAZASI",
                          "DONDURMA MAGAZA"                            ~ "DONDURMA MAGAZASI",
                          "DOSTLUQ MEBEL MAGAZA"                       ~ "DOSTLUQ MEBEL MAGAZASI",
                          "DUYMA ISTIRAHET MERKEZ"                     ~ "DUYMA ISTIRAHET MERKEZI",
                          .default = pos ))

## c herfi ####

fp_final = fp_final %>%
  mutate(pos = case_match(pos,
                          
                          "C GARS MAGAZA"                              ~ "C GARS MAGAZASI",
                          "CALZEDONIA INTIMISSIMI GEYIM MAGAZA"        ~ "CALZEDONIA INTIMISSIMI GEYIM MAGAZASI",
                          "CAPITAL TOVVERS"                            ~ "CAPITAL TOWERS",
                          "CARLO PAZOLINI MAGAZA GENCLIK MOLL"         ~ "CARLO PAZOLINI MAGAZASI GENCLIK MALL",
                          "CASAMIA MAGAZA"                             ~ "CASAMIA MAGAZASI",
                          "CASPIAN ELEKTRIK MAGAZA"                    ~ "CASPIAN ELEKTRIK MAGAZASI",
                          "CEKELEK MAGAZA"                             ~ "CEKELEK MAGAZASI",
                          "CELEBI MEBEL SALON"                         ~ "CELEBI MEBEL SALONU",
                          "CELILABAD MAGAZA"                           ~ "CELILABAD MAGAZASI",
                          "CELLEKLERLE PIVE SATIS"                     ~ "CELLEKLERLE PIVE SATISI",
                          "CENLIBEL MAGAZA"                            ~ "CENLIBEL MAGAZASI",
                          "CEREZ VE QURU MEYVELERIN SATIS"             ~ "CEREZ VE QURU MEYVELERIN SATISI",
                          "CHARLES AND KEITH AYAQQABI MAGAZA"          ~ "CHARLES & KEITH AYAQQABI MAGAZASI",
                          "CHICCO USAQ GEYIM MAGAZA"                   ~ "CHICCO USAQ GEYIM MAGAZASI",
                          "CHOPARD MAGAZA"                             ~ "CHOPARD MAGAZASI",
                          "CIL CIRAQ MAGAZA"                           ~ "CIL CIRAQ MAGAZASI",
                          "CINICI GEYIM MAGAZA"                        ~ "CINICI GEYIM MAGAZASI",
                          "CIRAGAN MAGAZA"                             ~ "CIRAGAN MAGAZASI",
                          "CIVIL GEYIM MAGAZA"                         ~ "CIVIL GEYIM MAGAZASI",
                          "CIVIL MAGAZA"                               ~ "CIVIL MAGAZASI",
                          "CLASS MAGAZA"                               ~ "CLASS MAGAZASI",
                          "CLOVER 3 MAGAZA"                            ~ "CLOVER 3 MAGAZASI",
                          "COCCODRILLO GEYIM MAGAZA"                   ~ "COCCODRILLO GEYIM MAGAZASI",
                          "COREK SATIS MAGAZA"                         ~ "COREK SATISI MAGAZASI",
                          "CS ELEKTRIK MAGAZA"                         ~ "CS ELEKTRIK MAGAZASI",
                          "CRESENT BEACH"                              ~ "CRESCENT BEACH",
                          .default = pos ))

## s herfi ####

fp_final = fp_final %>%
  mutate(pos = case_match(pos,
                          
                          "SUSE QAB QACAQ MAGAZA"                      ~ "SUSE QAB-QACAQ MAGAZASI",
                          "SUSE SATIS"                                 ~ "SUSE SATISI",
                          "SUVARI GEYIM MAGAZA"                        ~ "SUVARI GEYIM MAGAZASI",
                          "SUVARMA SISTEMLERI VE MATERIALLAR MAGAZA"   ~ "SUVARMA SISTEMLERI VE MATERIALLARI MAGAZASI",
                          "SUVELAN 5 RESTORAN"                         ~ "SUVELAN 5 RESTORANI",
                          "SUVENIR MAGAZA"                             ~ "SUVENIR MAGAZASI",
                          "SUVENIR MAGAZA SURAXANI GEMI MUZEYIN NEZDINDE" ~ "SUVENIR MAGAZASI SURAXANI GEMI MUZEYININ NEZDINDE",
                          "SUWEN QADIN ALT GEYIM MAGAZA"               ~ "SUWEN QADIN ALT GEYIMI MAGAZASI",
                          "SUWEN QADIN ALT GEYIMLERI MAGAZA"           ~ "SUWEN QADIN ALT GEYIMLERI MAGAZASI",
                          "SWAROVSKI MAGAZA"                           ~ "SWAROVSKI MAGAZASI",
                          "SWATCH MAGAZA"                              ~ "SWATCH MAGAZASI",
                          .default = pos ))

## t herfi ####

fp_final = fp_final %>%
  mutate(pos = case_match(pos,
                          
                          "TAAM KAFE"                                  ~ "TAAM KAFESI",
                          "TALASSEMIYA MERKEZ"                         ~ "TALASSEMIYA MERKEZI",
                          "TANTUNI DONER SATIS"                        ~ "TANTUNI DONER SATISI",
                          "TARGET ERZAQ MAGAZA"                        ~ "TARGET ERZAQ MAGAZASI",
                          "TARIK USTA RESTORAN"                        ~ "TARIK USTA RESTORANI",
                          "TAXTA MATERIALLAR SATIS"                    ~ "TAXTA MATERIALLARI SATISI",
                          "TEBIB APTEK"                                ~ "TEBIB APTEKI",
                          "TEDRIS STOMATOLOJI KLINIKA"                 ~ "TEDRIS STOMATOLOJI KLINIKASI",
                          "TEFAL MAGAZA"                               ~ "TEFAL MAGAZASI",
                          "TEKA MAGAZA"                                ~ "TEKA MAGAZASI",
                          "TEKER SATIS"                                ~ "TEKER SATISI",
                          "TEKER SATIS MAGAZA"                         ~ "TEKER SATISI MAGAZASI",
                          "TELEFON AKSESSUARLARI SATIS"                ~ "TELEFON AKSESSUARLARI SATISI",
                          "TELEFON MAGAZA"                             ~ "TELEFON MAGAZASI",
                          "TERMOKLIMAT SATIS VE SERVIS MERKEZ"         ~ "TERMOKLIMAT SATIS VE SERVIS MERKEZI",
                          "TESEERUFAT MALLARI MAGAZA"                  ~ "TESERRUFAT MALLARI MAGAZASI",
                          "TESERRUFAT MALLARIN SATIS MAGAZA"           ~ "TESERRUFAT MALLARININ SATISI MAGAZASI",
                          "TIBB AVADANLIQLAR MAGAZA"                   ~ "TIBBI AVADANLIQLAR MAGAZASI",
                          "TIBB AVADANLIQLARIN SATIS"                  ~ "TIBBI AVADANLIQLARIN SATISI",
                          "TIBB LABARATORIYA"                          ~ "TIBBI LABORATORIYA",
                          "TIBB LEVAZIMATLAR MAGAZA"                   ~ "TIBBI LEVAZIMATLAR MAGAZASI",
                          "TIBB MERKEZ"                                ~ "TIBBI MERKEZI",
                          "TICARET MAGAZA"                             ~ "TICARET MAGAZASI",
                          "TIKINITI MARERIALLARI MAGAZA"               ~ "TIKINTI MATERIALLARI MAGAZASI",
                          "TIKINTI INSAAT MATERIALLAR MAGAZA"          ~ "TIKINTI INSAAT MATERIALLARI MAGAZASI",
                          "TIKINTI MAGAZA"                             ~ "TIKINTI MAGAZASI",
                          "TIKINTI MALLARI MAGAZA"                     ~ "TIKINTI MALLARI MAGAZASI",
                          "TIKINTI MATERIALLAR MAGAZA"                 ~ "TIKINTI MATERIALLARI MAGAZASI",
                          "TIKINTI MATERIALLAR SATIISI MAGAZA"         ~ "TIKINTI MATERIALLARI SATISI MAGAZASI",
                          "TIKIS LEVAZIMATLARININ SATIS MAGAZA"        ~ "TIKIS LEVAZIMATLARININ SATISI MAGAZASI",
                          "TISSOT MAGAZA"                              ~ "TISSOT MAGAZASI",
                          "TOPDAN SATIS"                               ~ "TOPDAN SATISI",
                          "TOPDAN SATIS MAGAZA"                        ~ "TOPDAN SATISI MAGAZASI",
                          "TOPDANSATISI"                               ~ "TOPDAN SATISI",
                          "TOXUM MAGAZA"                               ~ "TOXUM MAGAZASI",
                          "TOXUMLARIN SATIS MAGAZA"                    ~ "TOXUMLARIN SATISI MAGAZASI",
                          "TRAKTOR EHTIYAT HISSE"                      ~ "TRAKTOR EHTIYAT HISSELERI",
                          "TRENAJOR MAGAZA"                            ~ "TRENAJOR MAGAZASI",
                          "TURAL FIRMA MAGAZA"                         ~ "TURAL FIRMA MAGAZASI",
                          "TUTUN MEHSULLARI VE SPIRTLI ICKILER MAGAZA" ~ "TUTUN MEHSULLARI VE SPIRTLI ICKILER MAGAZASI",
                          .default = pos ))

## g herfi ####

fp_final = fp_final %>%
  mutate(pos = case_match(pos,
                          
                          "G GROUP PLASTIK MEMULATLAR ISTEHSALI SAHESI" ~ "G GROUP PLASTIK MEMULATLAR ISTEHSALI SAHESI",
                          "GARDEN COSMETIC XACMAZ MAGAZA"              ~ "GARDEN COSMETIC XACMAZ MAGAZASI",
                          "GENCE APTEK"                                ~ "GENCE APTEKI",
                          "GENCE EDVIYYAT EV"                          ~ "GENCE EDVIYYAT EVI",
                          "GENCE MAGAZA"                               ~ "GENCE MAGAZASI",
                          "GENCE S 2 SAYLI MAGAZA"                     ~ "GENCE S 2 SAYLI MAGAZASI",
                          "GENCE SERAB 2 ASC NIN 2 SAYLI FIRMA MAGAZA" ~ "GENCE SERAB 2 ASC NIN 2 SAYLI FIRMA MAGAZASI",
                          "GENCE SERAB 2 ASC NIN MAGAZA"               ~ "GENCE SERAB 2 ASC NIN MAGAZASI",
                          "GENCLIK ELEKTRIK MEISET AVADANLIQLARI MAGAZA" ~ "GENCLIK ELEKTRIK MEISET AVADANLIQLARI MAGAZASI",
                          "GENCLIK MG"                                 ~ "GENCLIK MAGAZASI",
                          "GEYIM MAGAZA"                               ~ "GEYIM MAGAZASI",
                          "GLAMPIRE TIMEPIECES ZERGERLIK MAGAZA"       ~ "GLAMPIRE TIMEPIECES ZERGERLIK MAGAZASI",
                          "GLORY MEBEL MAGAZA"                         ~ "GLORY MEBEL MAGAZASI",
                          "GOLD BREND PARFUMERIYA MAGAZA"              ~ "GOLD BREND PARFUMERIYA MAGAZASI",
                          "GOLD BREND PARFUMEYIA"                      ~ "GOLD BREND PARFUMERIYA",
                          "GOLD HOUSE DAM ORTUKLERI MAGAZA"            ~ "GOLD HOUSE DAM ORTUKLERI MAGAZASI",
                          "GOLDEN ROSE KOSMETIKA MAGAZA"               ~ "GOLDEN ROSE KOSMETIKA MAGAZASI",
                          "GOLDEN ROSE MAGAZA"                         ~ "GOLDEN ROSE MAGAZASI",
                          "GOOD DOG CAT ZOO MAGAZA"                    ~ "GOOD DOG CAT ZOO MAGAZASI",
                          "GORAN MAGAZA"                               ~ "GORAN MAGAZASI",
                          "GORANBOY ELEKTRONIKS"                       ~ "GORANBOY ELECTRONICS",
                          "GOYCAY APTEK 2"                             ~ "GOYCAY APTEKI 2",
                          "GOYCAY MAGAZA"                              ~ "GOYCAY MAGAZASI",
                          "GOYCAY STORE MAGAZA"                        ~ "GOYCAY STORE MAGAZASI",
                          "GOYGOL SERAB ZAVODU FIRMA MAGAZA"           ~ "GOYGOL SERAB ZAVODU FIRMA MAGAZASI",
                          "GOZELLIK SALON VE KOSMETOLOJI AVADANLIQLARIN P S" ~ "GOZELLIK SALONU VE KOSMETOLOJI AVADANLIQLARIN SATISI",
                          "GREEN HOUSE SADLIQ EV"                      ~ "GREEN HOUSE SADLIQ EVI",
                          "GREEN LIFE BITGI VE AGAC SATIS"             ~ "GREEN LIFE BITKI VE AGAC SATISI",
                          "GREENVVICH AKSESUAR MAGAZA"                 ~ "GREENWICH AKSESUAR MAGAZASI",
                          "GREYDER AYAQQABI MAGAZA"                    ~ "GREYDER AYAQQABI MAGAZASI",
                          "GSN ELECTRIC MAGAZA"                        ~ "GSN ELECTRIC MAGAZASI",
                          "GUL MAGAZA"                                 ~ "GUL MAGAZASI",
                          "GUL SALON"                                  ~ "GUL SALONU",
                          "GUNDELIK ET SUD MAGAZA"                     ~ "GUNDELIK ET-SUD MAGAZASI",
                          "GUNEL APTEK"                                ~ "GUNEL APTEKI",
                          "GUNELNUR MAGAZA"                            ~ "GUNELNUR MAGAZASI",
                          "GUNES EYNEKLERI MAGAZA"                     ~ "GUNES EYNEKLERI MAGAZASI",
                          .default = pos ))


## e herfi ####

fp_final = fp_final %>%
  mutate(pos = case_match(pos,
                          
                          "ECCENTRIC MAGAZA"                           ~ "ECCENTRIC MAGAZASI",
                          "ECEMI MAGAZA"                               ~ "ECEMI MAGAZASI",
                          "ECODENT KLINIKA"                            ~ "ECODENT KLINIKASI",
                          "ECZACILIQ MALLAR PERAKENDE SATIS"           ~ "ECZACILIQ MALLARININ PERAKENDE SATISI",
                          "EDIYAL MAGAZA"                              ~ "EDIYAL MAGAZASI",
                          "EDVIYYAT MAGAZA"                            ~ "EDVIYYAT MAGAZASI",
                          "EHMED RECEBLI MAGAZA"                       ~ "EHMED RECEBLI MAGAZASI",
                          "EHMEDLI 2 SAYLI MAGAZA"                     ~ "EHMEDLI 2 SAYLI MAGAZASI",
                          "EHT HISS MAGAZA"                            ~ "EHTIYAT HISSELERI MAGAZASI",
                          "EHTIYAT HISSE MAGAZA"                       ~ "EHTIYAT HISSELERI MAGAZASI",
                          "EHTIYAT HISSE PERAKENDE SATIS VE AVTOSERV"  ~ "EHTIYAT HISSELERI PERAKENDE SATISI VE AVTOSERVISI",
                          "EKONOM MARKET MAGAZA"                       ~ "EKONOM MARKET MAGAZASI",
                          "EKSKLUZIV ELEKTRIK MALLARI MAGAZA"          ~ "EKSKLUZIV ELEKTRIK MALLARI MAGAZASI",
                          "EL TOPDAN SATIS"                            ~ "EL TOPDAN SATISI",
                          "ELEKTRIK AVADANLIQLARININ SATIS MAGAZA"     ~ "ELEKTRIK AVADANLIQLARININ SATISI MAGAZASI",
                          "ELEKTRIK CIHAZLARI MAGAZA"                  ~ "ELEKTRIK CIHAZLARI MAGAZASI",
                          "ELEKTRIK MALLAR SATIS"                      ~ "ELEKTRIK MALLARI SATISI",
                          "ELEKTRIK MALLAR SATSI"                      ~ "ELEKTRIK MALLARI SATISI",
                          "ELEKTRIK MALLARI MAGAZA"                    ~ "ELEKTRIK MALLARI MAGAZASI",
                          "ELEKTRIK MEISET MAGAZA"                     ~ "ELEKTRIK MEISET MAGAZASI",
                          "ELEKTRONIKA MAGAZA"                         ~ "ELEKTRONIKA MAGAZASI",
                          "ELEPHANT ELECTRONIKS MAGAZA"                ~ "ELEPHANT ELECTRONICS MAGAZASI",
                          "ELI VE NINO KITAB EV"                       ~ "ALI VE NINO KITAB EVI",
                          "ELI VE NINO KITAB MAGAZA"                   ~ "ALI VE NINO KITAB MAGAZASI",
                          "ELIKSIR APTEK"                              ~ "ELIKSIR APTEKI",
                          "ELIT APTEK"                                 ~ "ELIT APTEKI",
                          "ELITA RESTORAN"                             ~ "ELITA RESTORANI",
                          "ELMED TIBB MERKEZ"                          ~ "ELMED TIBBI MERKEZI",
                          "ELVAN METALLARIN TOPDAN VE PERAKENDE SATIS" ~ "ELVAN METALLARIN TOPDAN VE PERAKENDE SATISI",
                          "EMBAWOOD MEBEL EV"                          ~ "EMBAWOOD MEBEL EVI",
                          "EMBAWOOD MEBEL MAGAZA"                      ~ "EMBAWOOD MEBEL MAGAZASI",
                          "EMBAWOOD MEBEL SALON"                       ~ "EMBAWOOD MEBEL SALONU",
                          "EMBAWOOD MEBEL SATIS MAGAZA"                ~ "EMBAWOOD MEBEL SATISI MAGAZASI",
                          "EMILAND GEYIM MAGAZA"                       ~ "EMILAND GEYIM MAGAZASI",
                          "EMINE MARKET ERZAQ MAGAZA"                  ~ "EMINE MARKET ERZAQ MAGAZASI",
                          "EN UCUZ TICARET MERKEZ"                     ~ "EN UCUZ TICARET MERKEZI",
                          "ENTERTAINER 28 MALLDA OYUNCAQ MAGAZA"       ~ "ENTERTAINER 28 MALLDA OYUNCAQ MAGAZASI",
                          "ENTREE RESTORAN"                            ~ "ENTREE RESTORANI",
                          "ERA ELECTRONIKS MEHDIABAD"                  ~ "ERA ELECTRONICS MEHDIABAD",
                          "ERZAG MAGAZASI"                             ~ "ERZAQ MAGAZASI",
                          "ERZAQ MALLAR SATIS"                         ~ "ERZAQ MALLARI SATISI",
                          "ERZAQ MEHSUL PERAKENDE SATIS MAGAZA"        ~ "ERZAQ MEHSULLARI PERAKENDE SATISI MAGAZASI",
                          "ERZAQ VE QEYRI ERZAQ MALLAR PERAKENDE SATESI" ~ "ERZAQ VE QEYRI-ERZAQ MALLARI PERAKENDE SATISI",
                          "ESITME APARATLARININ SATIS"                 ~ "ESITME APARATLARININ SATISI",
                          "ET VE SUD MEHSULLARI MAGAZA"                ~ "ET VE SUD MEHSULLARI MAGAZASI",
                          "ETALON INSAAT MAGAZA"                       ~ "ETALON INSAAT MAGAZASI",
                          "ETAM MAGAZA"                                ~ "ETAM MAGAZASI",
                          "ETIR MAGAZA"                                ~ "ETIR MAGAZASI",
                          "ETRIYYAT MAGAZA"                            ~ "ETRIYYAT MAGAZASI",
                          # Ali ve Nino düzəlişləri
                          "ELI VE NINO DENIZ MALL"                     ~ "ALI VE NINO DENIZ MALL",
                          "ELI VE NINO KITAB EV"                       ~ "ALI VE NINO KITAB EVI",
                          "ELI VE NINO KITAB MAGAZA"                   ~ "ALI VE NINO KITAB MAGAZASI",
                          "ELI VE NINO KITAB MAGAZA METRO PARK"        ~ "ALI VE NINO KITAB MAGAZASI METRO PARK",
                          "ELI VE NINO SUMQAYIT"                       ~ "ALI VE NINO SUMQAYIT",
                          "ELI VE NINO ZEFIR MALL"                     ~ "ALI VE NINO ZEFIR MALL",
                          .default = pos ))

## b herfi ####

fp_final = fp_final %>%
  mutate(pos = case_match(pos,
                        
                          "BABEK PERAKENDE SATIS"                      ~ "BABEK PERAKENDE SATISI",
                          "BAHAR OPTIC MAGAZA"                         ~ "BAHAR OPTIK MAGAZASI",
                          "BAHAR OPTIK MAGAZA"                         ~ "BAHAR OPTIK MAGAZASI",
                          "BAHAR STORE MAGAZA NIZAMI KUCESI"           ~ "BAHAR STORE MAGAZASI NIZAMI KUCESI",
                          "BAKI MUSIQI ALETLERI MAGAZA"                ~ "BAKI MUSIQI ALETLERI MAGAZASI",
                          "BAKI SAGLAMLIQ MERKEZ"                      ~ "BAKI SAGLAMLIQ MERKEZI",
                          "BAKU ELECTRONIKS MAGAZA"                    ~ "BAKU ELECTRONICS MAGAZASI",
                          "BAKU ELECTRONIKS"                           ~ "BAKU ELECTRONICS",
                          "BALDININI AYAQQABI MAGAZA"                  ~ "BALDININI AYAQQABI MAGAZASI",
                          "BALIQ MAGAZA"                               ~ "BALIQ MAGAZASI",
                          "BALIQ MEHSULLARI MAGAZA"                    ~ "BALIQ MEHSULLARI MAGAZASI",
                          "BALMART ERZAQ MAGAZA"                       ~ "BALMART ERZAQ MAGAZASI",
                          "BATA AYAQQABI MAGAZA"                       ~ "BATA AYAQQABI MAGAZASI",
                          "BAYQIDA MAGAZA"                             ~ "BAYQIDA MAGAZASI",
                          "BAYTARLIQ APTEK"                            ~ "BAYTARLIQ APTEKI",
                          "BAYTARLIQ MAGAZA"                           ~ "BAYTARLIQ MAGAZASI",
                          "BAZARSTORE ERZAQ MAGAZA"                    ~ "BAZARSTORE ERZAQ MAGAZASI",
                          "BAZARSTORE M EHMEDLI MAGAZA"                ~ "BAZARSTORE M EHMEDLI MAGAZASI",
                          "BAZARSTORE M XUTOR MAGAZA"                  ~ "BAZARSTORE M XUTOR MAGAZASI",
                          "BEHRUD MAGAZA"                              ~ "BEHRUD MAGAZASI",
                          "BEHRUD TIKINTI MATERIALLAR MAGAZA"          ~ "BEHRUD TIKINTI MATERIALLARI MAGAZASI",
                          "BELARUSKI ERZAQ MAGAZA"                     ~ "BELARUSKI ERZAQ MAGAZASI",
                          "BELLONA MEBEL MAGAZA"                       ~ "BELLONA MEBEL MAGAZASI",
                          "BERDE MAGAZA"                               ~ "BERDE MAGAZASI",
                          "BERLIN MAGAZA"                              ~ "BERLIN MAGAZASI",
                          "BERSHKA GEYIM MAGAZA"                       ~ "BERSHKA GEYIM MAGAZASI",
                          "BERSHKA MAGAZA"                             ~ "BERSHKA MAGAZASI",
                          "BETA FIRMA MAGAZA"                          ~ "BETA FIRMA MAGAZASI",
                          "BEYLISS GEYIM MAGAZA"                       ~ "BEYLISS GEYIM MAGAZASI",
                          "BF HOME MEISET ESYALARI MAGAZA"             ~ "BF HOME MEISET ESYALARI MAGAZASI",
                          "BIJUTERIYA MAGAZA"                          ~ "BIJUTERIYA MAGAZASI",
                          "BILECERI MAGAZA"                            ~ "BILECERI MAGAZASI",
                          "BILFA MAGAZA"                               ~ "BILFA MAGAZASI",
                          "BILLUR MAGAZA"                              ~ "BILLUR MAGAZASI",
                          "BIOCOS 3 KOSMETIK"                          ~ "BIOCOS 3 KOSMETIKA",
                          "BIZIM INSAAT MAGAZA"                        ~ "BIZIM INSAAT MAGAZASI",
                          "BOL INSAAT TIKINTI MATERIALLAR MAGAZA"      ~ "BOL INSAAT TIKINTI MATERIALLARI MAGAZASI",
                          "BOYA MAGAZA"                                ~ "BOYA MAGAZASI",
                          "BT 1 NOBEL MAGAZA VE ANBAR"                 ~ "BT 1 NOBEL MAGAZASI VE ANBARI",
                          "BT 2 BAKIXANOV MAGAZA VE ANBAR"             ~ "BT 2 BAKIXANOV MAGAZASI VE ANBARI",
                          "BT 4 XETAI ANBAR VE MAGAZA"                 ~ "BT 4 XETAI ANBARI VE MAGAZASI",
                          "BT 5 EHMEDLI MAGAZA VE ANBAR"               ~ "BT 5 EHMEDLI MAGAZASI VE ANBARI",
                          "BULGARY MAGAZA"                             ~ "BVLGARI MAGAZASI",
                          "BUTY SHOES AYAQQABI MAGAZA"                 ~ "BYT SHOES AYAQQABI MAGAZASI",
                          .default = pos ))

## m herfi ####

fp_final = fp_final %>%
  mutate(pos = case_match(pos,
                          
                          "MACROBIOSIS ANTIAGE AND ESTHETIK CENTER"    ~ "MACROBIOSIS ANTIAGE AND AESTHETIC CENTER",
                          "MADAM COCO DECORIA MAGAZA"                  ~ "MADAM COCO DECORIA MAGAZASI",
                          "MADEYRA MEBEL SALON"                        ~ "MADEYRA MEBEL SALONU",
                          "MADO DONDURMA EV"                           ~ "MADO DONDURMA EVI",
                          "MAGAZA ADEM INSAAT 2"                       ~ "MAGAZASI ADEM INSAAT 2",
                          "MAGAZA ANBAR TOPDAN SATIS"                  ~ "MAGAZA ANBARI TOPDAN SATISI",
                          "MAGAZA EHTIYAT HISSE"                       ~ "EHTIYAT HISSELERI MAGAZASI",
                          "MAGAZA OPTIMAL ELEKTRONIKS"                 ~ "OPTIMAL ELECTRONICS MAGAZASI",
                          "MAGAZA TIKINTI MATERIALLAR SATIS"           ~ "TIKINTI MATERIALLARI SATISI MAGAZASI",
                          "MAGAZSTORE"                                 ~ "MAGAZA STORE",
                          "MAISON DE LUX MAGAZA"                       ~ "MAISON DE LUX MAGAZASI",
                          "MAJESTIC MAGAZA"                            ~ "MAJESTIC MAGAZASI",
                          "MANGO GEYIM MAGAZA"                         ~ "MANGO GEYIM MAGAZASI",
                          "MARICEL PARK RESTAURANT 2"                  ~ "MARICEL PARK RESTORANI 2",
                          "MARKET ERZAQ MAGAZA"                        ~ "ERZAQ MARKET MAGAZASI",
                          "MARKETVE ICARE"                             ~ "MARKET VE ICARE",
                          "MASAZIR ELEKTRONIKA"                        ~ "MASAZIR ELECTRONICS",
                          "MASSIMO DUTTI GEYIM MAGAZA"                 ~ "MASSIMO DUTTI GEYIM MAGAZASI",
                          "MASTER INSAAT MAGAZA"                       ~ "MASTER INSAAT MAGAZASI",
                          "MASTIB BAKU RESTORAN"                       ~ "MASTIB BAKU RESTORANI",
                          "MAVI GEYIM MAGAZA"                          ~ "MAVI GEYIM MAGAZASI",
                          "MAYORAL USAQ PALTARLARI MAGAZA"             ~ "MAYORAL USAQ PALTARLARI MAGAZASI",
                          "MCDONALDS SUMQAYIT RESTORAN"                ~ "MCDONALDS SUMQAYIT RESTORANI",
                          "MEBEL AKSESUARLARININ SATIS"                ~ "MEBEL AKSESSUARLARININ SATISI",
                          "MEBEL MAGAZA"                               ~ "MEBEL MAGAZASI",
                          "MEBEL VE AKSESSUAR MAGAZA"                  ~ "MEBEL VE AKSESSUAR MAGAZASI",
                          "MEDICAL LIFE STOMOTOLOGIYA MERKEZ"          ~ "MEDICAL LIFE STOMATOLOGIYA MERKEZI",
                          "MEDICLUB KLINIKA"                           ~ "MEDICLUB KLINIKASI",
                          "MEISET AVADANLIQLARI MAGAZA"                ~ "MEISET AVADANLIQLARI MAGAZASI",
                          "MEISET VE ELEKTRIK AVADANLIQLARININ PERAKENDE SATI" ~ "MEISET VE ELEKTRIK AVADANLIQLARININ PERAKENDE SATISI",
                          "MELEK APTEK"                                ~ "MELEK APTEKI",
                          "MERKEZ APTEK"                               ~ "MERKEZ APTEKI",
                          "MERMER SEXI VE SATIS"                       ~ "MERMER SEXI VE SATISI",
                          "MERSEDES BENZ AVTO EHTIYAT HISSE"           ~ "MERCEDES-BENZ AVTO EHTIYAT HISSELERI",
                          "METAL MEMULATLARININ PERAKENDE SATIS"       ~ "METAL MEMULATLARININ PERAKENDE SATISI",
                          "MEYSERI MEHSULLARI MAGAZA"                  ~ "MEYSERI MEHSULLARI MAGAZASI",
                          "MICROSYST KLINIKA"                          ~ "MICROSYST KLINIKASI",
                          "MINGECEVIR 11 SAYLI FIRMA MAGAZA"           ~ "MINGECEVIR 11 SAYLI FIRMA MAGAZASI",
                          "MORENA BALIQ MAGAZA"                        ~ "MORENA BALIQ MAGAZASI",
                          "MOTHERCARE MAGAZA"                          ~ "MOTHERCARE MAGAZASI",
                          "MUASIR DIAGNOSTIKA KLINIKA"                 ~ "MUASIR DIAGNOSTIKA KLINIKASI",
                          "MURAD INSAAT"                               ~ "MURAD INSAATI",
                          "MURADLI INSAAT MAGAZA N 1"                  ~ "MURADLI INSAAT MAGAZASI N 1",
                          "MURELLA ELISMAN OBOY MAGAZA"                ~ "MURELLA ELISMAN OBOY MAGAZASI",
                          "MY SHOPS MAGAZA"                            ~ "MY SHOPS MAGAZASI",
                          .default = pos ))

## o herfi ####

fp_final = fp_final %>%
  mutate(pos = case_match(pos,
                          # OA -> OBA düzəlişləri
                          "OA MARKET HEZI ASLANOV 14"                  ~ "OBA MARKET HEZI ASLANOV 14",
                          "OA MARKET QARACUXUR 1"                      ~ "OBA MARKET QARACUXUR 1",
                          # O hərfi üzrə düzəlişlər
                          "OBA AMERKET"                                ~ "OBA MARKET",
                          "OBA ERZAQ MAGAZA"                           ~ "OBA ERZAQ MAGAZASI",
                          "OBA MAREKT SEMKIR"                          ~ "OBA MARKET SEMKIR",
                          "OBA MAREKT XETAI 5"                         ~ "OBA MARKET XETAI 5",
                          "OBA MARKETBUZOVNA 1"                        ~ "OBA MARKET BUZOVNA 1",
                          "OBA MARKETET SALYAN 1"                      ~ "OBA MARKET SALYAN 1",
                          "OBA MARKET MAREKT KOHNE GUNESLI 7"          ~ "OBA MARKET KOHNE GUNESLI 7",
                          "OBOY MAGAZA"                                ~ "OBOY MAGAZASI",
                          "OBOY VE TIKINTI MATERIALLAR SATIS"          ~ "OBOY VE TIKINTI MATERIALLARI SATISI",
                          "OFIS LEVAZIMATLARI MAGAZA"                  ~ "OFIS LEVAZIMATLARI MAGAZASI",
                          "OFIS TOPDAN SATIS"                          ~ "OFIS TOPDAN SATISI",
                          "OFIS VE MEBEL SATIS SALON"                  ~ "OFIS VE MEBEL SATISI SALONU",
                          "OKI XESTEXANA"                              ~ "OKI XESTEXANASI",
                          "OKSIGEN YENI TIBB MERKEZ"                   ~ "OKSIGEN YENI TIBBI MERKEZI",
                          "OPTIMAL ELEKTRONIKA MAGAZA"                 ~ "OPTIMAL ELEKTRONIKA MAGAZASI",
                          "OPTIMAL ELECTRONIKS"                        ~ "OPTIMAL ELECTRONICS",
                          "OPTIMAL MEISET TEXNIKA MAGAZA"              ~ "OPTIMAL MEISET TEXNIKASI MAGAZASI",
                          "ORAL B TIBBI LEVAZIMATLARIN SATISI"         ~ "ORAL-B TIBBI LEVAZIMATLARININ SATISI",
                          "ORTOPED XESTEXANA"                          ~ "ORTOPED XESTEXANASI",
                          "OYUNCAQ MAGAZA"                             ~ "OYUNCAQ MAGAZASI",
                          "OYUNCAQ MAGAZA 2 ICARE OBYEKTI"             ~ "OYUNCAQ MAGAZASI 2 ICARE OBYEKTI",
                          "OZDILEK MAGAZA"                             ~ "OZDILEK MAGAZASI",
                          .default = pos ))

## a herfi ####

fp_final = fp_final %>%
  mutate(pos = case_match(pos,
                          
                          "A PLUS MASAZIR 1 SUPERMARKET"               ~ "A PLUS MASAZIR 1 SUPERMARKETI",
                          "ABU LIFE ELEKTRONIKS MAGAZA"                ~ "ABU LIFE ELECTRONICS MAGAZASI",
                          "AEOLUS TEKER SATIS MAGAZA"                  ~ "AEOLUS TEKER SATISI MAGAZASI",
                          "AG SARAY"                                   ~ "AG SARAY",
                          "AGDAS APTEK"                                ~ "AGDAS APTEKI",
                          "AHMADLI MAGAZA"                             ~ "AHMEDLI MAGAZASI",
                          "AILE TICARET MERKEZ"                        ~ "AILE TICARET MERKEZI",
                          "AKKUMULYATOR SATIS VE SERVIS MENTEGESI"     ~ "AKKUMULYATOR SATISI VE SERVIS MENTEGESI",
                          "AL MARKET MAGAZA"                           ~ "AL MARKET MAGAZASI",
                          "ALCO HALL FIRMA DUKANI"                     ~ "ALCO HALL FIRMA DUKANI",
                          "ALLIANCE BUSINESS CENTRE DE KAFE"           ~ "ALLIANCE BUSINESS CENTRE KAFESI",
                          "ANBAR MAGAZA"                               ~ "ANBAR MAGAZASI",
                          "ANBAR VE SATIS"                             ~ "ANBAR VE SATISI",
                          "ANBAR VE PERAKENDE SATIS"                   ~ "ANBAR VE PERAKENDE SATISI",
                          "ARAZ MARKET VE SUB ICARE OBYEKTI"           ~ "ARAZ MARKET VE SUB-ICARE OBYEKTI",
                          "ARIZI MARKET"                               ~ "ARAZ MARKET",
                          "ASTORIA TIBB MERKEZ VE ICARE"               ~ "ASTORIA TIBBI MERKEZI VE ICARE",
                          "ATU TEDRIS TERA PEVTIK KLINIKA"             ~ "ATU TEDRIS TERAPEVTIK KLINIKASI",
                          "AVROMART MAGAZA"                            ~ "AVROMART MAGAZASI",
                          "AVTO EHTIYAT HISSE"                         ~ "AVTO EHTIYAT HISSELERI",
                          "AVTO EHTIYAT HISSE SATIS MAGAZA"            ~ "AVTO EHTIYAT HISSELERI SATISI MAGAZASI",
                          "AVTO SERVIS"                                ~ "AVTO SERVISI",
                          "AVTO START SATIS VE ICARE OBYEKTI"          ~ "AVTO START SATISI VE ICARE OBYEKTI",
                          "AVTOYEDEK EHTIYAT HISSE"                    ~ "AVTOYEDEK EHTIYAT HISSELERI",
                          "AVTOMOBIL SATIS VE TEXNIKI XIDMET"          ~ "AVTOMOBIL SATISI VE TEXNIKI XIDMETI",
                          "AY MARKER"                                  ~ "AY MARKET",
                          "AZERCELL EKSKLUZIV MAGAZA"                  ~ "AZERCELL EKSKLUZIV MAGAZASI",
                          .default = pos ))
