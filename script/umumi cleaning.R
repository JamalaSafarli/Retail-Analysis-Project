library(data.table)
setDT(fp_final)

fp_final[, rec_id := {

  prefix = fifelse(stri_detect_fixed(pos, "AL MARKET"), 
                   "AL", 
                   stri_sub(pos, 1, 3))
  
  prefix = stri_trans_toupper(prefix, locale = "az")
  paste0(prefix, "_", seq_len(.N))
}]

## category ####

library(dplyr)
library(stringr)

fp_final = fp_final %>%

  mutate(pos = toupper(trimws(pos))) %>% 
  
  mutate(category = case_when(
 
    str_detect(pos, "APTEK|KLINIK|CLINIC|HOSPITAL|MEDICAL|MEDIC|
               DOKTA|ALOE|ZEYTUN|ZEFERAN|FARM|PHARMA|AVIS|BUTA|BIOLAB|OPTIK|
               OPTIQ|REFERANS|LABORATOR|LABARATOR|BONA DEA|MEMORIAL|EYE|STOMATOLOG|
               DENTAL|TIBB|DIAGNOST|SAQLAM|MED|MZ|STOM|HEAL|VITA") 
    ~ "Səhiyyə (Aptek/Xəstəxana)",
    
    # 2. Restoran, Kafe və Fast-Food ####
    str_detect(pos, "KFC|MCDONALD|PIDEM|BURGER|KING|STARBUCKS
               |RESTORAN|CAFE|KAFE|KABAB|YEMEK|PIZZA|DOMINO|PAPA|ENTREE|
               PAUL|MADO|BAKE|ROLL|SULTAN|DONER|PUB|BAR|LOUNGE|BEER|STEAK|SHISHA|
               DONDURMA|CAKE|BULLA|WOK|LAVAZZA|CINNABON|TAAM|RESTORANI") 
    ~ "Restoran və Kafe",
    
    # 3. Supermarket Şəbəkələri ####
    str_detect(pos, "BRAVO|ARAZ|OBA|BAZARSTORE|RAHAT|SPAR|NEPTUN|
               AL MARKET|BOLMART|BIZIM|GRANDMART|GRAND MART|MEGASTORE|FAVORIT|
               AVROMART|WESTMAR|A PLUS|BEE GROSS|QAYALI|5\\+|PORT BAKU|SULTAN MARKET|
               HIPER|PROMART|MAKRO|BOL") 
    ~ "Supermarket Şəbəkələri",
    
    # 4. Geyim, Moda və Aksesuar ####
    str_detect(pos, "ZARA|LCW|WAIKIKI|BERSHKA|PULL|BEAR|STRADIVARIUS|MANGO|DEFACTO|
               FLO|GEYIM|RODRIGO|TERRANOVA|OKAIDI|MOTHERCARE|ALDO|NEW YORKER|COLINS|TRENDYOL|
               KOTON|ADIDAS|NIKE|PUMA|REEBOK|MASSIMO|DUTTI|LADY|QUIZ|SCIMENTO|SUWARI|SUWEN|YVES ROCHER|
               GUCCI|TOMMY|POLO|BATA|BYT SHOES") 
    ~ "Geyim və Moda",
    
    # 5. Ev, Elektronika və Mebel ####
    str_detect(pos, "IRSHAD|IRSAD|KONTAKT|BAKU ELECTRONICS|
               BAKU ELEKTRONICS|MUSIC GALLERY|SOLITON|INTEGRAL|HUAWEI|
               SAMSUNG|XIAOMI|EMBAWOOD|YAGMUR|MEBEL|EVIM|SELHOME|VILLEROY|QAB QACAQ|
               TEXNIKA|OPTIMAL|BELLONA|DOGTAS|ISTIQBAL|KELEBEK|WOODPECKER|MEBEL SALON|
               SANTEXNIK|AKSESUAR|PARKET|KUTAHYA|LENOVO|LENOVA|EVALAR|MADEYRA|TEFAL|TEKA|TISSOT") 
    ~ "Ev və Elektronika",
    
    # 6. Nəqliyyat, Yanacaq və Tikinti ####
    str_detect(pos, "SOCAR|AZPETROL|LUKOIL|PETROL|YDM|BOLT|
               UBER|TAKSI|FAB|TIKINTI|MATERIALLAR|MATERIAL|VOLKSWAGEN|
               AUTO|AVTO|YAG|SERVIS|TEMIR|DETAL|EHTIYAT|OIL|YAGLAMA|TOYOTA|
               MERCEDES|BMW|HYUNDAI|TAKER|WOLKSWAGEN|TRUCK|REMTEX|PLASTIK|PVC|KERPIC") 
    ~ "Nəqliyyat və Tikinti",
    
    # 7. Kitab, Ofis və Təhsil ####
    str_detect(pos, "ALI VE NINO|ELI VE NINO|
               KITAB|OFFICE|OFIS|DEFTERXANA|KIRTASIYE|
               STUDIO|MEKTEB|UNIVER") 
    ~ "Kitab, Ofis və Təhsil",
    
    # 8. Kosmetika və Şəxsi Qulluq ####
    str_detect(pos, "IDEAL|ADORE|SABINA|KOSMETIKA|PARFUM|
               BEAUTY|BARBER|KUAXYOR|FITNESS|SPA|GOZELLIK
               SALONU|SAKAL|BERBER") 
    ~ "Kosmetika və Şəxsi Qulluq",
    
    # 9. Xidmət, Əyləncə ####
    str_detect(pos, "AZERCELL|BAKCELL|NAR|CINEMA|PARK|
               MALL|BANK|POCT|KINO|BEYNELXALQ|VAGZAL|AVIABAKET|
               TURIZM|HOTEL|OTEL|AZAL|CASH|TERMINAL|ZOO|KINOZALI") 
    ~ "Xidmət və Əyləncə",
    
    # 10. Kiçik Marketlər və Digər Ticarət ####
    str_detect(pos, "MARKET|MAGAZA|BAQQAL|ERZAQ|TICARET|
               STORE|DUKAN|DUNYA|ALIS VERIS|KIOSK|KOSK|MALLARI|SATISI") 
    ~ "Kiçik Marketlər",
    
    TRUE ~ "Digər"))
