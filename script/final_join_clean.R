saveRDS(fp_map_clean, "fp_map_clean.rds")
saveRDS(fp_rec_clean, "fp_rec_clean.rds")
saveRDS(fp_final, "fp_final.rds") 

setDT(fp_rec_clean)
setDT(fp_map_clean)

fp_map_fixed = fp_map_clean %>%
  group_by(product, period) %>%
  summarise(
    product_type = first(product_type),
    profit_margin = mean(profit_margin, na.rm = TRUE),
    .groups = "drop")

fp_final = fp_rec_clean %>%
  left_join(
    fp_map_fixed,
    by = c("product_name" = "product",
           "period" = "period"))


fp_final[is.na(product_type), product_type := "QEYD OLUNMAYIB"]



## joinden sonraki cleaning ####

library(data.table)
library(stringi)


clean_logic = function(x) {
  
  res = stri_replace_all_fixed(x, "?", "ə")
  res = stri_replace_all_fixed(res, "İ", "i")
  res = stri_replace_all_fixed(res, "I", "ı")
  res = stri_trans_tolower(res)
  

  res = stri_replace_all_regex(res, "(\\d+(?:\\.\\d+)?)\\s*(kg|kgr|kilogram)\\b", "$1 kq")
  res = stri_replace_all_regex(res, "(\\d+(?:\\.\\d+)?)\\s*(gr|qr|g|gram)\\b", "$1 q")
  res = stri_replace_all_regex(res, "(\\d+(?:\\.\\d+)?)\\s*(lt|ltr|liter|l)\\b", "$1 l")
  res = stri_replace_all_regex(res, "(\\d+(?:\\.\\d+)?)\\s*(ml|mlt)\\b", "$1 ml")
  res = stri_replace_all_regex(res, "(\\d+)\\s*(eded|ededi|ed)\\b", "$1 ed")
  res = stri_replace_all_regex(res, "(\\d+)\\s*(cm|sm)\\b", "$1 sm")

  res = stri_replace_all_regex(res, "\\bshampun\\b|\\bsampun\\b", "şampun")
  res = stri_replace_all_regex(res, "\\bduyu\\b", "düyü")
  res = stri_replace_all_regex(res, "\\bsud\\b", "süd")
  res = stri_replace_all_regex(res, "\\bsise\\b", "şüşə")
  res = stri_replace_all_regex(res, "\\bcay\\b", "çay")
  res = stri_replace_all_regex(res, "\\bsokolad\\b|\\bchokolad\\b", "şokolad")
  res = stri_replace_all_regex(res, "\\bcorek\\b", "çörək")
  res = stri_replace_all_regex(res, "\\bcoreyi\\b", "çörəyi")
  res = stri_replace_all_regex(res, "\\btursusu\\b|\\btursu\\b", "turşusu")
  res = stri_replace_all_regex(res, "\\bteze\\b", "təzə")
  res = stri_replace_all_regex(res, "\\btoyuq\\b", "toyuq")
  res = stri_replace_all_regex(res, "\\baciq\\b", "açıq")
  res = stri_replace_all_regex(res, "\\balbali\\b", "albalı")
  res = stri_replace_all_regex(res, "\\bsaftali\\b", "şaftalı")
  res = stri_replace_all_regex(res, "\\bkesmik\\b", "kəsmik")
  res = stri_replace_all_regex(res, "\\bpndr\\b", "pendir")
  res = stri_replace_all_regex(res, "\\byag\\b|\\byagi\\b", "yağ")
  res = stri_replace_all_regex(res, "\\bsire\\b|\\bsiresi\\b", "şirəsi")
  res = stri_replace_all_regex(res, "\\bmeyve\\b", "meyvə")
  res = stri_replace_all_regex(res, "\\bterevez\\b", "tərəvəz")
  res = stri_replace_all_regex(res, "\\bhinduska\\b", "hinduşka")
  res = stri_replace_all_regex(res, "\\bldman\\b", "idman")
  
  res = stri_replace_all_regex(res, "[^a-z0-9əçğöüşı\\s()\\+\\.]", " ")
  res = stri_trim_both(stri_replace_all_regex(res, "\\s+", " "))
  
  stri_trans_toupper(res, locale = "az")
}

setDT(fp_final)

fp_final[, product_name := clean_logic(product_name)]


library(dplyr)
library(stringr)

fp_final = fp_final %>%

  mutate(pos = str_to_upper(pos, locale = "az")) %>%
  
  mutate(pos = str_replace_all(pos, c(
    "BIZIM"     = "BİZİM",
    "INSAAT"    = "İNŞAAT",
    "MAGAZA"    = "MAĞAZA",
    "ICARƏ"     = "İCARƏ",
    "SUBICARƏ"  = "SUBİCARƏ",
    "GÜNƏSLİ"   = "GÜNƏŞLİ",
    "GUNASHLİ"  = "GÜNƏŞLİ",
    "SIRVAN"    = "ŞİRVAN",
    "AGDAM"     = "AĞDAM",
    "AGSTAFA"   = "AĞSTAFA",
    "AGDAS"     = "AĞDAŞ",
    "AGCABƏDİ"  = "AĞCABƏDİ",
    "NEFÇALA"   = "NEFTÇALA",
    "XETAI"     = "XƏTAİ",
    "MASTAGA"   = "MAŞTAĞA",
    "MINGƏÇEVIR"= "MİNGƏÇEVİR",
    "ƏHMƏDLI"   = "ƏHMƏDLİ",
    "GƏNCLIK"   = "GƏNCLİK",
    "NIZAMI"    = "NİZAMİ",
    "INQILAB"   = "İNQİLAB"
  ))) %>%
  
  mutate(pos = str_replace_all(pos, "GRANMART", "GRANDMART")) %>%
  mutate(pos = str_squish(pos))