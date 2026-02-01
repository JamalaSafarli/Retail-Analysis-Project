product_summary <- fp_map[, .N, by = .(product)][order(product)]

library(data.table)
library(stringi)


setDT(fp_map_clean)

fp_map_clean[, product := {
  
  res = stri_replace_all_fixed(product, "İ", "i")
  res = stri_replace_all_fixed(res, "I", "ı")
  res = stri_trans_tolower(res)
  
  # Çəki 
  res = stri_replace_all_regex(res, "\\bkg\\b|\\bkgr\\b|\\bkilogram\\b", "kq")
  res = stri_replace_all_regex(res, "\\bgr\\b|\\bqr\\b|\\bg\\b|\\bgram\\b", "q")
  res = stri_replace_all_regex(res, "\\bmg\\b", "mq")
  
  # Həcm 
  res = stri_replace_all_regex(res, "\\blt\\b|\\bltr\\b|\\bliter\\b", "l")
  res = stri_replace_all_regex(res, "\\bml\\b|\\bmlt\\b", "ml")
  res = stri_replace_all_regex(res, "\\bcl\\b", "cl")
  
  # Say və Ölçü 
  res = stri_replace_all_regex(res, "\\beded\\b|\\bededi\\b|\\bed\\b", "ed")
  res = stri_replace_all_regex(res, "\\bcm\\b", "sm")
  res = stri_replace_all_regex(res, "\\bmm\\b", "mm")
  res = stri_replace_all_regex(res, "\\bmetr\\b|\\bmt\\b", "m")
  
  # Vahidlər 
  res = stri_replace_all_regex(res, "(\\d+)\\s*(kg|kgr|kilogram)\\b", "$1 kq")
  res = stri_replace_all_regex(res, "(\\d+)\\s*(gr|qr|g|gram)\\b", "$1 q")
  res = stri_replace_all_regex(res, "(\\d+)\\s*(mg)\\b", "$1 mq")
  res = stri_replace_all_regex(res, "(\\d+)\\s*(lt|ltr|liter|l)\\b", "$1 l")
  res = stri_replace_all_regex(res, "(\\d+)\\s*(ml|mlt)\\b", "$1 ml")
  res = stri_replace_all_regex(res, "(\\d+)\\s*(eded|ededi|ed)\\b", "$1 ed")
  res = stri_replace_all_regex(res, "(\\d+)\\s*(cm)\\b", "$1 sm")
  

  res = stri_replace_all_regex(res, "\\bldman\\b", "idman")
  res = stri_replace_all_regex(res, "\\bcorek\\b|\\bcorekci\\b", "çörək")
  res = stri_replace_all_regex(res, "\\bsuse\\b", "şüşə")
  res = stri_replace_all_regex(res, "\\bcicek\\b", "çiçək")
  res = stri_replace_all_regex(res, "\\bsampun\\b|\\bshampun\\b", "şampun")
  res = stri_replace_all_regex(res, "\\byag\\b|\\byagi\\b", "yağ")
  res = stri_replace_all_regex(res, "\\bhediyye\\b|\\bhediyyeli\\b", "hədiyyə")
  res = stri_replace_all_regex(res, "\\bpendiri\\b", "pendir")
  res = stri_replace_all_regex(res, "[^a-z0-9əçğöüşı\\s()\\+\\.]", " ")
  res = stri_replace_all_regex(res, "\\s+", " ")
  res = stri_trim_both(res)
  stri_trans_toupper(res, locale = "az")
}]




