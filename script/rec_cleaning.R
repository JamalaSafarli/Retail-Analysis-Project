library(data.table)
library(stringi)

fp_rec_clean = copy(fp_receipts) 
setDT(fp_rec_clean)

fp_rec_clean[, pos := {

  res = stri_replace_all_regex(pos, "\\bldman\\b", "idman")
  res = stri_replace_all_regex(pos, "\\bIdman\\b", "idman")
  res = stri_replace_all_regex(pos, "\\bıdman\\b", "idman")
  res = stri_replace_all_regex(pos, "\\bIdman\\b", "idman")

  res = stri_replace_all_fixed(res, "İ", "i")
  res = stri_replace_all_fixed(res, "I", "ı")
  res = stri_trans_tolower(res)

  res = stri_replace_all_fixed(res, "kıng", "king")

  res = stri_replace_all_regex(res, "\\?", "ə")
  
  res = stri_replace_all_regex(res, "super[ -]?market|supermarkeet", "supermarket")
  res = stri_replace_all_regex(res, "\\bmarkeet\\b", "market")
  res = stri_replace_all_regex(res, "ma[gğ]aza(si|sı|i)", "mağazası")
  res = stri_replace_all_regex(res, "\\bma[gğ]aza\\b", "mağaza")
  res = stri_replace_all_regex(res, "ticaret|ticarət", "ticarət")
  res = stri_replace_all_regex(res, "subicare|subicarə", "subicarə")
  res = stri_replace_all_regex(res, "[[:punct:]]", " ")
  res = stri_replace_all_regex(res, "\\s+", " ")
  res = stri_trim_both(res)
  stri_trans_toupper(res, locale = "az")
}]


fp_rec_clean[, product_name := {
  
  res = stri_replace_all_fixed(product_name, "İ", "i")
  res = stri_replace_all_fixed(res, "I", "ı")
  res = stri_trans_tolower(res)
  res = stri_replace_all_regex(res, "\\bkg\\b", "kq")
  res = stri_replace_all_regex(res, "\\blt\\b|\\bltr\\b", "l")
  res = stri_replace_all_regex(res, "\\bgr\\b", "q")
  
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
  res = stri_replace_all_regex(res, "\\bcorek\\b", "çörək")
  res = stri_replace_all_regex(res, "\\bsuse\\b", "şüşə")
  res = stri_replace_all_regex(res, "\\bsise\\b", "şüşə")
  res = stri_replace_all_regex(res, "\\bcicek\\b", "çiçək")
  res = stri_replace_all_regex(res, "\\bsuni\\b", "süni")
  res = stri_replace_all_regex(res, "\\byag\\b", "yağ")
  res = stri_replace_all_regex(res, "\\bsebet\\b", "səbət")
  res = stri_replace_all_regex(res, "\\bpendir\\b", "pendir")
  res = stri_replace_all_regex(res, "[^a-z0-9əçğöüşı\\s()\\+]", " ")
  res = stri_replace_all_regex(res, "\\s+", " ")
  res = stri_trim_both(res)
  stri_trans_toupper(res, locale = "az")
}]


library(stringi)

fp_rec_clean[, product_name := stri_trans_toupper(product_name, locale = "az")]


# 2. Sütunların yaradılması (period və revenue) ####
fp_rec_clean[, `:=`(
  period = as.numeric(format(rec_date, "%Y%m")),
  revenue = qnt * unit_price
)]


