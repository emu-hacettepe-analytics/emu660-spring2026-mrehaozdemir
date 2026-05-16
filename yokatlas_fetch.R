# ============================================================================
# YÖK Atlas API'sinden 2022-2025 Mühendislik Verisi Çekme
# ============================================================================
# Bu script YÖK Atlas'ın resmi JSON API'sine HTTP isteği atar, 
# mühendislik bölümlerinin son 4 yıllık verisini CSV olarak kaydeder.
#
# KULLANIM:
#   1. RStudio'da bu dosyayı aç
#   2. En üstteki install.packages satırını bir kere çalıştır (zaten varsa atla)
#   3. Tüm scripti çalıştır: Ctrl+Shift+Enter (veya Source butonuna bas)
#   4. Sonuç: çalışma dizininde "yokatlas_2022_2025.csv" oluşur
#
# NOT: Türkiye dışından çalışmaz (API geo-block). Türkiye'den OK.
# ============================================================================

# Gerekli paketler (bir kere yüklenmesi yeterli, varsa atlanır)
need <- c("httr2", "dplyr", "jsonlite")
new  <- need[!need %in% installed.packages()[, "Package"]]
if (length(new)) install.packages(new)

library(httr2)
library(dplyr)
library(jsonlite)

BASE_URL <- "https://yokatlas.yok.gov.tr"
UA <- "R-yokatlas-fetch/1.0"

# ----------------------------------------------------------------------------
# 1. Program gruplarını çek (her bölüm tipinin ID'sini bulmak için)
# ----------------------------------------------------------------------------
get_program_groups <- function() {
  resp <- request(paste0(BASE_URL, "/api/tercih-kilavuz/universite-programlar")) |>
    req_headers(Accept = "application/json", `User-Agent` = UA) |>
    req_timeout(60) |>
    req_perform()

  data <- resp_body_json(resp, simplifyVector = TRUE)
  as_tibble(data)
}

# ----------------------------------------------------------------------------
# 2. Search endpoint'ine sorgu at (tek sayfa)
# ----------------------------------------------------------------------------
search_page <- function(birim_grup_ids, puan_turu = "SAY", page = 0, size = 50) {
  body <- list(
    filters = list(
      puanTuru        = puan_turu,
      universiteId    = list(),
      birimGrupId     = as.list(as.integer(birim_grup_ids)),
      ilKodu          = list(),
      birimTuruId     = NULL,
      universiteTuru  = NULL,
      bursOraniId     = NULL,
      ogrenimTuruId   = NULL,
      kilavuzKodu     = NULL,
      minBasariSirasi = NULL,
      maxBasariSirasi = NULL
    ),
    page      = as.integer(page),
    size      = as.integer(size),
    sortBy    = "basariSirasi",
    direction = "ASC"
  )

  resp <- request(paste0(BASE_URL, "/api/tercih-kilavuz/search")) |>
    req_headers(Accept = "application/json",
                `Content-Type` = "application/json",
                `User-Agent` = UA) |>
    req_body_raw(toJSON(body, auto_unbox = TRUE, null = "null"),
                 type = "application/json") |>
    req_timeout(60) |>
    req_perform()

  resp_body_json(resp, simplifyVector = TRUE)
}

# ----------------------------------------------------------------------------
# 3. Tüm sayfaları sırayla çek
# ----------------------------------------------------------------------------
fetch_all <- function(birim_grup_ids, puan_turu = "SAY", size = 50) {
  all_rows <- list()
  page <- 0
  total_pages <- NA_integer_

  repeat {
    cat(sprintf("  Sayfa %d", page + 1))
    if (!is.na(total_pages)) cat(sprintf(" / %d", total_pages))
    cat(" çekiliyor...\n")

    resp <- search_page(birim_grup_ids, puan_turu, page, size)
    total_pages <- resp$totalPages %||% 1L

    content <- resp$content
    if (is.null(content) || (is.data.frame(content) && nrow(content) == 0)) break
    all_rows[[length(all_rows) + 1]] <- as_tibble(content)

    page <- page + 1
    if (page >= total_pages) break
    Sys.sleep(0.3)  # API'ye nazik ol
  }

  bind_rows(all_rows)
}

# ----------------------------------------------------------------------------
# 4. Geniş formattan uzun formata çevir (yıl başına 1 satır)
# ----------------------------------------------------------------------------
to_long_format <- function(df) {
  base_cols <- intersect(
    c("universiteAdi", "birimAdi", "ilAdi", "universiteTuru",
      "puanTuru", "yil", "kilavuzKodu", "birimGrupAdi"),
    names(df)
  )

  make_year_df <- function(suffix, offset) {
    sub <- df[, base_cols, drop = FALSE]
    sub$year <- sub$yil - offset

    # Mevcut yıl için: kontenjan + gkY (yerleşen)
    # Eski yıllar için: sadece gk1/2/3 (yerleşen); kontenjan API'de yok
    if (suffix == "") {
      sub$kontenjan <- suppressWarnings(as.numeric(df[["kontenjan"]]))
      sub$yerlesen  <- suppressWarnings(as.numeric(df[["gkY"]]))
    } else {
      sub$kontenjan <- NA_real_  # eski yıl kontenjanı API'de mevcut değil
      sub$yerlesen  <- suppressWarnings(as.numeric(df[[paste0("gk", suffix)]]))
    }

    sub$min_puan    <- suppressWarnings(as.numeric(df[[paste0("minPuan",      suffix)]]))
    sub$basari_sira <- suppressWarnings(as.numeric(df[[paste0("basariSirasi", suffix)]]))
    sub$yil <- NULL
    sub
  }

  bind_rows(
    make_year_df("",  0),
    make_year_df("1", 1),
    make_year_df("2", 2),
    make_year_df("3", 3)
  ) |>
    filter(!is.na(min_puan) | !is.na(basari_sira)) |>
    arrange(year, universiteAdi, birimAdi)
}

# ============================================================================
# ÇALIŞTIRMA
# ============================================================================
cat("\n=== 1. Program grupları çekiliyor ===\n")
groups <- get_program_groups()
cat(sprintf("Toplam %d program grubu bulundu.\n", nrow(groups)))

cat("\n=== 2. Mühendislik bölümleri filtreleniyor ===\n")
eng_groups <- groups |>
  filter(grepl("ühendisliği|ühendislik", birimGrupAdi, ignore.case = TRUE)) |>
  filter(puanTuru == "SAY")
cat(sprintf("Toplam %d mühendislik bölüm tipi:\n", nrow(eng_groups)))
print(eng_groups$birimGrupAdi)

cat("\n=== 3. Programlar çekiliyor (birkaç dakika sürebilir) ===\n")
df_wide <- fetch_all(eng_groups$birimGrupId, puan_turu = "SAY", size = 50)
cat(sprintf("Toplam %d program kaydı çekildi.\n", nrow(df_wide)))

cat("\n=== 4. Uzun formata çevriliyor ===\n")
df_long <- to_long_format(df_wide)
cat(sprintf("Toplam %d yıl-program kaydı.\n", nrow(df_long)))
cat("Yıl dağılımı:\n")
print(table(df_long$year))

cat("\n=== 5. CSV'ye kaydediliyor ===\n")
out_file <- "yokatlas_2022_2025.csv"
write.csv(df_long, out_file, row.names = FALSE, fileEncoding = "UTF-8")
cat(sprintf("✓ Tamamlandı: %s\n", normalizePath(out_file)))
cat(sprintf("  Sütunlar: %s\n", paste(names(df_long), collapse = ", ")))
