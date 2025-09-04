#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# run_weekly_sentiment.R
# ---------------------------------------------------------------------------
# * Renders weekly_tweet_report.Rmd  → HTML
# * Prints the HTML to PDF (pagedown + headless Chrome)
# * Uploads the PDF to Supabase    (bucket: weekly-numeric/YYYYwWW/…)
# * [Optional] Emails the PDF via Mailjet when SEND_EMAIL=true
# ---------------------------------------------------------------------------

## ── 0. Packages ─────────────────────────────────────────────────────────────
required <- c(
  # tidy data / plotting
  "tidyverse", "tidytext", "lubridate", "stringi", "kableExtra",
  "forcats", "widyr", "ggraph", "igraph",
  # tables & data wrangling
  "data.table",
  # text-analytics
  "sentimentr",
  # report generation
  "rmarkdown", "pagedown", "knitr",
  # API / I/O
  "jsonlite", "httr2", "DBI", "RPostgres", "base64enc"
)

invisible(lapply(required, function(p){
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p, repos = "https://cloud.r-project.org", quiet = TRUE)
  }
  library(p, character.only = TRUE)
}))

`%||%` <- function(a,b){
  if (isTRUE(is.na(a)) || (is.character(a) && !nzchar(a))) b else a
}

## ── 1.  config / env vars ─────────────────────────────────────────────────
# Optional email toggle
SEND_EMAIL <- tolower(Sys.getenv("SEND_EMAIL","false")) %in% c("1","true","yes")

# WEEK_START may be blank. If so, use previous Monday so the report covers a full Mon–Sun window.
w_env       <- Sys.getenv("WEEK_START")
week_start  <- suppressWarnings(as.Date(w_env)) %||%
  lubridate::floor_date(Sys.Date() - 7, unit = "week", week_start = 1)
week_end    <- week_start + 6

RMD_FILE <- "weekly_tweet_report.Rmd"
HTML_OUT <- "weekly_tweet_report.html"
PDF_OUT  <- "weekly_tweet_report.pdf"

# Supabase storage
SB_URL         <- Sys.getenv("SUPABASE_URL")
SB_STORAGE_KEY <- Sys.getenv("SUPABASE_SERVICE_ROLE")
SB_BUCKET      <- "weekly-numeric"   # adjust if you use a different bucket

# Mailjet (only required if SEND_EMAIL=true)
MJ_API_KEY    <- Sys.getenv("MJ_API_KEY")
MJ_API_SECRET <- Sys.getenv("MJ_API_SECRET")
MAIL_FROM     <- Sys.getenv("MAIL_FROM")
MAIL_TO       <- Sys.getenv("MAIL_TO")

# Require Supabase creds always (we still upload), Mailjet only if emailing
stopifnot(SB_URL != "", SB_STORAGE_KEY != "")

if (SEND_EMAIL) {
  stopifnot(MJ_API_KEY != "", MJ_API_SECRET != "", MAIL_FROM != "", MAIL_TO != "")
}

## ── 2.  knit Rmd → HTML ───────────────────────────────────────────────────
rmarkdown::render(
  input       = RMD_FILE,
  output_file = HTML_OUT,
  params      = list(week_start = week_start),
  quiet       = TRUE
)

## ── 3. HTML → PDF (pagedown) ───────────────────────────────────────────────
chrome_path <- Sys.getenv("CHROME_BIN")
if (!nzchar(chrome_path)) chrome_path <- pagedown::find_chrome()

extra <- c("--headless=new", "--disable-gpu", "--no-sandbox")

pagedown::chrome_print(
  input      = HTML_OUT,
  output     = PDF_OUT,
  browser    = chrome_path,
  extra_args = extra,
  timeout    = 20000
)

if (!file.exists(PDF_OUT))
  stop("❌ PDF not generated – ", PDF_OUT, " missing")

## ── 4.  Upload PDF to Supabase storage ─────────────────────────────────────
iso_folder <- strftime(week_start, "%YW%V")      # e.g. "2025W35"
file_name  <- sprintf("%s_to_%s.pdf",
                      format(week_start, "%Y-%m-%d"),
                      format(week_end  , "%Y-%m-%d"))
object_path <- file.path(iso_folder, file_name)

upload_url <- sprintf("%s/storage/v1/object/%s/%s?upload=1",
                      SB_URL, SB_BUCKET, object_path)

resp <- httr2::request(upload_url) |>
  httr2::req_method("POST") ||
  httr2::req_headers(
    Authorization  = sprintf("Bearer %s", SB_STORAGE_KEY),
    `x-upsert`     = "true",
    `Content-Type` = "application/pdf"
  ) |>
  httr2::req_body_file(PDF_OUT) |>
  httr2::req_perform()

if (httr2::resp_status(resp) >= 300) {
  cat("Supabase upload error body:\n",
      httr2::resp_body_string(resp, encoding = "UTF-8"), "\n")
  stop("❌ Supabase returned status ", httr2::resp_status(resp))
}
cat("✔ Uploaded to Supabase: ", file.path(SB_BUCKET, object_path), "\n", sep = "")

## ── 5. Email the PDF via Mailjet (optional) ─────────────────────────────────
if (SEND_EMAIL) {
  # MAIL_FROM can be "Name <email@domain>"
  if (stringr::str_detect(MAIL_FROM, "<.+@.+>")) {
    from_email <- stringr::str_remove_all(stringr::str_extract(MAIL_FROM, "<.+@.+>"), "[<>]")
    from_name  <- stringr::str_trim(stringr::str_remove(MAIL_FROM, "<.+@.+>$"))
  } else {
    from_email <- MAIL_FROM
    from_name  <- "Numeric Bot"
  }

  stopifnot(from_email != "", MAIL_TO != "")

  mj_resp <- httr2::request("https://api.mailjet.com/v3.1/send") |>
    httr2::req_auth_basic(MJ_API_KEY, MJ_API_SECRET) |>
    httr2::req_body_json(list(
      Messages = list(list(
        From        = list(Email = from_email, Name = from_name),
        To          = list(list(Email = MAIL_TO)),
        Subject     = sprintf("Weekly Numeric Report – %s to %s",
                              format(week_start, "%d %b %Y"), format(week_end, "%d %b %Y")),
        TextPart    = "Attached is the weekly Twitter numeric report.",
        Attachments = list(list(
          ContentType   = "application/pdf",
          Filename      = file_name,
          Base64Content = base64enc::base64encode(PDF_OUT)
        ))
      ))
    )) |>
    httr2::req_perform()

  if (httr2::resp_status(mj_resp) >= 300) {
    cat("Mailjet error body:\n",
        httr2::resp_body_string(mj_resp, encoding = "UTF-8"), "\n")
    stop("❌ Mailjet returned status ", httr2::resp_status(mj_resp))
  }
  cat("📧 Mailjet response OK — report emailed\n")
} else {
  cat("↪ Skipping email step (SEND_EMAIL=false). Report generated & uploaded only.\n")
}

