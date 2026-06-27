#!/usr/bin/env Rscript
# clean.R — gapminder 데이터 품질 점검 스크립트
# 사용법: Rscript clean.R [입력경로]
# 기본 입력: data/gapminder.csv

suppressWarnings(suppressMessages({
  # base R만 사용 (외부 패키지 의존 없음)
}))

args <- commandArgs(trailingOnly = TRUE)
infile <- if (length(args) >= 1) args[1] else "data/gapminder.csv"

cat("===========================================\n")
cat(" gapminder 데이터 품질 점검 (clean.R)\n")
cat("===========================================\n")
cat("입력 파일:", infile, "\n\n")

stopifnot(file.exists(infile))

df <- read.csv(infile, stringsAsFactors = FALSE, encoding = "UTF-8")

# 결과 누적용
issues <- character(0)
flag   <- function(msg) issues <<- c(issues, msg)

## 1. 구조 -------------------------------------------------------------
cat("## 1. 구조\n")
cat(sprintf("  행: %d, 열: %d\n", nrow(df), ncol(df)))
cat("  컬럼:", paste(names(df), collapse = ", "), "\n")

expected_cols <- c("country", "year", "pop", "continent", "lifeExp", "gdpPercap")
missing_cols  <- setdiff(expected_cols, names(df))
if (length(missing_cols) > 0) flag(paste("누락 컬럼:", paste(missing_cols, collapse = ", ")))

cat("  타입:\n")
for (nm in names(df)) cat(sprintf("    %-10s %s\n", nm, class(df[[nm]])))
cat("\n")

## 2. 결측치 ----------------------------------------------------------
cat("## 2. 결측치 (NA)\n")
na_counts <- sapply(df, function(x) sum(is.na(x)))
for (nm in names(na_counts)) cat(sprintf("    %-10s %d\n", nm, na_counts[nm]))
if (sum(na_counts) > 0) flag(sprintf("결측치 총 %d개", sum(na_counts)))
cat("\n")

## 3. 중복 행 ---------------------------------------------------------
cat("## 3. 중복\n")
dup_rows <- sum(duplicated(df))
cat(sprintf("  완전 중복 행: %d\n", dup_rows))
if (dup_rows > 0) flag(sprintf("완전 중복 행 %d개", dup_rows))

if (all(c("country", "year") %in% names(df))) {
  dup_key <- sum(duplicated(df[, c("country", "year")]))
  cat(sprintf("  (country, year) 키 중복: %d\n", dup_key))
  if (dup_key > 0) flag(sprintf("(country, year) 키 중복 %d개", dup_key))
}
cat("\n")

## 4. 값 범위 / 유효성 ------------------------------------------------
cat("## 4. 값 범위 / 유효성\n")

check_range <- function(col, lo, hi, label) {
  if (!col %in% names(df)) return(invisible())
  x <- df[[col]]
  cat(sprintf("  %-10s min=%.3f  max=%.3f  mean=%.3f\n",
              col, min(x, na.rm = TRUE), max(x, na.rm = TRUE), mean(x, na.rm = TRUE)))
  bad <- which(x < lo | x > hi)
  if (length(bad) > 0)
    flag(sprintf("%s: 허용범위[%s,%s] 벗어난 값 %d개", label, lo, hi, length(bad)))
}

check_range("year",      1800, 2030,    "year")
check_range("pop",       0,    Inf,     "pop")
check_range("lifeExp",   0,    120,     "lifeExp")
check_range("gdpPercap", 0,    Inf,     "gdpPercap")

# 음수/0 점검 (양수여야 하는 값)
for (col in c("pop", "lifeExp", "gdpPercap")) {
  if (col %in% names(df)) {
    n_nonpos <- sum(df[[col]] <= 0, na.rm = TRUE)
    if (n_nonpos > 0) flag(sprintf("%s: 0 이하 값 %d개", col, n_nonpos))
  }
}
cat("\n")

## 5. 카테고리 / 패널 균형 -------------------------------------------
cat("## 5. 카테고리 / 패널 균형\n")
if ("continent" %in% names(df)) {
  cat("  continent 분포:\n")
  tb <- sort(table(df$continent), decreasing = TRUE)
  for (nm in names(tb)) cat(sprintf("    %-10s %d\n", nm, tb[nm]))
}
if ("country" %in% names(df)) {
  n_country <- length(unique(df$country))
  cat(sprintf("  고유 country: %d\n", n_country))
}
if ("year" %in% names(df)) {
  yrs <- sort(unique(df$year))
  cat(sprintf("  고유 year: %d  (%s)\n", length(yrs), paste(range(yrs), collapse = "–")))
}

# 패널 균형: 모든 country가 동일한 year 수를 갖는가
if (all(c("country", "year") %in% names(df))) {
  per_country <- table(df$country)
  if (length(unique(per_country)) == 1) {
    cat(sprintf("  패널 균형: OK (모든 국가 %d개 시점)\n", unique(per_country)))
  } else {
    flag("패널 불균형: 국가별 관측 수 불일치")
    cat("  패널 불균형! 관측 수 분포:\n")
    print(table(per_country))
  }

  # country별로 continent가 하나로 고정인지
  cc <- tapply(df$continent, df$country, function(x) length(unique(x)))
  bad_cc <- names(cc)[cc > 1]
  if (length(bad_cc) > 0)
    flag(sprintf("continent 불일치 국가 %d개: %s",
                 length(bad_cc), paste(head(bad_cc, 5), collapse = ", ")))
}
cat("\n")

## 6. 요약 -----------------------------------------------------------
cat("===========================================\n")
cat("## 요약\n")
if (length(issues) == 0) {
  cat("  ✅ 발견된 품질 문제 없음. 데이터 정상.\n")
} else {
  cat(sprintf("  ⚠️  발견된 문제 %d건:\n", length(issues)))
  for (i in seq_along(issues)) cat(sprintf("    %d. %s\n", i, issues[i]))
}
cat("===========================================\n")

quit(status = if (length(issues) == 0) 0 else 1)
