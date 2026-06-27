#!/usr/bin/env Rscript
# eda.R — gapminder 탐색적 데이터 분석(EDA)
# 사용법: Rscript eda.R [입력경로]
# 출력: 콘솔 요약 + figures/*.png 그래프
# 기본 입력: data/gapminder.csv

suppressWarnings(suppressMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(scales)
}))

# .Rprofile 등에서 stats::filter / lubridate 가 마스킹할 수 있어 dplyr 동사를 명시 고정
filter    <- dplyr::filter
summarise <- dplyr::summarise
mutate    <- dplyr::mutate
select    <- dplyr::select
arrange   <- dplyr::arrange

args   <- commandArgs(trailingOnly = TRUE)
infile <- if (length(args) >= 1) args[1] else "data/gapminder.csv"
figdir <- "figures"
dir.create(figdir, showWarnings = FALSE)

stopifnot(file.exists(infile))
gap <- read.csv(infile, stringsAsFactors = FALSE)

theme_set(theme_minimal(base_size = 12))
save_plot <- function(p, name, w = 8, h = 5) {
  path <- file.path(figdir, name)
  ggsave(path, p, width = w, height = h, dpi = 120)
  cat("  [저장]", path, "\n")
}
hr <- function(t) cat("\n================ ", t, " ================\n", sep = "")

## 0. 개요 -----------------------------------------------------------
hr("0. 개요")
cat(sprintf("관측치 %d개, 국가 %d개, 연도 %d개 (%s)\n",
            nrow(gap), n_distinct(gap$country), n_distinct(gap$year),
            paste(range(gap$year), collapse = "–")))
cat("\n수치형 변수 요약:\n")
print(summary(gap[c("lifeExp", "gdpPercap", "pop")]))

## 1. 단변량 분포 ----------------------------------------------------
hr("1. 단변량 분포")
cat("왜도 점검 (gdpPercap, pop은 우편향 → log 권장):\n")
skew <- function(x) mean((x - mean(x))^3) / sd(x)^3
for (v in c("lifeExp", "gdpPercap", "pop"))
  cat(sprintf("  %-10s skewness = %+.2f\n", v, skew(gap[[v]])))

p1 <- ggplot(gap, aes(lifeExp)) +
  geom_histogram(bins = 30, fill = "#2c7fb8", color = "white") +
  labs(title = "Distribution of Life Expectancy", x = "lifeExp", y = "count")
save_plot(p1, "01_hist_lifeExp.png")

p2 <- ggplot(gap, aes(gdpPercap)) +
  geom_histogram(bins = 30, fill = "#d95f0e", color = "white") +
  scale_x_log10(labels = comma) +
  labs(title = "Distribution of GDP per Capita (log10)", x = "gdpPercap (log10)", y = "count")
save_plot(p2, "02_hist_gdpPercap_log.png")

## 2. 대륙별 비교 ----------------------------------------------------
hr("2. 대륙별 요약 (최신연도 2007)")
latest <- max(gap$year)
by_cont <- gap %>%
  filter(year == latest) %>%
  group_by(continent) %>%
  summarise(n            = n(),
            lifeExp_med  = median(lifeExp),
            gdp_med      = median(gdpPercap),
            pop_total    = sum(pop), .groups = "drop") %>%
  arrange(desc(lifeExp_med))
print(as.data.frame(by_cont), row.names = FALSE)

p3 <- ggplot(filter(gap, year == latest),
             aes(reorder(continent, lifeExp, median), lifeExp, fill = continent)) +
  geom_boxplot(show.legend = FALSE) +
  coord_flip() +
  labs(title = sprintf("Life Expectancy by Continent (%d)", latest),
       x = NULL, y = "lifeExp")
save_plot(p3, "03_box_lifeExp_continent.png")

## 3. 시계열 추세 ----------------------------------------------------
hr("3. 시간에 따른 기대수명 추세 (대륙별 중앙값)")
trend <- gap %>%
  group_by(continent, year) %>%
  summarise(lifeExp = median(lifeExp), .groups = "drop")
print(trend %>% pivot_wider(names_from = continent, values_from = lifeExp) %>% as.data.frame(),
      row.names = FALSE)

p4 <- ggplot(trend, aes(year, lifeExp, color = continent)) +
  geom_line(linewidth = 1) + geom_point(size = 1.5) +
  labs(title = "Median Life Expectancy Over Time", x = "year", y = "lifeExp")
save_plot(p4, "04_trend_lifeExp.png")

## 4. 관계: GDP vs 기대수명 (gapminder 대표 그래프) ------------------
hr("4. GDP per Capita vs Life Expectancy")
cat(sprintf("상관계수 (lifeExp ~ log10 gdpPercap): r = %.3f\n",
            cor(gap$lifeExp, log10(gap$gdpPercap))))

p5 <- ggplot(filter(gap, year == latest),
             aes(gdpPercap, lifeExp, size = pop, color = continent)) +
  geom_point(alpha = 0.7) +
  scale_x_log10(labels = comma) +
  scale_size(range = c(1, 14), guide = "none") +
  labs(title = sprintf("GDP per Capita vs Life Expectancy (%d)", latest),
       x = "gdpPercap (log10)", y = "lifeExp")
save_plot(p5, "05_scatter_gdp_lifeExp.png", w = 9, h = 6)

## 5. 극단값: 최신연도 상·하위 국가 ---------------------------------
hr("5. 기대수명 상·하위 5개국 (2007)")
d07 <- filter(gap, year == latest)
cat("▼ 상위 5:\n")
print(d07 %>% arrange(desc(lifeExp)) %>% head(5) %>%
        select(country, continent, lifeExp, gdpPercap), row.names = FALSE)
cat("▼ 하위 5:\n")
print(d07 %>% arrange(lifeExp) %>% head(5) %>%
        select(country, continent, lifeExp, gdpPercap), row.names = FALSE)

## 6. 기대수명 증가폭 (1952→2007) ----------------------------------
hr("6. 기대수명 증가폭 1952→2007 (상·하위 5개국)")
growth <- gap %>%
  filter(year %in% c(min(year), max(year))) %>%
  select(country, continent, year, lifeExp) %>%
  pivot_wider(names_from = year, values_from = lifeExp) %>%
  mutate(gain = .data[[as.character(max(gap$year))]] - .data[[as.character(min(gap$year))]])
cat("▼ 최대 증가 5:\n")
print(growth %>% arrange(desc(gain)) %>% head(5) %>% as.data.frame(), row.names = FALSE)
cat("▼ 최소 증가 5:\n")
print(growth %>% arrange(gain) %>% head(5) %>% as.data.frame(), row.names = FALSE)

hr("완료")
cat(sprintf("그래프 %d개를 %s/ 에 저장했습니다.\n",
            length(list.files(figdir, pattern = "\\.png$")), figdir))
