#!/usr/bin/env Rscript
# eda.R — gapminder 탐색적 데이터 분석(EDA)  [개정판]
# 원칙: (1) 모든 주장은 스크립트가 출력한 수치에 근거한다(암기·단정 금지),
#       (2) 1국가=1표본(unweighted)과 1인=1표본(population-weighted)을 구분한다,
#       (3) EDA는 패턴을 보일 뿐 인과를 규명하지 않는다(인과는 잠정 표기).
# 사용법: Rscript eda.R [입력경로]   | 출력: 콘솔 요약 + figures/*.png
# 기본 입력: data/gapminder.csv

suppressWarnings(suppressMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(scales)
}))
# .Rprofile 등의 stats::filter / lubridate 마스킹 방지: dplyr 동사 명시 고정
filter <- dplyr::filter; summarise <- dplyr::summarise
mutate <- dplyr::mutate; select <- dplyr::select; arrange <- dplyr::arrange

args   <- commandArgs(trailingOnly = TRUE)
infile <- if (length(args) >= 1) args[1] else "data/gapminder.csv"
figdir <- "figures"; dir.create(figdir, showWarnings = FALSE)
stopifnot(file.exists(infile))
gap <- read.csv(infile, stringsAsFactors = FALSE)

theme_set(theme_minimal(base_size = 12))
save_plot <- function(p, name, w = 8, h = 5) {
  ggsave(file.path(figdir, name), p, width = w, height = h, dpi = 120)
  cat("  [저장] figures/", name, "\n", sep = "")
}
hr <- function(t) cat("\n================ ", t, " ================\n", sep = "")
yr_lo <- min(gap$year); yr_hi <- max(gap$year)

## 0. 개요 + 데이터 한계 ---------------------------------------------
hr("0. 개요 및 데이터 한계")
cat(sprintf("관측치 %d, 국가 %d, 연도 %d (%d–%d, 5년 간격)\n",
            nrow(gap), n_distinct(gap$country), n_distinct(gap$year), yr_lo, yr_hi))
cat("한계(해석 시 유의):\n")
cat("  - 5년 간격·142개국만 포함된 가공/보간 데이터(원자료 아님).\n")
cat("  - Oceania는", sum(gap$continent=="Oceania" & gap$year==yr_hi),
    "개국(호주·뉴질랜드)뿐 → '대륙 요약'으로 일반화 불가, 참고용.\n")
cat("\n수치형 요약:\n"); print(summary(gap[c("lifeExp","gdpPercap","pop")]))

## 1. 단변량 분포 + 이상치(데이터로 확인) ---------------------------
hr("1. 단변량 분포 및 이상치")
skew <- function(x) mean((x-mean(x))^3)/sd(x)^3
for (v in c("lifeExp","gdpPercap","pop"))
  cat(sprintf("  %-10s skewness = %+.2f\n", v, skew(gap[[v]])))
cat("\n최대 gdpPercap 관측(이상치 확인):\n")
print(gap[which.max(gap$gdpPercap), ], row.names = FALSE)
cat("→ gdpPercap 상위는 초기 산유국에 집중(아래 5건):\n")
print(head(arrange(gap, desc(gdpPercap))[c("country","year","gdpPercap")], 5), row.names = FALSE)

p1 <- ggplot(gap, aes(lifeExp)) +
  geom_histogram(bins = 30, fill = "#2c7fb8", color = "white") +
  labs(title = "Distribution of Life Expectancy (all years pooled)", x="lifeExp", y="count")
save_plot(p1, "01_hist_lifeExp.png")

# 소득 분포: 1국가=1표본 vs 1인=1표본(인구가중)을 같은 로그축에서 비교
p2 <- ggplot(gap, aes(gdpPercap)) +
  geom_density(aes(weight = 1/nrow(gap), color = "per country (unweighted)"), linewidth = 1) +
  geom_density(aes(weight = pop/sum(pop),  color = "per person (pop-weighted)"), linewidth = 1) +
  scale_x_log10(labels = comma) +
  scale_color_manual(values = c("per country (unweighted)"="#7570b3",
                                "per person (pop-weighted)"="#d95f02"), name=NULL) +
  labs(title="Income distribution: per-country vs per-person (pooled)",
       x="gdpPercap (log10)", y="density") + theme(legend.position="top")
save_plot(p2, "02_density_gdp_weighted.png")

## 2. 인구 가중 vs 비가중 (핵심) ------------------------------------
hr("2. 인구 가중 vs 비가중 — 1국가=1표본과 1인=1표본은 다르다")
cmp <- gap %>% filter(year==yr_hi) %>% group_by(continent) %>%
  summarise(n=n(),
            lifeExp_unw = mean(lifeExp),
            lifeExp_wt  = weighted.mean(lifeExp, pop),
            .groups="drop") %>%
  mutate(delta = lifeExp_wt - lifeExp_unw) %>% arrange(desc(lifeExp_wt))
cat(sprintf("[%d] 대륙별 기대수명: 비가중 vs 인구가중\n", yr_hi))
print(as.data.frame(cmp), row.names = FALSE, digits = 5)
glb <- gap %>% filter(year==yr_hi) %>%
  summarise(unw=mean(lifeExp), wt=weighted.mean(lifeExp,pop))
cat(sprintf("전세계: 비가중 %.2f vs 인구가중 %.2f (차이 %+.2f년)\n",
            glb$unw, glb$wt, glb$wt-glb$unw))
cat("→ 아시아처럼 인구 큰 빈국이 많으면 가중·비가중이 크게 달라짐. 둘을 구분해야 함.\n")

p3 <- ggplot(cmp, aes(y=reorder(continent, lifeExp_wt))) +
  geom_segment(aes(x=lifeExp_unw, xend=lifeExp_wt, yend=continent), color="grey70") +
  geom_point(aes(x=lifeExp_unw, color="unweighted"), size=3) +
  geom_point(aes(x=lifeExp_wt,  color="pop-weighted"), size=3) +
  scale_color_manual(values=c(unweighted="#1b9e77","pop-weighted"="#d95f02"), name=NULL) +
  labs(title=sprintf("Continent life expectancy %d: unweighted vs pop-weighted", yr_hi),
       x="lifeExp", y=NULL) + theme(legend.position="top")
save_plot(p3, "03_weighted_vs_unweighted.png")

## 3. 시계열 추세 (전세계, 가중/비가중 함께) ------------------------
hr("3. 전세계 기대수명 추세 — 가중/비가중")
gtrend <- gap %>% group_by(year) %>%
  summarise(unweighted = mean(lifeExp),
            pop_weighted = weighted.mean(lifeExp, pop), .groups="drop")
print(as.data.frame(gtrend), row.names = FALSE, digits = 4)
p4 <- gtrend %>% pivot_longer(-year, names_to="type", values_to="lifeExp") %>%
  ggplot(aes(year, lifeExp, color=type)) + geom_line(linewidth=1) + geom_point() +
  labs(title="Global life expectancy over time", x="year", y="lifeExp") +
  theme(legend.position="top")
save_plot(p4, "04_global_trend_weighted.png")

## 4. 수렴 검정: 국가 간 기대수명 분산이 줄었나 ---------------------
hr("4. 수렴(convergence) — 국가 간 기대수명 격차의 시간 변화")
conv <- gap %>% group_by(year) %>%
  summarise(sd = sd(lifeExp), cv = sd(lifeExp)/mean(lifeExp),
            iqr = IQR(lifeExp), .groups="drop")
print(as.data.frame(conv), row.names = FALSE, digits = 4)
cat(sprintf("CV: %.4f(%d) → %.4f(%d).  %s\n",
            conv$cv[1], yr_lo, conv$cv[nrow(conv)], yr_hi,
            ifelse(conv$cv[nrow(conv)] < conv$cv[1],
                   "감소 = 국가 간 기대수명 격차 축소(수렴) 패턴",
                   "증가 = 격차 확대(발산) 패턴")))
p5 <- ggplot(conv, aes(year, cv)) + geom_line(linewidth=1, color="#377eb8") +
  geom_point() + labs(title="Cross-country dispersion of life expectancy (CV)",
                      x="year", y="coefficient of variation")
save_plot(p5, "05_convergence_cv.png")

## 5. GDP↔기대수명: 풀링 상관은 오해를 부른다 ----------------------
hr("5. GDP per capita ↔ 기대수명 — 풀링 vs 연도별")
r_pool <- cor(gap$lifeExp, log10(gap$gdpPercap))
cat(sprintf("풀링(1952–2007 전체) 상관 r = %.3f  ← 시간추세가 섞여 단면 해석에 부적절\n", r_pool))
peryear <- gap %>% group_by(year) %>%
  summarise(r = cor(lifeExp, log10(gdpPercap)), .groups="drop")
cat("연도별 단면 상관:\n"); print(as.data.frame(peryear), row.names=FALSE, digits=3)
cat(sprintf("연도별 r 범위 %.3f–%.3f (평균 %.3f)\n",
            min(peryear$r), max(peryear$r), mean(peryear$r)))
fit <- lm(lifeExp ~ log10(gdpPercap), data = filter(gap, year==yr_hi))
cat(sprintf("[%d] 단순회귀 lifeExp ~ log10(gdp): R² = %.3f\n", yr_hi, summary(fit)$r.squared))

p6 <- ggplot(peryear, aes(year, r)) + geom_line(linewidth=1, color="#984ea3") +
  geom_point() + ylim(0, 1) +
  labs(title="Within-year correlation: lifeExp ~ log10(gdpPercap)",
       x="year", y="Pearson r")
save_plot(p6, "06_corr_by_year.png")

p7 <- ggplot(filter(gap, year==yr_hi), aes(gdpPercap, lifeExp)) +
  geom_point(aes(size=pop, color=continent), alpha=0.7) +
  geom_smooth(aes(weight=pop), method="lm", se=FALSE,
              color="grey30", linewidth=0.6) +
  scale_x_log10(labels=comma) + scale_size(range=c(1,14), guide="none") +
  labs(title=sprintf("GDP per capita vs life expectancy (%d)", yr_hi),
       x="gdpPercap (log10)", y="lifeExp")
save_plot(p7, "07_scatter_gdp_lifeExp.png", w=9, h=6)

## 6. 극단값·변화 (패턴만 기술, 인과는 잠정) ------------------------
hr("6. 극단값 및 변화 — 패턴 기술(인과 단정 아님)")
d_hi <- filter(gap, year==yr_hi)
cat(sprintf("[%d] 기대수명 상위 3 / 하위 3:\n", yr_hi))
print(head(arrange(d_hi, desc(lifeExp))[c("country","continent","lifeExp","gdpPercap")],3), row.names=FALSE)
print(head(arrange(d_hi, lifeExp)[c("country","continent","lifeExp","gdpPercap")],3), row.names=FALSE)
growth <- gap %>% filter(year %in% c(yr_lo,yr_hi)) %>%
  select(country,continent,year,lifeExp) %>%
  pivot_wider(names_from=year, values_from=lifeExp) %>%
  mutate(gain = .data[[as.character(yr_hi)]] - .data[[as.character(yr_lo)]])
cat(sprintf("\n%d→%d 기대수명 증가 상위 3:\n", yr_lo, yr_hi))
print(head(arrange(growth, desc(gain)),3) %>% as.data.frame(), row.names=FALSE)
cat("역행(감소) 사례:\n")
print(filter(growth, gain < 0) %>% arrange(gain) %>% as.data.frame(), row.names=FALSE)
cat("주의: 위 정체·역행은 '패턴'일 뿐. 원인(질병·분쟁 등)은 EDA로 규명 불가 → 별도 검증 필요.\n")

## 7. 재현성 --------------------------------------------------------
hr("7. 재현성")
cat("R", as.character(getRversion()), "| key pkgs:",
    paste(sprintf("%s %s", c("dplyr","ggplot2"),
                  c(as.character(packageVersion("dplyr")),
                    as.character(packageVersion("ggplot2")))), collapse=", "), "\n")
hr("완료")
cat(sprintf("그래프 %d개를 figures/ 에 저장했습니다.\n",
            length(list.files(figdir, pattern="\\.png$"))))
