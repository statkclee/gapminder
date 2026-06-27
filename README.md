# gapminder

gapminder 데이터셋 기반 탐색적 데이터 분석(EDA) — Quarto 프로젝트.

## 프로젝트 구조

```
.
├── _quarto.yml            # Quarto 프로젝트 설정 (output-dir: docs)
├── index.qmd              # EDA 대시보드 소스 (→ docs/index.html, Pages 랜딩)
├── eda_report.qmd         # EDA 보고서 (HTML) 소스
├── code/                  # R 스크립트
│   ├── clean.R            #   데이터 품질 점검
│   └── eda.R              #   EDA + figures/ 그래프 생성
├── data/
│   └── gapminder.csv      # 원본 데이터
├── figures/               # eda.R 가 생성하는 PNG 그래프 (7종)
├── document/              # 서술형 보고서 (Markdown)
│   ├── data_quality_report.md
│   └── eda_report.md
└── docs/                  # 렌더 산출물 (index.html=대시보드, eda_report.html)
```

## 사용법

모든 명령은 **프로젝트 루트**에서 실행합니다.

```bash
# 데이터 품질 점검
Rscript code/clean.R

# EDA 실행 (figures/ 그래프 생성)
Rscript code/eda.R

# Quarto 문서 렌더 → docs/
quarto render
```
