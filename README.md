# The Effect of Pre-Bedtime Habits on Sleep Quality 

A complete self-directed randomized controlled trial investigating how different pre-bedtime habits affect sleep quality and sleep efficacy. The study compares the effects of reading a physical book versus using a phone for 20 minutes before bed.

## Contents 
1. [Experimental Design](#experimental-design)
2. [Data](#data) 
3. [Methodology](#methodology)
4. [Results](#results) 
5. [Repository Structure](#repository-structure)
6. [Replication Instructions](#replication-instructions)
7. [Limitations](#limitations)

## Experimental Design 

**Study Type**: Randomized Controlled Trial \
**Duration**: April 2025 (one month) \
**Participants**: 2 college students (21 years old) \
**Sample Size**: 43 nights of data \
**Randomization**: Complete random assignment, blocked by participant using R's `sample()` function \
**Treatment Conditions**:

- **Control**: Using phone for 20 minutes before bed \
- **Treatment**: Reading physical book for 20 minutes before bed

**Primary Outcomes**:

- Sleep Quality Score (0-100) from Sleep Cycle app
- Sleep Efficacy (% time asleep while in bed)

## Data 

- `name`: Participant name (Elyse or Felice)
- `date`: Date of sleep measurement
- `assignment`: Treatment assignment (Phone-Control or Read-Treatment)
- `stress`: Self-reported stress level (0-5 scale)
- `caffeine`: Caffeine consumption (How many drinks)
- `smoking`: Whether participant smoked (Yes/No)
- `alcohol`: Whether participant consumed alcohol (Yes/No)
- `exercise`: Whether participant exercised (Yes/No)
- `sub_rating`: Subjective sleep quality rating (0-5 scale)
- `bedtime_aw`: Bedtime recorded by Apple Watch
- `time_in_bed_aw`: Total time in bed recorded by Apple Watch
- `sleep_duration_aw`: Sleep duration recorded by Apple Watch
- `bedtime_app`: Bedtime recorded by Sleep Cycle app
- `time_in_bed_app`: Total time in bed recorded by Sleep Cycle app
- `sleep_duration_app`: Sleep duration recorded by Sleep Cycle app
- `sleep_quality_app`: Sleep quality score from Sleep Cycle app (0-100 scale)
- `efficacy_aw`: Sleep efficacy calculated from Apple Watch data (percentage)
- `efficacy_app`: Sleep efficacy calculated from Sleep Cycle app data (percentage)

## Methodology 

#### Data Collection: 

- Apple Watch + Sleep Cycle app for objective sleep metrics
- Daily subjective sleep quality ratings (0-5 scale)
- Lifestyle covariates: stress, caffeine, exercise, alcohol, smoking

#### Statistical Analysis:

- Difference-in-means estimation with robust standard errors
- Multiple regression models controlling for covariates
- Conditional Average Treatment Effects (CATEs) by participant
- Power analysis and confidence intervals

## Results 

| Outcome | Phone Mean | Book Mean | Effect | p-value |
|---------|------------|-----------|---------|---------|
| Sleep Quality | 64.84 | 71.62 | +6.78 | 0.17 |
| Sleep Efficacy (App) | 89.92% | 88.62% | -1.30% | 0.53 |
| Sleep Efficacy (Watch) | 92.91% | 90.58% | -2.33% | 0.25 |

While not statistically significant in pooled analysis, substantial heterogeneity between participants emerged. Felice showed +14.4 point improvement in sleep quality with reading books, while Elyse showed minimal negative effects (-0.55 points).

## Repository Structure 

```
├── analysis_script.R             # Complete R analysis pipeline
├── sleep_data_clean.csv          # Cleaned dataset (43 observations)
├── README.md                     # Study documentation
├── preanalysis_plan.pdf          # Original study protocol with power analysis
├── final_report.pdf              # Complete results and methodology
└── outputs/
    ├── plots/                    # Data visualizations
    ├── tables/                   # Regression results (CSV + HTML)
    └── summary_statistics/       # Descriptive statistics by treatment
```

## Replication Instructions

#### Software Requirements
- R (version 4.2.0 or higher)
- Required R packages:
  - tidyverse
  - ggplot2
  - pubtheme (https://github.com/bmacGTPM/pubtheme)
  - estimatr
  - broom
  - modelsummary
  - lubridate
  - readxl
  - knitr
  - kableExtra

#### Steps to Replicate Analysis
1. Ensure all required R packages are installed
2. Place the `sleep_data_clean.csv` file in your working directory
3. Run the `analysis_script.R` script from start to finish
4. All output files (tables, plots, and CSV files) will be generated in your working directory

## Limitations 

The following are some limitations of the study for future considerations: 

#### 1. Sample Size & Power:
The study was imited to 43 observations instead of an expected 60 due to missing data. On some nights, participants forgot to wear their watch or fell asleep before recording data. The small sample size thus limited statistical power for detecting effects. As a result, the study was underpowered to effectively detect differences in sleep efficacy. 

#### 2. Measurement & Data Collection:

Differences between subjective ratings, app measurements, and watch measurements highlight challenges in reliably quantifying sleep quality. Particularly, app-based sleep metrics may have inherent measurement limitations. Future research should a standardized method of data collection that addresses these issues. 

#### 3. Generalizability:

The results were limited to two 21-year-old female college students, meaning they likely will not generalize beyond the study population. In fact, individual heterogeneity suggests personalized approaches may be more effective than one-size-fits-all recommendations, which future research should continue investigating. 
