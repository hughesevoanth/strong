# STRONG: Substrate Oxidation in Girls

Analysis code for:

> Lowe AC, Fearnbach N, Allaway HCM, Katzmarzyk PT, Reed I, Litsey A, Hughes DA, Redman LM, Johannsen NM, Hsia DS, Ravussin E.
> **Effect of exercise on energy balance and substrate oxidation in adolescent girls with overweight or obesity.** *(in revision)*

## The study in brief

Does a bout of vigorous exercise change how much adolescent girls eat for the rest of the day?

Twelve post-menarcheal girls (13–17 y) with overweight or obesity each completed two 23-hour stays in a whole-room metabolic chamber, in randomized order:

- **EX:** cycling at 70% VO₂peak until 200 kcal above resting metabolic rate was expended
- **REST:** a time-matched period of quiet sitting

Both visits were scheduled in the follicular phase of the menstrual cycle. Breakfast was standardized; lunch, snack and dinner were ad libitum and weighed before and after eating. The chamber measured energy expenditure and substrate oxidation minute by minute, and appetite was rated on visual analogue scales throughout the day.

**Main finding:** after exercise, girls ate more, almost all of it at dinner, so energy balance ended up similar on both days.

## What's in this repository

| File | What it does |
|---|---|
| `00_STRONG_Simulations.Rmd` | Simulation-based power analysis for meal-by-condition effects on energy intake with n = 12 |
| `01_primary_analysis.Rmd` | All analyses reported in the paper (see below) |
| `functions/` | Helper functions for simulation, model fitting and plotting |

### `01_primary_analysis.Rmd` step by step

1. **Effective number of tests.** Hierarchical clustering of the 39 outcomes on Spearman correlations (cut at |ρ| = 0.5) gives 10 clusters, so the study alpha is 0.05 / 10 = 0.005. A PCA of the outcomes is included for comparison.
2. **Condition effects.** A linear mixed model for each outcome: `outcome ~ Condition + (1 | SubjectID)`, fit with REML and Satterthwaite df. Each model is run three ways: original units, scaled (mean 0, SD 1), and inverse rank normal transformed.
3. **Model checks and sensitivity analyses.** Shapiro–Wilk tests of residuals, singular-fit flags, and cluster-robust (CR2) standard errors.
4. **Descriptives.** n, mean and SD by condition, and the SD ratio, for every outcome.
5. **Tables and figures** for the manuscript and supplement.

## Running the code

**Requirements:** R ≥ 4.3 and the following packages:

```r
install.packages(c("tidyverse", "readxl", "lme4", "lmerTest", "broom.mixed",
                   "clubSandwich", "emmeans", "dendextend", "gt", "gtsummary",
                   "patchwork"))
```

**Folder layout.** The code expects the data and an output folder to sit next to the repository:

```
project/
├── data/
│   └── STRONG_data spreadsheet.xlsx   # not included (see Data availability)
├── results/
│   ├── figures/
│   └── tables/
└── strong/                            # this repository
```

Then knit `01_primary_analysis.Rmd` from inside `strong/`.

## Outputs and where they appear in the paper

| Output | Paper |
|---|---|
| Condition-effects table (all 39 outcomes, three model versions) | Supplementary Table 3 |
| Outcome dendrogram | Supplementary Figure 1 |
| Model-estimated means by condition | Table 2 |
| Energy intake by meal | Figure 2 |

## Data availability

The participant-level data are not included in this repository. Requests can be made to senior author.

## License

MIT. See `LICENSE`.