# SyntheticTest

This directory contains synthetic test study data for use in package examples,
vignettes, and tests.

## File: `Syn_test.RDS`

A data frame suitable for use as `Input_tdat` in `Bayesian_Test_Example()`,
with the following columns:

| Column | Type | Description |
|---|---|---|
| `animal_code` | character | Unique animal identifier (e.g., `"animal_1"`) |
| `treatment_code` | integer | Treatment level code (1–4) |
| `period_code` | integer | Period code (1–3) |
| `time` | character | Time bin label (e.g., `"0 to 3 hr"`) |
| `time1` | numeric | Start of time bin (hours) |
| `time2` | numeric | End of time bin (hours) |
| `value` | numeric | Synthetic measured assay value |

## Structure

- 8 animals
- 4 treatment levels (1–4)
- 3 periods (1–3)
- 5 time bins (0–3, 3–6, 6–12, 12–18, 18–24 hr)

## How to Regenerate

From the package root directory, run:

```r
source("data-raw/generate_synthetic_data.R")
```

Requires the `PriorRhythm` package to be installed with `set.seed(42)`.
