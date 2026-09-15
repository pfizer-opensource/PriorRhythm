# SyntheticHistorical

This directory contains synthetic historical data for use in package examples,
vignettes, and tests.

## File: `Syn_long.RDS`

A long-format data frame with the following columns:

| Column | Type | Description |
|---|---|---|
| `study_code` | integer | Unique study identifier (1–5) |
| `animal_code` | character | Unique animal identifier (e.g., `"s1_a2"`) |
| `time` | character | Time bin label (e.g., `"0 to 3 hr"`) |
| `time1` | numeric | Start of time bin (hours) |
| `time2` | numeric | End of time bin (hours) |
| `treatment_code` | integer | Treatment level code (1–4) |
| `period_code` | integer | Period code (1–2) |
| `parameter` | character | Assay name (`"assay1"` or `"assay2"`) |
| `value` | numeric | Synthetic measured value |

## Structure

- 5 studies
- 4 animals per study per treatment×period×time combination
- 4 treatment levels (1–4)
- 2 periods (1–2)
- 5 time bins (0–3, 3–6, 6–12, 12–18, 18–24 hr)
- 2 parameters (assay1, assay2)

## How to Regenerate

From the package root directory, run:

```r
source("data-raw/make_synthetic_data.R")
```

Requires the `PriorRhythm` package to be installed with `set.seed(42)`.
