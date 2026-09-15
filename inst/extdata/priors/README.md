# inst/extdata/priors

This directory contains the reviewed, precomputed prior bundle that is shipped
with the installed PriorRhythm package.

## Expected artifact

**Filename:** `list_historical_prior.RDS`

This file is loaded by `run_app()` when `prior_file = NULL` (the default).

## Prior-bundle schema

Two shapes are accepted. The **versioned-envelope shape** (preferred) is:

```r
list(
  schema_version = 1L,           # integer; required for versioned shape
  metadata = list(               # optional non-confidential metadata
    bundle_id      = "...",
    created_at     = "...",
    created_with   = "PriorRhythm x.y.z",
    package_version = "..."
  ),
  priors = list(                 # required
    <species> = list(
      <endpoint> = list(
        pa = <data.frame with columns: alpha0, A0, beta0, phi0>,
        diagnostics = list(      # required — see "Convergence diagnostics" below
          parameters      = c("alpha0", "A0", "beta0", "phi0"),
          rhat            = <named numeric vector, one per parameter>,
          ess             = <named numeric vector, one per parameter>,
          status          = "pass" | "acceptable" | "failed" | "unassessed",
          policy          = list(rhat_target = 1.01, rhat_acceptable = 1.1, ess_target = 400),
          assessed_at     = "...",
          package_version = "..."
        )
        # additional components (e.g. model objects) are permitted
      )
    )
  )
)
```

The **legacy flat shape** (backward compatible) is also accepted. In this
shape, species keys are top-level alongside recognized metadata fields:

```r
list(
  schema_version = 1L,           # recognized metadata — not treated as a species
  metadata       = list(...),    # recognized metadata — not treated as a species
  <species> = list(
    <endpoint> = list(pa = ..., diagnostics = ...)
  )
)
```

Recognized top-level metadata keys (never treated as species): `schema_version`,
`metadata`, `bundle_id`, `created_with`, `created_at`, `package_version`.

Both shapes are normalised internally to:
```r
list(schema_version = ..., metadata = ..., priors = list(...))
```

## Convergence diagnostics

Runtime prior bundles keep only pooled posterior draws (`pa`) and, by design,
cannot validly reconstruct chain-aware R-hat/ESS diagnostics — pooling
discards which draw came from which MCMC chain. To retain auditable evidence
of convergence without carrying confidential or bulky chain-level data, every
endpoint's `diagnostics` object records a compact, non-confidential summary
of the chain-aware assessment performed **once**, during the controlled
Stage 1 build/refresh, from the fit's `mcmc_diag` (coda `mcmc.list`):

- `parameters`: the hyperparameters assessed (`alpha0`, `A0`, `beta0`, `phi0`).
- `rhat`, `ess`: Gelman-Rubin R-hat and Effective Sample Size, named to match
  `parameters` — the actual evidence supporting the convergence decision.
- `status`: one of `"pass"`, `"acceptable"`, `"failed"`, `"unassessed"`
  (see policy below).
- `policy`: the convergence-acceptance thresholds in force when assessed.
- `assessed_at`, `package_version`: lightweight provenance of the assessment.

This `diagnostics` object is the single source of auditable convergence
evidence for the packaged bundle. It cannot be used to rerun R-hat or render
chain trace plots — those require the full chain-aware Stage 1 fit, which is
never packaged.

### Convergence policy

`convergence_policy()` defines the documented, single-source-of-truth
thresholds:

- R-hat **target**: `< 1.01`
- R-hat **acceptable**: `< 1.1`
- ESS **target**: `> 400` per parameter

The internal `make_runtime_prior_bundle()` (see `R/BuildRuntimePriorBundle.R`)
applies this policy by **filtering** species/endpoint priors, rather than
aborting the whole build:

- `"pass"` and `"acceptable"` endpoints are retained (`"acceptable"` endpoints
  emit a warning identifying the species/endpoint);
- `"failed"` and `"unassessed"` endpoints are excluded from the packaged
  bundle, each with a warning naming the species/endpoint and status;
- the build aborts only when no endpoint remains eligible for packaging.

An informational message lists every `species/endpoint` prior retained in
the packaged bundle, so the controlled build log states exactly what was
packaged.

## Privacy and confidentiality

**This directory must never contain raw historical observations, animal
identifiers, source-file paths, or any other confidential historical-data
fields.** The bundle must contain only posterior parameter draws (`pa`),
the non-confidential `diagnostics` summary described above, and
non-confidential metadata.

The `validate_prior_bundle()` function checks for the presence of known
confidential column names (including inside `diagnostics`) and aborts if any
are found.

## How to build a compatible bundle

Advanced users who wish to supply their own prior bundle should:

1. Run `chain_priors_from_data()` on approved historical data.
2. Organise results into the species → endpoint → `pa`/`mcmc_diag` structure
   expected by `make_runtime_prior_bundle()` (see
   `R/BuildRuntimePriorBundle.R`), the internal, tested source of truth for
   runtime-bundle eligibility.
3. Call `make_runtime_prior_bundle()` to compute each endpoint's
   `diagnostics` (see `summarize_convergence_diagnostics()`) and apply the
   convergence policy by filtering out non-eligible endpoints.
4. Retain only the `pa` component (posterior draws) and `diagnostics`
   summary, plus any other non-confidential metadata; do not include raw
   long-format historical data, animal IDs, or chain draws/trace data.
5. Save with `saveRDS(bundle, "my_priors.RDS", compress = "xz")`.
6. Pass the path to `run_app(prior_file = "my_priors.RDS")`.

See `data-raw/build_priors_for_package.R` for the internal refresh workflow,
which only orchestrates fitting, calls `make_runtime_prior_bundle()`, saves
the resulting RDS, and validates the generated artifact.

## Release check

Before each release, the packaged bundle must pass `validate_prior_bundle()`,
which verifies that:

- the required species, endpoint, `pa`, and `diagnostics` structure is
  present;
- `pa` contains the required columns `alpha0`, `A0`, `beta0`, `phi0`;
- `diagnostics` is well-formed (required fields, `rhat`/`ess` aligned to
  `parameters`, finite/positive `ess`, finite-or-`NA` `rhat`, valid `status`,
  `policy`, and provenance fields), the recorded `policy` thresholds
  themselves are sane (each field finite, `ess_target > 0`, and
  `0 < rhat_target < rhat_acceptable`), **and** semantically consistent: the
  recorded `status` must match what `.convergence_status()` recomputes from
  the recorded `rhat`/`ess`/`policy` (missing `rhat` is only permitted when
  `status` is `"unassessed"`); and
- no confidential column names (e.g. `animal`, `subject`, `study`,
  `source_file`, `raw_data`) are present anywhere in the bundle, including
  inside `diagnostics`.
