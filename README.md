# Dairy_inbreeding
Replication code, data-processing scripts, and figures for the paper:

> **Competing for Quality: Coordination Failure in Dairy Genetics Markets**
> Victor Funes-Leal (University of Arkansas) and Jared Hutchins (University of Illinois Urbana-Champaign)

This is the repository for the Job Market Paper. It contains everything needed to
reproduce the empirical results, figures, and tables from the raw National Association
of Animal Breeders (NAAB) sire data.

## Summary

The paper studies how genomic testing — introduced to U.S. dairy cattle breeding in 2008 —
reshaped breeders' incentives to inbreed. Using NAAB data on Holstein bulls marketed from
2001 to 2020, we compare inbreeding rates in genetic lines that were **popular before**
genomic testing against a propensity-score-matched control of comparable but less-popular
lines. A difference-in-differences design shows inbreeding rose about 50% faster in the
popular lines after 2010, consistent with a coordination failure: breeders draw down a
shared, common-pool genetic resource (breed-wide diversity) faster than is socially optimal.

## Repository structure

```
Dairy_inbreeding/
├── code/            # R scripts: data construction, estimation, revision analyses
├── data/            # processed analysis data (raw NAAB/CDCB inputs — see Data below)
├── figures/         # generated figures (.png) used in the manuscript
├── tables/          # generated LaTeX tables (\input-ed by the paper)
├── reviews/         # referee reports and response materials
├── renv/            # renv library + lockfile for a reproducible R environment
├── Dairy_inbreeding.Rproj
├── LICENSE          # MIT
└── README.md
```

## Requirements

- **R ≥ 4.5** (the `renv` lockfile targets R 4.5 on Windows; other platforms work but may
  resolve slightly different binaries).
- Package management via [`renv`](https://rstudio.github.io/renv/). Key packages include
  `data.table`, `dplyr`, `tidyr`, `fixest` (estimation and event studies),
  `MatchIt` (propensity-score matching), `clubSandwich` and `sandwich` (clustered/CR2
  variance), `car` (`linearHypothesis`), `modelsummary`/`etable`, and `ggplot2`.

## Reproducing the results

1. Clone the repository and open `Dairy_inbreeding.Rproj` in RStudio (sets the working
   directory to the project root).
2. Restore the exact package environment:
   ```r
   renv::restore()
   ```
3. Run the scripts in order (adjust filenames to match `code/`):

   | Step | Script | What it does |
   |------|--------|--------------|
   | 1 | `code/generate_db_rr.r` | Builds the analysis dataset from raw NAAB/CDCB files: defines lines by founder sire (`sire_id`), flags "popular" bulls (95th percentile of number of sons), runs propensity-score matching on PTAs/haplotypes, and assembles the bull-level panel with inbreeding coefficients. |
   | 2 | `code/DiD_models_new.R` | Main analysis: Models 1–4 difference-in-differences (`fixest`), balance table, event-study coefficient plots, two-way clustering robustness, and the cost/benefit appraisal. Writes outputs to `figures/` and `tables/`. |

### Revision (R&R) analyses

These scripts refine the welfare appraisal in response to referee comments. They assume the
objects created by `DiD_models_new.R` are in memory (`tab`, `data_full`, `fit4`, `disc`) —
run that script first in the same session.

| Script | Purpose |
|--------|---------|
| `code/cost_recompute_R2.R` | Recomputes the inbreeding-cost NPV applying the differential ATT only to the **treated (popular-line) population**, not the whole herd. |
| `code/cost_sensitivity_R2.R` | Sensitivity of the cost NPV to the assumed popular-line share of the national herd; writes `tables/cost_npv_table.tex`. |
| `code/benefit_recompute_R2.R` | Net Merit benefit as a treatment–control differential on the same base as the cost. |
| `code/bootstrap_bc_R2.R` | Line-clustered bootstrap of the benefit–cost ratio and net NPV, with percentile confidence intervals. |

> Filenames above reflect the analysis pipeline; rename to match whatever is committed in
> `code/`.

## Data

The primary data are the NAAB published sire summaries and the Council on Dairy Cattle
Breeding (CDCB) genetic evaluations (predicted transmitting abilities and inbreeding
coefficients) for Holstein bulls, 2001–2020.

> **Note on availability:** portions of the NAAB/CDCB data may be subject to redistribution
> restrictions. Confirm what can be shared publicly; if the raw inputs cannot be posted,
> keep only the derived/aggregated analysis file in `data/` and document how to obtain the
> raw data from NAAB and CDCB. *(Edit this section to state the actual terms.)*

## Key identifiers in the data

- `sire_id` — founder line identifier; the clustering unit for all standard errors.
- `treat` — 1 if a bull descends from a "popular" (95th-percentile) founder line, 0 if from a matched control line.
- `yob` — year of birth (event time; genomic testing effective from 2010).
- `inbreeding` — Wright inbreeding coefficient (percent) at first release.
- PTA columns (`pta_*`) — predicted transmitting abilities used for matching and controls.
- `NM`, `NM_unadj` — Net Merit index (reported / inbreeding-unadjusted).

## Citation

```bibtex
@unpublished{funesleal_hutchins_dairy,
  author = {Funes-Leal, Victor and Hutchins, Jared},
  title  = {Competing for Quality: Coordination Failure in Dairy Genetics Markets},
  year   = {2025},
  note   = {Working paper}
}
```
*(Update year and venue when the paper is published.)*

## License

Code is released under the [MIT License](LICENSE). Data are subject to their original
providers' terms.

## Contact

Victor Funes-Leal — Department of Agricultural Economics and Agribusiness, University of
Arkansas.
=======
Code and figures for my Job Market Paper
