# seabORD offal / Berwick Bank study — working notes

Handoff document. Everything needed to resume without prior conversation context.

## Goal

How much offal, deposited daily near the Isle of May, compensates for **Berwick
Bank's** impact on kittiwake **adult annual survival** and **productivity**?

Design: run NnG+IC+SG (target) → add BB (the loss) → add offal and sweep the
deposit until survival and productivity return to the without-BB target.

## Environment (this machine)

- **R 4.6.0.** Package installed to `C:/Users/dallas.jordan/AppData/Local/R/win-library/4.6`.
- **renv is disabled** via a local, git-ignored `.Renviron`
  (`RENV_CONFIG_AUTOLOADER_ENABLED=FALSE`). Its library was never usable
  (OneDrive file locks + R-version skew).
- **After any change to `R/*.R` you must reinstall AND restart R.** Reinstalling
  does not update an already-loaded namespace. This silently invalidated a full
  sweep once. Check with:
  `any(grepl("OffalCell", deparse(body(seabORD::seabord))))`
- Repo is a **fork** of NERC-CEH/seabORD_pkg. PRs must target
  `dallasjordan/seabORD_testing:main` — GitHub defaults the base to upstream.
- Scripts are `source()`d, so `experiments/*.R` edits need no reinstall.

## Files

| file | role |
|---|---|
| `experiments/_setup_inputs.R` | shared inputs, `CALIBRATED_PMEDIAN`, displacement params, colony point |
| `experiments/03_calibrate_pmedian.R` | prey calibration (no ORDs) |
| `experiments/04_bb_compensation_experiment.R` | the main experiment |
| `experiments/05_comparable_survival.R` | post-hoc survival on a common reference |
| `experiments/transect_helpers.R` | `make_point_prey`, `make_area_prey`, diagnostics |
| `experiments/windfarms.R` | loads the 4-windfarm shapefile, EPSG:3857 → 3035 |

## Settled parameters

- **`CALIBRATED_PMEDIAN = 175` g/cell.** From experiment 03. Chicks-per-nest is
  the *binding* constraint (steep); adult mass loss is satisfied across a wide
  prey range. The package example's 158 was far too low — productivity ≈ 0.
- **Displacement: 30%, 2 km footprint buffer** (NatureScot), set study-wide in
  `_setup_inputs.R`. The package example ships 60% — a demo value, not guidance.
- **Offal: 9 kJ/g**, dropped **every timestep**, 47% of adults fly to the patch.
- Kittiwake: `energy_prey` 6.52 kJ/g · `IR_max` 4.369 g/min · `IR_half_a` 900 g ·
  `daylength` 36 h · `seasonlength` 30 · `adult_DEE_mn` 802 kJ · `beta` 0.038 ·
  `basesurv` poor/modr/good = 0.65/0.80/0.90 at mass loss 20/10/0%.

## Measured results (10% population, 20 reps)

| | productivity | mass loss |
|---|---|---|
| without_BB (NnG+IC+SG) | **0.5434** | 9.21% |
| with_BB (+Berwick Bank) | **0.4734** | 9.33% |

**BB costs −0.070 chicks/nest (−13% relative).** Its direct effect on adult
survival is negligible (mass loss barely moves).

## Mechanism findings (these drive interpretation)

1. **Chicks die of UNATTENDANCE, not starvation.** Cause-of-death: ~137 of ~150
   deaths are `CoD.unattended`; `CoD.starved` is exactly **1.0** in every config.
   BB's impact runs through *parents being away from the nest longer*, not food
   shortage. So interventions help mainly by getting parents home sooner.
2. **Adults protect themselves and sacrifice the chick.** `calc_strategy` defends
   adult condition down to an abandonment threshold. Hence unchanged adult mass
   and failed nests. Within-season adult survival is **1.0 by construction** —
   confirmed zero adult deaths in the package's own scenario *and* calibration
   examples.
3. **`ChicksPerNest` is bounded [0, 1].** seabORD models **one chick per nest**;
   the metric is *proportion of nests fledging*, not a brood count. Report it as
   such — real kittiwakes can fledge 2.

## Model quirks that have caused errors

1. **Prey never depletes** between birds or timesteps. A cell's value is offered
   *in full* to *every* bird, *every* timestep. Putting a raw daily drop in the
   cell lets 273 birds each eat the whole drop → everything above ~1 kg saturates
   identically. Hence `SHARE_DEPOSIT = TRUE` divides the daily drop by the number
   of feeding birds. (There *is* within-bout depletion for a single bird.)
2. **`AdultsSurvivingYr` is referenced to the same config's own base season**
   (`meanbm <- mean(YearBirds$base$BM_adult)`). It measures the ORD effect
   *within* a config. Offal is present in both seasons, so it cancels out and
   survival looks flat — even slightly negative. **Use
   `05_comparable_survival.R`**, which re-references all configs to one fixed
   mass and cross-checks against the mass-loss bands. On the stale run the
   correction was +8.0 pp (common ref) / +4.3 pp (band) vs −1.5 pp native.
3. **Birds forage to a requirement, then stop.** A richer patch means the same
   food gathered *faster*, not more food. Benefits show in the time budget.
4. **Destinations come from `BrdData` only** — birds do *not* move toward prey.
   `PreyMap` changes what they find, never where they go.
5. **1 km is the spatial atom.** Sub-km features are averaged away. A single
   enriched cell is reached by ~0.04% of foraging pressure.
6. **Competition is nearly inert** (`IR_half_b = 0.02`): even 1000× crowding
   raises the half-saturation constant by ~15%.
7. Each replicate runs **two seasons**: `base` (no ORD effect) and `scen` (ORD
   displacement on). Experiment 04 compares `scen` *across configs*; all configs
   share `initialseed`, so those comparisons are paired.

## Open / unresolved

- **`Prob_Barrier` is still 1.0** — inherited from the package example, not
  guidance. It drives the extra flight cost, so it scales BB's impact. Confirm
  before reporting.
- The offal-cell scenario bundles **offal energy + shorter commute + immunity
  from displacement** (birds no longer route past the windfarms). Report as a
  dump-site intervention, not as "offal energy alone".
- The corrected sweep `c(0, 100, 500, 1000, 2000, 4000)` kg/day has **not been
  run yet**. If the curve crosses the target below 100 kg, add low-end points.
- Survival should be reported via `05_comparable_survival.R`; prefer the
  band-interpolation figure (more conservative, uses the model's own anchors).
  It is computed from *mean* mass — good for comparing configs, not an absolute
  colony rate.

## Working practice

Quote **model output**, not arithmetic done alongside it. Several errors came
from estimating quantities (`~250 g` when the measured value was 203 g;
constant-rate foraging times when the model depletes within a bout). Call
`seabORD:::calc_foragecapture()`, read `energeticsandpreydata`, or inspect the
saved outputs — and flag anything unverified as an estimate.

`04` now saves the full seabord return (minus `BirdFlightMap`) per config to
`outputs/bb_compensation_raw.rds`, so metrics can be recomputed without
re-simulating. Discarding data has already forced one full re-run.
