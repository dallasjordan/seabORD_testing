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
| `experiments/01_calibrate_pmedian.R` | prey calibration (no ORDs) |
| `experiments/02_bb_compensation_experiment.R` | the main experiment |
| `experiments/03_comparable_survival.R` | post-hoc survival on a common reference |
| `experiments/04_final_summary.R` | reporting layer: chicks, figure, study answers |
| `experiments/transect_helpers.R` | `make_point_prey`, `make_area_prey`, diagnostics |
| `experiments/windfarms.R` | loads the 4-windfarm shapefile, EPSG:3857 → 3035 |
| `experiments/archive/` | superseded scripts, kept for reference only |

Run order: `01` (only if inputs change) → `02` → `03` → `04`.
`03` and `04` read saved output and are safe to re-run at any time.

## Settled parameters

- **`CALIBRATED_PMEDIAN = 175` g/cell.** From experiment 01. Chicks-per-nest is
  the *binding* constraint (steep); adult mass loss is satisfied across a wide
  prey range. The package example's 158 was far too low — productivity ≈ 0.
- **Displacement: 30%, 2 km footprint buffer** (NatureScot), set study-wide in
  `_setup_inputs.R`. The package example ships 60% — a demo value, not guidance.
- **Offal: 9 kJ/g**, dropped **every timestep**. The share of adults that fly to
  the patch is **derived from the deposit** (`offal_access_frac` in `02`), not
  assumed. The old fixed 47% is superseded — it made the sweep flat, because
  every dose fed the same birds.
- **Colony size: `Npairspercol` should be 6068, not the package's 2898.**
  6,068 apparently occupied nests (AON), black-legged kittiwake, Isle of May,
  2025 — NatureScot *Isle of May NNR Annual Report 2025*, stated three times
  (section 2.3.3, Table 7 section totals, Table 10 series). The package example
  value 2898 is closest to the **2016** count (2,922); the colony has roughly
  doubled since, five consecutive years of increase. There is **no provenance for
  2898 anywhere in the package** — same class of stale demo value as the 60%
  displacement and the Pmedian of 158.
  - Note the site code is misleading: `UK9004171` is *Forth Islands SPA*
    (~4,500 pairs in 2021, 8,400 at designation) but the value is Isle of May
    scale. Isle of May is the right scope for this study — the colony point,
    the dump site and the framing are all Isle of May.
  - 5-year average is 5,252 AON if a single peak year feels too fragile to
    report against.
- Kittiwake: `energy_prey` 6.52 kJ/g · `IR_max` 4.369 g/min · `IR_half_a` 900 g ·
  `daylength` 36 h · `seasonlength` 30 · `adult_DEE_mn` 802 kJ · `beta` 0.038 ·
  `basesurv` poor/modr/good = 0.65/0.80/0.90 at mass loss 20/10/0%.

## Status

**The reportable sweep has not been run yet.** `02` is configured for
`COLONY_PAIRS = 6068` and writes to `bb_compensation_2c_6068_*`. Budget ~53 h;
it saves after each config and resumes, so an interruption costs one config.

Everything in `outputs/` predating that run was produced at the package's stale
2898 pairs and is superseded, including `final_results.csv` and
`final_doseresponse.png`.

For orientation only, the 2898 run gave without_BB 0.5434 and with_BB 0.4734
(BB costing −0.070, about −13% relative), with adult mass loss barely moving.
Expect the same *shape* at 6068 — a given tonnage feeds the same number of birds
but a smaller *share* of the colony, so the dose axis stretches by ~2x.

## What the re-run does and does not fix

Fixes: the colony size, the access fractions, and the old 10-vs-20 replicate
mismatch between sweep and baselines. Precision should improve — 607 nests per
replicate instead of 290.

Does **not** fix, and these matter more for reporting:

1. **The intervention is bundled.** Diverted birds get offal energy *plus* a
   shorter commute *plus* immunity from displacement, since they no longer route
   past the wind farms. Intrinsic to the design. Report it as a dump-site
   intervention, never as "offal energy".
2. **Crowding is unpenalised.** Every fed bird forages in one 1 km cell and the
   competition term is near-inert. Results are an upper bound.
3. **`Prob_Barrier` is still 1.0**, a package default rather than guidance. It
   drives the extra flight cost, so it scales BB's impact and therefore the whole
   answer. The cheapest outstanding item, and it moves everything.

## Colony size only reaches the model two ways

Worth knowing, because it explains what is and is not sensitive to `COLONY_PAIRS`:

1. **The access fraction.** `frac = kg * 1000 * (1-LOSS) / DAILY_G / n_adults`.
   A given tonnage buys a fixed number of kittiwake-days, so doubling the colony
   halves the share fed. This is why a colony-size change requires a re-run and
   cannot be rescaled after the fact.
2. **Competition**, via `popbirdsperkm2`. Near-inert: `IR_half_b = 0.02` means
   doubling the colony moves `IRhalf` from 900 to 913 g (+1.4%). Measured once at
   4.4 h (archived `06_colonysize_check.R`): productivity shifted +0.0167, CI
   −0.0008 to +0.0342 — consistent with zero, underpowered, and the *wrong sign*
   for competition, which raises `IRhalf` and so should push productivity down.

Note the dose itself does **not** change how well a fed bird eats: substituting
`frac` into `standing_g` cancels the tonnage, leaving ~280 g per fed bird at
every dose. The deposit controls **how many** birds are fed, not how richly.

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
3. **`ChicksPerNest` is bounded [0, 1].** Confirmed in source, not inferred:
   `set_initialchickstate` creates `Nc <- N / 2` chick rows — "one per pair of
   adults" — with `is_chick_alive` a 0/1 flag, and there is no clutch or brood
   parameter anywhere in `Par`
   (`functions-seabordsetup.R:295`). The metric itself is
   `N_alive_ch / nrow(data)` where the denominator is the number of pairs
   (`functions-seabordsummaries.R:289`). So it is *proportion of nests fledging*.
   - **The package labels it "Mean number of chicks per nest surviving"**
     (`functions-seabordsummaries.R:594`), which reads like a brood count. The
     name is wrong; the implementation is a proportion. This has caused confusion
     more than once.
   - **DO NOT compare it directly to field productivity.** Published Isle of May
     figures (0.68 in 2025, 0.70 average) are *chicks fledged per nest* — a
     COUNT, which can exceed 1 because real kittiwakes fledge 1–2 chicks. The
     two are related by:

         chicks per nest = proportion fledging × mean brood of successful nests

     seabORD measures only the first factor and pins the second at 1. So the
     field number is the larger of the two, and setting them side by side makes
     the model look pessimistic when it may not be. Converting requires the mean
     fledged brood size of successful nests, which we do not currently have
     (UKCEH monitor it).
   - This error was made once already: it produced a claim that the calibration
     was ~20% low and a recommendation to re-target experiment 01 at 0.70, which
     would have inflated the model badly. On a correct conversion the existing
     0.50 target looks about right.

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
   `03_comparable_survival.R`**, which re-references all configs to one fixed
   mass and cross-checks against the mass-loss bands. On the stale run the
   correction was +8.0 pp (common ref) / +4.3 pp (band) vs −1.5 pp native.
3. **Birds forage to a requirement, then stop.** A richer patch means the same
   food gathered *faster*, not more food. Benefits show in the time budget.
4. **Destinations come from `BrdData` only** — birds do *not* move toward prey.
   `PreyMap` changes what they find, never where they go.
5. **1 km is the spatial atom.** Sub-km features are averaged away. A single
   enriched cell is reached by ~0.04% of foraging pressure.
6. **Competition is nearly inert** (`IR_half_b = 0.02`, verified in
   `energeticsandpreydata`, not assumed): a 2× change in crowding moves the
   half-saturation constant 900 → 913 g (+1.4%); 100× moves it under 10%.
   Chain: `ComFactor = Birdsperkm2 / popbirdsperkm2` →
   `IRhalf = IR_half_a * ComFactor^IR_half_b` (`functions-seabordday.R:173-174`)
   → `IRhalf` is the half-saturation of a Holling type II depletion solved in
   `calc_foragecapture` (`functions-seabordbirds.R:562`). **Larger `IRhalf` =
   slower capture = worse foraging = lower productivity.** Know this sign: it is
   what tells you a *positive* productivity shift from a *bigger* colony cannot
   be competition.
7. Each replicate runs **two seasons**: `base` (no ORD effect) and `scen` (ORD
   displacement on). Experiment 02 compares `scen` *across configs*; all configs
   share `initialseed`, so those comparisons are paired.
8. **Replicate-count mismatches masquerade as real effects.** `with_BB` (0.4734)
   and `offalcell_0kg` (0.4828) are the *same scenario* and their reps 1–10 are
   **bit-identical** — the 0.009 gap is only 20-rep mean vs first-10 mean
   (reps 11–20 happened to run low, 0.4641). Two consequences:
   - The bit-identical output also proves the legacy baselines came from the same
     model build and seed stream as the 2c sweep — the reinstall hazard above did
     *not* bite there.
   - **The 2c threshold is scored against a mismatched reference**: the sweep is
     10 reps, the target is 20. Seed-matching the target (0.5472 rather than
     0.5434) moves the crossing 156 → 169 kg/day at the old colony size. Known
     and accepted for these initial experiments; fix before reporting.

## Open / unresolved

- **`Prob_Barrier` is still 1.0** — inherited from the package example, not
  guidance. It drives the extra flight cost, so it scales BB's impact. Confirm
  before reporting.
- The offal-cell scenario bundles **offal energy + shorter commute + immunity
  from displacement** (birds no longer route past the windfarms). Report as a
  dump-site intervention, not as "offal energy alone".
- The 2c sweep is **run and complete** (10 doses + both baselines,
  `outputs/bb_compensation_2c_*`). Outstanding on it:
  - **The crossing is inside replicate noise.** It sits between the two doses
    either side of the target, and the rise across that bracket (0.0145) is under
    2×SE (0.0221) — the script's own guard flags it unresolved. ~25 reps at those
    two doses would close it. Accepted as a known flaw for these initial runs.
  - **Drop the top two doses from any report.** At those the whole colony is
    diverted, so *no bird is displaced by BB at all* — that measures "BB switched
    off", not "BB compensated". It also puts the entire colony in one 1 km cell,
    which the near-inert competition model does not penalise.
  - Set `Npairspercol <- 6068` before any further runs; the existing results are
    valid but their kg axis needs the ×2.094 relabel.
- Survival should be reported via `03_comparable_survival.R`; prefer the
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
