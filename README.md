# Interactive Data Features on Indigenous Communities and Disaster Resilience and Recovery

Analysis supporting two interactive features relating to Indigenous
communities and disaster recovery and resilience.

- **Feature 1:** [LINK HERE]
- **Feature 2:** [LINK HERE]

## Repository layout

The overarching project convention is that specific tasks are in dedicated
`.R` files, which are then sourced in `_feature1.qmd` and `_feature2.qmd`,
respectively. It is these `.qmd` files that generate the final data, charts,
and statistics reflected in the two interactive features on urban.org. (Note
that charts are included merely as references; actual interactive charts are
developed separately, outside of this repository.)

Scripts, data, and metadata are organized as below:

```
scripts/
  feature_1/    # Interactive data tool featuring tribes' histories of presidentially-declared disasters
  feature_2/    # The charts and statistics included in the narrative feature
  utilities/    # shared helpers (name normalization, substring matching, coverage scoring)
data/
  nri-tracts/   # National Risk Index tract-level hazard data, cached locally for convenience
  crosswalks/   # For translating from source data geographies to our target geographies (primarily federally-recognized tribes)
outputs/        # Rendered figures and data reflecting what appears in the two features
renv.lock       # A snapshot of the specific package versions used to generate all outputs
```

## How to run

Dependencies are pinned with `renv`. From an R session at the repo root:

```r
renv::restore()
```

This will ensure that you have loaded all of the same package versions as
were used to develop these data originally.

## Feature 1

The primary work of Feature 1 is to relate FEMA presidential disaster
declarations data back to a consistent set of Indigenous entities. While this
seems a simple task at the outset, in practice it entails dealing with
inconsistently named and identified FEMA data, Indigenous entities, and
Indigenous geographies.

Interested users can step through individual scripts in `/feature_1` to
better understand how exactly this process was implemented, but
`/feature_1/_feature1.qmd` compiles the final set of Indigenous disaster
declarations data that are likely to be of primary interest.

## Feature 2

This feature is focused on assembling data describing Indigenous communities'
prior disaster costs, disaster exposure and risk, and disaster assistance
funding. Component scripts within `/feature_2` handle data preparation for
each figure--or, where a single dataset is used across multiple figures, for
each dataset--and `/feature_2/_feature2.qmd` then sources these data
preparation scripts to assemble and plot the final data and generate any
statistics that are used in the feature. Note that the data for Feature 2
reliese on the universe of Indigenous entities created as part of the
Feature 1 workflow, which should be run first.

At a high level, Feature 2 figures include:

| Figure | Topic                                          |
| ------ | ---------------------------------------------- |
| 1      | SHELDUS cumulative hazard damages              |
| 2      | Distribution of major disaster declarations    |
| 3      | Per-capita estimated annual losses (NRI)       |
| 4      | Distribution of community risk scores (NRI)    |
| 5      | Awarded recovery funding (IHP + PA) by program |
| 6      | SBA approved loan amounts                      |
| 7      | Awarded HMA (mitigation) funding by program    |
