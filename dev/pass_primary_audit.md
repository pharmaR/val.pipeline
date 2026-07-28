# `pass_primary` audit (2026-07-28)

The `pass_primary` block in `inst/config.yml` is meant for packages that would
otherwise be filtered out by the `downloads_1yr` primary metric in
`remote_reduce` (High risk when `< 80,000` annual downloads). Packages that
comfortably clear that threshold on their own don't need the bypass — they'd
be Medium/Low regardless — so keeping them in `pass_primary` just clutters
the list and hides its intent.

This audit pruned every entry whose rolling 12-month CRAN downloads (per
`cranlogs.r-pkg.org`, range `2025-07-28:2026-07-27`) were `>= 80,000`.
Packages with `0` reported downloads (base R, Bioconductor-only,
GitHub-only, org-internal, and packages not on CRAN in general) were
**kept**, since the bypass is exactly what they need.

## Summary

| category                                     | count |
|----------------------------------------------|------:|
| original `pass_primary`                      |  575 |
| kept (`< 80,000` annual downloads, or off-CRAN) |  184 |
| **removed** (`>= 80,000` annual downloads)   | ** 391** |

## Removed packages (with annual downloads)

| package | annual downloads |
|---|---:|
| `rlang` | 27,153,864 |
| `ggplot2` | 27,086,797 |
| `cli` | 23,964,574 |
| `vctrs` | 23,758,137 |
| `lifecycle` | 23,676,743 |
| `tibble` | 22,416,215 |
| `Rcpp` | 21,657,786 |
| `dplyr` | 21,592,784 |
| `glue` | 20,881,211 |
| `magrittr` | 20,528,032 |
| `R6` | 20,265,595 |
| `scales` | 20,060,731 |
| `pillar` | 19,800,995 |
| `withr` | 19,280,708 |
| `jsonlite` | 17,883,999 |
| `rmarkdown` | 17,852,042 |
| `cpp11` | 17,655,285 |
| `gtable` | 17,232,511 |
| `purrr` | 17,082,345 |
| `utf8` | 16,796,690 |
| `curl` | 16,353,004 |
| `fs` | 16,330,159 |
| `pkgconfig` | 16,119,293 |
| `generics` | 16,006,715 |
| `isoband` | 15,798,640 |
| `tidyr` | 15,789,175 |
| `xfun` | 15,474,367 |
| `stringr` | 15,259,422 |
| `viridisLite` | 15,054,414 |
| `farver` | 14,875,518 |
| `RColorBrewer` | 14,794,936 |
| `digest` | 14,632,700 |
| `bslib` | 14,549,342 |
| `knitr` | 14,544,385 |
| `processx` | 14,499,969 |
| `labeling` | 14,249,936 |
| `tidyselect` | 14,248,236 |
| `stringi` | 14,246,746 |
| `htmltools` | 14,191,471 |
| `sass` | 14,100,030 |
| `callr` | 14,061,455 |
| `ps` | 14,008,220 |
| `yaml` | 13,776,179 |
| `evaluate` | 13,747,146 |
| `mime` | 13,558,238 |
| `systemfonts` | 13,550,282 |
| `tinytex` | 13,392,671 |
| `crayon` | 13,296,632 |
| `RcppEigen` | 13,245,178 |
| `httr` | 13,192,192 |
| `fastmap` | 13,167,288 |
| `tidyverse` | 13,164,435 |
| `jquerylib` | 13,156,791 |
| `readxl` | 13,147,263 |
| `base64enc` | 13,005,168 |
| `cachem` | 12,794,408 |
| `highr` | 12,770,130 |
| `rappdirs` | 12,712,876 |
| `textshaping` | 12,675,647 |
| `readr` | 12,659,182 |
| `ragg` | 12,581,943 |
| `fontawesome` | 12,530,493 |
| `memoise` | 12,398,342 |
| `openssl` | 12,135,171 |
| `xml2` | 12,102,351 |
| `testthat` | 12,028,674 |
| `backports` | 11,914,006 |
| `data.table` | 11,901,774 |
| `vroom` | 10,993,440 |
| `pkgbuild` | 10,947,085 |
| `progress` | 10,616,975 |
| `hms` | 10,414,459 |
| `lubridate` | 10,211,032 |
| `desc` | 10,204,327 |
| `waldo` | 10,132,496 |
| `askpass` | 10,113,513 |
| `sys` | 10,029,682 |
| `diffobj` | 9,877,814 |
| `htmlwidgets` | 9,872,715 |
| `rstudioapi` | 9,796,565 |
| `gridExtra` | 9,716,406 |
| `prettyunits` | 9,660,873 |
| `bit64` | 9,653,698 |
| `bit` | 9,445,143 |
| `rprojroot` | 9,308,566 |
| `tzdb` | 9,101,943 |
| `broom` | 8,943,894 |
| `zoo` | 8,889,387 |
| `pkgload` | 8,887,114 |
| `clipr` | 8,483,347 |
| `DBI` | 8,478,301 |
| `commonmark` | 8,471,996 |
| `abind` | 8,419,319 |
| `promises` | 8,377,245 |
| `numDeriv` | 8,283,114 |
| `later` | 8,262,920 |
| `checkmate` | 8,023,979 |
| `timechange` | 7,994,743 |
| `haven` | 7,934,868 |
| `shiny` | 7,879,216 |
| `BH` | 7,757,684 |
| `forcats` | 7,597,323 |
| `httpuv` | 7,336,552 |
| `dbplyr` | 7,285,842 |
| `modelr` | 7,234,699 |
| `RcppArmadillo` | 7,117,740 |
| `lme4` | 7,110,325 |
| `rvest` | 7,094,756 |
| `lazyeval` | 7,083,074 |
| `cellranger` | 7,071,460 |
| `rematch` | 6,981,805 |
| `brio` | 6,879,848 |
| `uuid` | 6,827,800 |
| `blob` | 6,809,446 |
| `conflicted` | 6,792,946 |
| `praise` | 6,615,459 |
| `googlesheets4` | 6,601,478 |
| `xtable` | 6,545,190 |
| `googledrive` | 6,533,388 |
| `gargle` | 6,516,727 |
| `remotes` | 6,500,754 |
| `car` | 6,446,771 |
| `matrixStats` | 6,330,166 |
| `sourcetools` | 6,093,612 |
| `plyr` | 6,055,142 |
| `dtplyr` | 6,050,783 |
| `reprex` | 6,046,700 |
| `distributional` | 6,021,742 |
| `reshape2` | 5,998,958 |
| `colorspace` | 5,927,033 |
| `nloptr` | 5,880,453 |
| `selectr` | 5,854,642 |
| `rematch2` | 5,739,582 |
| `ids` | 5,471,989 |
| `e1071` | 5,410,817 |
| `quantreg` | 5,286,307 |
| `png` | 5,236,889 |
| `MatrixModels` | 5,214,808 |
| `openxlsx` | 5,109,288 |
| `zip` | 5,061,892 |
| `markdown` | 5,033,737 |
| `xts` | 5,002,305 |
| `proxy` | 4,963,575 |
| `ggrepel` | 4,731,931 |
| `pbkrtest` | 4,720,002 |
| `minqa` | 4,705,703 |
| `renv` | 4,678,644 |
| `cowplot` | 4,635,497 |
| `httr2` | 4,628,868 |
| `Formula` | 4,590,423 |
| `mvtnorm` | 4,489,488 |
| `future` | 4,431,272 |
| `V8` | 4,324,184 |
| `bitops` | 4,189,113 |
| `miniUI` | 4,188,682 |
| `units` | 4,158,380 |
| `parallelly` | 4,153,411 |
| `SparseM` | 4,092,701 |
| `doBy` | 4,007,437 |
| `svglite` | 3,963,652 |
| `globals` | 3,903,979 |
| `foreach` | 3,763,845 |
| `patchwork` | 3,756,378 |
| `carData` | 3,745,627 |
| `iterators` | 3,684,809 |
| `microbenchmark` | 3,666,361 |
| `classInt` | 3,654,067 |
| `here` | 3,644,638 |
| `Deriv` | 3,574,212 |
| `listenv` | 3,474,718 |
| `sessioninfo` | 3,438,597 |
| `devtools` | 3,398,562 |
| `gh` | 3,334,742 |
| `future.apply` | 3,276,096 |
| `RcppTOML` | 3,261,879 |
| `janitor` | 3,252,063 |
| `XML` | 3,232,520 |
| `gtools` | 3,124,843 |
| `roxygen2` | 3,076,584 |
| `ini` | 3,060,188 |
| `corrplot` | 3,033,760 |
| `ggpubr` | 3,030,672 |
| `sandwich` | 3,025,225 |
| `reticulate` | 3,023,055 |
| `gitcreds` | 3,014,185 |
| `BiocManager` | 2,967,719 |
| `assertthat` | 2,904,502 |
| `rstatix` | 2,786,821 |
| `quadprog` | 2,756,694 |
| `writexl` | 2,737,733 |
| `usethis` | 2,684,805 |
| `psych` | 2,634,239 |
| `shinyjs` | 2,629,504 |
| `pkgdown` | 2,622,720 |
| `R.utils` | 2,585,669 |
| `gert` | 2,577,913 |
| `progressr` | 2,547,999 |
| `R.oo` | 2,523,402 |
| `rex` | 2,516,660 |
| `viridis` | 2,504,907 |
| `magick` | 2,498,604 |
| `Hmisc` | 2,454,874 |
| `R.methodsS3` | 2,422,995 |
| `brew` | 2,417,981 |
| `snakecase` | 2,411,611 |
| `fansi` | 2,392,741 |
| `kableExtra` | 2,347,200 |
| `credentials` | 2,327,462 |
| `ggsci` | 2,315,883 |
| `insight` | 2,302,638 |
| `glmnet` | 2,301,395 |
| `whisker` | 2,299,185 |
| `ellipsis` | 2,264,025 |
| `downlit` | 2,148,133 |
| `profvis` | 2,147,960 |
| `rcmdcheck` | 2,140,291 |
| `reactR` | 2,138,190 |
| `jpeg` | 2,132,733 |
| `hardhat` | 2,130,002 |
| `caret` | 2,105,238 |
| `reactable` | 2,092,500 |
| `gt` | 2,068,898 |
| `xopen` | 2,060,196 |
| `emmeans` | 2,055,618 |
| `RCurl` | 2,044,028 |
| `htmlTable` | 2,042,646 |
| `juicyjuice` | 2,037,415 |
| `pROC` | 2,028,825 |
| `bigD` | 2,020,310 |
| `rversions` | 2,004,574 |
| `polynom` | 1,960,814 |
| `gdtools` | 1,908,829 |
| `ggsignif` | 1,854,879 |
| `mnormt` | 1,842,073 |
| `MASS` | 1,836,358 |
| `multcomp` | 1,813,155 |
| `datawizard` | 1,783,527 |
| `doParallel` | 1,772,068 |
| `urlchecker` | 1,768,244 |
| `Matrix` | 1,754,121 |
| `GPArotation` | 1,720,865 |
| `survival` | 1,692,244 |
| `pak` | 1,684,861 |
| `pdftools` | 1,678,704 |
| `TH.data` | 1,676,081 |
| `pacman` | 1,636,846 |
| `caTools` | 1,520,972 |
| `polyclip` | 1,503,049 |
| `statmod` | 1,465,795 |
| `ggthemes` | 1,461,613 |
| `lattice` | 1,455,626 |
| `officer` | 1,439,240 |
| `deldir` | 1,413,962 |
| `cluster` | 1,404,696 |
| `ggforce` | 1,388,575 |
| `furrr` | 1,378,873 |
| `formatR` | 1,365,360 |
| `fontquiver` | 1,290,480 |
| `fontLiberation` | 1,279,085 |
| `tweenr` | 1,270,363 |
| `nlme` | 1,255,207 |
| `fontBitstreamVera` | 1,251,257 |
| `mgcv` | 1,229,083 |
| `flextable` | 1,222,983 |
| `multcompView` | 1,220,749 |
| `leaps` | 1,212,144 |
| `bayestestR` | 1,205,171 |
| `plotrix` | 1,174,484 |
| `rpart` | 1,153,114 |
| `gridtext` | 1,122,749 |
| `jose` | 1,114,687 |
| `mice` | 1,085,316 |
| `repr` | 1,085,039 |
| `labelled` | 1,060,994 |
| `rJava` | 1,058,802 |
| `boot` | 1,047,238 |
| `R.cache` | 986,166 |
| `bookdown` | 978,927 |
| `GGally` | 962,104 |
| `ggtext` | 949,580 |
| `lobstr` | 935,238 |
| `foreign` | 923,136 |
| `xgboost` | 904,539 |
| `tidymodels` | 886,198 |
| `ggstats` | 861,978 |
| `Cairo` | 803,524 |
| `lintr` | 803,141 |
| `styler` | 794,523 |
| `moments` | 767,181 |
| `webshot` | 762,372 |
| `bdsmatrix` | 757,477 |
| `xmlparsedata` | 755,163 |
| `websocket` | 742,723 |
| `gridGraphics` | 733,560 |
| `fastDummies` | 731,722 |
| `nnet` | 730,101 |
| `deSolve` | 721,846 |
| `interp` | 711,912 |
| `gtsummary` | 696,226 |
| `latticeExtra` | 692,323 |
| `munsell` | 691,772 |
| `AsioHeaders` | 686,219 |
| `chromote` | 678,694 |
| `pander` | 672,523 |
| `reshape` | 665,783 |
| `survminer` | 644,188 |
| `rio` | 639,577 |
| `class` | 631,863 |
| `servr` | 631,126 |
| `cards` | 615,291 |
| `KernSmooth` | 595,452 |
| `skimr` | 588,166 |
| `qpdf` | 566,018 |
| `chron` | 551,274 |
| `AzureAuth` | 549,801 |
| `ggdist` | 530,806 |
| `cardx` | 519,510 |
| `AzureGraph` | 517,350 |
| `codetools` | 507,080 |
| `exactRankTests` | 473,834 |
| `rngtools` | 452,853 |
| `maxstat` | 441,473 |
| `webshot2` | 432,727 |
| `gam` | 429,864 |
| `doRNG` | 429,630 |
| `PerformanceAnalytics` | 422,338 |
| `bbmle` | 411,122 |
| `spatial` | 409,663 |
| `ggfortify` | 398,822 |
| `sjlabelled` | 396,603 |
| `data.tree` | 386,948 |
| `crul` | 380,353 |
| `collections` | 374,677 |
| `cmprsk` | 364,692 |
| `sem` | 357,345 |
| `extrafont` | 349,545 |
| `geepack` | 345,959 |
| `httpcode` | 328,850 |
| `Rttf2pt1` | 321,263 |
| `waiter` | 316,028 |
| `extrafontdb` | 313,880 |
| `cubature` | 310,848 |
| `languageserver` | 300,261 |
| `ggpp` | 293,456 |
| `egg` | 286,747 |
| `ggh4x` | 285,686 |
| `KMsurv` | 273,489 |
| `summarytools` | 261,094 |
| `whoami` | 258,613 |
| `keyring` | 256,465 |
| `VIM` | 253,905 |
| `km.ci` | 251,833 |
| `fastGHQuad` | 248,805 |
| `survMisc` | 230,713 |
| `muhaz` | 216,399 |
| `timereg` | 209,279 |
| `torch` | 202,262 |
| `lmodel2` | 200,350 |
| `latex2exp` | 197,007 |
| `ggpmisc` | 193,486 |
| `shinyAce` | 191,375 |
| `splines2` | 180,345 |
| `rstpm2` | 176,518 |
| `cyclocomp` | 171,187 |
| `mstate` | 170,824 |
| `pryr` | 169,867 |
| `flexsurv` | 164,714 |
| `rapportools` | 164,714 |
| `ellmer` | 148,366 |
| `hash` | 143,138 |
| `table1` | 140,151 |
| `np` | 133,915 |
| `splus2R` | 133,699 |
| `confintr` | 129,487 |
| `doconv` | 125,776 |
| `lemon` | 111,310 |
| `mgsub` | 109,268 |
| `clisymbols` | 108,228 |
| `maditr` | 107,997 |
| `expss` | 107,365 |
| `r2rtf` | 105,334 |
| `configr` | 101,086 |
| `locatexec` | 98,475 |
| `mmrm` | 91,630 |
| `ggsurvfit` | 90,452 |
| `formatters` | 88,374 |
| `pointblank` | 87,781 |
| `log4r` | 85,623 |
| `XLConnect` | 85,265 |
| `glmtoolbox` | 81,266 |
| `arsenal` | 81,139 |

## Data-quality follow-ups noticed during the audit

Two entries are left in place but look like bugs in the source config;
they should be resolved in a separate PR to keep this audit reviewable
in isolation:

- `noindex` — the original config had `noindex)` with a stray closing
  paren; the paren was removed during extraction but the word `noindex`
  is unlikely to be a real CRAN package name.
- `randomForrest` — almost certainly a typo of `randomForest`
  (single 'r'). `randomForest` itself has millions of annual downloads
  and would be pruned anyway; the typo has `0` downloads and was kept.

## Reproducing this audit

```python
import urllib.request, json
from datetime import date, timedelta
end = date.today() - timedelta(days=1)
start = end - timedelta(days=364)
url = f"https://cranlogs.r-pkg.org/downloads/total/{start}:{end}/PKG1,PKG2,..."
```

Batches of 50 packages, `User-Agent` header required, `2s` per batch is
plenty. Any package returning `0` downloads should be inspected — it may
be off-CRAN (keep) or misspelled (fix).
