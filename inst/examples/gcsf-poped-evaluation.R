# G-CSF / filgrastim PK-PD population FIM (PopED K/PD example).
# Canonical script: inst/vignette-scripts/vignette_05_gcse_pkpd_execute.R (vignette Vignette_05_gcse_pkpd).
#
# Usage:
#   source(system.file("examples", "gcsf-poped-evaluation.R", package = "PFIM"))

script = system.file("vignette-scripts", "vignette_05_gcse_pkpd_execute.R", package = "PFIM")
if (!nzchar(script) || !file.exists(script))
  script = file.path("inst", "vignette-scripts", "vignette_05_gcse_pkpd_execute.R")
if (!file.exists(script))
  stop("vignette_05_gcse_pkpd_execute.R not found.", call. = FALSE)

env = new.env(parent = globalenv())
source(script, local = env)
invisible(list(
  evaluation = env$evaluationPop,
  rse = env$rse,
  cmp_mu = env$cmp_mu,
  cmp_d = env$cmp_d
))
