## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>", 
  warning = FALSE, 
  message = FALSE
)

## ---- eval = FALSE------------------------------------------------------------
#  vignette("start-here", "spsurvey")

## -----------------------------------------------------------------------------
library(spsurvey)
set.seed(51)

## -----------------------------------------------------------------------------
eqprob <- grts(NE_Lakes, n_base = 50)

## ---- eval = FALSE------------------------------------------------------------
#  sp_plot(eqprob, NE_Lakes, key.width = lcm(3))

## ---- echo = FALSE------------------------------------------------------------
sp_plot(eqprob, NE_Lakes, key.width = lcm(3), key.pos = 4)

## -----------------------------------------------------------------------------
caty_n <- c(small = 40, large = 10)
uneqprob <- grts(
  NE_Lakes,
  n_base = 50,
  caty_var = "AREA_CAT",
  caty_n = caty_n
)

## -----------------------------------------------------------------------------
propprob <- grts(
  NE_Lakes,
  n_base = 50,
  aux_var = "AREA"
)

## -----------------------------------------------------------------------------
strata_n <- c(low = 25, high = 15)
strat_eqprob <- grts(NE_Lakes, n_base = strata_n, stratum_var = "ELEV_CAT")

## ---- eval = FALSE------------------------------------------------------------
#  sp_plot(
#    strat_eqprob,
#    formula = siteuse ~ ELEV_CAT,
#    NE_Lakes,
#    key.width = lcm(3)
#  )

## ---- echo = FALSE------------------------------------------------------------
sp_plot(
  strat_eqprob,
  formula = siteuse ~ ELEV_CAT,
  NE_Lakes,
  key.width = lcm(3),
  key.pos = 4
)

## -----------------------------------------------------------------------------
caty_n <- list(
  low = c(small = 20, large = 5),
  high = c(small = 10, large = 5)
)
strat_uneqprob <- grts(
  NE_Lakes,
  n_base = strata_n,
  stratum_var = "ELEV_CAT",
  caty_var = "AREA_CAT",
  caty_n = caty_n
)

## -----------------------------------------------------------------------------
strat_propprob <- grts(
  NE_Lakes,
  n_base = strata_n,
  stratum_var = "ELEV_CAT",
  aux_var = "AREA"
)

## -----------------------------------------------------------------------------
legacy <- grts(NE_Lakes, n_base = 50, legacy_sites = NE_Lakes_Legacy)

## ---- eval = FALSE------------------------------------------------------------
#  sp_plot(legacy, key.width = lcm(3))

## ---- echo = FALSE------------------------------------------------------------
sp_plot(legacy, key.width = lcm(3), key.pos = 4)

## -----------------------------------------------------------------------------
mindis <- grts(NE_Lakes, n_base = 50, mindis = 1600)

## -----------------------------------------------------------------------------
mindis_list <- list(low = 1400, high = 1000)
strat_mindis <- grts(
  NE_Lakes,
  strata_n,
  stratum_var = "ELEV_CAT",
  mindis = mindis_list
)

## -----------------------------------------------------------------------------
rho_replace <- grts(NE_Lakes, n_base = 50, n_over = 25)

## ---- eval = FALSE------------------------------------------------------------
#  sp_plot(rho_replace, key.width = lcm(3))

## ---- echo = FALSE------------------------------------------------------------
sp_plot(rho_replace, key.width = lcm(3), key.pos = 4)

## -----------------------------------------------------------------------------
over_list <- list(low = 2, high = 5)
strat_rho_replace <- grts(
  NE_Lakes,
  strata_n,
  stratum_var = "ELEV_CAT",
  n_over = over_list
)

## -----------------------------------------------------------------------------
nn_replace <- grts(NE_Lakes, n_base = 50, n_near = 1)

## -----------------------------------------------------------------------------
near_list <- list(low = 1, high = 2)
strat_nn_replace <- grts(
  NE_Lakes,
  strata_n,
  stratum_var = "ELEV_CAT",
  n_near = near_list
)

## -----------------------------------------------------------------------------
eqprob_irs <- irs(NE_Lakes, n_base = 50)

## ---- eval = FALSE------------------------------------------------------------
#  sp_plot(eqprob_irs, NE_Lakes, key.width = lcm(3))

## ---- echo = FALSE------------------------------------------------------------
sp_plot(eqprob_irs, NE_Lakes, key.width = lcm(3), key.pos = 4)

## -----------------------------------------------------------------------------
strata_n <- c(low = 25, high = 15)
strat_eqprob_irs <- irs(NE_Lakes, n_base = strata_n, stratum_var = "ELEV_CAT")

## -----------------------------------------------------------------------------
sp_balance(eqprob$sites_base, NE_Lakes) # grts
sp_balance(eqprob_irs$sites_base, NE_Lakes) # irs

## -----------------------------------------------------------------------------
sp_balance(strat_eqprob$sites_base, NE_Lakes, stratum_var = "ELEV_CAT") # grts
sp_balance(strat_eqprob_irs$sites_base, NE_Lakes, stratum_var = "ELEV_CAT") # irs

## -----------------------------------------------------------------------------
eqprob <- grts(Illinois_River, n_base = 50)

## ---- eval = FALSE------------------------------------------------------------
#  sp_plot(eqprob, Illinois_River, key.width = lcm(3))

## ---- echo = FALSE------------------------------------------------------------
sp_plot(eqprob, Illinois_River, key.width = lcm(3), key.pos = 4)

## -----------------------------------------------------------------------------
legacy <- grts(Illinois_River, n_base = 50, legacy_sites = Illinois_River_Legacy)

## -----------------------------------------------------------------------------
eqprob <- grts(Lake_Ontario, n_base = 50)

## ---- eval = FALSE------------------------------------------------------------
#  sp_plot(eqprob, Lake_Ontario, pch = 19, key.width = lcm(3))

## ---- echo = FALSE------------------------------------------------------------
sp_plot(eqprob, Lake_Ontario, pch = 19, key.width = lcm(3), key.pos = 4)

## -----------------------------------------------------------------------------
combined <- sp_rbind(rho_replace)

## ---- eval = FALSE------------------------------------------------------------
#  write_sf(combined, "file_path/file_name.shp")

