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

## -----------------------------------------------------------------------------
sp_summary(NE_Lakes, formula = ~ ELEV)

## ---- eval = FALSE------------------------------------------------------------
#  sp_plot(NE_Lakes, formula = ~ ELEV)

## ---- echo = FALSE------------------------------------------------------------
sp_plot(NE_Lakes, formula = ~ ELEV, key.pos = 4)

## -----------------------------------------------------------------------------
sp_summary(NE_Lakes, formula = ~ ELEV_CAT)

## ---- eval = FALSE------------------------------------------------------------
#  sp_plot(NE_Lakes, formula = ~ ELEV_CAT, key.width = lcm(3))

## ---- echo = FALSE------------------------------------------------------------
sp_plot(NE_Lakes, formula = ~ ELEV_CAT, key.width = lcm(3), key.pos = 4)

## -----------------------------------------------------------------------------
sp_summary(NE_Lakes, formula = ~ ELEV_CAT + AREA_CAT)

## ---- eval = FALSE------------------------------------------------------------
#  sp_plot(NE_Lakes, formula = ~ ELEV_CAT + AREA_CAT, key.width = lcm(3))

## ---- echo = FALSE------------------------------------------------------------
sp_plot(NE_Lakes, formula = ~ ELEV_CAT + AREA_CAT, key.width = lcm(3), key.pos = 4)

## -----------------------------------------------------------------------------
sp_summary(NE_Lakes, formula = ~ ELEV_CAT:AREA_CAT)

## ---- eval = FALSE------------------------------------------------------------
#  sp_plot(NE_Lakes, formula = ~ ELEV_CAT:AREA_CAT, key.width = lcm(3))

## ---- echo = FALSE------------------------------------------------------------
sp_plot(NE_Lakes, formula = ~ ELEV_CAT:AREA_CAT, key.width = lcm(3), key.pos = 4)

## -----------------------------------------------------------------------------
sp_summary(NE_Lakes, formula = ELEV ~ AREA_CAT)

## ---- eval = FALSE------------------------------------------------------------
#  sp_plot(NE_Lakes, formula = ELEV ~ AREA_CAT)

## ---- echo = FALSE------------------------------------------------------------
sp_plot(NE_Lakes, formula = ELEV ~ AREA_CAT, key.pos = 4)

## -----------------------------------------------------------------------------
sp_summary(NE_Lakes, formula = ELEV ~ AREA_CAT, onlyshow = "small")

## ---- eval = FALSE------------------------------------------------------------
#  sp_plot(NE_Lakes, formula = ELEV ~ AREA_CAT, onlyshow = "small")

## ---- echo = FALSE------------------------------------------------------------
sp_plot(NE_Lakes, formula = ELEV ~ AREA_CAT, onlyshow = "small", key.pos = 4)

## -----------------------------------------------------------------------------
sp_summary(NE_Lakes, formula = ELEV_CAT ~ AREA_CAT)

## ---- eval = FALSE------------------------------------------------------------
#  sp_plot(NE_Lakes, formula = ELEV_CAT ~ AREA_CAT, key.width = lcm(3))

## ---- echo = FALSE------------------------------------------------------------
sp_plot(NE_Lakes, formula = ELEV_CAT ~ AREA_CAT, key.width = lcm(3), key.pos = 4)

## ---- eval = FALSE------------------------------------------------------------
#  list1 <- list(main = "Elevation Categories", pal = rainbow)
#  list2 <- list(main = "Area Categories")
#  list3 <- list(levels = c("small", "large"), pch = c(4, 19))
#  sp_plot(
#    NE_Lakes,
#    formula = ~ ELEV_CAT + AREA_CAT,
#    var_args = list(ELEV_CAT = list1, AREA_CAT = list2),
#    varlevel_args = list(AREA_CAT = list3),
#    cex = 0.75,
#    key.width = lcm(3)
#  )

## ---- echo = FALSE------------------------------------------------------------
list1 <- list(main = "Elevation Categories", pal = rainbow)
list2 <- list(main = "Area Categories")
list3 <- list(levels = c("small", "large"), pch = c(4, 19))
sp_plot(
  NE_Lakes,
  formula = ~ ELEV_CAT + AREA_CAT,
  var_args = list(ELEV_CAT = list1, AREA_CAT = list2),
  varlevel_args = list(AREA_CAT = list3),
  cex = 0.75,
  key.width = lcm(3),
  key.pos = 4
)

## ---- eval = FALSE------------------------------------------------------------
#  sublist <- list(AREA_CAT = list3)
#  sp_plot(
#    NE_Lakes,
#    formula = AREA_CAT ~ ELEV_CAT,
#    var_args = list(ELEV_CAT = sublist),
#    key.width = lcm(3)
#  )

## ---- echo = FALSE------------------------------------------------------------
sublist <- list(AREA_CAT = list3)
sp_plot(
  NE_Lakes,
  formula = AREA_CAT ~ ELEV_CAT,
  var_args = list(ELEV_CAT = sublist),
  key.width = lcm(3),
  key.pos = 4
)

## -----------------------------------------------------------------------------
set.seed(5)

## -----------------------------------------------------------------------------
eqprob_rho <- grts(NE_Lakes, n_base = 50, n_over = 10)

## -----------------------------------------------------------------------------
sp_summary(eqprob_rho)

## ---- eval = FALSE------------------------------------------------------------
#  sp_plot(eqprob_rho, key.width = lcm(3))

## ---- echo = FALSE------------------------------------------------------------
sp_plot(eqprob_rho, key.width = lcm(3), key.pos = 4)

## ---- eval = FALSE------------------------------------------------------------
#  sp_plot(eqprob_rho, NE_Lakes, key.width = lcm(3))

## ---- echo = FALSE------------------------------------------------------------
sp_plot(eqprob_rho, NE_Lakes, key.width = lcm(3), key.pos = 4)

## -----------------------------------------------------------------------------
sp_summary(eqprob_rho, formula = siteuse ~ AREA_CAT)

## ---- eval = FALSE------------------------------------------------------------
#  sp_plot(eqprob_rho, formula = siteuse ~ AREA_CAT, key.width = lcm(3))

## ---- echo = FALSE------------------------------------------------------------
sp_plot(eqprob_rho, formula = siteuse ~ AREA_CAT, key.width = lcm(3), key.pos = 4)

## -----------------------------------------------------------------------------
sp_summary(eqprob_rho, formula = ELEV ~ siteuse)

## ---- eval = FALSE------------------------------------------------------------
#  sp_plot(eqprob_rho, formula = ELEV ~ siteuse)

## ---- echo = FALSE------------------------------------------------------------
sp_plot(eqprob_rho, formula = ELEV ~ siteuse, key.pos = 4)

## -----------------------------------------------------------------------------
sp_summary(NLA_PNW, formula = ~ NITR_COND)

## ---- eval = FALSE------------------------------------------------------------
#  sp_plot(NLA_PNW, formula = ~ NITR_COND, key.width = lcm(3))

## ---- echo = FALSE------------------------------------------------------------
sp_plot(NLA_PNW, formula = ~ NITR_COND, key.width = lcm(3), key.pos = 4)

## -----------------------------------------------------------------------------
sp_summary(NLA_PNW, formula = NITR_COND ~ STATE)

## ---- eval = FALSE------------------------------------------------------------
#  sp_plot(NLA_PNW, formula = NITR_COND ~ STATE, key.width = lcm(3))

## ---- echo = FALSE------------------------------------------------------------
sp_plot(NLA_PNW, formula = NITR_COND ~ STATE, key.width = lcm(3), key.pos = 4)

