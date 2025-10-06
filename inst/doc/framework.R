## ----echo = FALSE-------------------------------------------------------------
knitr::opts_chunk$set(
  warning = FALSE,
  message = FALSE,
  fig.width = 7,
  fig.height = 3
)

## ----packages-----------------------------------------------------------------
library(simstudy)
library(data.table)
library(parallel)
library(lmerTest) 

## ----framwork, eval = FALSE---------------------------------------------------
# s_define <- function() {
# 
#   #--- add data definition code ---#
# 
#   return(list_of_defs) # list_of_defs is a list of simstudy data definitions
# }
# 
# s_generate <- function(list_of_defs, argsvec) {
# 
#   list2env(list_of_defs, envir = environment())
#   list2env(as.list(argsvec), envir = environment())
# 
#   #--- add data generation code ---#
# 
#   return(generated_data) # generated_data is a data.table
# }
# 
# s_model <- function(generated_data) {
# 
#   #--- add model code ---#
# 
#   return(model_results) # model_results is a data.table
# }
# 
# 
# s_replicate <- function(argsvec) {
# 
#   list_of_defs <- s_define()
# 
#   generated_data <- s_generate(list_of_defs, argsvec)
#   model_results <- s_model(generated_data)
# 
#   #--- add summary statistics code ---#
# 
#   return(model_results)
# 
# }
# 
# 
# model_fits <- mclapply(scenarios, function(a) s_replicate(a))

## -----------------------------------------------------------------------------
a <- c(0.5, 0.7, 0.9)
b <- c(8, 16)
d <- c(12, 18)

# Independent parameters
scenario_list(a, b)

# Grouped parameters
scenario_list(a, grouped(b, d))

# With replications
scenario_list(b, d, each = 2)

## ----eval=FALSE---------------------------------------------------------------
# s_define <- function() {
# 
#   #--- data definition code ---#
# 
#   def1 <- defData(varname = "site_eff",
#     formula = 0, variance = "..svar", dist = "normal", id = "site")
#   def1 <- defData(def1, "n", formula = "..npat", dist = "poisson")
# 
#   def2 <- defDataAdd(varname = "Y", formula = "5 + site_eff + ..delta * rx",
#     variance = "..ivar", dist = "normal")
# 
#   return(list(def1 = def1, def2 = def2))
# }
# 
# s_generate <- function(list_of_defs, argsvec) {
# 
#   list2env(list_of_defs, envir = environment())
#   list2env(as.list(argsvec), envir = environment())
# 
#   #--- data generation code ---#
# 
#   ds <- genData(40, def1)
#   ds <- trtAssign(ds, grpName = "rx")
#   dd <- genCluster(ds, "site", "n", "id")
#   dd <- addColumns(def2, dd)
# 
#   return(dd)
# }
# 
# s_model <- function(generated_data) {
# 
#   #--- model code ---#
# 
#   lmefit <- lmer(Y ~ rx + (1|site), data = generated_data)
# 
#   return(data.table(tidy(lmefit)))
# }
# 
# s_replicate <- function(argsvec) {
# 
#   list_of_defs <- s_define()
#   generated_data <- s_generate(list_of_defs, argsvec)
#   model_results <- s_model(generated_data)
# 
#   return(list(argsvec, model_results))
# }

## ----eval = FALSE-------------------------------------------------------------
# 
# #------ set simulation parameters
# 
# npat <- c(8, 16, 24)
# svar <- c(0.40, 0.80)
# ivar <- c(3, 6)
# delta <- c(0.50, 0.75, 1.00)
# 
# scenarios <- scenario_list(delta, npat, grouped(svar, ivar), each = 1000)
# 
# model_fits <- mclapply(scenarios, function(a) s_replicate(a), mc.cores = 5)

## ----eval = FALSE-------------------------------------------------------------
# summarize <- function(m_fit) {
#   args <- data.table(t(m_fit[[1]]))
#   reject <- m_fit[[2]][term == "rx", p.value <= 0.05]
#   cbind(args, reject)
# }
# 
# reject <- rbindlist(lapply(model_fits, function(a) summarize(a)))
# power <- reject[, .(power = mean(reject)), keyby = .(delta, npat, svar, ivar, scenario)]

## ----power_plot, echo=FALSE, fig.width = 7, fig.height=3----------------------
load("data/power.rda")
power[, highvar := (svar ==.8)]

library(ggplot2)

ggplot(data = power, 
       aes(x = factor(npat), y = power, 
        group = factor(delta), color = factor(delta))) +
  geom_hline(yintercept = 0.8, color = "white") +
  geom_line(linewidth = 0.9) +
  facet_grid(. ~ factor(highvar, labels = c("low variance", "high variance"))) +
  theme(panel.grid = element_blank()) +
  scale_y_continuous(limits = c(0,1)) +
  scale_x_discrete(name = "number of patients per site", expand = c(.1, .1)) +
  scale_color_manual(
    values = c("1" = "#4477AA", "0.75" = "#EE6677", "0.5" = "#228833"),
    breaks = c("1", "0.75", "0.5"),
    labels = c("1.00", "0.75", "0.50"),
    name = "effect size"
  )

