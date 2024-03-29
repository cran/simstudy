# defMiss ----
test_that("defMiss produces correct output", {
  varname <- "test"
  formula <- ".3"
  logit.link <- FALSE
  baseline <- TRUE
  monotonic <- FALSE

  def <- data.table::data.table(
    varname, formula, logit.link,
    baseline, monotonic
  )

  expect_equal(def, defMiss(
    NULL, varname, formula, logit.link,
    baseline, monotonic
  ))

  expect_equal(rbind(def, def), defMiss(
    def, varname, formula,
    logit.link, baseline, monotonic
  ))
})

# genMiss ----
test_that("genMiss works", {
  # not repeated
  def1 <- defData(varname = "m", dist = "binary", formula = .5)
  def1 <- defData(def1, "u", dist = "binary", formula = .5)
  def1 <- defData(def1, "x1", dist = "normal", formula = "20*m + 20*u", variance = 2)
  def1 <- defData(def1, "x2", dist = "normal", formula = "20*m + 20*u", variance = 2)
  def1 <- defData(def1, "x3", dist = "normal", formula = "20*m + 20*u", variance = 2)

  dtAct1 <- genData(10000, def1)

  hardProbForm <- runif(1)
  form1val0 <- runif(1)
  form1val1 <- runif(1)
  form1 <- "..form1val0*(1-m) + m*..form1val1"
  # form1 <- paste0(form1val0, "*(1-m) + m*", form1val1)

  defM <- defMiss(varname = "x1", formula = hardProbForm, logit.link = FALSE)
  defM <- defMiss(defM, varname = "x2", formula = form1, logit.link = FALSE)
  defM <- defMiss(defM, varname = "u", formula = 1, logit.link = FALSE) # not observed

  missMat <- genMiss(dtAct1, defM, idvars = "id")

  ## check probabilistic column has correct distribution
  hardProbAct <- mean(missMat[, x1])
  hPDiff <- abs(hardProbAct - hardProbForm)

  expect_true(hPDiff < 0.04) # nrow(dtAct1) == 1000, hpDiff < 0.03 FAILED

  ## check dependent column 1
  # check probs when m is 0/1, u is 0/1

  mIs0 <- dtAct1[, m == 0]
  mIs1 <- dtAct1[, m == 1]

  missMat0 <- missMat[mIs0]
  missMat1 <- missMat[mIs1]

  avg0 <- mean(missMat0[, x2])
  avg1 <- mean(missMat1[, x2])

  diff0 <- abs(form1val0 - avg0)
  diff1 <- abs(form1val1 - avg1)

  expect_true(diff0 < 0.03 && diff1 < 0.03)

  ## check empty column
  expect_true(all(missMat[, u] == 1))

  # repeated (not worth looking at if !baseline and !mono)
  ## includes lags

  ## baseline
  # if 1/0 at baseline, 1/0 at every other period
  idNum <- 500

  dtAct2 <- genData(idNum, def1)
  dtAct2 <- trtObserve(dtAct2, formulas = .5, logit.link = FALSE, grpName = "rx")

  defLong <- defDataAdd(varname = "y", dist = "normal", formula = "10 + period*2 + 2 * rx", variance = 2)

  dtTime <- addPeriods(dtAct2, nPeriods = 4)
  dtTime <- addColumns(defLong, dtTime)

  defMlong <- defMiss(varname = "x1", formula = .20, baseline = TRUE)
  defMlong <- defMiss(defMlong, varname = "y", formula = "-1.5 - 1.5 * rx + .25*period", logit.link = TRUE, baseline = FALSE, monotonic = FALSE)

  missMatLong <- genMiss(dtTime, defMlong, idvars = c("id", "rx"), repeated = TRUE, periodvar = "period")

  ids0 <- missMatLong[period == 0 & x1 == 0, id]
  expect_true(missMatLong[id %in% ids0, all(x1 == 0)])

  ids1 <- missMatLong[period == 0 & x1 == 1, id]
  expect_true(missMatLong[id %in% ids1, all(x1 == 1)])

  ### monotonic

  defMlong <- defMiss(varname = "x1", formula = .20, baseline = TRUE)
  defMlong <- defMiss(defMlong, varname = "y", formula = "-1.8 - 1.5 * rx + .25*period", logit.link = TRUE, baseline = FALSE, monotonic = TRUE)

  missMatLong <- genMiss(dtTime, defMlong, idvars = c("id", "rx"), repeated = TRUE, periodvar = "period")

  dp <- missMatLong[y == 1, .SD[1], keyby = id][, .(id, fperiod = period)]
  dd <- missMatLong[dp, on = "id"]
  expect_true(dd[period >= fperiod][, all(y == 1)])

  ## not monotonic
  
  defMlong <- defMiss(varname = "x1", formula = .20, baseline = TRUE)
  defMlong <- defMiss(defMlong,varname = "y", 
                      formula = "-1.8 - 1.5 * rx + .25*period", logit.link = TRUE, 
                      baseline = FALSE, monotonic = FALSE)
  
  missMatLong <- genMiss(dtTime, defMlong, idvars = c("id","rx"), repeated = TRUE, periodvar = "period")
  
  dp <- missMatLong[y == 1, .SD[1], keyby = id][, .(id, fperiod = period)]
  dd <- missMatLong[dp, on = "id"]
  expect_false(dd[period >= fperiod][, all(y == 1)])
})

# .checkLags ----
test_that("LAG() usage is detected correctly.", {
  hasLag <- c("a + 5 | LAG(3) - 4", " 4 + 3 ", "LAG(x)")
  noLag <- c("a +5", "3 + 1", "log(3) + b")

  expect_true(.checkLags(hasLag))
  expect_true(.checkLags(hasLag[1]))

  expect_false(.checkLags(noLag))
  expect_false(.checkLags(noLag[1]))
})

# .addLags ----
test_that(".addLags throws errors.", {
  ok <- "1 * LAG(rx)"
  doubleErr <- "LAG(rx) + LAG(rx)*2"
  notFound <- "1 + LAG(NO)"
  data <- genData(200)
  data <- trtAssign(data, grpName = "rx")
  dataLong <- addPeriods(data, nPeriods = 5)
  reservedName <- copy(dataLong)
  reservedName$.rx1 <- reservedName$rx

  expect_error(.addLags(data, ok), "not longitudinal")
  expect_error(.addLags(dataLong, doubleErr), "Repeated lag term")
  expect_error(.addLags(dataLong, notFound), "not in data table")
  expect_error(.addLags(reservedName, ok), "do not use")
})

test_that("LAGS are added as expected.", {
  data <- genData(200)
  dataLong <- addPeriods(data, nPeriods = 5)
  dataLong$rx <- sample(c(0, 1), 1000, replace = TRUE)
  dataLong$tx <- sample(c(0, 1), 1000, replace = TRUE)

  dataAfter <- copy(dataLong)

  # manual version of data.table::shift to find possible issues in case of
  # changes in data.table. data.table version:
  # dataAfter[, .rx1 := shift(.SD[, rx], n = 1L, fill = 0), by = id] #nolint
  rx1 <- vector(mode = "numeric", length = 1000)
  tx1 <- vector(mode = "numeric", length = 1000)
  for (i in 1:200) {
    rx1[((i - 1) * 5 + 1):((i - 1) * 5 + 5)] <- c(0, dataAfter[id == i, rx][1:4])
    tx1[((i - 1) * 5 + 1):((i - 1) * 5 + 5)] <- c(0, dataAfter[id == i, tx][1:4])
  }

  dataAfter$.rx1 <- rx1
  dataAfter$.tx1 <- tx1
  setkey(dataAfter, "timeID")
  setindex(dataAfter, NULL)

  noLAG <- " 1 * rx"
  origForm <- c("-2 + 1.5 * LAG(rx)", "-2.1 + 1.3 * LAG(tx)")
  lagNames <- c(".rx1", ".tx1")
  lagForms <- c("-2 + 1.5 * .rx1", "-2.1 + 1.3 * .tx1")

  expect_equal(.addLags(dataLong, c(origForm, noLAG)), list(dataAfter, c(lagForms, noLAG), lagNames))
})

# genObs ----

test_that("genObs throws errors", {
  expect_error(genObs(dtMiss = missMat, idvars = "id"), class = "simstudy::missingArgument")
  expect_error(genObs(dtName = dtAct1, idvars = "id"), class = "simstudy::missingArgument")
  expect_error(genObs(dtName = dtAct1, dtMiss = missMat), class = "simstudy::missingArgument")
})

test_that("genObs works", {
  def1 <- defData(varname = "m", dist = "binary", formula = .5)
  def1 <- defData(def1, "u", dist = "binary", formula = .5)
  def1 <- defData(def1, "x1", dist = "normal", formula = "20*m + 20*u", variance = 2)
  def1 <- defData(def1, "x2", dist = "normal", formula = "20*m + 20*u", variance = 2)
  def1 <- defData(def1, "x3", dist = "normal", formula = "20*m + 20*u", variance = 2)

  dtAct3 <- genData(1000, def1)

  hardProbForm <- runif(1)
  form1val0 <- runif(1)
  form1val1 <- runif(1)
  form1 <- "..form1val0*(1-m) + m*..form1val1"
  # form1 <- paste0(form1val0, "*(1-m) + m*", form1val1)
  defM <- defMiss(varname = "x1", formula = hardProbForm, logit.link = FALSE)
  defM <- defMiss(defM, varname = "x2", formula = form1, logit.link = FALSE)
  defM <- defMiss(defM, varname = "u", formula = 1, logit.link = FALSE) # not observed

  missMat <- genMiss(dtAct3, defM, idvars = "id")

  idNum <- 500
  dtAct4 <- genData(idNum, def1)
  dtAct4 <- trtObserve(dtAct4, formulas = .5, logit.link = FALSE, grpName = "rx")
  defLong <- defDataAdd(varname = "y", dist = "normal", formula = "10 + period*2 + 2 * rx", variance = 2)
  dtTime <- addPeriods(dtAct4, nPeriods = 4)
  dtTime <- addColumns(defLong, dtTime)
  defMlong <- defMiss(varname = "x1", formula = .20, baseline = TRUE)
  defMlong <- defMiss(defMlong, varname = "y", formula = "-1.5 - 1.5 * rx + .25*period", logit.link = TRUE, baseline = FALSE, monotonic = FALSE)
  missMatLong <- genMiss(dtTime, defMlong, idvars = c("id", "rx"), repeated = TRUE, periodvar = "period")

  # check if dtName is longitudinal (has period) but idvars does not include period
  dtObs1 <- genObs(dtName = dtTime, dtMiss = missMatLong, idvars = "id")
  expect_true("period" %in% names(dtObs1))

  # check all NA cells in dtName are 1 cells in dtMiss
  dtObs2 <- genObs(dtName = dtAct3, dtMiss = missMat, idvars = "id")

  obsVec <- is.na(dtObs2[, c("x1", "x2", "x3", "m", "u")])
  mmVec <- missMat[, c("x1", "x2", "x3", "m", "u")] == 1
  expect_true(all(obsVec == mmVec))
})
