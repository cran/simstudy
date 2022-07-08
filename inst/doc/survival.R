## ---- echo = FALSE, message = FALSE-------------------------------------------
library(simstudy)
# library(ggplot2)
library(scales)
library(grid)
library(gridExtra)
library(survival)
library(gee)
library(data.table)

## ---- echo = FALSE------------------------------------------------------------
plotcolors <- c("#B84226", "#1B8445", "#1C5974")

cbbPalette <- c("#B84226","#B88F26", "#A5B435", "#1B8446",
                "#B87326","#B8A526", "#6CA723", "#1C5974") 

ggtheme <- function(panelback = "white") {
  
  ggplot2::theme(
    panel.background = ggplot2::element_rect(fill = panelback),
    panel.grid = ggplot2::element_blank(),
    axis.ticks =  ggplot2::element_line(colour = "black"),
    panel.spacing = ggplot2::unit(0.25, "lines"),  # requires package grid
    panel.border = ggplot2::element_rect(fill = NA, colour="gray90"), 
    plot.title = ggplot2::element_text(size = 8,vjust=.5,hjust=0),
    axis.text = ggplot2::element_text(size=8),
    axis.title = ggplot2::element_text(size = 8)
  )  
  
}

## ---- tidy = TRUE-------------------------------------------------------------

# Baseline data definitions

def <- defData(varname = "x1", formula = .5, dist = "binary")
def <- defData(def,varname = "grp", formula = .5, dist = "binary")

# Survival data definitions

set.seed(282716)

sdef <- defSurv(varname = "survTime", formula = "1.5*x1", scale = "grp*50 + (1-grp)*25", shape = "grp*1 + (1-grp)*1.5")
sdef <- defSurv(sdef, varname = "censorTime", scale = 80, shape = 1)

sdef


## ---- tidy = TRUE-------------------------------------------------------------

# Baseline data definitions

dtSurv <- genData(300, def)
dtSurv <- genSurv(dtSurv, sdef)

head(dtSurv)

# A comparison of survival by group and x1

dtSurv[,round(mean(survTime),1), keyby = .(grp,x1)]


## ---- tidy = TRUE-------------------------------------------------------------
dtSurv <- genData(300, def)
dtSurv <- genSurv(dtSurv, sdef, timeName = "obsTime", 
            censorName = "censorTime", eventName = "status", 
            keepEvents = TRUE)

head(dtSurv)

# estimate proportion of censoring by x1 and group

dtSurv[,round(1-mean(status),2), keyby = .(grp,x1)]

## ---- tidy = TRUE, echo = FALSE, fig.width = 6.5, fig.height = 3.5, warning=FALSE----
fit <- survfit(Surv(obsTime, status) ~ x1+grp, data=dtSurv)

survminer::ggsurvplot(fit, data = dtSurv,
  palette = cbbPalette,
  size = .5,
  ggtheme = ggtheme("grey94")
    # ggplot2::theme(axis.title = ggplot2::element_text(size = 9),
    #                        panel.grid = ggplot2::element_blank())
)

## ---- tidy = TRUE-------------------------------------------------------------

# Baseline data definitions

def <- defData(varname = "x1", formula = .5, dist = "binary")
def <- defData(def,varname = "x2", formula = .5, dist = "binary")

# Survival data definitions

sdef <- defSurv(varname = "survTime", formula = "1.5*x1 - .8*x2", scale = 50, shape = 1/2)
sdef <- defSurv(sdef, varname = "censorTime", scale = 80, shape = 1)

dtSurv <- genData(300, def)
dtSurv <- genSurv(dtSurv, sdef, timeName = "obsTime", 
            censorName = "censorTime", eventName = "status")

coxfit <- survival::coxph(Surv(obsTime, status) ~ x1 + x2, data = dtSurv)

## ---- echo=FALSE--------------------------------------------------------------
gtsummary::tbl_regression(coxfit)

## -----------------------------------------------------------------------------
d1 <- defData(varname = "x1", formula = .5, dist = "binary")
d1 <- defData(d1, "x2", .5, dist = "binary")

dS <- defSurv(varname = "event_1", formula = "-10 - 0.6*x1 + 0.4*x2", shape = 0.3)
dS <- defSurv(dS, "event_2", "-6.5 + 0.3*x1 - 0.5*x2", shape = 0.5)
dS <- defSurv(dS, "censor", "-7", shape = 0.55)

dtSurv <- genData(1001, d1)
dtSurv <- genSurv(dtSurv, dS)

dtSurv

## -----------------------------------------------------------------------------
dtSurv <- addCompRisk(dtSurv, events = c("event_1", "event_2", "censor"), 
            timeName = "time", censorName = "censor")
dtSurv

## ---- tidy = TRUE, echo = FALSE, fig.width = 6.5, fig.height = 3.5, warning=FALSE----
fit <- survfit(Surv(time, event, type="mstate") ~ 1, data=dtSurv)
survminer::ggcompetingrisks(fit, ggtheme = ggtheme("grey94"))  + 
  ggplot2::scale_fill_manual(values = cbbPalette)

## -----------------------------------------------------------------------------
dtSurv <- genData(101, d1)
dtSurv <- genSurv(dtSurv, dS, timeName = "time", censorName = "censor")
dtSurv

## ---- tidy = TRUE-------------------------------------------------------------
def <- defData(varname = "x", formula = .4, dist="binary")

defS <- defSurv(varname = "death", formula = "-14.6 - 0.7*x", shape = .35)
defS <- defSurv(defS, varname = "censor", scale = exp(13), shape = .5)

dd <- genData(500, def)
dd <- genSurv(dd, defS, digits = 2, timeName = "time", censorName = "censor")

fit <- survfit( Surv(time, event) ~ x, data = dd )

## ---- tidy = TRUE, echo = FALSE, fig.width = 6.5, fig.height = 3.5, warning=FALSE----
survminer::ggsurvplot(fit, data = dd, 
                      ggtheme = ggtheme("grey94"),
                      palette = cbbPalette
)

## -----------------------------------------------------------------------------
coxfit <- coxph(formula = Surv(time, event) ~ x, data = dd)

## ---- echo=FALSE--------------------------------------------------------------
gtsummary::tbl_regression(coxfit)

## -----------------------------------------------------------------------------
cox.zph(coxfit)

## ---- tidy = TRUE-------------------------------------------------------------
def <- defData(varname = "x", formula = .4, dist="binary")

defS <- defSurv(varname = "death", formula = "-14.6 - 1.3*x", shape = .35, transition = 0)
defS <- defSurv(defS, varname = "death", formula = "-14.6 - 0.4*x", shape = .35, transition = 150)
defS <- defSurv(defS, varname = "censor", scale = exp(13), shape = .5)

dd <- genData(500, def)
dd <- genSurv(dd, defS, digits = 2, timeName = "time", censorName = "censor")

fit <- survfit( Surv(time, event) ~ x, data = dd )

## ---- tidy = TRUE, echo = FALSE, fig.width = 6.5, fig.height = 3.5, warning=FALSE----
survminer::ggsurvplot(fit, data = dd, 
                      ggtheme = ggtheme("grey94"),
                      palette = cbbPalette
)

## -----------------------------------------------------------------------------
coxfit <- survival::coxph(formula = Surv(time, event) ~ x, data = dd)

## ---- echo=FALSE--------------------------------------------------------------
gtsummary::tbl_regression(coxfit)

## -----------------------------------------------------------------------------
cox.zph(coxfit)

## -----------------------------------------------------------------------------
dd2 <- survSplit(Surv(time, event) ~ ., data= dd, cut=c(150),
                 episode= "tgroup", id="newid")

coxfit2 <- survival::coxph(Surv(tstart, time, event) ~ x:strata(tgroup), data=dd2)

## ---- echo=FALSE--------------------------------------------------------------
gtsummary::tbl_regression(coxfit2)

## -----------------------------------------------------------------------------
cox.zph(coxfit2)

## -----------------------------------------------------------------------------
points <- list(c(100, 0.80), c(200, 0.10))
r <- survGetParams(points)
r

## ---- tidy = TRUE, fig.width = 6.5, fig.height = 3.5, warning=FALSE-----------
survParamPlot(f = r[1], shape = r[2], points)

## -----------------------------------------------------------------------------

defS <- defSurv(varname = "death", formula = -17, scale = 1, shape = 0.3)
defS <- defSurv(defS, varname = "censor", formula = 0, scale = exp(18.5), shape = 0.3)

dd <- genData(500)
dd <- genSurv(dd, defS, timeName = "time", censorName = "censor")

## ---- tidy = TRUE, echo = FALSE, fig.width = 6.5, fig.height = 3.5, warning=FALSE----
fit <- survfit( Surv(time, event) ~ 1, data = dd )

survminer::ggsurvplot(fit, data = dd, 
                      ggtheme = ggtheme("grey94"),
                      palette = cbbPalette,
                      legend = "none"
)

