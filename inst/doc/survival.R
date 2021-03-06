## ---- echo = FALSE, message = FALSE-------------------------------------------
library(simstudy)
library(ggplot2)
library(scales)
library(grid)
library(gridExtra)
library(survival)
library(gee)
library(data.table)

plotcolors <- c("#B84226", "#1B8445", "#1C5974")

cbbPalette <- c("#B84226","#B88F26", "#A5B435", "#1B8446",
                "#B87326","#B8A526", "#6CA723", "#1C5974") 

ggtheme <- function(panelback = "white") {
  
  ggplot2::theme(
    panel.background = element_rect(fill = panelback),
    panel.grid = element_blank(),
    axis.ticks =  element_line(colour = "black"),
    panel.spacing =unit(0.25, "lines"),  # requires package grid
    panel.border = element_rect(fill = NA, colour="gray90"), 
    plot.title = element_text(size = 8,vjust=.5,hjust=0),
    axis.text = element_text(size=8),
    axis.title = element_text(size = 8)
  )  
  
}

ggsurv_m <- function(
  s,
  CI         = 'def',
  plot.cens  = TRUE,
  surv.col   = 'gg.def',
  cens.col   = 'gg.def',
  lty.est    = 1,
  lty.ci     = 2,
  cens.shape = 3,
  back.white = FALSE,
  xlab       = 'Time',
  ylab       = 'Survival',
  main       = '',
  strata     = length(s$strata),
  labels     = NULL
) {
  
  s <- fit
  
  n <- s$strata
  
  strataEqualNames <- unlist(strsplit(names(s$strata), '='))
  groups <- factor(
    strataEqualNames[seq(2, 2 * strata, by = 2)],
    levels = strataEqualNames[seq(2, 2 * strata, by = 2)]
  )
  
  gr.name <-  strataEqualNames[1]
  gr.df   <- vector('list', strata)
  n.ind   <- cumsum(c(0,n))
  
  for (i in 1:strata) {
    indI <- (n.ind[i]+1):n.ind[i+1]
    gr.df[[i]] <- data.frame(
      time  = c(0, s$time[ indI ]),
      surv  = c(1, s$surv[ indI ]),
      up    = c(1, s$upper[ indI ]),
      low   = c(1, s$lower[ indI ]),
      cens  = c(0, s$n.censor[ indI ]),
      group = rep(groups[i], n[i] + 1)
    )
  }
  
  dat      <- do.call(rbind, gr.df)
  dat.cens <- subset(dat, cens != 0)
  
  pl <- ggplot(dat, aes(x = time, y = surv, group = group)) +
    geom_step(aes(col = group, lty = group)) +
    xlab(xlab) +
    ylab(ylab) +
    ggtitle(main)
  
  pl <- if(surv.col[1] != 'gg.def'){
    scaleValues <- if (length(surv.col) == 1) {
      rep(surv.col, strata)
    } else{
      surv.col
    }
    pl + scale_colour_manual(name = gr.name, values = scaleValues, labels=labels)
    
  } else {
    pl + scale_colour_discrete(name = gr.name, labels=labels)
  }
  
  lineScaleValues <- if (length(lty.est) == 1) {
    rep(lty.est, strata)
  } else {
    lty.est
  }
  pl <- pl + scale_linetype_manual(name = gr.name, values = lineScaleValues)

  if(identical(CI,TRUE)) {
    if(length(surv.col) > 1 || length(lty.est) > 1){
      stop('Either surv.col or lty.est should be of length 1 in order to plot 95% CI with multiple strata')
    }

    stepLty <- if ((length(surv.col) > 1 | surv.col == 'gg.def')[1]) {
      lty.ci
    } else {
      surv.col
    }
    pl <- pl +
      geom_step(aes(y = up, lty = group), lty = stepLty) +
      geom_step(aes(y = low,lty = group), lty = stepLty)
  }

  if (identical(plot.cens, TRUE) ){
    if (nrow(dat.cens) == 0) {
      stop('There are no censored observations')
    }
    if (length(cens.col) == 1) {
      col <- ifelse(cens.col == 'gg.def', 'red', cens.col)
      pl <- pl + geom_point(
        data    = dat.cens,
        mapping = aes(y = surv),
        shape   = cens.shape,
        col     = col
      )

    } else if (length(cens.col) > 0) {
    # if(!(identical(cens.col,surv.col) || is.null(cens.col))) {
      #   warning ("Color scales for survival curves and censored points don't match.\nOnly one color scale can be used. Defaulting to surv.col")
      # }


      if (! identical(cens.col, "gg.def")) {
        if (length(cens.col) != strata) {
          warning("Color scales for censored points don't match the number of groups. Defaulting to ggplot2 default color scale")
          cens.col <- "gg.def"
        }
      }

      if (identical(cens.col, "gg.def")) {
        pl <- pl + geom_point(
          data = dat.cens,
          mapping = aes(y=surv, col = group),
          shape = cens.shape,
          show.legend = FALSE
        )
      } else {

        uniqueGroupVals = unique(dat.cens$group)
        if (length(cens.shape) == 1) {
          cens.shape = rep(cens.shape, strata)
        }

        if (length(cens.shape) != strata) {
          warning("The length of the censored shapes does not match the number of groups (or 1). Defaulting shape = 3 (+)")
          cens.shape = rep(3, strata)
        }
        for (i in seq_along(uniqueGroupVals)) {
          groupVal = uniqueGroupVals[i]
          dtGroup <- subset(dat.cens, group == groupVal)

          pl <- pl + geom_point(
            data = dtGroup,
            mapping = aes(y=surv),
            color = I(cens.col[i]),
            shape = cens.shape[i],
            show.legend = FALSE
          )

        }
      }

    }
  }

  if(identical(back.white, TRUE)) {
    pl <- pl + theme_bw()
  }
  
  pl
}

## ---- tidy = TRUE-------------------------------------------------------------

# Baseline data definitions

def <- defData(varname = "x1", formula = .5, dist = "binary")
def <- defData(def,varname = "x2", formula = .5, dist = "binary")
def <- defData(def,varname = "grp", formula = .5, dist = "binary")

# Survival data definitions

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

cdef <- defDataAdd(varname = "obsTime", formula = "pmin(survTime, censorTime)", dist="nonrandom")
cdef <- defDataAdd(cdef, varname = "status", formula = "I(survTime <= censorTime)",dist="nonrandom")

dtSurv <- addColumns(cdef, dtSurv)

head(dtSurv)

# estimate proportion of censoring by x1 and group

dtSurv[,round(1-mean(status),2), keyby = .(grp,x1)]

## ---- tidy = TRUE, echo = FALSE, fig.width = 6.5, fig.height = 3.5------------
fit <- survfit(Surv(obsTime, status) ~ x1+grp, data=dtSurv)
# ggsurvplot(fit, palette = cbbPalette, font.tickslab = c(8), font.x = 10, font.y = 10,
#            legend = c(0.8, 0.8))

ggsurv_m(fit, cens.col = "grey50", surv.col = cbbPalette, 
         labels = c("grp=0 & x1=0","grp=1 & x1=0","grp=0 & x1=1","grp=1 & x1=1")) +
  ggplot2::guides(linetype = FALSE) +
  ggtheme("grey95") +
  theme(legend.position=c(.8,.8), 
        legend.title = element_blank(),
        legend.key = element_rect(fill="grey95" , color = "grey95"),
        legend.background = element_rect(fill="grey95"),
        legend.key.width = unit(1, "cm")) +
  guides(colour = guide_legend(override.aes = list(size=1)))


