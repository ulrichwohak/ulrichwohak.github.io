## ----xaringan-themer, include=FALSE, warning=FALSE----------------------------
library(xaringanthemer)
style_mono_accent(
  base_color = "#1c5253",
  text_font_google   = google_font("Montserrat", "300", "300i"),
  code_font_google   = google_font("Fira Mono")
)


## ----echo=FALSE---------------------------------------------------------------
step_by_step_eq <- function(eqlist, before="", after="", title=" ") {
  
  # drop slide pauses in before content
  before_inner <- gsub("--", "", before)
  
  for (i in 2:length(eqlist)) {
    eqlist[i] <- paste0(eqlist[i-1],"\\\\\n",eqlist[i])
  }

  out <- ""
  for (i in 1:length(eqlist)) {
    if (i > 1) out <- paste0(out, "count:false", "\n")
    out <- paste0(out, "# ", title, "\n") # print title
    # print before content
    if (i == 1) out <- paste0(out, before, "\n") else out <- paste0(out, before_inner, "\n") 
    out <- paste0(out, "$$\n\\begin{aligned}\n",eqlist[[i]],"\n\\end{aligned}\n$$\n\n") # print equation
    if (i < length(eqlist)) out <- paste0(out, "---\n\n")
  }
  out <- paste0(out, after, "\n") # print after content
  out <- paste0(out, "---\n")
  cat(out)
}


## ----echo=FALSE, results="asis"-----------------------------------------------

title <- "Recovering ATT under conditional PTA"

before <- "Under the conditional parallel trends assumption,"

eqlist <- list("ATT &= \\E[Y_{t^*}(1) | D=1] - \\E[Y_{t^*}(0) | D=1] \\hspace{150pt}",
               "&= \\E[Y_{t^*}(1) - Y_{t^*-1}(0) | D=1] - \\E[Y_{t^*}(0) - Y_{t^*-1}(0) | D=1]",
               "&= \\E[Y_{t^*}(1) - Y_{t^*-1}(0) | D=1] - \\E\\big[ \\E[Y_{t^*}(0) - Y_{t^*-1}(0) | X, D=1] \\big| D=1\\big]",
               "&= \\E[Y_{t^*}(1) - Y_{t^*-1}(0) | D=1] - \\E\\big[ \\E[Y_{t^*}(0) - Y_{t^*-1}(0) | X, D=0] \\big| D=1\\big]",
               "&= \\E[\\Delta Y_{t^*} | D=1] - \\E\\big[ \\E[\\Delta Y_{t^*} | X, D=0] \\big| D=1\\big]")

after <- "Everything is identified here

--

- but estimation may be more challenging"

step_by_step_eq(title=title,
                before=before,
                eqlist=eqlist,
                after=after)



## -----------------------------------------------------------------------------
library(did)
library(fixest)
library(modelsummary)
load("mw_data2.RData")

# create post treatment dummy
mw_data2$post <- 1*(mw_data2$year >= mw_data2$first.treat & mw_data2$first.treat > 0)

# run TWFE regression
twfe_x <- feols(lemp ~ post | countyreal + region^year,
                data=mw_data2)



## -----------------------------------------------------------------------------
modelsummary(twfe_x, gof_omit=".*")


## -----------------------------------------------------------------------------
cs_x <- att_gt(yname="lemp",
               tname="year",
               idname="countyreal",
               gname="first.treat",
               xformla=~region,
               data=mw_data2)
cs_x_res <- aggte(cs_x, type="group")


## -----------------------------------------------------------------------------
summary(cs_x_res)


## -----------------------------------------------------------------------------
# regression
cs_x_reg <- att_gt(yname="lemp",
               tname="year",
               idname="countyreal",
               gname="first.treat",
               xformla=~region + lpop,
               est_method = "reg",
               data=mw_data2)
cs_x0_reg <- aggte(cs_x_reg, type="group")


## -----------------------------------------------------------------------------
# propensity score weighting
cs_x_ipw <- att_gt(yname="lemp",
               tname="year",
               idname="countyreal",
               gname="first.treat",
               xformla=~region + lpop,
               est_method = "ipw",
               data=mw_data2)
cs_x0_ipw <- aggte(cs_x_ipw, type="group")


## -----------------------------------------------------------------------------
# doubly robust
cs_x_dr <- att_gt(yname="lemp",
               tname="year",
               idname="countyreal",
               gname="first.treat",
               xformla=~region + lpop,
               est_method = "dr",
               data=mw_data2)
cs_x0_dr <- aggte(cs_x_dr, type="group")


## -----------------------------------------------------------------------------
# show results
round(cbind.data.frame(reg=cs_x0_reg$overall.att, 
                       ipw=cs_x0_ipw$overall.att,
                       dr=cs_x0_dr$overall.att), 6)


## ---- fig.align="center", fig.width=10, fig.height=8, echo=FALSE--------------
cs_res <- att_gt(yname="lemp",
               tname="year",
               idname="countyreal",
               gname="first.treat",
               data=mw_data2)
ggdid(cs_res, ylim=c(-.2,.1))


## ---- echo=FALSE, cache=TRUE, fig.align="center", fig.width=14, fig.height=8, message=FALSE, warning=FALSE----
library(HonestDiD)
cs_es <- aggte(cs_res, type="dynamic")
# recover influence function for event study estimates
es_inf_func <- cs_es$inf.function$dynamic.inf.func.e

# recover variance-covariance matrix
n <- nrow(es_inf_func)
V <- t(es_inf_func) %*% es_inf_func / (n*n) 

eventplot <- ggdid(cs_es)

# edit below here

es.effects <- cs_es$att.egt

preperiod <- 4
postperiod <- 5

baseVec1 <- basisVector(index=1,size=postperiod)

RobustResultsPost1 <- createSensitivityResults_relativeMagnitudes(betahat = es.effects, sigma = V, 
                                                                  numPrePeriods = preperiod, 
                                                                  numPostPeriods = postperiod, 
                                                                  l_vec = baseVec1,
                                                                  Mbarvec = seq(from = 0, to = 2, by = 0.5),
                                                                  gridPoints = 100, grid.lb = -1, grid.ub = 1)

# plots

# first period post adoption
Orig1 <- constructOriginalCS(betahat = es.effects,
                             sigma = V, numPrePeriods = preperiod, numPostPeriods = postperiod,
                             l_vec = baseVec1)

SensPlot1 <- createSensitivityPlot_relativeMagnitudes(robustResults = RobustResultsPost1,
                                                      originalResults = Orig1)

# sensitivity plots for each post-adoption period
gridExtra::grid.arrange(eventplot,SensPlot1,ncol = 2)

