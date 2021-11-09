## ----echo=FALSE-------------------------------------------------------------------------
base.dir <- "~/Dropbox/website/"
base.url <- "/"
fig.path <- "files/figures/event-study-universal-v-varying-base-period/"
# set knitr parameters
knitr::opts_knit$set(base.dir = base.dir, base.url = base.url)
knitr::opts_chunk$set(fig.path = fig.path) 


## ----echo=FALSE, message=FALSE----------------------------------------------------------
devtools::load_all("~/Dropbox/did")


## ----eval=FALSE-------------------------------------------------------------------------
## library(did) # need to load version 2.1 of package


## ---- fig.align='center', fig.width=10, fig.height=5------------------------------------
# create data with no pre-trends
time.periods <- 5
sp <- reset.sim(time.periods=time.periods)
sp$te <- 1
data <- build_sim_dataset(sp)
data <- subset(data, G==time.periods | G==0)

# varying base period
res1_varying <- att_gt(yname="Y", xformla=~X, data=data, tname="period",
              idname="id",
              control_group="nevertreated",
              gname="G", est_method="dr")
dynamic1_varying <- aggte(res1_varying, type="dynamic")
p1_varying <- ggdid(dynamic1_varying, ylim=c(-2,2))

# universal base period
res1_universal <- att_gt(yname="Y", xformla=~X, data=data, tname="period",
              idname="id",
              control_group="nevertreated",
              gname="G", est_method="dr", base_period="universal"
              )
dynamic1_universal <- aggte(res1_universal, type="dynamic")
p1_universal <- ggdid(dynamic1_universal, ylim=c(-2,2))

ggpubr::ggarrange(p1_varying, p1_universal, nrow=1)


## ----fig.align='center', fig.width=10, fig.height=5, echo=FALSE-------------------------
# treatment anticipation effects
time.periods <- 5
sp <- reset.sim(time.periods=time.periods)
sp$te <- 0
sp$te.e <- c(-1,1)
data <- build_sim_dataset(sp)
# add anticipation
data <- subset(data, G==(time.periods-1) | G==0)
data$G[data$G > 0] <- 5 

# varying base period
res2_varying <- att_gt(yname="Y", xformla=~X, data=data, tname="period",
              idname="id",
              control_group="nevertreated",
              gname="G", est_method="dr")
dynamic2_varying <- aggte(res2_varying, type="dynamic")
p2_varying <- ggdid(dynamic2_varying, ylim=c(-2,3))

# universal base period
res2_universal <- att_gt(yname="Y", xformla=~X, data=data, tname="period",
              idname="id",
              control_group="nevertreated",
              gname="G", est_method="dr", base_period="universal"
              )
dynamic2_universal <- aggte(res2_universal, type="dynamic")
p2_universal <- ggdid(dynamic2_universal, ylim=c(-2,3))

ggpubr::ggarrange(p2_varying, p2_universal, nrow=1)


## ----fig.align='center', fig.width=10, fig.height=5, echo=FALSE-------------------------
# linear trends 
time.periods <- 5
sp <- reset.sim(time.periods=time.periods)
sp$te <- 0
sp$te.e <- 1:(time.periods-1)#-1:(time.periods-2)
data <- build_sim_dataset(sp)
# add anticipation
data <- subset(data, G==2 | G==0)
data$G[data$G > 0] <- 5 

# varying base period
res3_varying <- att_gt(yname="Y", xformla=~X, data=data, tname="period",
              idname="id",
              control_group="nevertreated",
              gname="G", est_method="dr")
dynamic3_varying <- aggte(res3_varying, type="dynamic")
p3_varying <- ggdid(dynamic3_varying, ylim=c(-5,2))

# universal base period
res3_universal <- att_gt(yname="Y", xformla=~X, data=data, tname="period",
              idname="id",
              control_group="nevertreated",
              gname="G", est_method="dr", base_period="universal"
              )
dynamic3_universal <- aggte(res3_universal, type="dynamic")
p3_universal <- ggdid(dynamic3_universal, ylim=c(-5,2))

ggpubr::ggarrange(p3_varying, p3_universal, nrow=1)

