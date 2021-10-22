## -----------------------------------------------------------------------------
library(fixest)
library(did)
library(ggplot2)
load("mw_data2.RData")

# create event time variable
mw_data2$e <- ifelse(mw_data2$treat==1, 
                  mw_data2$year - mw_data2$first.treat,
                  0)

# run event study regression
es_reg <- feols(lemp ~ i(e, ref=-1) | year + countyreal,
                data=mw_data2,
                cluster="countyreal")



## ---- fig.align='center', fig.width=10, fig.height=6--------------------------
# plot event study regression
iplot(es_reg, ylim.add=c(-.5,.5))


## -----------------------------------------------------------------------------
# callaway and sant'anna
cs_res <- att_gt(yname="lemp",
                 tname="year",
                 idname="countyreal",
                 gname="first.treat",
                 data=mw_data2)
cs_es <- aggte(cs_res, type="dynamic")


## ---- fig.align='center', fig.width=10, fig.height=6--------------------------
ggdid(cs_es, ylim=c(-.25,.2))


## ---- warning=FALSE, message=FALSE--------------------------------------------
# same data as before
data <-readRDS("sim_data.RDS")

# run event study regression
es_reg <- feols(Y ~ i(e, ref=-1) | id + time.period,
                data=data,
                cluster="id")


## ---- fig.align="center", fig.width=10, fig.height=6--------------------------
# plot event study regression
iplot(es_reg)


## -----------------------------------------------------------------------------
# callaway and sant'anna
cs_res <- att_gt(yname="Y",
                 tname="time.period",
                 idname="id",
                 gname="G",
                 data=data,
                 control_group="notyettreated")

# aggregate into event study (dynamic effects)
cs_es <- aggte(cs_res, type="dynamic") 


## ---- fig.align="center", fig.width=10, fig.height=6--------------------------
ggdid(cs_es)

