---
layout: single
title: Code
permalink: /code/
---

## Main R packages

These are my R packages that currently seem to get the most attention and that I am actively working on and maintaining.  I welcome comments, issues, complaints, etc.

**1. R `qte` Package** [[Website](http://bcallaway11.github.io/qte/)][[CRAN](https://cran.r-project.org/web/packages/qte/index.html)] [[Github](https://github.com/bcallaway11/qte)] [![](http://cranlogs.r-pkg.org/badges/grand-total/qte)](http://cran.rstudio.com/web/packages/qte/index.html)

The R `qte` package provides many methods for estimating the Quantile Treatment Effect (QTE) or the Quantile Treatment Effect on the Treated (QTET). This package contains the code to implement the estimators of quantile treatment effects in difference in differences designs that we proposed in Callaway and Li (2019) and Callaway, Li, Oka (2018).  In addition, the package includes code for estimating the QTE and QTET under the assumption of selection on observables (Firpo, 2007), the QTET in the Changes in Changes model (Athey and Imbens, 2006), the QTET in the Quantile Difference in Differences model.

  * **Difference in Differences Methods**

    * `panel.qtet` from Callaway and Li, 2019. [[Tutorial](http://bcallaway11.github.io/qte/articles/panel-qtet.html)] [[Documentation](http://bcallaway11.github.io/qte/reference/panel.qtet.html)]
    * `ddid2` from Callaway, Li, and Oka, 2017.  [[Tutorial](http://bcallaway11.github.io/qte/articles/ddid2.html)] [[Documentation](http://bcallaway11.github.io/qte/reference/ddid2.html)]
    * `CiC` from Athey and Imbens, 2006 [[Documentation](http://bcallaway11.github.io/qte/reference/CiC.html)]
    * `QDiD` Quantile Difference in Differences [[Documentation](http://bcallaway11.github.io/qte/reference/QDiD.html)]

  * **Selection on Observables Methods**

    * `ci.qte` from Firpo, 2007.  [[Documentation](http://bcallaway11.github.io/qte/reference/ci.qte.html)]
    * `ci.qtet` from Firpo, 2007 [[Documentation](http://bcallaway11.github.io/qte/reference/ci.qtet.html)]

**2. R `did` Package** [[Website](http://bcallaway11.github.io/did/)][[CRAN](https://cran.r-project.org/web/packages/did/index.html)] [[Github](https://github.com/bcallaway11/did)] [![](http://cranlogs.r-pkg.org/badges/grand-total/did)](http://cran.rstudio.com/web/packages/did/index.html)

This package contains tools for computing average treatment effect parameters in a difference in differences framework with more than two periods and with variation in treatment timing; it is based on Callaway and Sant'Anna (2020).  The main parameters are group-time average treatment effects which are the average treatment effect for a particular group at a a particular time.  These can be aggregated into a fewer number of treatment effect parameters, and the package provides aggregations into overall average treatment effects, dynamic treatment effects (event studies), group-specific treatment effects, and calendar time-specific treatment effects, or combinations of these.  There are also functions for pre-testing the parallel trends assumption (the main assumption underlying DID), and plotting group-time average treatment effects and/or particular aggregations of interest.

**3. R `ife` Package** [[Github](https://github.com/bcallaway11/ife)] 

The `ife` package contains code for estimating treatment effects in interactive fixed effects models with a small number of time periods.  Interactive fixed effects models allow for the "return" to unobserved heterogeneity (i.e., unit fixed effects) to change over time --- which is at least a key concern in many application in economics.  It implements the approach suggested in my paper "Treatment Effects in Interactive Fixed Effects Models" (co-authored with Sonia Karami).  Interactive fixed effects models are typically challenging to identify/estimate when a researcher only has access to a few time periods.  Our idea is to exploit having access to a covariate whose effect on the outcome does not change over time; if this sort of covariate is available, we show that it be used to identify the parameters in the interactive fixed effects model and therefore to be able to recover the average treatment effect on the treated.

**4. R `ppe` Package** [[Github](https://github.com/bcallaway11/ppe)]

The `ppe` package contains code for implementing the approaches suggested in "Policy Evaluation during a Pandemic" (co-authored with Tong Li).  In that paper, we suggest alternative approaches to policy evaluation during a pandemic (essentially conditioning on lagged outcomes) and recommend these over difference in differences approaches due to the possibly highly nonlinear nature of a pandemic.  This package contains code both for the case where the number of Covid-19 cases is the outcome of interest as well as code for when some other economic outcome is of interest but may itself depend on the number of Covid-19 cases.


## Generic/Helper R Packages

These are packages that I am actively working on.  They are probably less general interest than the other packages on this page, but are perhaps useful enough to make publicly available.  You'll find these packages imported in many of the other packages listed on this page.

**5. R `BMisc` Package** [[Website](http://bcallaway11.github.io/BMisc/)][[CRAN](https://cran.r-project.org/web/packages/BMisc/index.html)] [[Github](https://github.com/bcallaway11/BMisc)] [![](http://cranlogs.r-pkg.org/badges/grand-total/BMisc)](http://cran.rstudio.com/web/packages/BMisc/index.html)

The `BMisc` package contains various functions that I have found useful in my research and in writing `R` packages.  In particular, it contains functions for working with panel data, distribution and quantile functions, working with formulas, and generating summary statistics.

**6. R `pte` Package** [[Github](https://github.com/bcallaway11/pte)]

The `pte` package contains a generic framework for policy evaluation with panel data.  A number of my recent projects (`ife`, `ppe`, and, to some extent, `did`) have built on this framework as the backbone of their codebase.

## More R packages

I update these packages less frequently, mainly due to demand, but I am happy to offer support (and perhaps expanded functionality) if it would be helpful.

**7. R `distreg` Package** [[Website](http://bcallaway11.github.io/distreg/)] [[Github](https://github.com/bcallaway11/distreg)] 

The `distreg` package (formerly `TempleMetrics`) contains code for distribution regression.


**8. R `csabounds` Package**[[Website](https://bcallaway11.github.io/csabounds/)] [[CRAN](https://cran.r-project.org/web/packages/csabounds/index.html)] [[Github](https://github.com/bcallaway11/csabounds)] [![](http://cranlogs.r-pkg.org/badges/grand-total/csabounds)](http://cran.rstudio.com/web/packages/csabounds/index.html)

The `csabounds` package contains functions written for my project "Bounds on Distributional Treatment Effect Parameters using Panel Data with an Application on Job Displacement."  The main functions are `csa.bounds` which computes bounds on the distribution and quantile of the treatment effect and `attcpo` which computes the average treatment effect conditional on the previous outcome.

**9. R `ccfa` Package**[[Website](https://WeigeHuangEcon.github.io/ccfa/)] [[Github](https://github.com/WeigeHuangEcon/ccfa)]

The `ccfa` package contains functions for computing counterfactual distributions with a continuous treatment variable.  Weige Huang and I developed this package in conjunction with our project "Distributional Effects of a Continuous Treatment with an Application on Intergenerational Mobility." 

**10. R `lige` Package** [[Github](https://github.com/bcallaway11/lige)] 

The `lige` package implements the approach to estimating "local" versions of the intergenerational elasticity.  This comes from the project "Local Intergenerational Elasticities" (co-authored with Weige Huang).  This amounts to a smooth varying coefficient model where the parameters are local to a particular value of a continuous "treatment" variable; therefore, this code could be of use in other continuous treatment applications as well.

**11. R `qrme` Package** [[Github](https://github.com/bcallaway11/qrme)]

The `qrme` package implements the approach to nonlinear models with measurement error proposed in "Nonlinear Approaches to Intergenerational Income Mobility allowing for Measurement Error" (co-authored with Tong Li and Irina Murtazashvili).  The package contains code for (i) quantile regression with measurement error and (ii) a variety of nonlinear models (particularly related to intergenerational mobility) with measurement error in both the outcome and a "treatment" variable.

## Miscellaneous (perhaps useful) Code

This is code that is less than a full package.

**12. `kotlarski` code** [[Github](https://github.com/bcallaway11/kotlarski)]

This is code for implementing Kotlarski's result (as in Li and Vuong (1998)) for estimating the distribution of a latent variable when repeated observations are available for that variable.  
