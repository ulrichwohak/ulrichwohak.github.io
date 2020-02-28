---
layout: page
title: Code
permalink: /code/
---

## Main R packages

These are my R packages that seem to get the most attention.  These are actively maintained, and I welcome comments, issues, complaints, etc.

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

This package contains tools for computing average treatment effect parameters in Difference in Differences models with more than two periods and with variation in treatment timing; it is based on Callaway and Sant'Anna (2019).  The main parameters are group-time average treatment effects which are the average treatment effect for a particular group at a a particular time.  These can be aggregated into a fewer number of treatment effect parameters, and the package deals with the cases where there is selective treatment timing, dynamic treatment effects, calendar time effects, or combinations of these.  There are also functions for testing the Difference in Differences assumption, and plotting group-time average treatment effects.


## More Code

**3. R `BMisc` Package** [[Website](http://bcallaway11.github.io/BMisc/)][[CRAN](https://cran.r-project.org/web/packages/BMisc/index.html)] [[Github](https://github.com/bcallaway11/BMisc)] [![](http://cranlogs.r-pkg.org/badges/grand-total/BMisc)](http://cran.rstudio.com/web/packages/BMisc/index.html)

The `BMisc` package contains various functions that I have found useful in my research and in writing `R` packages.  In particular, it contains functions for working with panel data, distribution and quantile functions, working with formulas, and generating summary statistics.

**4. R `TempleMetrics` Package** [[Website](http://bcallaway11.github.io/TempleMetrics/)][[CRAN](https://cran.r-project.org/web/packages/TempleMetrics/index.html)] [[Github](https://github.com/bcallaway11/TempleMetrics)] [![](http://cranlogs.r-pkg.org/badges/grand-total/TempleMetrics)](http://cran.rstudio.com/web/packages/TempleMetrics/index.html)

The `TempleMetrics` package contains various functions that members of the Econometrics Reading Group at Temple University wrote from 2016-2019.  It mainly contains code for distribution regression.


**5. R `csabounds` Package**[[Website](https://bcallaway11.github.io/csabounds/)] [[CRAN](https://cran.r-project.org/web/packages/csabounds/index.html)] [[Github](https://github.com/bcallaway11/csabounds)] [![](http://cranlogs.r-pkg.org/badges/grand-total/csabounds)](http://cran.rstudio.com/web/packages/csabounds/index.html)

The `csabounds` package contains functions written for my project "Bounds on Distributional Treatment Effect Parameters using Panel Data with an Application on Job Displacement."  The main functions are `csa.bounds` which computes bounds on the distribution and quantile of the treatment effect and `attcpo` which computes the average treatment effect conditional on the previous outcome.

**6. R `ccfa` Package**[[Website](https://WeigeHuangEcon.github.io/ccfa/)] [[CRAN](https://cran.r-project.org/web/packages/ccfa/index.html)] [[Github](https://github.com/WeigeHuangEcon/ccfa)] [![](http://cranlogs.r-pkg.org/badges/grand-total/ccfa)](http://cran.rstudio.com/web/packages/ccfa/index.html)

The `ccfa` package contains functions for computing counterfactual distributions with a continuous treatment variable.  Weige Huang and I developed this package in conjunction with our project "Distributional Effects of a Continuous Treatment with an Application on Intergenerational Mobility." 

**7. `kotlarski` code** [[Github](https://github.com/bcallaway11/kotlarski)]

This is code for implementing Kotlarski's result (as in Li and Vuong (1998)) for estimating the distribution of a latent variable when repeated observations are available for that variable.  