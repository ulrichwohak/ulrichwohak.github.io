---
layout: page
title: Code
permalink: /code/
---

**1. R `qte` Package** [[Website](http://bcallaway11.github.io/qte/)][[CRAN](https://cran.r-project.org/web/packages/qte/index.html)] [[Github](https://github.com/bcallaway11/qte)] [![](http://cranlogs.r-pkg.org/badges/grand-total/qte)](http://cran.rstudio.com/web/packages/qte/index.html)

The R `qte` package provides many methods for estimating the Quantile Treatment Effect (QTE) or the Quantile Treatment Effect on the Treated (QTET). These include estimating the QTE and QTET under the assumption of selection on observables (Firpo, 2007), the QTET in the Changes in Changes model (Athey and Imbens, 2006), the QTET in the Quantile Difference in Differences model, and the QTET under a Distributional Difference in Differences assumption (Callaway and Li, 2016; Callaway, Li, Oka (2017)).

  * **Difference in Differences Methods**

    * `panel.qtet` from Callaway and Li, 2016. [[Tutorial](http://bcallaway11.github.io/qte/articles/panel-qtet.html)] [[Documentation](http://bcallaway11.github.io/qte/reference/panel.qtet.html)]
    * `ddid2` from Callaway, Li, and Oka, 2017.  [[Tutorial](http://bcallaway11.github.io/qte/articles/ddid2.html)] [[Documentation](http://bcallaway11.github.io/qte/reference/ddid2.html)]
    * `CiC` from Athey and Imbens, 2006 [[Documentation](http://bcallaway11.github.io/qte/reference/CiC.html)]
    * `QDiD` Quantile Difference in Differences [[Documentation](http://bcallaway11.github.io/qte/reference/QDiD.html)]

  * **Selection on Observables Methods**

    * `ci.qte` from Firpo, 2007.  [[Documentation](http://bcallaway11.github.io/qte/reference/ci-qte.html)]
    * `ci.qtet` from Firpo, 2007 [[Documentation](http://bcallaway11.github.io/qte/reference/ci-qtet.html)]

**2. R `BMisc` Package** [[Website](http://bcallaway11.github.io/BMisc/)][[CRAN](https://cran.r-project.org/web/packages/BMisc/index.html)] [[Github](https://github.com/bcallaway11/BMisc)] [![](http://cranlogs.r-pkg.org/badges/grand-total/BMisc)](http://cran.rstudio.com/web/packages/BMisc/index.html)

The `BMisc` package contains various functions that I have found useful in my research and in writing `R` packages.  In particular, it contains functions for working with panel data, quantiles, and printing results.