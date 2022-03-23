---
author: brant
categories:
- Econometrics
- Policy Evaluation
- Panel Data
- TWFE Regressions
date: 2022-02-08
layout: single
output:
  md_document:
    preserve_yaml: true
    variant: markdown
permalink: /posts/fms-did-time-varying-covariates
title: "Five Minute Summary: Difference in differences with time-varying
  covariates"
---

$$\newcommand{\E}{\mathbb{E}}$$

My colleague [Carol Caetano](http://carolinacaetano.net/), and two UGA
grad students [Hugo Sant'Anna
Rodrigues](https://entr.uga.edu/directory/people/hugo-rodrigues) and
Stroud Payne just posted [a new working
paper](https://arxiv.org/abs/2202.02903) about DID with time varying
covariates. This is a topic that has been in the back of my mind for a
long time --- and I get a lot of
[questions](https://github.com/bcallaway11/did/issues/71) about time
varying covariates from people using the [`did`
package](https://bcallaway11.github.io/did/).

My sense has been there are perhaps a number of limitations to the sorts
of two way fixed effects (TWFE) regressions that include covariates that
are very common in applied work. And, in particular, that there could be
distinct issues from those that show up in the literature on TWFE
regressions with multiple periods and variation in treatment timing
(e.g., [Goodman-Bacon
(2021)](https://doi.org/10.1016/j.jeconom.2021.03.014), [de Chaisemartin
and d'Haultfoeuille (2020)](https://doi.org/10.1257/aer.20181169), and
[Borusyak, Jaravel, and Spiess
(2021)](https://arxiv.org/abs/2108.12419))

In this paper, we have worked out a lot of these issues -- particularly,
in the case with exactly two time periods (which is a case where TWFE
regressions work well under unconditional parallel trends). We also
provide alternative strategies that (i) are able to get around these
issues and (ii) are only slightly more complicated to implement than
TWFE regressions.

**TWFE Regressions**

To fix ideas, let me write down how most researchers implement DID
identification strategies when they think that the underlying parallel
trends assumption ought to be conditional on some covariates:

$$
\begin{aligned}
  Y_{it} = \theta_t + \eta_i + \alpha D_{it} + X_{it}'\beta + v_{it}
\end{aligned}
$$

where $$Y_{it}$$ is the outcome of interest (for unit $$i$$ in time
period $$t$$), $$\theta_t$$ is a time fixed effect, $$\eta_i$$ is an
individual fixed effect, $$D_{it}$$ is a treatment dummy variable,
$$\alpha$$ is what will be reported as *the* causal effect of the
treatment (or, maybe loosely as some kind of average causal effect),
$$X_{it}$$ are the time-varying covariates, and $$v_{it}$$ are
idiosyncratic time-varying unobservables.

We show that there are a number of potential limitations with using this
two-way fixed effects (TWFE) regression:

1.  In cases with multiple periods and variation in treatment timing,
    this sort of TWFE regression uses already treated units as the
    comparison group, and therefore suffers from all well-known
    weaknesses in this case. Both Goodman-Bacon (2021) and de
    Chaisemartin and d'Haultfoeuille (2020) already have results along
    these lines, so I'm going to only talk about the case with two time
    periods below (which is a case where, at least in the case of
    unconditional parallel trends, TWFE regressions work fine).

2.  TWFE regressions won't work well if the time-varying covariates are
    affected by the treatment. This issue is often referred to as a "bad
    control" problem. It seems to be standard practice just not to
    include covariates that are potentially affected by the treatment. I
    agree that it's a bad idea to include a time-varying covariate that
    is itself affected by the treatment, but I am less sure that a good
    solution is to just not include it.

    For example, suppose that a labor economist is studying the effect
    of a treatment on a person's earnings and thinks parallel trends
    holds after conditioning on a person's occupation, but occupation is
    potentially affected by the treatment (there is tons of work in
    labor economics that would be concerned with this issue). Both
    ignoring occupation and including occupation run into issues. One
    helpful way to think about this is to try to condition on *untreated
    potential occupation* --- that is, what occupation would have
    occurred if a person had not been treated. TWFE regressions don't
    naturally accommodate this, but we propose some solutions for this
    case (I'll come back to this below).

3.  TWFE regressions like this are *highly* sensitive to the functional
    form. Since we are considering the case with two periods (and, like
    the "textbook" version of DID, where no units are treated yet in the
    first period), we can write

    $$
       \begin{aligned}
       \Delta Y_{it^*} = (\theta_{t^*} - \theta_{t^*-1}) + \alpha D_i + \Delta X_{it^*} \beta + \Delta v_{it^*}
       \end{aligned}
     $$

    where $$t^*$$ indicates the second time period. You can see from
    this specification that, due to our linear functional form for the
    levels, we are effectively only controlling for the change in
    covariates over time.

    We show that TWFE regressions implicitly rely on (i) the conditional
    parallel trends assumption only depending on the change in the
    time-varying covariates over time, and (ii) similarly, that TWFE
    regressions rely on conditional ATTs only depending on changes in
    time-covariates over time. Thus, TWFE can perform poorly in cases
    where, for example, the path of untreated potential outcomes also
    depends on the *level* of the time-varying covariate.

    Let me give you a concrete example where only controlling for
    changes in covariates seems undesirable. Suppose you are using
    county-level data and are studying the effect of a treatment in
    Georgia using counties from Tennessee as the comparison group, and
    that you think that parallel trends holds after you condition on
    county population. I live in Oconee County, Georgia. From 2010 to
    2021 (which were the dates I could most easily find for county
    population), Oconee County grew from about 33,000 to about 42,000.
    In Tennessee, the county with the most similar population change was
    Sevier County which increased from about 90,000 to about 99,000. But
    Sevier County is more than twice as big as Oconee County. This is
    probably not what we had in mind when we said we wanted to condition
    on county population. Maybe this is just bad luck, let's check the
    county with the next most similar population change. It is Shelby
    County --- this is Memphis! --- which increased from 928,500 to
    938,800. I don't think anyone would think that comparing paths of
    outcomes for Shelby County and Oconee County is what any researcher
    has in mind for DID conditioning on county population. As a
    side-comment, if you switch to, say, the change in log population
    over time, you do not do much better either --- in that case, the
    closest match is Montgomery County, TN which has over 5 times the
    population of Oconee County.

    Perhaps somewhat surprisingly TWFE regressions also require strong
    functional form assumptions on the propensity score (see paper for
    details).

4.  Similarly, we show that TWFE regressions are not robust to parallel
    trends assumptions and conditional ATTs depending on *time-invariant
    covariates*. However, conditioning on time-invariant covariates in
    the parallel trends assumption is important in many applications.
    For example, if you are a labor economist studying the effect of
    some treatment on people's earnings, the most important covariates
    to condition on in the parallel trends assumption are all likely to
    be time invariant --- e.g., demographics, education, etc.

5.  $$\alpha$$ is hard to interpret in the presence of treatment effect
    heterogeneity. Even if none of the issues above apply in a
    particular application, if treatment effects are heterogeneous
    (particularly, if they can vary across different values of the
    covariates), (under some additional conditions) $$\alpha$$ will be
    equal to a weighted average of conditional ATT parameters but they
    will suffer from the "weight reversal" property pointed out in
    [Sloczynski (2020)](https://doi.org/10.1162/rest_a_00953) in a
    different context --- conditional ATTs for values of the covariates
    that are uncommon for the treated group relative to the untreated
    group get lots of weight, and the opposite happens for relatively
    common values of the covariates.

If a researcher is fortunate enough that none of these issues apply in
their application, then a TWFE regression would recover the ATT.

**Existing work in econometrics**

Most work on DID under conditional parallel trends (e.g., [Abadie
(2005)](https://doi.org/10.1111/0034-6527.00321), [Sant'Anna and Zhao
(2020)](https://doi.org/10.1016/j.jeconom.2020.06.003), and [Chang
(2020)](https://doi.org/10.1093/ectj/utaa001)) considers the case with
time-invariant covariates or uses "pre-treatment" values of time-varying
covariates (which effectively just makes time-varying covariates time
invariant by using their value in the pre-treatment period). This
already solves most of the above issues: they can be adapted to handle
cases multiple periods and variation in treatment timing in (1), they do
not require the same strong functional form assumptions as in (3), they
solve (4) above because they include time-invariant covariates, and they
recover the overall ATT directly rather than a hard-to-interpret
weighted average of conditional ATTs as in (5).

**What's new in our paper**

1.  First, in order to address (2), where the time-varying covariates
    could themselves be affected by the treatment, we provide specific
    conditions under which it is sufficient to condition on
    pre-treatment values of the time-varying covariates as is common in
    the econometrics literature. In particular, the condition that
    rationalizes conditioning on pre-treatment covariates is

    $$
       \begin{aligned}
       X_{t^*}(0) \perp D | X_{t^*-1}, Z
       \end{aligned}
     $$

    where $$X_{t^*}(0)$$ is the value that $$X$$ would take in time
    period $$t^*$$ if the treatment had not occurred and $$Z$$ is the
    vector of time-invariant covariates in the parallel trends
    assumption. This is an unconfoundedness assumption, but for
    time-varying covariates rather than the outcome. In words, it says
    that covariates are evolving similarly among treated and untreated
    units that have the same pre-treatment characteristics $$X_{t^*-1}$$
    and time-invariant covariates $$Z$$.

    This condition may or may not be reasonable in particular
    applications, but it is the sort of thing that reseachers ought to
    think about. It is also "pre-testable" (i.e., you can look at data
    in pre-treatment periods and potentially find evidence for or
    against it).

    In cases where this assumption does not hold, the strategy of just
    conditioning on pre-treatment covariates does not generally work.
    But we consider a number of other possible assumptions that can lead
    to alternative identification arguments in the paper. A big part of
    the paper is about these cases, but it is perhaps best just to
    consult the paper itself on this front as these arguments are
    somewhat more complicated.

2.  Another important case is when a researcher is confident that
    covariates are evolving exogenously from the treatment; a simple
    version of this is just where $$X_{it^*}(1) = X_{it^*}(0)$$ for all
    units (that is, the value of the covariates is the same under the
    treatment as without the treatment). Ignoring the issue of
    time-invariant covariates, the main issue with TWFE in this case are
    the functional form issues pointed out in (3) above. In this case,
    we provide a doubly robust expression for that ATT that does not
    rely on those sorts of functional form assumptions. These
    expressions involve outcome regressions and propensity scores that
    depend on both $$X_{t^*}$$ and $$X_{t^*-1}$$ --- these can be
    challenging to estimate well because $$X_{t^*}$$ and $$X_{t^*-1}$$
    are likely to be highly collinear in many applications. However, the
    doubly robust expression for the ATT allows us to connect to the
    literature on DID with machine learning (Chang (2020)) which
    provides an attractive way to try to estimate these functions.

3.  Finally, in cases where these kinds of doubly robust / machine
    learning approaches are more complicated than a researcher actually
    wants to implement, we provide strategies for all of the cases
    discussed above that can be implemented using just regressions and
    averaging. Relative to the previous two points, these approaches
    require additional linearity assumptions (though substantially less
    restrictive than the issues discussed earlier for TWFE regressions),
    but have the benefit of being easier to implement; these ideas build
    on the ideas of regression adjustment and imputation that have shown
    up recently in the DID literature ([Liu, Wang, and
    Xu (2021)](https://arxiv.org/abs/2107.00856),
    [Gardner (2021)](https://jrgcmu.github.io/2sdd_current.pdf),
    Borusyak, Jaravel, and Spiess (2021)).

    Let me just give the example of what we propose to do in cases where
    the time-varying covariates evolve exogenously. Similar to the
    "imputation" literature, we can exploit the connection between
    parallel trends assumptions and a model for untreated potential
    outcomes:

    $$
     \begin{aligned}
     Y_{it}(0) = Z_i'\delta_t + \eta_i + X_{it}(0) \beta_t + v_{it}
     \end{aligned}
    $$

where we take $$Z$$ to include an intercept. The $$\beta_t$$ is perhaps
non-standard (see discussion in next paragraph). Taking the difference
over time implies

$$
    \begin{aligned}
    \Delta Y_{it^*}(0) = Z_i'\delta^*_{t^*} + \Delta X_{it^*}(0) \beta_{t^*} + X_{it^*-1}(0) \beta^*_{t^*} + \Delta v_{it^*}
    \end{aligned}
  $$

where we define $$\delta^*_{t^*} := (\delta_{t^*} - \delta_{t^*-1})$$
and $$\beta^*_{t^*} := (\beta_{t^*} - \beta_{t^*-1})$$. In my view, this
is particularly attractive specification for untreated potential
outcomes in terms of time-varying covariates. It includes both the
initial level of the covariates (which is similar to including the
"pre-treatment" value of the covariate) as well as the change in
covariates over time. And, for example, (up to the parametric
assumptions) this expression would avoid the issues of comparing
counties with similar changes in population over time but very
dissimilar overall populations.

Moreover, since we observe untreated potential outcomes and covariates
for the untreated group, we can recover all of the parameters from the
regression of $$\Delta Y_{t^*}$$ on $$Z$$, $$\Delta X_{t^*}$$, and
$$X_{t^*-1}$$ using the untreated group. Next, notice that

$$
  \begin{aligned}
    ATT &= \E[\Delta Y_{t^*} | D=1] - \E[\Delta Y_{t^*}(0) | D=1] \\
    &= \E[\Delta Y_{t^*} | D=1] - \Big(\E[Z|D=1]'\delta^*_{t^*} + \E[\Delta X_{t^*}(0) | D=1] \beta_{t^*} + \E[X_{t^*-1}|D=1]\beta^*_{t^*} \Big) \\
  \end{aligned}
  $$

where the second equality holds by plugging in the expression for
$$\Delta Y_{t^*}(0)$$ from the previous display. Everything is
identified in the last line except for $$\E[\Delta X_{t^*}(0) | D=1]$$.
If we believe that covariates evolve exogenously though, it means that
this term is equal to $$\E[\Delta X_{t^*} | D=1]$$ which is identified.
We consider 5 additional scenarios for recovering
$$\E[\Delta X_{t^*}(0) | D=1]$$ in the paper.

To summarize, this suggests a simple two-step estimation procedure: (i)
estimate a regression using untreated observations and recover the
estimates of the parameters in the model for untreated potential
outcomes, (ii) combine these with estimates of the averages of the
change in outcomes over time and averages of covariates for the treated
group (as in the previous display) to compute the ATT.

**Conclusion**

In my view, the sorts of TWFE regressions that show up in many
applications in economics have a number of limitations -- when these
TWFE regressions include time-varying covariates, we are arguing that
they are likely to have a number of disadvantages even in "textbook"
cases with only two time periods. Fortunately, it is quite
straightforward to use other approaches (that are not much more
complicated) that can essentially avoid all of these issues.

We don't have code yet, but we are working on it. If you have
comments/questions, please feel free to get in touch.

**References**

-   Abadie, Alberto. "Semiparametric difference-in-differences
    estimators." The Review of Economic Studies 72.1 (2005): 1-19.

-   Borusyak, Kirill, Xavier Jaravel, and Jann Spiess. "Revisiting event
    study designs: Robust and efficient estimation." arXiv preprint
    arXiv:2108.12419 (2021).

-   Chang, Neng-Chieh. "Double/debiased machine learning for
    difference-in-differences models." The Econometrics Journal 23.2
    (2020): 177-191.

-   de Chaisemartin, Clément, and Xavier d'Haultfoeuille. "Two-way fixed
    effects estimators with heterogeneous treatment effects." American
    Economic Review 110.9 (2020): 2964-96.

-   Gardner, John. "Two-stage differences in differences." (2021).

-   Goodman-Bacon, Andrew. "Difference-in-differences with variation in
    treatment timing." Journal of Econometrics (2021).

-   Liu, Licheng, Ye Wang, and Yiqing Xu. "A practical guide to
    counterfactual estimators for causal inference with time-series
    cross-sectional data." arXiv preprint arXiv:2107.00856 (2021).

-   Sant'Anna, Pedro HC, and Jun Zhao. "Doubly robust
    difference-in-differences estimators." Journal of Econometrics 219.1
    (2020): 101-122.

-   Słoczyński, Tymon. "Interpreting ols estimands when treatment
    effects are heterogeneous: Smaller groups get larger weights." The
    Review of Economics and Statistics (2020): 1-27.

<script src="https://giscus.app/client.js"
        data-repo="bcallaway11/bcallaway11.github.io"
        data-repo-id="MDEwOlJlcG9zaXRvcnk3NDQyMTEyMQ=="
        data-category="Announcements"
        data-category-id="DIC_kwDOBG-Tgc4COCq4"
        data-mapping="pathname"
        data-reactions-enabled="1"
        data-emit-metadata="0"
        data-input-position="bottom"
        data-theme="light"
        data-lang="en"
        crossorigin="anonymous"
        async>
</script>
