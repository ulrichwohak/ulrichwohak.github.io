---
layout: single
title: Software
permalink: /software/
---

## [BipartiteGMRF.jl](https://github.com/ulrichwohak/BipartiteGMRF.jl)

Julia package for fitting bipartite-graph Gaussian Markov random-field random-effects models. [GitHub](https://github.com/ulrichwohak/BipartiteGMRF.jl) / [DOI](https://doi.org/10.5281/zenodo.19048278)

<p align="justify">BipartiteGMRF.jl provides tools for estimating random-effects models on matched bipartite data, such as worker-firm, student-school, or patient-provider networks. The package places a joint Gaussian prior on the two sets of latent node effects and estimates variance components and local dependence using sparse linear algebra. It was developed as the computational backbone for <em>A Random-Effects Model Reveals Strong Positive Sorting in CEO Labor Markets</em>, and is intended for reusable likelihood-based estimation, variance decomposition, and covariance extraction in sparse mobility networks.</p>
