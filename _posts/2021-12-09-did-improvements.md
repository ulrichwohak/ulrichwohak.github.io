---
author: brant
categories:
- Econometrics
- Panel Data
- did package
comments: True
date: '2021-12-10'
layout: single
output:
  md_document:
    preserve_yaml: True
    variant: 'markdown\_github'
permalink: '/posts/did-improvements'
title: Faster did Code
---

Introduction
============

We just put out a big update to the `did` package (version 2.1). It is
on GitHub and should be on CRAN in a day or two. This version contains a
lot of behind-the-scenes improvement such as better error handling and
substantially more thorough testing. It also allows users to [use a
universal base period in event
studies](posts/event-study-universal-v-varying-base-period).

This post though is about speed improvements in the new version of the
`did` package. These are almost all coming from improved performance of
our bootstrap procedure. As a side-comment: we have always used the
multiplier bootstrap, which is much, much faster than the empirical
bootstrap to begin with, but we implemented the new version in C++ which
results in notable improvements in computation time. Using the
multiplier bootstrap is what allows the `did` package to report, for
example, uniform confidence bands for group-time average treatment
effects and event studies; that is, they tend to be somewhat wider than
pointwise confidence intervals but are robust to the multiple testing
arising in both of these cases.

<table style="width:100%;">
<colgroup>
<col style="width: 14%" />
<col style="width: 14%" />
<col style="width: 14%" />
<col style="width: 14%" />
<col style="width: 14%" />
<col style="width: 14%" />
<col style="width: 14%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: center;">Type</th>
<th style="text-align: center;"># units</th>
<th style="text-align: center;"># time periods</th>
<th style="text-align: center;">rows of data</th>
<th style="text-align: center;">old computation time</th>
<th style="text-align: center;">new computation time</th>
<th style="text-align: center;">percentage reduction</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: center;">Balanced Panel</td>
<td style="text-align: center;">5000</td>
<td style="text-align: center;">8</td>
<td style="text-align: center;">40,000</td>
<td style="text-align: center;">12.5</td>
<td style="text-align: center;">3.5</td>
<td style="text-align: center;">72%</td>
</tr>
<tr class="even">
<td style="text-align: center;">Unbalanced Panel</td>
<td style="text-align: center;">5000</td>
<td style="text-align: center;">8</td>
<td style="text-align: center;">39,999</td>
<td style="text-align: center;">283.4</td>
<td style="text-align: center;">3.4</td>
<td style="text-align: center;">99%</td>
</tr>
<tr class="odd">
<td style="text-align: center;">Repeated Cross Sections</td>
<td style="text-align: center;">5000</td>
<td style="text-align: center;">8</td>
<td style="text-align: center;">5000</td>
<td style="text-align: center;">9.3</td>
<td style="text-align: center;">3.4</td>
<td style="text-align: center;">63%</td>
</tr>
<tr class="even">
<td style="text-align: center;">Larger Panel Data</td>
<td style="text-align: center;">20,000</td>
<td style="text-align: center;">20</td>
<td style="text-align: center;">400,000</td>
<td style="text-align: center;">328.5</td>
<td style="text-align: center;">97.2</td>
<td style="text-align: center;">70%</td>
</tr>
</tbody>
</table>

For the full code for this post, see:
<https://github.com/bcallaway11/did/blob/master/tests/code_profiling.Rmd>
