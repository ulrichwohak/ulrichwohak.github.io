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

title <- "Parallel Trends Assumption"

before <- "## Parallel Trends Assumption

$$\\E[\\Delta Y_{t^*}(0) | D=1] = \\E[\\Delta Y_{t^*}(0) | D=0]$$
--

<br><br>

Recovering $ATT$ under parallel trends:
"

eqlist <- list("ATT &= \\E[Y_{t^*}(1) | D=1] - \\E[Y_{t^*}(0) | D=1] \\hspace{150pt}",
                "&= \\E[Y_{t^*}(1) - Y_{t^*-1}(0) | D=1] - \\E[Y_{t^*}(0) - Y_{t^*-1}(0) | D=1]",
                "&= \\E[Y_{t^*}(1) - Y_{t^*-1}(0) | D=1] - \\E[Y_{t^*}(0) - Y_{t^*-1}(0) | D=0]",
                "&= \\E[\\Delta Y_{t^*} | D=1] - \\E[\\Delta Y_{t^*} | D=0]")

after <- "

--

which is where difference in differences gets its name

"

step_by_step_eq(before=before,
                eqlist=eqlist,
                after=after,
                title=title)



## ----echo=FALSE, results="asis"-----------------------------------------------

title <- "Estimation"

before <- "Given the above discussion, estimation of the $ATT$ is very easy.

--

"

eqlist <- list("\\widehat{ATT} &= \\hat{\\E}[\\Delta Y_{t^*} | D=1] - \\hat{\\E}[\\Delta Y_{t^*}|D=0] \\hspace{150pt}",
               "&= \\frac{1}{n} \\sum_{i=1}^n \\frac{D_i}{\\hat{p}} \\Delta Y_{it^*} - \\frac{1}{n} \\sum_{i=1}^n \\frac{(1-D_i)}{(1-\\hat{p})} \\Delta Y_{it^*}")

after <- "

--

Or, even more easily, run the following <span class=\"alert\">two-way fixed effects regression (TWFE):</span>

$$Y_{it} = \\theta_t + \\eta_i + \\alpha D_{it} + v_{it}$$

--

<span class=\"alert-blue\">Pros:</span> Economists know a lot about this sort of regression and you can just read off standard errors, etc.

"

step_by_step_eq(title=title,
                before=before,
                eqlist=eqlist,
                after=after)


