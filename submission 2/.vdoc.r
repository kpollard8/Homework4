#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| include: false

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, readr, readxl, hrbrthemes, fixest,
               scales, gganimate, gapminder, gifski, png, tufte, plotly, OECD,
               ggrepel, survey, foreign, devtools, pdftools, kableExtra, modelsummary,
               kableExtra)
#
#
#
#
knitr::opts_chunk$set(warning = FALSE)
#
#
#
#
#| include: false
#| eval: true
load("Hw4_workspace.Rdata")
#
#
#
#
#
#
#
#
#
#
#
#
#| echo: false
#| label: plancountscounty
#| fig-cap: Question 1 Graph

question1
#
#
#
#
#
#
#
#
#
#
#
#
#| echo: false
#| label: starratingsdist
#| fig-cap: Question 2 Graph

question2
#
#
#
#
#
#
#
#
#
#
#| echo: false
#| label: avbench
#| fig-cap: Question 3 Graph

question3
#
#
#
#
#
#
#
#
#
#| echo: false
#| label: shareofMA
#| fig-cap: Question 4 Graph

question4
#
#
#
#
#
#
#
#| echo: false
#| label: runningvariable
#| tbl-cap: "Number of rounded plans"

library(kableExtra)
options(knitr.kable.NA = 0)
knitr::kable(star_rating_counts, 
             col.names=c("Star Ratings", "Number of Plans"),
             format.args=list(big.mark=","), booktabs = TRUE) %>%
             kable_styling(latex_options=c("scale_down"))

#
#
#
#
#
#
#
#| echo: false
#| label: ratings3
#| fig-cap: "3 star vs 2.5 star"

kable(summary_df, 
      col.names = c("Method", "Coefficient", "Std. Error", "z-value", "Pr(>|z|)", "CI Lower", "CI Upper"),
      format.args = list(big.mark = ","), 
      align = c("l", "c", "c", "c", "c", "c", "c"),
      caption = "RD Estimation Summary",
      booktabs = TRUE) %>%
  kable_styling(latex_options = c("scale_down"))

#
#
#
#
#| echo: false
#| label: ratings35
#| fig-cap: "3.5 star vs 3 star"

est35
#
#
#
#
#
#
#
#
#
#
#| echo: false
#| label: diffbandwiths
#| fig-cap: Question 7 Graph

Q7
#
#
#
#
#
#
#
#
#
#| echo: false
#| label: dens3
#| fig-cap: Question 8 First Graph

dens3
#
#
#
#| echo: false
#| label: dens35
#| fig-cap: Question 8 Second Graph

dens35
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
