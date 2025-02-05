adversarial-collab: AI Forecasting Adversarial Collaboration replication package
================

<!-- badges: start -->

![GitHub R package version](https://img.shields.io/github/r-package/v/forecastingresearch/adversarial-collab)
![GitHub forks](https://img.shields.io/github/forks/forecastingresearch/adversarial-collab)

<!-- badges: end -->

This repository contains all the code used to produce the analysis in the [AI Adversarial Collaboration report](linkToCome) from the Forecasting Research Institute ([FRI](https://forecastingresearch.org)).

## To run

You must first install the [VOI/VOD library](https://github.com/forecastingresearch/voi-vod), e.g. using devtools: `devtools::install_github("forecastingresearch/voi-vod")`

Once that's installed, you can run `RUN.Rmd`. That contains all the code used to generate the figures and tables in the report.

## Data

`FORECASTS.csv` contains participants' conditional forecasts on each of the cruxes. The file consists of the following columns:

- `person`: The identifier for the participant making the forecasts (these are aliases)
- `Camp`: The group that the individual belongs to (skeptical of AI x-risk or concerned)
- `ID`: The short name for the crux being forecasted on (you can find the full text operationalizations of these in the report)
- `PU`: The probability (as a decimal) assigned to human extinction due to AI by 2100
- `Pc`: The probability (as a decimal) assigned to the crux question (ID) resolving positively
- `PUc`: The probability (as a decimal) assigned to U resolving positively conditional on the crux resolving positively

Note that participants changed their P(U)'s over the course of the study. From crux to crux, you may notice that the same participant reported a different value for P(U).

### Example entry

| person | Camp      | ID                          | PU   | Pc    | PUc |
|--------|-----------|-----------------------------|------|-------|-----|
| Eve    | Skeptical | Cyberattacks                | 0.03 | 0.150 | 0.03|

In this example, Eve, who belongs to the skeptics camp, provides a conditional forecast for "Cyberattacks" (operationalized as: 1. At least 50% of the top 10 most expensive cyberattacks before 2030 rely on the use of Artificial Intelligence, AND 2. An external cybersecurity breach caused the death or injury of at least one hundred people before 2030). Eve's personal unconditional P(AI doom) is 3%. They believe there's a 15% chance that "cyberattacks" resolves positively. Their P(AI doom **conditional on** cyberattacks resolving positively) is unchanged - 3%, yielding a VOI of 0 (i.e. whether or not the cyberattacks question resolves positively will have no bearing on their P(AI doom)).

## Questions

Please feel free to contact zach@forecastingresearch.org with any questions about the project or the analysis herein.
