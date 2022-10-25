# Flexible Bayesian Nowcasting with application to COVID-19 fatalities in Sweden

This repository contains data and code for the manuscript *Flexible Bayesian Nowcasting with application to COVID-19 fatalities in Sweden* by Fanny Bergström, Felix Günther, Michael Höhle and Tom Britton at Stockholm University. The manuscript is available on [arXiv](https://arxiv.org/abs/2202.04569/).

Weekly updated nowcast estimates of COVID-19 fatalities and ICU admissions in Sweden using our proposed method are found at 
<p align="center">
  https://staff.math.su.se/fanny.bergstrom/covid19-nowcasting.
</p>

## Repo structure

The implementation of the different Nowcasting models are given in the folder [stan_models](https://github.com/fannybergstrom/nowcasting_covid19/tree/main/code/stan_models).

The R code used for our analysis is structures as follows

1. [Data](https://github.com/fannybergstrom/nowcasting_covid19/tree/main/data/fohm) preprocessing.
2. Definition of functions for fitting the nowcast models to given reporting dates for each model 
   and saving the results of the post-processing of a fit.
3. Estimation of chosen nowcast models and reporting dates.
4. Summarize results.
5. Plots (incl. results of single days).
