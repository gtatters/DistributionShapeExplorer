# Distribution Shape Explorer

An interactive Shiny app for BIOL 3P96 (Biostatistics) at Brock University.

## What this app does

Choose a distribution, adjust its parameters with sliders, and press
**Resample** to generate a new dataset. The app plots the simulated data
as a histogram (or spike plot for discrete distributions) alongside the
theoretical curve or probability mass function. A summary table below the
plot explains what the distribution looks like, where it is used in biology,
and how its mean and variance respond to changes in the parameters.

## Distributions included

Normal, Lognormal, Exponential, Uniform, Gamma, Beta, Binomial,
Negative Binomial, Geometric, Hypergeometric, Poisson.

## Learning goals

- Recognise the visual shapes of common distributions
- Understand how parameters control the centre, spread, and skew
- Connect each distribution to real biological examples
- See the difference between continuous (smooth curve) and discrete
  (spike plot) distributions

## How to use

1. Select a distribution from the dropdown menu.
2. Use the sliders to change its parameters and watch the shape update.
3. Press **Resample** to draw a new random sample and observe natural variation.
4. Read the description table below the plot for biological context.

## Course context

Developed for BIOL 3P96 — Biostatistics, Brock University.
Built with R and Shiny (base R graphics only).