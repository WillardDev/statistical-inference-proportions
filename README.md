# Statistical Inference for Proportions

This repository contains R code for analyzing and implementing statistical methods related to confidence intervals and hypothesis testing for proportions.

## Overview

This project demonstrates several key concepts in statistical inference:

1. **Confidence Interval Construction**: Implementation of a function to calculate confidence intervals for proportions with customizable confidence levels.

2. **Case Study: Florida Homeowners Insurance**: Analysis of self-insurance rates among Florida homeowners, including:
   - Standard error estimation
   - Confidence interval construction
   - Exploration of interval properties under different true population parameters

3. **Hypothesis Testing: E-cigarette Usage**: Complete hypothesis testing workflow examining whether e-cigarette usage among high school students has decreased, featuring:
   - Null hypothesis formulation
   - P-value calculation using exact binomial methods
   - Critical value determination
   - Type I error analysis
   - Statistical power calculations

## Key Functions

The repository includes a custom R function `ci.for.proportion()` that calculates confidence intervals for proportions based on:
- Sample proportion (phat)
- Sample size (n)
- Desired confidence level (conf)

## Statistical Concepts Demonstrated

- Standard error calculation for proportions
- Confidence interval interpretation
- Hypothesis testing framework
- Type I error analysis
- Statistical power
- Exact binomial probability calculations
- Z-score computation

## Usage

The R script can be run in any R environment. Each problem is clearly commented to explain the statistical concepts being applied.

```r
# Example usage of the confidence interval function
ci.for.proportion(0.168, 487, 0.90)
```

## Requirements

- R (any recent version)
- Base R is sufficient (no additional packages required)
