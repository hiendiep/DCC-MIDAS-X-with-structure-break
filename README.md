Dynamic Correlation and Hedging Effectiveness of Bitcoin vs. Brent Crude Oil: DCC-MIDAS-X Analysis

1. Introduction
+ This repository contains an R-based implementation of the DCC-MIDAS-X model with structural breaks, developed to analyze the dynamic correlation and hedging effectiveness of Bitcoin against Brent crude oil under the influence of Global Economic Policy Uncertainty (GEPU).
+ The code supports the thesis, "The Dynamic Correlation and Hedging Effectiveness of Bitcoin against Brent Crude Oil under the Influence of Global Economic Policy Uncertainty"
+ The thesis investigates how Bitcoin's volatility and correlation with Brent oil evolve from January 2016 to September 2024, focusing on GEPU's role and macroeconomic regime shifts identified via structural break analysis.
+ The codebase estimates time-varying volatility and correlations using the GARCH-MIDAS-X and DCC-MIDAS-X frameworks, incorporating low-frequency GEPU data and structural breaks detected through the Bai-Perron test.

  
2. Purpose: The primary objectives of this code are to:
+ Estimate the dynamic volatility of Bitcoin and Brent oil using GARCH-MIDAS-X, accounting for GEPU and structural breaks.
+ Model the time-varying correlation between Bitcoin and Brent oil with DCC-MIDAS-X, integrating GEPU as a macroeconomic driver.
+ Assess Bitcoin's hedging effectiveness against Brent oil price volatility, calculating optimal hedge ratios and hedging effectiveness (HE) metrics.
+ Provide a robust framework for analyzing asset relationships under macroeconomic uncertainty, applicable beyond the Bitcoin-oil context.
  
3. Features
+ Data Loading: Imports daily Bitcoin and Brent oil prices, and monthly GEPU data from local Excel files (adaptable to other sources).
+ Structural Break Analysis: Implements the Bai-Perron test to detect regime shifts in GEPU, generating dummy variables for model integration.
+ Volatility Modeling: Fits GARCH-MIDAS-X to estimate short- and long-run volatility components with GEPU effects.
+ Correlation Modeling: Applies DCC-MIDAS-X to compute dynamic correlations, incorporating structural breaks and GEPU.
+ Hedging Evaluation: Calculates hedge ratios, hedged returns, and hedging effectiveness metrics for performance analysis.

4. File
+ Thesis report pdf file
+ R code for methodology implementation
+ Slide presentation 
