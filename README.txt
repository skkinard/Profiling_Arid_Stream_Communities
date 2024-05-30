# Arid-Stream-Community-Characteristics 

#### Research Summary:
Anthropogenic climate change is expected to increase the aridity of many regions of the world. Surface water ecosystems are particularly vulnerable to changes in the water-cycle and may suffer adverse impacts in affected regions. To enhance our understanding of how freshwater communities will respond to predicted shifts in water-cycle dynamics, we employed a space for time approach along a natural precipitation gradient on the Texas Coastal Prairie. In the Spring of 2017, we conducted surveys of 10 USGS-gauged, wadeable streams spanning a semi-arid to sub-humid rainfall gradient; we measured nutrients, water chemistry, habitat characteristics, benthic macroinvertebrates, and fish communities. Fish diversity correlated positively with precipitation and was negatively correlated with conductivity. Macroinvertebrate diversity peaked within the middle of the gradient. Semi-arid fish and invertebrate communities were dominated by euryhaline and live-bearing taxa. Sub-humid communities contained environmentally sensitive trichopterans and ephemeropterans as well as a variety of predatory fish which may impose top-down controls on primary consumers. These results warn that aridification coincides with the loss of competitive and environmentally sensitive taxa which could yield less desirable community states.

#### Data Preparation
- Loading and merging datasets using R
- Quality control and Integrity checks: string detection/correction
- Preprocessing environmental data: Sourcing USGS gauge and climate data
- Preprocessing community data: QaQc, identifyng missing data
- Outlier removal (using standard deviation thresholds)

#### Statistical Analyses
- Population Estimates: Regression
- Hypothesis Testing: Linear regressions (lm)
- Exploratory Data Analysis: Multivariate regression analysis (MRA)
- Dimension Reduction: Principal Component Analysis (PCA)
- Ordination: Redundancy Analysis (RDA)

#### User Guide:
- data_files contain data and outputs (see data_files/data_summary.txt)
- r_scripts contain calculations (see r_scripts/readme.txt)
- r_source_files contain helper functions commonly used throughout calculation scripts

#### Author Note: 
This is my first major application of R analyses. So scripts are roughly coded (2018-2020) and highly specific to the data provided. The formal results and additional details can be found in the published manuscript at PeerJ (https://peerj.com/articles/12137/).

