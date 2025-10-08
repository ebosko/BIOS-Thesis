# Master's Thesis in Biostatistics

**PI: Edward Chan, MD; National Jewish Health, Denver, CO**

**Analyst: Edward Bosko; University of Colorado, Anschutz Medical Campus, Aurora, CO**

This is the repository for my Thesis project, involving the study of CT images from a cohort of 166 subjects with Non-Tuberculous Mycobacteria Lung Disease (NTM-LD). Our research questions of interest were as follows:

1. Are there differences in the frequencies and severities for each of the specific CT features among the lung lobes in NTM-LD?
2. Are there differences in (1) between MAC-LD and M. abscessus-LD?

To account for the complex correlation structure from repeated ratings across multiple lobes and two raters per subject, we fit mixed-effects regression models, logistic for binary outcomes and ordinal logistic for ordinal outcomes, with random intercepts for subject and a nested random effect for rater.

Key findings of our research suggest that atelectasis and bronchiectasis were most severe in the RML and LLS; consolidation in the RML and RUL; ground-glass opacities and nodules in the RLL, RUL, and LLL; thick wall cavities in the RUL and RLL; tree-in-bud opacities in the RLL and LLL; while thin wall cavities had no significant differences in lobar severity. These results confirm preferential involvement of lung regions with NTM-LD which can focus surveillance on the most affected regions and inform treatment recommendations.

Key Skills Demonstrated:
*Statistical Modeling (GLMM, Ordinal Logistic Regression)
*R Programming (tidyverse, ggplot2, Shiny)
*SAS Programming (PROC GLIMMIX)
*Data Cleaning
*Reproducible Research

Directory structure:

File | Description
------|----------------------
Background | contains background information for the analysis (e.g., references)
Code | contains all R and SAS scripts for this project
DataRaw | contains all raw data provided by investigators
DataProcessed | contains the processed data used for the analyses
Reports | contains all output, markdown files, and final written report
