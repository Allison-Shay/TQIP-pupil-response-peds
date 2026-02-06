# Pupillary response

## 1. Setup

This was performed using **R version 4.1.3** or **R version 4.5.2 (2025-10-31)** and the following R packages:

<pre>required_packages <- c(
  "tidyverse",
  "dplyr",
  "stringr",
  "readr",
  "MatchIt",
  "cobalt",
  "sandwich",
  "lmtest",
  "estimatr",
  "cowplot"
)</pre>

Install any missing packages by running:

<pre>install.packages(
  setdiff(required_packages, rownames(installed.packages()))
)</pre>

## 2. Clean and organize data

Make sure your data files are in a directory named **TQIP 2007-2023**; for example, the relevant CSV files for year 2023 should be located at **./TQIP 2007-2023/PUF AY 2023/CSV/**.

We'll process years 2018-2023 using the following:

<pre>Rscript PUF_process.R 2023 ICD
Rscript PUF_process.R 2022
Rscript PUF_process.R 2021
Rscript PUF_process.R 2020
Rscript PUF_process.R 2019
Rscript PUF_process.R 2018
</pre>

The output will be stored in a directory named **final_data**

*(note: optionally supplying the ICD argument means to output the ICD code descriptions into the output directory as final_data/ICD_codes.csv)*


## 3. Merge the data

<pre>Rscript PUF_merge.R</pre>

The output will be stored in a directory named **final_data_merged**

## 4. Analyze

<pre>Rscript analysis.R</pre>

The output will be stored in a directory named **analysis**

# 5. Make forest plots

<pre>Rscript forestplot.R</pre>

# Acknowledgments

The initial codebase was developed by Allison Shay and Delaney Sullivan here: https://github.com/Allison-Shay/TBI-outcomes

This repository builds off that codebase.
