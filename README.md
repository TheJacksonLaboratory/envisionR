# envisionR

`envisionR` is an R package designed to facilitate the analysis of experimental data exported from the JAX Envision™ software. This package provides tools for data wrangling, quality control, visualization, and statistical analysis of high-throughput phenomics data.

## Installation

<!-- This commented set of lines will change when envisionR is available on CRAN or Bioconductor.
You can install the `envisionR` package from CRAN using:

```R
install.packages("envisionR")
```
-->

You can install the development version from GitHub:

```R
# install.packages("devtools")
devtools::install_github("TheJacksonLaboratory/envisionR")
```

## Usage

### Loading the Package

```R
library(envisionR)
```

### Generating JAX Envision URLs

Create URLs to visualize specific moments of video flagged in an analysis:

```R
safety_vidstart <- as.POSIXct("2023-06-12 09:00:00", tz = "US/Central")
envisionR::make_envision_url(org = 4, study = 84, cage = 442, vidstart = safety_vidstart)
```

### Data Wrangling and Quality Control

Import and clean your data using `envisionR` functions:

```R
# Importing libraries
library(tidyverse)
library(janitor)
library(ggplot2)
library(here)
library(envisionR)

# Importing annotation data
metadata <- envisionR::envision_metadata(study_name = "Two Drug Study", tzone = "US/Central", lights_on = "06:00:00", lights_off = "18:00:00", study_url = "https://app.murine.net/org/4/study/84/")
annotation <- envisionR::read_annotation_csv("../data/annotation.csv", metadata = metadata)
```

### Initial Data Visualization

Visualize your data with built-in plotting functions:

```R
# Spaghetti plot
spaghetti_plot(activity_data = activity, metadata = metadata, yvar = "movement_mean_per_cage_cm_s_hour", occupancy_norm = TRUE) + ggokabeito::scale_color_okabe_ito(order = okabe_order)
```

### Statistical Analysis

Perform statistical tests and visualize results:

```R
# ANOVA
activity_4hr_post_dose_aov <- aov(baseline_subtract_postdose_0to4hr ~ group_name, data = activity_1min_summarize)
summary(activity_4hr_post_dose_aov)

# Dunnett's Test
library(DescTools)
DunnettTest(x = activity_1min_summarize %>% dplyr::pull(baseline_subtract_postdose_0to4hr), g = activity_1min_summarize %>% dplyr::pull(group_name), control = "Vehicle (0 mg/kg)")
```

## Vignettes

Detailed walkthroughs and examples are available in the package vignettes. To view the vignettes, use:

```R
browseVignettes("envisionR")
```

## Contributing

We welcome contributions to the `envisionR` package. Please fork the repository and submit pull requests for any enhancements or bug fixes.

## License

This package is licensed under the MIT License.

## Acknowledgements

This package was developed by Michael C. Saul, Computational Scientist at The Jackson Laboratory.
