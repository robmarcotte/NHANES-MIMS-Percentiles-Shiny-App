# NHANES Activity Monitor Percentiles Calculator

An interactive Shiny application designed to calculate and visualize population-referenced percentiles for physical activity output, specifically physical activity volume (i.e., total activity). The tool utilizes data from the National Health and Nutrition Examination Survey (NHANES) to map individual activity levels against demographic peers.

## Features

* **Individual Calculator:** Input age, sex, and activity metrics to instantly see your percentile placement mapped onto population curves.
* **Batch CSV Processing:** Upload a bulk summary dataset (`.csv`) to compute percentiles for hundreds of participants simultaneously.
* **Dual-Metric Support:** Accommodates both Waist-Worn Vertical Axis Counts (TAC) and Non-Dominant Wrist-Worn Triaxial Monitor Independent Movement Summary (MIMS) Units.
  * More on how to get your data into MIMS units can be found at https://github.com/mHealthGroup/MIMSunit and in the John et al. 2019 in the Journal for the Measurement of Physical Behaviour (https://doi.org/10.1123/jmpb.2018-0068)

---

## Getting Started

### Prerequisites
To run this application locally, you will need [RStudio](https://posit.co/download/rstudio-desktop/) and the following R packages installed:

## Required Libraries

```r
install.packages(c("shiny", "devtools", "dplyr", "tidyr", "ggplot2", "stringr", "shinyWidgets", "ggthemes", "english"))
```
You also need the core calculation engine for the percentile curves developed by @brightantwiboasiako:

```r
devtools::install_github('brightantwiboasiako/wwadapercentiles')
```

### Running the App
Clone or download this repository.

Open app.R in RStudio.

Click the Run App button at the top right of the source pane, or run the following command in your R console: 

```r
shiny::runApp()
```

## Using the Batch Process Functionality

You will need to upload a (`.csv`) file with the following headers:
* id
* age (6-85 y inclusive)
* sex ('M' or 'F')
* metric ('Counts' or 'MIMS')
* value of either counts or MIMS
  
Please note that Counts should be Vertical Axis counts from a Waist-Worn device and MIMS should be triaxial values from a Non-Dominant Wrist-Worn Device

### Try it out:

* You can download a pre-formatted [nhanes_batch_test_template.csv](nhanes_batch_test_template.csv) to test the Batch Processing tab immediately.

* Once uploaded, the app will generate a 'calculated_percentile' column with the percentile for each row. This can be downloaded to a .csv with the date appended to the original file name.

* This will allow you to generate NHANES percentiles for your study to compare total activity volume to a U.S. representative sample.
    * This may be useful for cross-sectional comparisons or for individual- or group-level change with interventions or clinical trials.

## References
1. Wolff-Hughes DL, Fitzhugh EC, Bassett DR, Churilla JR. Waist-Worn Actigraphy: Population-Referenced Percentiles for Total Activity Counts in U.S. Adults. J Phys Act Health. 2015 Apr;12(4):447-53. doi: 10.1123/jpah.2013-0464.

2. Wolff-Hughes DL, Bassett DR, Fitzhugh EC. Population-referenced percentiles for waist-worn accelerometer-derived total activity counts in U.S. youth: 2003 - 2006 NHANES. PLoS One. 2014 Dec 22;9(12):e115915. doi: 10.1371/journal.pone.0115915.

3. Belcher BR, Wolff-Hughes DL, Dooley EE, Staudenmayer J, Berrigan D, Eberhardt MS, Troiano RP. U.S. Population-referenced Percentiles for Wrist-Worn Accelerometer-derived Activity. Med Sci Sports Exerc. 2021 Jun 11. doi: 10.1249/MSS.0000000000002726.
