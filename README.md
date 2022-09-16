# Combining ground and remote sensing data to estimate forest canopy damage and recovery from tropical cyclones over a pantropical soil phosphorus gradient

This project is a part of the
[NGEE-tropics](https://ngee-tropics.lbl.gov) initiative. This specific
research was conducted through Lawrence Berkeley National Laboratory and
University of California, Berkeley and administered by the principle
investigator, [Dr. Lara
Kueppers](https://eesa.lbl.gov/profiles/lara-kueppers/). [Dellena
Bloom]() is the main author of this repository, [data
produced](http://dx.doi.org/10.15486/ngt/1847332), and [manuscript]()
detailing results of this project, with coauthors including [Dr. Barbara
Bomfim](https://eesa.lbl.gov/profiles/barbara-bomfim/), and [Yanlei
Feng](https://eesa.lbl.gov/profiles/yanlei-feng/).

#### – Status: Completed

## Objective

This project is under the
[RFA2](https://ngee-tropics.lbl.gov/research/research-focus-area-2/)
section of NGEE-tropics, tasked to study variations in forest
composition and structure with varying resource availability and
disturbance. Here we studied the correlation between tropical forest
damage due to cyclone events and soil phosphorus concentrations
utilizing vegetative indices attained from [Google Earth
Engine](https://earthengine.google.com).

### Acknowledgements

-   This work was supported in part by the U.S. Department of Energy,
    Office of Science, Office of Workforce Development for Teachers and
    Scientists (WDTS) under the Science Undergraduate Laboratory
    Internship (SULI) program.

### Methods Used

-   Correlations
-   Data Visualization
-   Regressions
-   Mixed Effects Models

### Technologies

-   R
-   Google Earth Engine

## Project Overview

Cyclones alter tropical forest function, composition and structure,
making effects of intensifying cyclones on carbon-rich forests a
critical topic of study. Here, we quantified cyclone-induced damage and
recovery of 21 cyclone disturbances affecting 9 sites between 2004-2017
through pantropical forests utilizing selected leaf area index (LAI) and
enhanced vegetation index (EVI). Although LAI and EVI were chosen to
report, seven vegetative indices total were collected including
normalized difference vegetation index (NDVI) at three different
resolutions, EVI at two different resolutions, LAI, and transformed NDVI
(kNDVI). Field observations collected in a meta-analysis were used to
ground-truth and test effects of soil resource availability and
disturbance factors on damage and recovery. We found large variations of
cyclone-induced deltaLAI and deltaEVI across the tropics. The highest
reduction in LAI, -76.6%, and EVI, -76.7%, occurred in Mexico and Puerto
Rico, respectively. Pantropical deltaLAI (r = -0.52) and deltaEVI (r =
-0.60) correlated with delta litterfall. Post-cyclone data showed
recovery of LAI by 4 months, EVI by 2 months, and litterfall by 10
months post-cyclone. deltaLAI was correlated with soil phosphorus (r =
-0.47, P-value = 0.031). PCA demonstrates grouping of regions among
variables. Results show higher cyclone-induced damage in areas with high
soil phosphorus, corroborating previous studies, and in areas with
higher litterfall. This study demonstrates the potential for
extrapolation of relationships established using ground-based
observations across a wider array of cyclone prone tropical forests and
for observing effects of tropical cyclone regime alterations under
climate change.

## Getting Started

1.  Clone this repo (for help see this
    [tutorial](https://help.github.com/articles/cloning-a-repository/)).
2.  Published raw Data is being kept
    [here](http://dx.doi.org/10.15486/ngt/1847332).
3.  GEE documented data extraction code is kept
    [here](https://github.com/DelliB/NGEEt_cyclone_VI_study/blob/main/doc/Bloom_GEE_code_documentation.pdf).
    -   Bloom_GEE_code_doc.pdf
4.  Data visualization code is kept
    [here](https://github.com/DelliB/NGEEt_cyclone_VI_study/tree/main/plots).
    -   Code to produce all plots found in
        Bloom_NGEEtropics_paper_plots.R
5.  Code used to run statistical analyses is kept
    [here](https://github.com/DelliB/NGEEt_cyclone_VI_study/tree/main/stats).
    -   Bloom_NGEEtropics_paper_stats.R

## Featured Deliverables

-   [Data product](http://dx.doi.org/10.15486/ngt/1847332)
    -   Dellena Bloom, Barbara Bomfim, Lara Kueppers, Yanlei Feng(2022).
        LAI, EVI, NDVI, and kNDVI in 23 pantropical forests affected by
        21 cyclones. NGEE Tropics Data Collection. Accessed at
        <http://dx.doi.org/10.15486/ngt/1847332>.
-   [Manuscript](link)
    -   Citation

## Collaborators

**Project lead: [Dellena
Bloom](https://github.com/DelliB)(<develyn.bloom@ufl.edu>)**

#### Other Members:

| Name                                           | GitHub Handle |
|------------------------------------------------|---------------|
| [Barbara Bomfim](https://github.com/bdbomfim)  | @bdbomfim     |
| [Yanlei Feng]()                                | @             |
| [Lara Kueppers](https://github.com/lmkueppers) | @lmkueppers   |

## Contact

-   Dellena Bloom (<develyn.bloom@ufl.edu>)
