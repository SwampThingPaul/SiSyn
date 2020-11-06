WRTDS
================

### Weighted Regressions on Time, Discharge and Season (WRTDS)

  - WRTDS employs the use of weighted regressions of concentrations on
    time, discharge, and season.

  - The goal of this approach is to increase the amount of information
    that is extracted from the types of rich water-quality datasets that
    now exist.

**Relevant literature:**

Hirsch RM, Moyer DL, Archfield SA (2010) Weighted Regressions on Time,
Discharge, and Season (WRTDS), with an Application to Chesapeake Bay
River Inputs. JAWRA Journal of the American Water Resources Association
46:857–880.
[DOI: 10.1111/j.1752-1688.2010.00482.x](https://doi.org/10.1111/j.1752-1688.2010.00482.x)
[Link](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3307614/)

Murphy J, Sprague L (2019) Water-quality trends in US rivers: Exploring
effects from streamflow trends and changes in watershed management.
Science of The Total Environment 656:645–658.
[DOI: 10.1016/j.scitotenv.2018.11.255](https://doi.org/10.1016/j.scitotenv.2018.11.255)
[Link](https://www.sciencedirect.com/science/article/pii/S0048969718346126?via%3Dihub)

### R-Code

Example workflow for WRTDS modeling using HJ Andrews LTER data prepared
by [Keira Johnson](mailto:johnkeir@oregonstate.edu).

In this folder there are three scripts used to format data, run a WRTDS
model using `library(EGRET)` and exploring outputs.

1.  **HJA\_Si\_WRTDS\_prep.R** - Preps data for input to WRTDS

2.  **HJA\_WRTDS\_Si.R** - Runs the WRTDS analysis

3.  **HJA\_Si\_Exploratory.R** - Explores WRTDS outputs

-----
