
# adlgraphs <a href="https://jdenn0514.github.io/adlgraphs/"><img src="man/figures/logo.png" align="right" height="150" alt="adlgraphs website" /></a>

## Overview

adlgraphs was initially built to ensure that the data visualizations
produced by the Center for Antisemitism Research (CAR) at the
Anti-Defamation League (ADL) are consistent with ADL’s brand guidelines
and that CAR’s data visualizations are consistent and cohesive. (Hence
the use of “graphs” in the name.)

Over time, the focus has shifted toward improving the efficiency and
ease of cleaning, analyzing, and visualizing survey data, so that the
turnaround time between data collection and the presentation of analysis
and findings could be as minimized as much as possible.

## Installation

You can install the development version of adlgraphs from
[GitHub](https://github.com/JDenn0514/adlgraphs) with:

``` r
# install.packages("pak")
pak::pak("JDenn0514/adlgraphs")
```

### Note

This package is regularly updated. Sometimes these updates include
adding new functions, other times it involves updating existing
functions (fixing bugs, changing how they operate slightly, adding
additional functionality, speeding it up, etc.). While I try to maintain
backward compatibility, that is not always possible. If you are suddenly
getting an error message when re-running old code, it is possible that
the function has been updated so check the documentation to make sure
that you are still using it properly. If you are still experiencing
issues, please submit an issue on the
[Github](https://github.com/JDenn0514/adlgraphs/issues)
