Casco Bay Physics Model
================

Convenient access to [FVCOM](http://fvcom.smast.umassd.edu/fvcom/) Casco
Bay physics model from R.

### Requirements

-   [R 4+](https://www.r-project.org/)
-   [ncdf4](https://cran.r-project.org/package=ncfd4)
-   [rlang](https://cran.r-project.org/package=rlang)
-   [dplyr](https://cran.r-project.org/package=dplyr)
-   [sf](https://cran.r-project.org/package=sf)
-   [R6](https://cran.r-project.org/package=R6)
-   [fvcom](https://github.com/BigelowLab/fvcom) **note this is not from
    CRAN**
-   [locate](https://github.com/BigelowLab/locate) **note this is not
    from CRAN**

### Installation

    devtools::install_github("BigelowLab/locate")
    devtools::install_github("BigelowLab/fvcom")
    devtools::install_github("BigelowLab/cascophys")

## Data Access

``` r
library(cascophys)
CB <- CascoBayPhysics() # optional arguments, too.  See \code{?CascoBayPhysics}
```
