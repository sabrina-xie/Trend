
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Long-term trend residuals analysis

<!-- badges: start -->
<!-- badges: end -->

The long-term lake monitoring data were collected through New York Dept
of Environmental Conservation’s Lake Classification and Inventory (LCI)
and Citizen Statewide Lake Assessment Program (CSLAP) between 1986 and
2020. Samples were collected mid-May to September. At each sampling
event, water was collected from the epilimnion in open water. The
following parameters were consistently collected among the lakes over 4
decades: acidity (H+ ion), Chl a, Secchi depth, phosphorus, nitrogen
(nitrate-nitrite), specific conductance, temperature, and true color.
For a lake to be included in the long-term analysis, the lake needed to
have at least two sampling events in the 1980s and two sampling events
in the 2010s. The dataset used in this analysis included 54202 records
from 43 lakes.

7 parameters from the dataset were tested against the linear model
residuals using a linear regression to determine if there was a
significant relationship between the characteristic gradient and the
residuals (Quinlan et al., 2020).

## Characteristic variables

The parameters tested were: clarity (as Secchi depth), acid deposition,
sample date, lake temperature, CDOC (as color), N:P ratio and
precipitation. Color as CDOC data was segmented into high color (above
20 color units) and low color (below 20 color units) lakes, as color
becomes a poor surrogate for CDOC at high levels (Wallage & Holden,
2010). N:P ratio and color in high color lakes were log-transformed for
normality. Acid nitrogen deposition data was aggregated from National
Atmospheric Deposition Program monitoring sites by averaging raster data
over each lake’s watershed.

## Required packages

-   tidyverse: <https://www.tidyverse.org>
-   mgcv: <https://cran.r-project.org/web/packages/mgcv/index.html>
-   grid: <https://www.rdocumentation.org/packages/grid/versions/3.6.2>
-   MASS: <https://cran.r-project.org/web/packages/MASS/index.html>
-   readxl: <https://cran.r-project.org/web/packages/readxl/readxl.pdf>
-   huxtable: <https://hughjonesd.github.io/huxtable/>
-   maps: <https://cran.r-project.org/web/packages/maps/index.html>
-   ggmap: <https://cran.r-project.org/web/packages/ggmap/index.html>
