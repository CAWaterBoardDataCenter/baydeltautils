# baydeltautils

The baydeltautils package provides utilities for analysis of hydrological and 
biological data to support water management planning in California's San 
Francisco Bay/Sacramento-San Joaquin Delta Estuary.

## Installation

You can install the latest version from [GitHub](https://github.com/).

First, you must [generate a Personal Access Token (classic)](https://github.com/settings/tokens/new?scopes=repo) 
from your account and grant it repo permissions (the pre-checked options if you click the earlier link). This is equivalent to a password so treat it accordingly. 

Then, you can install `baydeltautils` using the code below, replacing `PAT` with your quoted Personal Access Token.

``` r
# install.packages("remotes")
remotes::install_github("CAWaterBoardDataCenter/baydeltautils",
                        auth_token = PAT)
```
