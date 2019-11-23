# RstoxFDA
Fisheries Dependent Analysis with Rstox.

Install from github with:
devtools::install_github("https://github.com/StoXProject/RstoxFDA").

## Reca
Contains functions for adapting data to Reca, running estimates, and plotting or tabulating results. These functions are formulated to be adaptable to other data formats than just the ones supported by Rstox.

Reca is availabel at: https://github.com/NorskRegnesentral/Reca .
For Windows and Linux, Reca can be installed via devtools::install_github("https://github.com/NorskRegnesentral/Reca").
For Mac, one might consider the fork at: https://github.com/iambaim/new-reca.

## StoX
Contains functions that adheres to StoX-function contracts so that they can be included in StoX-processes. This includes functions for a StoX-Reca template.