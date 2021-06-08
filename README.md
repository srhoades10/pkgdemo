# pkgdemo

## Overview

A basic package to add numbers, create tables and PCA plots

## Installation

``` r
# Or the development version from GitHub:
# install.packages("devtools")
devtools::install_github("srhoades10/pkgdemo")
```

## Usage

``` r
library(pkgdemo)

dat <- createRandomTbl(nRow=30,nCol=1000)
pcaTbl <- plotPCA(data=dat, confidence=95, ggTitle="PCA Scores")
plot(pcaTbl)

```
