# mcapomorphy
[![Build Status](https://travis-ci.com/dongzhuoer/mcapomorphy.svg?token=CyRgUWsqWCctKvAxMXto&branch=master)](https://travis-ci.com/dongzhuoer/mcapomorphy)
[![codecov](https://codecov.io/gh/dongzhuoer/mcapomorphy/branch/master/graph/badge.svg?token=Asvq6pUwFH)](https://codecov.io/gh/dongzhuoer/mcapomorphy)

## Overview

**M**olecular **C**lassfication based on amino acid **apomorphy**


## Installation

```r
if (!('remotes' %in% .packages(T))) install.packages('remotes');
remotes::install_github('dongzhuoer/mcapomorphy');
```

## Usage

refer to `vignette('mcapomorphy')`.

## to do

unitest `*_to_fasta`

- [ ] find a OG containing 'U' to test `mcapomorphy::fasta_to_nexus()`


## extdata

`EOG090F05Z3.fasta` aligned by mafft + muscle
`omics.tre` copied from aves