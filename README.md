# zzworld

WORLD Test Scoring and Edit Distance Analysis

## Overview

`zzworld` provides functions for analyzing WORLD test responses in cognitive
assessments. The WORLD test asks patients to spell "WORLD" backwards as part
of the Mini-Mental State Examination (MMSE).

## Features

- Edit distance calculation between responses and target "WORLD"
- Multiple scoring method comparisons
- MMSE calculation integration
- Report generation for edit distance analysis

## Installation

```r
devtools::install_github("rgt47/zzworld")
```

## Functions

- `edit.dist()` - Calculate edit distance for a response string
- `edit_distance_valiente()` - Alternative edit distance implementation
- `compare_scoring_methods()` - Compare different scoring approaches
- `mmse_calc()` - MMSE score calculations
- `gen_edit_distance_report()` - Generate analysis reports

## Usage

```r
library(zzworld)

# Calculate edit distance for a response
edit.dist("DLROW")  # Returns 0 (correct backwards spelling)
edit.dist("DLROW")  # Returns distance from target
```

## License

GPL-3

## Author

Ronald (Ryy) G. Thomas (rgthomas@ucsd.edu)
