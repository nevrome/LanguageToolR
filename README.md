<!-- badges: start -->
[![Project Status: Inactive – The project has reached a stable, usable state but is no longer being actively developed; support/maintenance will be provided as time allows.](https://www.repostatus.org/badges/latest/inactive.svg)](https://www.repostatus.org/#inactive)
[![R-CMD-check](https://github.com/nevrome/LanguageToolR/actions/workflows/check.yml/badge.svg)](https://github.com/nevrome/LanguageToolR/actions/workflows/check.yml)
[![Coverage Status](https://codecov.io/gh/nevrome/LanguageToolR/branch/master/graph/badge.svg)](https://codecov.io/github/nevrome/LanguageToolR?branch=master)
[![license](https://img.shields.io/badge/license-GPL%203-B50B82.svg)](https://www.r-project.org/Licenses/GPL-2)
<!-- badges: end -->

# LanguageToolR

`LanguageToolR` provides a wrapper for the [LanguageTool CLI tool](http://wiki.languagetool.org/command-line-options) for spelling, grammar and language checking.

:heavy_exclamation_mark: We're not part of the LanguageTool team. This is an unofficial interface.

We only tested with LanguageTool **v6.5**, but it will work as well with other versions, if more obscure command line options are avoided.

## Installation


1. Install this package via **remotes**

```r
if (!require(remotes)) install.packages("remotes")
remotes::install_github("nevrome/LanguageToolR")
```

2. Install languagetool for your system. You can do this with the following setup function or directly from package sources for your OS or manually following the instructions here: https://github.com/languagetool-org/languagetool

```r
LanguageToolR::lato_quick_setup()
```

This will install the currently supported version (see above).

## Usecase

```r
testtext <- c(
  "LanguageTool offers spell and grammar checking.", 
  "Just paste your text here and click the 'Check Text' button.", 
  "Click the colored phrases for details on potential errors.", 
  "or use this text too see an few of of the problems that LanguageTool can detecd.", 
  "What do you thinks of grammar checkers? Please not that they are not perfect.", 
  "Style issues get a blue marker: It's 5 P.M. in the afternoon.", 
  "The weather was nice on Thursday, 27 June 2017."
)

LanguageToolR::languagetool(testtext)
```
