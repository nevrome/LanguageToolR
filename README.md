[![Travis-CI Build Status](https://travis-ci.org/ISAAKiel/LanguageToolR.svg?branch=master)](https://travis-ci.org/ISAAKiel/LanguageToolR) [![Coverage Status](https://img.shields.io/codecov/c/github/ISAAKiel/LanguageToolR/master.svg)](https://codecov.io/github/ISAAKiel/LanguageToolR?branch=master)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/LanguageToolR)](https://cran.r-project.org/package=LanguageToolR)
[![license](https://img.shields.io/badge/license-GPL%203-B50B82.svg)](https://www.r-project.org/Licenses/GPL-2)

# LanguageToolR

`LanguageToolR` provides a wrapper for the [LanguageTool CLI tool](http://wiki.languagetool.org/command-line-options) for spelling, grammar and language checking. The wrapper provides all current options of the command line tool.

:heavy_exclamation_mark: I'm not part of the LanguageTool team. This is an unofficial interface.

:heavy_exclamation_mark: The package currently only works on Unix-like systems. 


## Installation

1. Install languagetool for your system. You can do this with the following setup function or directly from package sources for your OS or manually following the instructions here: https://github.com/languagetool-org/languagetool

```
LanguageToolR::quick_setup()
```

2. Install this package via devtools

```
devtools::install_github("nevrome/LanguageToolR")
```

## Usecase

```
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
