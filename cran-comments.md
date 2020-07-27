## Test environments
* local ubuntu 18.04, R 3.6.3
* ubuntu 16.04 (on travis-ci), R 4.0.0
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:

* checking examples ...
** running examples for arch 'i386' ... [19s] NOTE
Examples with CPU (user + system) or elapsed time > 10s
             user system elapsed
languagetool 4.41   2.25   18.39
** running examples for arch 'x64' ... [8s] OK

The LanguageTool CLI unfortunately takes rather long for its checks.
