---
title: "README"
author: "Robin J. Evans"
date: "10/06/2021"
output: html_document
---

## dependence

This is a package of functions that replicates the results of the 2021 paper 
'Dependency in DAG models with Hidden Variables' by Robin Evans.

To install the package, you will need the `rje` package, as well as the 
`MixedGraphs` and `ADMGs2` packages.  Use the following commands to obtain 
these:
```
install.packages("rje")
install.packages("devtools")
devtools::install_github("rje42/MixedGraphs")
devtools::install_github("rje42/ADMGs2")
```
Then finally run:
```
devtools::install_github("rje42/dependence")
```
