# BeSDTI
Behavioural and Social Drivers of Vaccination - Tools for Impact

The BeSD-TI R package is an open-source set of R programs meant to analyse survey data on the [behavioural and social drivers of vaccination (BeSD)](https://www.who.int/publications/i/item/who-wer9720-209-224>). BeSD-TI is designed to summarise survey questions specified in the WHO publication [Behavioural and social drivers of vaccination: Tools and practical guidance for achieving high uptake](https://iris.who.int/handle/10665/354459). 

To install this package, ensure you (a) have R version 4.2.1 or later and (b) have installed [RTools](https://cran.r-project.org/bin/windows/Rtools/). Then run the following commands: 

``` r
if (!requireNamespace("pak")){install.packages("pak")}

pak::pkg_install("BiostatGlobalConsulting/BeSDTI")
```
