## Test environments

  * local OS X install: 
    * R version 4.1.3 (2022-03-10)
    * Platform: x86_64, darwin17.0
    * Running under: macOS Monterey 12.3
  
  * win-builder via devtools
    * R version R Under development (unstable) (2022-07-06 r82554 ucrt)
    * Platform: x86_64-w64-mingw32 (64-bit)
    
## R CMD check results

OS X test environment: There were no ERRORs, WARNINGs, or NOTEs.

win-builder via devtools: No ERRORs or WARNINGs. One NOTE relating to a claimed mis-spelling "ggformula" in the DESCRIPTION field of the DESCRIPTION file. I believe this is a false-positive: "ggformula" is a genuine package available on CRAN.

## Downstream dependencies

fastR2, MMAC, and RISCA are the reverse dependencies on CRAN. They has been checked.

## Other notes

This is a substantial update and extension of a package currently on CRAN. 
