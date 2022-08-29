## Test environments

  * local OS X install: 
    * R version 4.2.1 (2022-06-23)
    * Platform: x86_64-apple-darwin17.0 (64-bit)
    * Running under: macOS Monterey 12.3
  
  * win-builder via devtools
    * R version R Under development (unstable) (2022-07-06 r82554 ucrt)
    * Platform: x86_64-w64-mingw32 (64-bit)
    
## R CMD check results

OS X test environment: No ERRORs, WARNINGs, or NOTEs.

win-builder via devtools: No ERRORs or WARNINGs. One NOTE relating to a possible misspelling of "ggformula" in the DESCRIPTION field of the DESCRIPTION file. This is a false-positive: "ggformula" is a genuine package available on CRAN.

## Downstream dependencies

fastR2, MMAC, and RISCA are the reverse dependencies on CRAN. They have been checked.

## Other notes

This is a substantial update and extension of a package currently on CRAN. 
