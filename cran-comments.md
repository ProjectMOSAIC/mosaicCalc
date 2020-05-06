## Test environments

  * local OS X install: 
    * R version 3.6.3 (2020-02-29)
    * Platform: x86_64-apple-darwin15.6.0 (64-bit)
    * Running under: macOS Catalina 10.15.3
  
  * win-builder via devtools
    * R version 4.0.0 (2020-04-24)
    * Platform: x86_64-w64-mingw32 (64-bit)
    
## R CMD check results

This is a re-submission of a package that was archived on CRAN for falling afoul of new documentation standards. These are fixed.

There were no ERRORs or WARNINGs or NOTES on OS X R CMD check.

One NOTE in win-builder R CMD check:

Possibly mis-spelled words in DESCRIPTION:
  Antidifferentiation (4:9)
  antidifferentiation (8:5)

I believe these are properly spelled.


## Downstream dependencies

fastR2 is the only reverse dependency on CRAN. This has been checked.


## Other notes

The only changes were light changes to documentation.
