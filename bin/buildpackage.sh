#! /bin/bash
export _R_CHECK_FORCE_SUGGESTS_=false
bin/roxy -p "." --clean
mv *_*.tar.gz builds/
cp vignettes/Resampling/Resampling.pdf inst/doc/Resampling-src.pdf
cp vignettes/Calculus/mosaic-calculus.pdf inst/doc/Calculus-src.pdf
cp vignettes/MinimalR/MinimalR.pdf inst/doc/MinimalR-src.pdf
R CMD build --resave-data .
