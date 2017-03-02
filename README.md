# CausalRF

## Package Development in R
### Setup
This package is current in development, which means you may need to install
some development toolkits to test and improve the library. Here is a quick
tutorial on the process.

You might need to first install the following libraries:
```
install.packages(c("devtools", "roxygen2", "testthat", "knitr"))
```

To have the best development experience, you should install/update to the latest
version of [RStudio](http://www.rstudio.com/products/rstudio/download/preview/).

### R Code Workflow
To save all the editing files in the package and rebuild the package, you can
use the command
```
devtools::load_all()
```
or in RStudio you can use the shortcut `Ctrl/Cmd + Shift + L`. Once the package
is built, all the code under the folder `R/` is executed and made available
in the console.

### Documentation Workflow
Documentations are not created manually but instead in R codes. To convert all
roxygen comments in files under the folder `R/`, you can use the command
```
devtools::document()
```
or in RStudio you can use the shortcut `Ctrl/Cmd + Shift + D`. Once the
documentation is complete, you can preview documentation with `?`.

### Testing Workflow (Current not available)
First we need to set up `testthat`. You can use the command
```
devtools::use_testthat()
```
which creates a `tests/testthat` directory. To test all
testcases in files under the folder `testthat/`, you can use the command
```
devtools::test()
```
or in RStudio you can use the shortcut `Ctrl/Cmd + Shift + T`.

### Vignette Workflow (Current not available)
Vignette is used for the overall tutorial of the package. It can be created
using the following command
```
devtools::use_vignette("tutorial")
```
Modifying the file inside under the folder `vignette` and in RStudio you can
press `Ctrl/Cmd + Shift + K` to knit the vignette and preview the output.

### C++ Workflow
To integrate with C++, we need to set up Rcpp in the package. The command
```
devtools::use_rcpp()
```
will create a `src/` directory to hold all the .cpp files. 

To generate the corresponding R code, use the following command
```
Rcpp::compileAttributes()
```

Once your code is ready to test, you can click `Build & Reload` in the build 
pane, or press `Ctrl/Cmd + Shift + B`. You can also use the standard 
`devtools::load_all()` process but it is more risky.
