`hte` provides functions for estimating heterogenous treatment effects. 

## How to install

The latest development version can be installed directly from Github using [devtools](https://github.com/hadley/devtools):

```R
if (!require("devtools")) install.packages("devtools")
devtools::install_github("soerenkuenzel/hte")
```

The package contains compiled code, and you must have a development environment to install the development version. (Use `devtools::has_devel()` to check whether you do.) If no development environment exists, Windows users download and install [Rtools](https://cran.r-project.org/bin/windows/Rtools/) and macOS users download and install [Xcode](https://itunes.apple.com/us/app/xcode/id497799835).


## Example (this will not execute because there is no data)

```R
library(hte)
packageVersion("hte")

#This is the autotune version. Faster version is to simply use X_RF()
#create the hte object

xl <- X_RF_autotune_simple(feat = X.train, tr = Tr.train, yobs = Y.train, nthread = ?)

#estimate the CATE
xl.cate <- EstimateCate(xl, X.test)

#create confidence intervals via bootstrapping

xl_ci <- CateCI(xl, X, B=500)
```
