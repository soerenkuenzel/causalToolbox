`hte` provides functions for estimating heterogenous treatment effects. 

## How to install

The latest development version can be installed directly from Github using [devtools](https://github.com/hadley/devtools):

```R
if (!require("devtools")) install.packages("devtools")
devtools::install_github("soerenkuenzel/hte")
```

The package contains compiled code, and you must have a development environment to install the development version. (Use `devtools::has_devel()` to check whether you do.) If no development environment exists, Windows users download and install [Rtools](https://cran.r-project.org/bin/windows/Rtools/) and macOS users download and install [Xcode](https://itunes.apple.com/us/app/xcode/id497799835).


## Example 

For details please read our paper: https://arxiv.org/pdf/1706.03461.pdf
This example will not execute because there is no data. 

```R
library(hte)
packageVersion("hte")

#create the hte object using honest Random Forests (RF)

xl <- X_RF(feat = X.train, tr = Tr.train, yobs = Y.train)

#alternatively, use BART instead of honest Random Forests. 
#If you are not going to be careful about hyperparemeter tuning,
#we suggest using BART. 

xl <- X_BART(feat = X.train, tr = Tr.train, yobs = Y.train)

#estimate the CATE
xl.cate <- EstimateCate(xl, X.test)

#Create confidence intervals via bootstrapping. Note thayt honest Random Forests have theoretically
#valid CIs, BART does not. However, BART often performs well for prediction without extensive
#tuning. See our paper.

xl_ci <- CateCI(xl, X, B=500)
```
