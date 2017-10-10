

data_folder <- 'tests/performance/CATEestimators/sim_data/'
MSE_files <- dir(data_folder)
MSE_rates_CF <-
  do.call(rbind,
          lapply(as.list(MSE_files[1:2]),
                 function(file) read.csv(paste0(data_folder, file))))
MSE_rates <-
  do.call(rbind,
          lapply(as.list(MSE_files[3:4]),
                 function(file) read.csv(paste0(data_folder, file))))

colnames(MSE_rates)
colnames(MSE_rates_CF)

do.call(function(file) read.csv(paste0(data_folder, file)),
        as.list(MSE_files[1:2]))

## we need c() to add further arguments
tmp <- expand.grid(letters[1:2], 1:3, c("+", "-"))
do.call("paste", c(tmp, sep = ""))

do.call(paste, list(as.name("A"), as.name("B")), quote = TRUE)

## examples of where objects will be found.
A <- 2
f <- function(x) print(x^2)
env <- new.env()
assign("A", 10, envir = env)
assign("f", f, envir = env)
f <- function(x) print(x)
f(A)                                      # 2
do.call("f", list(A))                     # 2
do.call("f", list(A), envir = env)        # 4
do.call(f, list(A), envir = env)          # 2
do.call("f", list(quote(A)), envir = env) # 100
do.call(f, list(quote(A)), envir = env)   # 10
do.call("f", list(as.name("A")), envir = env) # 100

eval(call("f", A))                      # 2
eval(call("f", quote(A)))               # 2
eval(call("f", A), envir = env)         # 4
eval(call("f", quote(A)), envir = env)  # 100
