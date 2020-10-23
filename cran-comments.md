## Release summary

The CRAN checks spot an error on the Solaris platform when running the first 
example. So I modified the code to skip on Solaris platforms:

```
  if(grepl("sunos", tolower(Sys.info()["sysname"]))){ # skip on Solaris
    message("This package is currently not supported by 'Solaris' platforms.")
    return(invisible(NULL))
  }
```

Perhaps I will update the package in order that it works on Solaris platforms, 
if I find the appropriate Solaris command.


## Test environments

* ubuntu 18.04, R 3.6.3
* r-hub
* win-builder (devel & release)

## R CMD check results

OK
