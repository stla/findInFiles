# findInFiles 0.5.0

* Files without base name (such as `.gitignore`) were excluded from the search. 
This is no longer the case.

* Hidden folders (those whose name starts with a dot) were not excluded from 
the search when running a full recursive search (i.e. `depth` is `NULL` or a
negative integer). Now they are always excluded.

* The argument `ext` of the `findInFiles` function has been renamed to 
`extensions` and it can be a vector of file extensions now.

* The arguments `excludePattern` and `excludeFoldersPattern` of the 
`findInFiles` function can be some vectors now, to give multiple patterns.

* The `findInFiles` function has now an argument `includePattern`. If a vector 
of patterns is supplied to this argument, then only the files whose name matches
one of these patterns are included in the search.

* The `findInFiles` function has two new arguments `maxCountPerFile` and
`maxCount`. If an integer is supplied to `maxCountPerFile`, then this integer 
is passed to the `--max-count` option of the `grep` command and then `grep` 
stops to search in a file whenever it has found `maxCountPerFile` results in 
this file. If an integer is supplied to `maxCount`, then only the 
first `maxCount` results found by `grep` are returned; but `grep` does not 
stop to search after it has found `maxCount` results: it internally returns all
the search results, but only the first `maxCount` results are returned to the 
user.

* The `findInFiles` function has now an alias function named `fif`. 

* Since I most often use the `findInFiles` function to search in R files, I 
added the function `fifR` which is the same as `findInFiles` with the 
`extensions` argument set to `"R"`.


# findInFiles 0.4.0

Now the package provides a Shiny application (`shinyFIF()`) allowing to run 
`findInFiles` and to navigate in the results.


# findInFiles 0.3.0

* Added the option `--with-filename` to the `grep` command. Without this 
option, the file name does not appear in the output when there is a unique 
file in the results.

* The `output` argument must now be one of `"viewer"` (the default), 
`"tibble"`, or `"viewer+tibble"`. These two latter options allow to get the 
results of the search in a tibble, in which the matched pattern is colored in 
red.


# findInFiles 0.2.0

The `findInFiles` function has a new argument `output`. It can be `"viewer"` 
(the default), `"dataframe"`, or `"viewer+dataframe"`. These two latter options 
allow to get the results of the search in a dataframe. See the examples.


# findInFiles 0.1.2

Support for Solaris, requiring the 'ggrep' system command.


# findInFiles 0.1.1

Skip on Solaris platforms, because of an issue with the 'grep' system command.


# findInFiles 0.1.0

First release.
