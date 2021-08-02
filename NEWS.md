# findInFiles 0.3.0

* Added the option '--with-filename' to 'grep'. Without this option, the file 
name does not appear in the output when there is a unique file in the results.

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
