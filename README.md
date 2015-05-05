# R application ancestor

[![Build Status](https://travis-ci.org/keboola/r-application.svg?branch=master)](https://travis-ci.org/keboola/r-application)

Application "framework" which provides very basic functions for logging and error handling.

## Installation
Package is available only on Github, so you need to use `devtools` to install the package
```
library('devtools')
install_github('keboola/r-application', ref = 'master')
```

## Examples
```
# Subclass the class to do something useful
MyApplication <- setRefClass(
    'MyApplication',
    contains = c("Application"),
    fields = list(
    ...
    ),
    methods = list(
    ...
    )
)

app <- MyApplication$new()
app->logDebug("Some message")
```
