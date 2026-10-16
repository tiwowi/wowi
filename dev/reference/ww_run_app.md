# Initialise built-in Shiny application

Initialise built-in Shiny application

## Usage

``` r
ww_run_app(package = "wowi")
```

## Arguments

- package:

  package name ("wowi").

## Value

Called for its side effect of launching the `wowi` Shiny application in
the user's default web browser. Returns `NULL` invisibly when the app
session ends.

## Examples

``` r
if(interactive()) ww_run_app()
```
