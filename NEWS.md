# wowi 1.0.2

## Bug fixes

* Resolved issue [#61](https://github.com/tiwowi/wowi/issues/61).
In the previous version, the regular expressions used to match text patterns into tables would break whenever the list of location IDs in a cluster wrapped onto a new line, altering the expected match order. This caused several columns to remain empty. The parser now handles these cases programmatically. 

# wowi 1.0.1

## General updates 

* Branded built-in app; it now has a well‑put‑together, nice‑looking user 
interface and a revamped logo.
* Branded package website.
* Bundled server code into modules.
* Re-built vignettes using `quarto` engine.

# wowi 1.0.0

## New features ✨

* Added a Shiny application. Hereafter, not-well-versed R users and non-R users 
can now benefit from this package’s handy workflow through a web application.

## Bug fixes 

* Resolved mislabelled geographic coordinates.

# wowi 0.1.0

* Initial pre-release version for alpha-testing.
