## Resubmission

This is a resubmission. In this version, I have addressed the issues raised by the CRAN team as listed below:

* Updated the package description in the `DESCRIPTION` file to: 
    - Put SaTScan software name in single quotes - 'SaTScan'.
    - Follow the required format for auto-linking: <https://www.satscan.org/>.

* Added `\value` section to the `.Rd` files of exported functions, documenting the structure and meaning of the returned objects:
  - ww_configure_satscan.Rd
  - ww_run_app.Rd

## R CMD check results

### Local checks

0 errors | 0 warnings | 0 notes

* All changes were limited to addressing CRAN’s feedback.