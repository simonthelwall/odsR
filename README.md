
<!-- README.md is generated from README.Rmd. Please edit that file -->
odsR Package
============

This is an R package to facilitate the extraction of NHS organisation data from the NHS Digital ODS API into the R environment

Any feedback would be appreciated and can be provided using the Issues section of the GitLab repository, or by emailing <PHDS@phe.gov.uk>

<br/> <br/>

Installation
------------

#### From zip

Download this repository from GitLab and either build from source or do:

``` r
source <- devtools:::source_pkg("C:/path/to/odsR-master")
install(source)
```

#### With devtools

You can install the latest version of odsR from GitLab with:

``` r
if (!require(devtools)) install.packages("devtools")
devtools::install_git('https://gitlab.phe.gov.uk/georgina.anderson/odsR', build_vignettes=TRUE)
```

<br/> <br/>

Package Versioning
------------------

Following installation of this package, type 'packageVersion("odsR")' in the R console to show the package version. If it is suffixed with a 9000 number then you are using an unapproved development version.

Released versions of this package will have version numbers consisting of three parts: <major>.<minor>.<patch> In-development versions of this package will have a fourth component, the development version number, which will increment from 9000.

See <http://r-pkgs.had.co.nz/description.html> for further information on package versioning

Package Contents
----------------

The package contains the following functions - see individual item documentation for full details

**Functions:**
- getODS (get summary organisation data for multiple organisations) - getODS1 (get full organisation data for a single organisation - not yet included) - getODSsync (get organisation API urls for any organisations whose details have changed since a specific date - not yet included)

**Datasets:**
- none

**Vignettes:**
- none