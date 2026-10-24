# Install sonicscrewdriver Python environment

This function installs scikit-maad in the `sonicscrewdriver` environment
using `reticulate`.

## Usage

``` r
pythonInstall(unattended = FALSE)
```

## Arguments

- unattended:

  If TRUE then the function will not prompt the user to install the
  environment in a non-interactive session.

## Value

No return value, called for its side effect of creating the Python
environment.

## Examples

``` r
if (FALSE) { # \dontrun{
pythonInstall()
pythonInstall(unattended=TRUE)
} # }
```
