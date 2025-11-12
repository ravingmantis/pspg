# Postgres Pager for R

This package wraps the [Postgres Pager](https://github.com/okbob/pspg) to provide a console-based data structure display tool.

## Installation

You first need to install pspg. [See the docs for more detail](https://github.com/okbob/pspg?tab=readme-ov-file#installation-and-basic-configuration), but essentially:

```
# Debian (Ubuntu)
sudo apt-get install pspg

# RedHat (Fedora)
sudo dnf install pspg
```

Then you can install the package using [remotes](https://github.com/r-lib/remotes)

```r
> remotes::install_github("ravingmantis/pspg")
```

## Usage

Use the ``pspg`` generic to view a data structure:

```r
# View a data.frame
pspg(expand.grid(a=1:10 * 10, b = 2:20 * 100))

# View a list
pspg(list(a=1:10,b=2))

# View an array
pspg(array(1, dim = c(10, 10), dimnames = list(paste0("y", 1:10), paste0("x", 1:10))))
```

It returns the input, so a pspg step can be included in a pipeline:

```r
do_a_thing() |> pspg() |> do_something_else()
```
