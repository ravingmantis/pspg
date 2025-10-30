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
pspg(my_data_frame)
```

It returns the input by default, so it can be passed into a pipeline:

```r
do_a_thing() |> pspg() |> do_something_else()
```
