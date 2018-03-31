# Replication code for Anthoff and Emmerling (2018) "Inequality and the Social Cost of Carbon"

This repository holds all code required to replicate the results of:

Anthoff, David and Johannes Emmerling (2018) "Inequality and the Social Cost of Carbon".

## Software requirements

You need to install [julia](http://julialang.org/) and [R](https://www.r-project.org/) to run the replication code.

## Preparing the software environment

On the julia side of things you need to install a number of packages. You can use the following julia code to do so:

````julia
Pkg.add("Mimi")
Pkg.add("DataFrames")
Pkg.add("Query")
Pkg.add("ProgressMeter")
Pkg.add("PyPlot")
Pkg.add("RCall")
Pkg.add("ExcelReaders")
Pkg.add("Distributions")
````

On the R side of things you also need to install a number packages. You can use the following R code to do so:

````R
install.packages("openxlsx")
install.packages("stringr")
install.packages("data.table")
install.packages("reshape2")
install.packages("plyr")
install.packages("ggplot2")
install.packages("ggrepel")
install.packages("scales")
````

## Cloning the repository

This git repository uses a git submodule. To ensure the submodule gets properly downloaded, make sure to use the
git ``--recurse-submodules`` option when cloning the repository. If you cloned the repository without that option,
you can issue the following two git commands to make sure the submodule is present on your system:
``git submodule init``, followed by ``git submodule update``.

## Running the replication script

To recreate all outputs for this paper, run the ``main.ipynb`` file in the folder ``src`` in IJulia.

## Result and figure files

All results and figures will be stored in the folder ``output``.
