This repository contains experimental materials (data, scripts, etc.) for a
study of concepts and their relation to alternatives as understood in formal
semantics (joint work with [Emmanuel Chemla][EC] and [Manuel Križ][MK]).

There are two branches: `pilot` contains materials for a pilot version of the
experiment, and `master` contains materials for the main experiment reported in
our paper.

The stimuli (and instructions, etc.) reside in `chunk_includes/`. They were
created using the Bash script `generating_items/generateitems.sh`, which is a
wrapper script for the main Python script `generating_items/generateitem.py`.
The results of the experiment are located in `results/`, and the R script we
used to analyze the results is `analysis/Script_Public.R`.

The experiment uses the [Ibex][] platform for running psycholinguistic
experiments. See the [Ibex repository][] for license details, documentation,
etc.

[EC]: http://www.emmanuel.chemla.free.fr/
[MK]: http://manuel.kriz.fr/
[Ibex]: http://spellout.net/ibexfarm/
[Ibex repository]: https://github.com/addrummond/ibex
