# README

* [Synapsis](vol_tools#markdown-header-synapsis)
* [Modules](vol_tools#markdown-header-modules)
    * [vol_misc](vol_tools#markdown-header-vol_misc)
    * [lst_ext](vol_tools#markdown-header-lst_ext)
    * [csvparser](vol_tools#markdown-header-csvparser)
    * [vol_date](vol_tools#markdown-header-vol_date)
    * [vol_stat](vol_tools#markdown-header-vol_stat)
    * [vol_struct](vol_tools#markdown-header-vol_struct)


# Synapsis

This app contains some sets of tools of various kinds.

# Modules

These are the different modules, or tool collections, that are currently part of this app.
As you can see, the naming is not coherent, yet, and might be changed in the future.
The full documentation for all the functions of each module is in edoc format in the file itself. (Is, or will be.)

## vol_misc
A module with things that didn't fit in the other categories. Contains app reloading functions (basically like the application:ensure_all_loaded/1 which was introduced in erlang 1.7)

## lst_ext
List handling procedures.

## csvparser
Reading from and writing to csv files.

## vol_date
Date and time related functions. Right now, it only contains functions to add or subtract intervals of time of different kinds. 

## vol_stat
Functions for calculating standard deviation and pals.

## vol_struct
Contains functions for e.g. mapping/folding over structures other than lists.