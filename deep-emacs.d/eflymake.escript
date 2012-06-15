#!/usr/bin/env escript
%%! -env ERL_LIBS ../deps:../../../apps:../../../deps
%% -*- erlang -*-

main([File_Name]) ->
  Includes0 =
    %% filelib:wildcard("{apps,lib}/*/{src,include}") ++
    %% filelib:wildcard("{deps,lib}/*/{src,include}") ++
    ["../src", "../include", "../gen_src", "../gen_include"],
  Includes = [{i, D} || D <- Includes0, filelib:is_dir(D)],
  compile:file(File_Name, [warn_obsolete_guard, warn_unused_import,
                           warn_shadow_vars, warn_export_vars,
                           strong_validation, report | Includes]).
