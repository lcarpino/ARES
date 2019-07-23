(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Config *)

(* :Author: *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)

BeginPackage["ARES`Config`"]

  $ARESDirectory = ""
  $ARESGrids::usage = ""

  Begin["`Private`"]

	  $ARESDirectory = DirectoryName[$InputFileName];
    $ARESGrids = {FileNameJoin[{$HomeDirectory, ".ARES"}]};

  End[]

EndPackage[]