(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: ObservableConfig *)

(* :Author: *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)

BeginPackage["ARES`Observables`Config`"]

  $ObsInitOpt::usage = ""
  $ObsBuildOpt::usage = ""

  Begin["`Private`"]

    $ObsInitOpt =
      {
        "UseGrids" -> False
      };

    $ObsBuildOpt = {};

  End[]

EndPackage[]