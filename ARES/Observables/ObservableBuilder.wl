(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: ObservableBuilder *)

(* :Author: *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)

BeginPackage["ARES`Observables`ObservableBuilder`"]

  BuildObservable::usage = ""

  Begin["`Private`"]
    Needs["ARES`Observables`DParameter`Initialise`"];
    Needs["ARES`Observables`CParameter`Initialise`"];
    Needs["ARES`Observables`Thrust`Initialise`"];

    BuildObservable[Observable_] :=
      Module[
        {},

        Which[
          Observable == "DParameter",
            InitialiseDParameter[];
            BuildDParameter[],
          Observable == "CParameter",
            InitialiseCParameter[];
            BuildCParameter[],
          Observable == "Thrust",
            InitialiseThrust[];
            BuildThrust[]
        ]
      ]

  End[]

EndPackage[]
