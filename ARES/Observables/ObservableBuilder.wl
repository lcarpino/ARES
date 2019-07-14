(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: ObservableBuilder *)

(* :Author: *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)

BeginPackage["ARES`Observables`ObservableBuilder`"]

  InitialiseObservable::usage = ""

  Begin["`Private`"]
    Needs["ARES`Observables`DParameter`Initialise`"];
    Needs["ARES`Observables`CParameter`Initialise`"];

    InitialiseObservable[Observable_] :=
      Module[
        {},

        Which[
          Observable == "DParameter",
            InitialiseDParameter[];
            BuildDParameter[],
          Observable == "CParameter",
            InitialiseCParameter[];
            BuildCParameter[]
        ]
      ]

  End[]

EndPackage[]
