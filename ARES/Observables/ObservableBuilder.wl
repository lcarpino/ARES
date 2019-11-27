(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: ObservableBuilder *)

(* :Author: *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)

BeginPackage["ARES`Observables`ObservableBuilder`"]

  BuildObservable::usage = ""

  Begin["`Private`"]
    Needs["ARES`Observables`Config`"];
    Needs["ARES`Observables`DParameter`Initialise`"];
    Needs["ARES`Observables`CParameter`Initialise`"];
    Needs["ARES`Observables`Thrust`Initialise`"];

    BuildObservable[Observable_, opt: OptionsPattern[$ObsInitOpt]] :=
      Module[
        {},

        Which[
          Observable == "DParameter",
            InitialiseDParameter[opt];
            BuildDParameter[],
          Observable == "CParameter",
            InitialiseCParameter[opt];
            BuildCParameter[],
          Observable == "Thrust",
            InitialiseThrust[opt];
            BuildThrust[]
        ]
      ]

  End[]

EndPackage[]
