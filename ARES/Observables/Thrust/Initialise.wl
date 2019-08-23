(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Initialise *)

(* :Author: *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)

BeginPackage["ARES`Observables`Thrust`Initialise`"]

  InitialiseThrust::usage = ""
  BuildThrust::usage = ""

  Begin["`Private`"]

    Needs["ARES`Observables`Thrust`SoftCollinearParametrisation`"]
    Needs["ARES`Observables`Thrust`SoftCollinearCorrections`"]

    InitialiseThrust[] :=
      Module[
        {},
        InitialiseSoftCollinearParametrisation[];
        InitialiseCorrections[];
      ]

    Additive = True;
    NJets = 2;

    BuildThrust[] :=
      Association[
        "Additive" -> Additive,
        "NJets"    -> NJets,
        "SCParametrisation" -> BuildMapThrust,
        "TransferFunctions" -> BuildMapICorrection[]
      ]

  End[]

EndPackage[]