(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Initialise *)

(* :Author: *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)

BeginPackage["ARES`Observables`DParameter`Initialise`"]

  InitialiseDParameter::usage = ""
  BuildDParameter::usage = ""

  Begin["`Private`"]

    Needs["ARES`Observables`DParameter`SoftCollinearParametrisation`"]
    Needs["ARES`Observables`DParameter`SoftCollinearCorrections`"]
    Needs["ARES`Observables`DParameter`NPCorrections`"]

    InitialiseDParameter[] :=
      Module[
        {},
        InitialiseSoftCollinearParametrisation[];
        InitialiseCorrections[];
        InitialiseNPCorrections[];
      ]

    Additive = True;
    NJets = 3;

    BuildDParameter[] :=
      Association[
        "Additive" -> Additive,
        "NJets"    -> NJets,
        "SCParametrisation" -> BuildMapDParameter,
        "TransferFunctions" -> BuildMapICorrection[],
        "DeltaNP"           -> DeltaNP
      ]

  End[]

EndPackage[]