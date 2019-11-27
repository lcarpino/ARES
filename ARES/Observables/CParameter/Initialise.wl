(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Initialise *)

(* :Author: *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)

BeginPackage["ARES`Observables`CParameter`Initialise`"]

  InitialiseCParameter::usage = ""
  BuildCParameter::usage = ""

  Begin["`Private`"]

    Needs["ARES`Observables`Config`"];
    Needs["ARES`Observables`CParameter`SoftCollinearParametrisation`"]
    Needs["ARES`Observables`CParameter`SoftCollinearCorrections`"]

    InitialiseCParameter[opt: OptionsPattern[$ObsInitOpt]] :=
      Module[
        {},
        InitialiseSoftCollinearParametrisation[];
        InitialiseCorrections[];
      ]

    Additive = True;
    NJets = 2;

    BuildCParameter[] :=
      Association[
        "Additive" -> Additive,
        "NJets"    -> NJets,
        "SCParametrisation" -> BuildMapCParameter,
        "TransferFunctions" -> BuildMapICorrection[]
      ]

  End[]

EndPackage[]