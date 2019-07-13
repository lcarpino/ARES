(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Initialise *)

(* :Author: *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)

BeginPackage["ARES`Observables`DParameter`Initialise`"]

  InitialiseDParameter::usage = ""
  Additive::usage = ""
  SoftCollinearMap::usage = ""
  ICorrectionMap::usage = ""

  Begin["`Private`"]

    Needs["ARES`Observables`DParameter`SoftCollinearParametrisation`"]
    Needs["ARES`Observables`DParameter`SoftCollinearCorrections`"]

    InitialiseDParameter[] :=
      InitialiseSoftCollinearParametrisation[]
      InitialiseCorrections[]

    Additive[] := True

    SoftCollinearMap[] := BuildMapDParameter

    ICorrectionMap[] := BuildMapICorrection

  End[]

EndPackage[]