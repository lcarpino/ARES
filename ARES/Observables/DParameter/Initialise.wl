(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Initialise *)

(* :Author: *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)

BeginPackage["ARES`Observables`DParameter`Initialise`"]

  InitialiseDParameter::usage = ""

  Begin["`Private`"]

    Needs["ARES`Observables`DParameter`SoftCollinearParametrisation`"]
    Needs["ARES`Observables`DParameter`SoftCollinearCorrections`"]

    InitialiseDParameter[] := 0

  End[]

EndPackage[]