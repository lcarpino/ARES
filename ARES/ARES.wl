(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: ARES *)

(* :Author: *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)

context = 
  {
    (* Configuration *)
    "ARES`Config`",

    (* Drivers *)
    "ARES`Driver`Resummation`",
    "ARES`Driver`Expansion`",

    (* QCD *)
    "ARES`QCD`Constants`",
    "ARES`QCD`AlphaS`",
    "ARES`QCD`ScaleChoices`",
    "ARES`EPA`MatrixElements`",

    (* Event Geometry *)
    "ARES`Event`EventThreeJets`",

    (* Radiator *)
    "ARES`Radiator`SoftRadiator`",
    "ARES`Radiator`HardCollinearRadiator`",
    "ARES`Radiator`DerivativeRadiator`",

    (* Multiple emission functions *)
    "ARES`MultipleEmission`AdditiveInterface`",

    (* Resummation *)
    "ARES`Resummation`EPAThreeJets`",

    (* Expansion of the resummation *)
    "ARES`Expansion`SoftRadiatorExpansion`",
    "ARES`Expansion`CombinedExpansion`",

    (* Observables *)
    "ARES`Observables`ObservableBuilder`"
  };

BeginPackage["ARES`", context]

  Begin["`Private`"]

  End[]

EndPackage[]
