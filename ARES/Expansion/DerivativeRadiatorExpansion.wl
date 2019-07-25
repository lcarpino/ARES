(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DerivativeRadiatorExpansion *)

(* :Author: *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)

BeginPackage["ARES`Expansion`DerivativeRadiatorExpansion`"]

  RadpNLL11l::usage = ""
  RadpNLL11::usage = ""

  RadsNNLL10l::usage = ""
  RadsNNLL10::usage = ""

  Begin["`Private`"]

    Needs["ARES`QCD`Constants`"]

    RadpNLL11[legs_?ListQ, obsSC_?AssociationQ] :=
      Total[Map[RadpNLL11l[#, obsSC] &, legs]]

    RadsNNLL11[legs_?ListQ, obsSC_?AssociationQ] :=
      Total[Map[RadsNNLL11l[#, obsSC] &, legs]]


    RadpNLL11l[leg_?AssociationQ, obsSC_?AssociationQ] :=
      Module[
        {
          a, b
        },
  
        a = obsSC["ktpow"];
        b = obsSC["etapow"][[leg["num"]]];
  
        leg["col"] 4/(a (a + b))
      ]

    RadsNNLL10l[leg_?AssociationQ, obsSC_?AssociationQ] :=
      Module[
        {
          a, b
        },
  
        a = obsSC["ktpow"];
        b = obsSC["etapow"][[leg["num"]]];
  
        leg["col"] 4/(a (a + b))
      ]

  End[]

EndPackage[]