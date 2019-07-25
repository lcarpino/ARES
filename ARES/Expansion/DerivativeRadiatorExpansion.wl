(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DerivativeRadiatorExpansion *)

(* :Author: *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)

BeginPackage["ARES`Expansion`DerivativeRadiatorExpansion`", {"ARES`QCD`Constants`"}]

  Begin["`Private`"]

    RadpNLL11l[leg_?AssociationQ, obsSC_?AssociationQ] :=
      Module[
        {
          a, b
        },
  
        a = obspar["ktpow"];
        b = obspar["etapow"][[leg["num"]]];
  
        leg["col"] 4/(a (a + b))
      ]

    RadsNNLL10l[leg_?AssociationQ, obsSC_?AssociationQ] :=
      Module[
        {
          a, b
        },
  
        a = obspar["ktpow"];
        b = obspar["etapow"][[leg["num"]]];
  
        leg["col"] 4/(a (a + b))
      ]

  End[]

EndPackage[]