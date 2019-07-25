(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: AdditiveInterface *)

(* :Author: *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)

BeginPackage["ARES`Expansion`AdditiveInterface`"]

  FNLL22::usage = ""
  Fsc21::usage = ""
  Frec10::usage = ""
  Frec21::usage = ""
  Fhc21::usage = ""
  Fwa10::usage = ""
  Fwa21::usage = ""
  Fcorrel21::usage = ""
  Fclust21::usage = ""

  Begin["`Private`"]

  Needs["ARES`QCD`Constants`"]
  Needs["ARES`Expansion`DerivativeRadiatiorExpansion`"]

    FNLL22[RpNLL11_?NumericQ] :=
      Module[
        {
        },
        -Pi^2/12 RpNLL11^2
      ]

    Fsc21l[RpNLL11_?NumericQ, leg_?AssociationQ, obsSC_?AssociationQ] :=
      Module[
        {
          a, b, logdlbar, res
        },

        a = obsSC["ktpow"];
        b = obsSC["etapow"][[leg["num"]]];
        logdlbar = obsSC["logdlbar"][[leg["num"]]];

        res = -(RadsNNLL10l[leg, obsSC] logdlbar Pi^2/6 RpNLL11
                + RadsNNLL10l[leg, obsSC]/2 (-PolyGamma[2, 1] RpNLL11))
      ]

    Frec10l[RpNLL11_?NumericQ, leg_?AssociationQ, obsSC_?AssociationQ, I_] :=
      Module[
        {
          a, b, I, res
        },

        a = obsSC["ktpow"];
        b = obsSC["etapow"][[leg["num"]]];

        res = 2/(a + b) I
      ]

    Frec21l[RpNLL11_?NumericQ, leg_?AssociationQ, obsSC_?AssociationQ, I_] :=
      Module[
        {
          a, b, I, res
        },

        a = obsSC["ktpow"];
        b = obsSC["etapow"][[leg["num"]]];

        res = (8 Pi beta0)/(a + b)^2 I
      ]

    Fhc21l[RpNLL11_?NumericQ, leg_?AssociationQ, obsSC_?AssociationQ] := 
      Module[
        {
          a, b, ga0, res
        },
  
        a = obsSC["ktpow"];
        b = obsSC["etapow"][[leg["num"]]];
  
        Which[
          leg["flav"] == "q" || leg["flav"] == "qb", {ga0 = Ga0q},
          leg["flav"] == "g", {ga0 = Ga0g}
        ];

        res = 2 Pi^2/6 RpNLL11 (-ga0/(a + b))
      ]

    Fwa10ab[RpNLL11_?NumericQ, dipole_?AssociationQ, obsSC_?AssociationQ, I_] :=
      Module[
        {
          a, res
        },
   
        a = obsSC["ktpow"];
   
        res = dipole["col"] 2/a I
      ]

    Fwa21ab[RpNLL11_?NumericQ, dipole_?AssociationQ, obsSC_?AssociationQ, I_] :=
      Module[
        {
          a, res
        },
   
        a = obsSC["ktpow"];
   
        res = dipole["col"] (8 Pi beta0)/a^2 I
      ]

    Fcorrel21l[RpNLL11_?NumericQ, leg_?AssociationQ, obsSC_?AssociationQ, I_] :=
      Module[
        {
          a, b, res
        },
  
        a = obsSC["ktpow"];
        b = obsSC["etapow"][[leg["num"]]];
  
        res = -(1/a) RadsNNLL10l[leg, obsSC] I
      ]

    Fclust21l[RpNLL11_?NumericQ, leg_?AssociationQ, obsSC_?AssociationQ] :=
      Module[
        {
        },

        res = 0
      ]

  End[]

EndPackage[]