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
  Needs["ARES`Expansion`DerivativeRadiatorExpansion`"]

    (* Main Interfaces *)

    (* FNLL *)
    FNLL22[RpNLL11_?NumericQ] :=
      Module[
        {
        },
        -Pi^2/12 RpNLL11^2
      ]

    (* FNNLL *) 

    (* soft-collinear correction *)
    Fsc21[RpNLL11_?NumericQ, legs_?ListQ, obsSC_?AssociationQ] :=
      Total[Map[Fsc21l[RpNLL11, #, obsSC] &, legs]]

    (* recoil correction *)
    Frec10[RpNLL11_?NumericQ, legs_?ListQ, obsSC_?AssociationQ, Irec_] :=
      Total[Map[Frec10l[RpNLL11, #, obsSC, Irec] &, legs]]

    Frec21[RpNLL11_?NumericQ, legs_?ListQ, obsSC_?AssociationQ, Irec_] :=
      Total[Map[Frec21l[RpNLL11, #, obsSC, Irec] &, legs]]

    (* hard-collinear correction *)
    Fhc21[RpNLL11_?NumericQ, legs_?ListQ, obsSC_?AssociationQ] :=
      Total[Map[Fhc21l[RpNLL11, #, obsSC] &, legs]]

    (* wide-angle correction *)
    Fwa10[RpNLL11_?NumericQ, dipoles_?ListQ, obsSC_?AssociationQ, Iwa_] :=
      Total[Map[Fwa10ab[RpNLL11, #, obsSC, Iwa] &, dipoles]]

    Fwa21[RpNLL11_?NumericQ, dipoles_?ListQ, obsSC_?AssociationQ, Iwa_] :=
      Total[Map[Fwa21ab[RpNLL11, #, obsSC, Iwa] &, dipoles]]

    (* correlated correction *)
    Fcorrel21[RpNLL11_?NumericQ, legs_?ListQ, obsSC_?AssociationQ, Icorrel_] :=
      Total[Map[Fcorrel21l[RpNLL11, #, obsSC, Icorrel] &, legs]]

    (* clustering correction *)
    Fclust21[RpNLL11_?NumericQ, legs_?ListQ, obsSC_?AssociationQ] :=
      Total[Map[Fclust21l[RpNLL11, #, obsSC] &, legs]]



    (* Implementation of specific corrections in terms of legs/dipoles *)

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
          a, b, res
        },

        a = obsSC["ktpow"];
        b = obsSC["etapow"][[leg["num"]]];

        res = 2/(a + b) I[leg, obsSC]
      ]

    Frec21l[RpNLL11_?NumericQ, leg_?AssociationQ, obsSC_?AssociationQ, I_] :=
      Module[
        {
          a, b, res
        },

        a = obsSC["ktpow"];
        b = obsSC["etapow"][[leg["num"]]];

        res = (8 Pi beta0)/(a + b)^2 I[leg, obsSC]
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
   
        res = dipole["col"] 2/a I[dipole, obsSC]
      ]

    Fwa21ab[RpNLL11_?NumericQ, dipole_?AssociationQ, obsSC_?AssociationQ, I_] :=
      Module[
        {
          a, res
        },
   
        a = obsSC["ktpow"];
   
        res = dipole["col"] (8 Pi beta0)/a^2 I[dipole, obsSC]
      ]

    Fcorrel21l[RpNLL11_?NumericQ, leg_?AssociationQ, obsSC_?AssociationQ, I_] :=
      Module[
        {
          a, b, res
        },
  
        a = obsSC["ktpow"];
        b = obsSC["etapow"][[leg["num"]]];
  
        res = -(1/a) RadsNNLL10l[leg, obsSC] I[leg, obsSC]
      ]

    Fclust21l[RpNLL11_?NumericQ, leg_?AssociationQ, obsSC_?AssociationQ] :=
      Module[
        {
        },

        res = 0
      ]

  End[]

EndPackage[]