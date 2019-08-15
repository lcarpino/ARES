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

  g1pl::usage = ""
  g1p::usage = ""

  g1sl::usage = ""
  g1s::usage = ""

  Begin["`Private`"]

    Needs["ARES`QCD`Constants`"]

    RadpNLL11[legs_?ListQ, obsSC_?AssociationQ] :=
      Total[Map[RadpNLL11l[#, obsSC] &, legs]]

    RadsNNLL11[legs_?ListQ, obsSC_?AssociationQ] :=
      Total[Map[RadsNNLL11l[#, obsSC] &, legs]]

    g1p[legs_?ListQ, obsSC_?AssociationQ] :=
      Total[Map[g1pl[#, obsSC] &, legs]]

    g1s[legs_?ListQ, obsSC_?AssociationQ] :=
      Total[Map[g1sl[#, obsSC] &, legs]]


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

    g1pl[leg_?AssociationQ, obsSC_?AssociationQ] :=
      Module[
        {
          a, b
        },

        a = obsSC["ktpow"];
        b = obsSC["etapow"][[leg["num"]]];

        leg["col"] (-1/(Pi beta0 a(a+b)))
      ]

    g1sl[leg_?AssociationQ, obsSC_?AssociationQ] :=
      Module[
        {
          a, b
        },

        a = obsSC["ktpow"];
        b = obsSC["etapow"][[leg["num"]]];

        leg["col"] (-4/(Pi beta0 3 a^2) (2 a + b)/(a + b)^2)
      ]


  End[]

EndPackage[]