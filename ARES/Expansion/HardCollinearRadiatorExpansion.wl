(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: HardCollinearRadiatorExpansion *)

(* :Author: *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)

BeginPackage["ARES`Expansion`HardCollinearRadiatorExpansion`", {"ARES`QCD`Constants`"}]

  H11::usage = ""
  H10::usage = ""

  H22::usage = ""
  H21::usage = ""

  Begin["`Private`"]

    (**)

    H11[legs_?ListQ, obspar_?AssociationQ, muR_?NumericQ, Q_?NumericQ] :=
      Total[Map[H11l[#, obspar, muR, Q] &, legs]]

    H10[legs_?ListQ, obspar_?AssociationQ, muR_?NumericQ, Q_?NumericQ] :=
      Total[Map[H10l[#, obspar, muR, Q] &, legs]]

    H22[legs_?ListQ, obspar_?AssociationQ, muR_?NumericQ, Q_?NumericQ] :=
      Total[Map[H22l[#, obspar, muR, Q] &, legs]]

    H21[legs_?ListQ, obspar_?AssociationQ, muR_?NumericQ, Q_?NumericQ] :=
      Total[Map[H21l[#, obspar, muR, Q] &, legs]]

    (* leg definitions *)

    (* O(as) expansion in terms of legs *)

    H11l[leg_?AssociationQ, obspar_?AssociationQ, muR_?NumericQ, Q_?NumericQ] :=
      Module[
        {a, b, ga},

        a = obspar["ktpow"];
        b = obspar["etapow"][[leg["num"]]];

        Which[
          leg["flav"] == "q" || leg["flav"] == "qb",
            ga = Ga0q,
          leg["flav"] == "g",
            ga = Ga0g
        ];

        -((2 ga)/(a + b))
      ]

    H10l[leg_?AssociationQ, obspar_?AssociationQ, muR_?NumericQ, Q_?NumericQ] :=
      Module[
        {a, b},

        a = obspar["ktpow"];
        b = obspar["etapow"][[leg["num"]]];

        0
      ]


    (* O(as^2) expansion in terms of legs *)

    H22l[leg_?AssociationQ, obspar_?AssociationQ, muR_?NumericQ, Q_?NumericQ] :=
      Module[
        {a, b, ga},

        a = obspar["ktpow"];
        b = obspar["etapow"][[leg["num"]]];

        Which[
          leg["flav"] == "q" || leg["flav"] == "qb",
            ga = Ga0q,
          leg["flav"] == "g",
            ga = Ga0g
        ];

        -4 Pi beta0 ga/(a + b)^2

      ]

    H21l[leg_?AssociationQ, obspar_?AssociationQ, muR_?NumericQ, Q_?NumericQ] :=
      Module[
        {a, b},

        a = obspar["ktpow"];
        b = obspar["etapow"][[leg["num"]]];

        Which[
          leg["flav"] == "q" || leg["flav"] == "qb",
            ga = Ga1q,
          leg["flav"] == "g",
            ga = Ga1g
        ];

        -((2 ga)/(a + b))
      ]

  End[]

EndPackage[]