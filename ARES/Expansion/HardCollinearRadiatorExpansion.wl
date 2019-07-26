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

  LogXlegs::usage = ""

  Begin["`Private`"]

    ExpOpts =
      {
        "RadiatorScheme" -> "Physical"
      };

    Options[H11] = ExpOpts;
    Options[H10] = ExpOpts;
    Options[H22] = ExpOpts;
    Options[H21] = ExpOpts;

    LogXlegs[legs_?ListQ, xmuR_?NumericQ] :=
      Map[LogXl[#, xmuR] &, legs]

    H11[legs_?ListQ, obspar_?AssociationQ, OptionsPattern[]] :=
      Module[
        {
          RadiatorScheme = OptionValue["RadiatorScheme"]
        },

        Map[H11l[#, obspar] &, legs]
      ]

    H10[legs_?ListQ, obspar_?AssociationQ, OptionsPattern[]] :=
      Module[
        {
          RadiatorScheme = OptionValue["RadiatorScheme"]
        },

        Map[H10l[#, obspar] &, legs]
      ]

    H22[legs_?ListQ, obspar_?AssociationQ, OptionsPattern[]] :=
      Module[
        {
          RadiatorScheme = OptionValue["RadiatorScheme"]
        },

        Map[H22l[#, obspar] &, legs]
      ]

    H21[legs_?ListQ, obspar_?AssociationQ, OptionsPattern[]] :=
      Module[
        {
          RadiatorScheme = OptionValue["RadiatorScheme"]
        },

        Map[H21l[#, obspar] &, legs]
      ]

    LogXl[leg_?AssociationQ, xmuR_?NumericQ] :=
      Module[
        {
          xl
        },

        xl = leg["x"];

        Log[xmuR^2]
      ]

    (* leg definitions *)

    (* O(as) expansion in terms of legs *)

    H11l[leg_?AssociationQ, obspar_?AssociationQ] :=
      Module[
        {
          a, b, Ga0
        },

        a = obspar["ktpow"];
        b = obspar["etapow"][[leg["num"]]];

        Which[
          leg["flav"] == "q" || leg["flav"] == "qb",
            Ga0 = Ga0q,
          leg["flav"] == "g",
            Ga0 = Ga0g
        ];

        -((2 Ga0)/(a + b))
      ]

    H10l[leg_?AssociationQ, obspar_?AssociationQ] :=
      Module[
        {
          a, b
        },

        a = obspar["ktpow"];
        b = obspar["etapow"][[leg["num"]]];

        0
      ]


    (* O(as^2) expansion in terms of legs *)

    H22l[leg_?AssociationQ, obspar_?AssociationQ] :=
      Module[
        {
          a, b, Ga0
        },

        a = obspar["ktpow"];
        b = obspar["etapow"][[leg["num"]]];

        Which[
          leg["flav"] == "q" || leg["flav"] == "qb",
            Ga0 = Ga0q,
          leg["flav"] == "g",
            Ga0 = Ga0g
        ];

        -4 Pi beta0 Ga0/(a + b)^2
      ]

    H21l[leg_?AssociationQ, obspar_?AssociationQ] :=
      Module[
        {
          a, b, Ga0, Ga1
        },

        a = obspar["ktpow"];
        b = obspar["etapow"][[leg["num"]]];

        Which[
          leg["flav"] == "q" || leg["flav"] == "qb",
            {Ga0 = Ga0q, Ga1 = Ga1q},
          leg["flav"] == "g",
            {Ga0 = Ga0g, Ga1 = Ga1g}
        ];

        -(2 Ga1)/(a + b)
      ]

  End[]

EndPackage[]