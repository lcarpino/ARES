(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SoftRadiatorExpansion *)

(* :Author: *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)

BeginPackage["ARES`Expansion`SoftRadiatorExpansion`", {"ARES`QCD`Constants`"}]

  G12::usage = ""
  G11::usage = ""
  G10::usage = ""

  G23::usage = ""
  G22::usage = ""
  G21::usage = ""

  LogXdipoles::usage = ""

  Begin["`Private`"]

    ExpOpts =
      {
        "RadiatorScheme" -> "Physical"
      };

    Options[G12] = ExpOpts;
    Options[G11] = ExpOpts;
    Options[G10] = ExpOpts;
    Options[G23] = ExpOpts;
    Options[G22] = ExpOpts;
    Options[G21] = ExpOpts;

    LogXdipoles[dips_?ListQ, xmuR_?NumericQ] :=
      Map[LogXab[#, xmuR] &, dips]

    G12[dips_?ListQ, obspar_?AssociationQ, OptionsPattern[]] :=
      Module[
        {
          RadiatorScheme = OptionValue["RadiatorScheme"]
        },

        Map[G12ab[#, obspar] &, dips]
      ]

    G11[dips_?ListQ, obspar_?AssociationQ, OptionsPattern[]] :=
      Module[
        {
          RadiatorScheme = OptionValue["RadiatorScheme"]
        },

        Map[G11ab[#, obspar] &, dips]
      ]

    G10[dips_?ListQ, obspar_?AssociationQ, OptionsPattern[]] :=
      Module[
        {
          RadiatorScheme = OptionValue["RadiatorScheme"]
        },

        Which[
          RadiatorScheme == "Physical",
            Map[G10ab[#, obspar] &, dips],
          RadiatorScheme == "ConstantFree",
            Map[G10abConstantFree[#, obspar] &, dips]
        ]
      ]

    G23[dips_?ListQ, obspar_?AssociationQ, OptionsPattern[]] :=
      Module[
        {
          RadiatorScheme = OptionValue["RadiatorScheme"]
        },

        Map[G23ab[#, obspar] &, dips]
      ]

    G22[dips_?ListQ, obspar_?AssociationQ, OptionsPattern[]] :=
      Module[
        {
          RadiatorScheme = OptionValue["RadiatorScheme"]
        },

        Map[G22ab[#, obspar] &, dips]
      ]

    G21[dips_?ListQ, obspar_?AssociationQ, OptionsPattern[]] :=
      Module[
        {
          RadiatorScheme = OptionValue["RadiatorScheme"]
        },

        Map[G21ab[#, obspar] &, dips]
      ]

    LogXab[dip_?AssociationQ, xmuR_?NumericQ] :=
      Module[
        {
          xa, xb
        },

        xa = dip["legs"][[1]]["x"];
        xb = dip["legs"][[2]]["x"];

        Log[xmuR^2/(xa+xb-1)]
      ]


    (* dipole definitions *)

    (* O(as) expansion in terms of dipoles *)

    G12ab[dip_?AssociationQ, obspar_?AssociationQ] :=
      Module[
        {
          a, b1, b2
        },

        a = obspar["ktpow"];
        b1 = obspar["etapow"][[dip["legs"][[1]]["num"]]];
        b2 = obspar["etapow"][[dip["legs"][[2]]["num"]]];

        -dip["col"] (1/(a (a + b1)) + 1/(a (a + b2)))
      ]

    G11ab[dip_?AssociationQ, obspar_?AssociationQ] :=
      Module[
        {
          a, b1, b2, logd1bar, logd2bar
        },

        a = obspar["ktpow"];
        b1 = obspar["etapow"][[dip["legs"][[1]]["num"]]];
        b2 = obspar["etapow"][[dip["legs"][[2]]["num"]]];
        logd1bar = obspar["logdabbar"][dip["num"]][[1]];
        logd2bar = obspar["logdabbar"][dip["num"]][[2]];

        -dip["col"] (2/(a (a + b1)) logd1bar + 2/(a (a + b2)) logd2bar)
      ]

    G10ab[dip_?AssociationQ, obspar_?AssociationQ] :=
      Module[
        {
          a, b1, b2, log2d1bar, log2d2bar
        },

        a = obspar["ktpow"];
        b1 = obspar["etapow"][[dip["legs"][[1]]["num"]]];
        b2 = obspar["etapow"][[dip["legs"][[2]]["num"]]];
        log2d1bar = obspar["log2dabbar"][dip["num"]][[1]];
        log2d2bar = obspar["log2dabbar"][dip["num"]][[2]];

        -dip["col"] (1/(a (a + b1)) log2d1bar + 1/(a (a + b2)) log2d2bar)
      ]

    G10abConstantFree[dip_?AssociationQ, obspar_?AssociationQ] :=
      Module[
        {
        },
        0
      ]


    (* O(as^2) expansion in terms of dipoles *)

    G23ab[dip_?AssociationQ, obspar_?AssociationQ] :=
      Module[
        {
          a, b1, b2, leg1res, leg2res
        },

        a = obspar["ktpow"];
        b1 = obspar["etapow"][[dip["legs"][[1]]["num"]]];
        b2 = obspar["etapow"][[dip["legs"][[2]]["num"]]];

        leg1res = (4 Pi beta0)/(3 a^2) (2 a + b1)/(a + b1)^2;
        leg2res = (4 Pi beta0)/(3 a^2) (2 a + b2)/(a + b2)^2;

        -dip["col"] (leg1res + leg2res)
      ]

    G22ab[dip_?AssociationQ, obspar_?AssociationQ] :=
      Module[
        {
          a, b1, b2, logd1bar, logd2bar,
          leg1res, leg2res
        },

        a = obspar["ktpow"];
        b1 = obspar["etapow"][[dip["legs"][[1]]["num"]]];
        b2 = obspar["etapow"][[dip["legs"][[2]]["num"]]];
        logd1bar = obspar["logdabbar"][dip["num"]][[1]];
        logd2bar = obspar["logdabbar"][dip["num"]][[2]];

        leg1res = (4 Pi beta0)/a^2 (2 a + b1)/(a + b1)^2 logd1bar + K1/(a (a + b1));
        leg2res = (4 Pi beta0)/a^2 (2 a + b2)/(a + b2)^2 logd2bar + K1/(a (a + b2));

        -dip["col"] (leg1res + leg2res)
      ]

    G21ab[dip_?AssociationQ, obspar_?AssociationQ] :=
      Module[
        {
          a, b1, b2, logd1bar, logd2bar, log2d1bar, log2d2bar,
          leg1res, leg2res
        },

        a = obspar["ktpow"];
        b1 = obspar["etapow"][[dip["legs"][[1]]["num"]]];
        b2 = obspar["etapow"][[dip["legs"][[2]]["num"]]];
        logd1bar = obspar["logdabbar"][dip["num"]][[1]];
        logd2bar = obspar["logdabbar"][dip["num"]][[2]];
        log2d1bar = obspar["log2dabbar"][dip["num"]][[1]];
        log2d2bar = obspar["log2dabbar"][dip["num"]][[2]];

        leg1res = ((Pi^3 beta0)/3 1/(a + b1) + (2 K1)/(a (a + b1)) logd1bar
                    + (4 Pi beta0)/a^2 (2 a + b1)/(a + b1)^2 log2d1bar);
        leg2res = ((Pi^3 beta0)/3 1/(a + b2) + (2 K1)/(a (a + b2)) logd2bar
                    + (4 Pi beta0)/a^2 (2 a + b2)/(a + b2)^2 log2d2bar);

        -dip["col"] (leg1res + leg2res)
      ]

  End[]

EndPackage[]