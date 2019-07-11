(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SoftRadiatorExpansion *)

(* :Author: *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)

BeginPackage["`ARES`Expansion`SoftRadiatorExpansion`", {"`ARES`QCD`Constants`"}]

  G12::usage = ""
  G11::usage = ""
  G10::usage = ""

  G23::usage = ""
  G22::usage = ""
  G21::usage = ""

  Begin["`Private`"]

    (**)

    G12[dips_?ListQ, obspar_?AssociationQ, muR_?NumericQ, Q_?NumericQ] :=
      Total[Map[G12ab[#, obspar, muR, Q] &, dips]]

    G11[dips_?ListQ, obspar_?AssociationQ, muR_?NumericQ, Q_?NumericQ] :=
      Total[Map[G11ab[#, obspar, muR, Q] &, dips]]

    G10[dips_?ListQ, obspar_?AssociationQ, muR_?NumericQ, Q_?NumericQ] :=
      Total[Map[G10ab[#, obspar, muR, Q] &, dips]]

    G23[dips_?ListQ, obspar_?AssociationQ, muR_?NumericQ, Q_?NumericQ] :=
      Total[Map[G23ab[#, obspar, muR, Q] &, dips]]

    G22[dips_?ListQ, obspar_?AssociationQ, muR_?NumericQ, Q_?NumericQ] :=
      Total[Map[G22ab[#, obspar, muR, Q] &, dips]]

    G21[dips_?ListQ, obspar_?AssociationQ, muR_?NumericQ, Q_?NumericQ] :=
      Total[Map[G21ab[#, obspar, muR, Q] &, dips]]

    (* dipole definitions *)

    (* O(as) expansion in terms of dipoles *)

    G12ab[dip_?AssociationQ, obspar_?AssociationQ, muR_?NumericQ, Q_?NumericQ] :=
      Module[
        {a, b1, b2},

        a = obspar["ktpow"];
        b1 = obspar["etapow"][[dip["legs"][[1]]["num"]]];
        b2 = obspar["etapow"][[dip["legs"][[2]]["num"]]];

        -dip["col"] (1/(a (a + b1)) + 1/(a (a + b2)))
      ];

    G11ab[dip_?AssociationQ, obspar_?AssociationQ, muR_?NumericQ, Q_?NumericQ] :=
      Module[
        {a, b1, b2, logd1bar, logd2bar},

        a = obspar["ktpow"];
        b1 = obspar["etapow"][[dip["legs"][[1]]["num"]]];
        b2 = obspar["etapow"][[dip["legs"][[2]]["num"]]];
        logd1bar = obspar["logdabbar"][dip["num"]][[1]];
        logd2bar = obspar["logdabbar"][dip["num"]][[2]];

        -dip["col"] (2/(a (a + b1)) logd1bar + 2/(a (a + b2)) logd2bar)
      ];

    G10ab[dip_?AssociationQ, obspar_?AssociationQ, muR_?NumericQ, Q_?NumericQ] :=
      Module[
        {a, b1, b2, log2d1bar, log2d2bar},

        a = obspar["ktpow"];
        b1 = obspar["etapow"][[dip["legs"][[1]]["num"]]];
        b2 = obspar["etapow"][[dip["legs"][[2]]["num"]]];
        log2d1bar = obspar["log2dabbar"][dip["num"]][[1]];
        log2d2bar = obspar["log2dabbar"][dip["num"]][[2]];

        -dip["col"] (1/(a (a + b1)) log2d1bar + 1/(a (a + b2)) log2d2bar)
      ];


    (* O(as^2) expansion in terms of dipoles *)

    G23ab[dip_?AssociationQ, obspar_?AssociationQ, muR_?NumericQ, Q_?NumericQ] :=
      Module[
        {a, b1, b2, leg1res, leg2res},

        a = obspar["ktpow"];
        b1 = obspar["etapow"][[dip["legs"][[1]]["num"]]];
        b2 = obspar["etapow"][[dip["legs"][[2]]["num"]]];

        leg1res = (4 Pi beta0)/(3 a^2) (2 a + b1)/(a + b1)^2;
        leg2res = (4 Pi beta0)/(3 a^2) (2 a + b2)/(a + b2)^2;

        -dip["col"] (leg1res + leg2res)
      ];

    G22ab[dip_?AssociationQ, obspar_?AssociationQ, muR_?NumericQ, Q_?NumericQ] :=
      Module[
        {
          a, b1, b2, xa, xb, logd1bar, logd2bar,
          leg1res, leg2res
        },

        a = obspar["ktpow"];
        b1 = obspar["etapow"][[dip["legs"][[1]]["num"]]];
        b2 = obspar["etapow"][[dip["legs"][[2]]["num"]]];
        logd1bar = obspar["logdabbar"][dip["num"]][[1]];
        logd2bar = obspar["logdabbar"][dip["num"]][[2]];
        xa = dip["legs"][[1]]["x"];
        xb = dip["legs"][[2]]["x"];

        leg1res = (4 Pi beta0)/a^2 (2 a + b1)/(a + b1)^2 logd1bar + K1/(a (a + b1)) \
                    + (2 Pi beta0)/(a (a + b1)) Log[muR^2/((xa + xb - 1) Q^2)];
        leg2res = (4 Pi beta0)/a^2 (2 a + b2)/(a + b2)^2 logd2bar + K1/(a (a + b2)) \
                    + (2 Pi beta0)/(a (a + b2)) Log[muR^2/((xa + xb - 1) Q^2)];

        -dip["col"] (leg1res + leg2res)
      ];

    G21ab[dip_?AssociationQ, obspar_?AssociationQ, muR_?NumericQ, Q_?NumericQ] :=
      Module[
        {
          a, b1, b2, xa, xb, logd1bar, logd2bar, log2d1bar, log2d2bar,
          leg1res, leg2res
        },

        a = obspar["ktpow"];
        b1 = obspar["etapow"][[dip["legs"][[1]]["num"]]];
        b2 = obspar["etapow"][[dip["legs"][[2]]["num"]]];
        logd1bar = obspar["logdabbar"][dip["num"]][[1]];
        logd2bar = obspar["logdabbar"][dip["num"]][[2]];
        log2d1bar = obspar["log2dabbar"][dip["num"]][[1]];
        log2d2bar = obspar["log2dabbar"][dip["num"]][[2]];
        xa = dip["legs"][[1]]["x"];
        xb = dip["legs"][[2]]["x"];

        leg1res = (Pi^3 beta0)/3 1/(a + b1) + (2 K1)/(a (a + b1)) logd1bar \
                    + (4 Pi beta0)/a^2 (2 a + b1)/(a + b1)^2 log2d1bar \
                    + (4 Pi beta0)/(a (a + b1)) logd1bar Log[muR^2/((xa + xb - 1) Q^2)];
        leg2res = (Pi^3 beta0)/3 1/(a + b2) + (2 K1)/(a (a + b2)) logd2bar \
                    + (4 Pi beta0)/a^2 (2 a + b2)/(a + b2)^2 log2d2bar \
                    + (4 Pi beta0)/(a (a + b2)) logd2bar Log[muR^2/((xa + xb - 1) Q^2)];

        -dip["col"] (leg1res + leg2res)
      ];

  End[]

EndPackage[]