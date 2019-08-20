(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: ScaleChoices *)

(* :Author: *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)

BeginPackage["ARES`Logarithms`ScaleChoices`"]

  LogXconst::usage = ""
  LogXproddcwt::usage = ""
  LogXproddbarcwt::usage = ""
  LogXproddlbarcwt::usage = ""
  LogXproddabbarcwt::usage = ""

  Begin["`Private`"]

    LogXconst[dip_?ListQ, leg_?ListQ, obspar_?AssociationQ] := 0

    LogXproddcwt[dips_?ListQ, legs_?ListQ, obspar_?AssociationQ] :=
      Module[
        {ct, res},

        ct = Total[Map[#["col"] &, legs]];
        res = 1/ct Total[Map[LogXproddcwtl[#, obspar] &, legs]]
      ]

    LogXproddbarcwt[dips_?ListQ, legs_?ListQ, obspar_?AssociationQ] :=
      Module[
        {ct, res},

        ct = Total[Map[#["col"] &, legs]];
        res = 1/ct Total[Map[LogXproddbarcwtl[#, obspar] &, legs]]
      ]

    LogXproddlbarcwt[dips_?ListQ, legs_?ListQ, obspar_?AssociationQ] :=
      Module[
        {ct, res},

        ct = Total[Map[#["col"] &, legs]];
        res = 1/ct Total[Map[LogXproddlbarcwtl[#, obspar] &, legs]]
      ]

    LogXproddabbarcwt[dips_?ListQ, legs_?ListQ, obspar_?AssociationQ] :=
      Module[
        {ct, res},

        ct = Total[Map[#["col"] &, dips]];
        res = 1/ct Total[Map[LogXproddabbarcwtab[#, obspar] &, dips]]
      ]

    LogXproddcwtl[leg_?AssociationQ, obspar_?AssociationQ] :=
      Module[
        {logd, res},

        logd = obspar["logd"][[leg["num"]]];

        res = leg["col"] logd
      ]

    LogXproddbarcwtl[leg_?AssociationQ, obspar_?AssociationQ] :=
      Module[
        {logdbar, res},

        logdbar = obspar["logdbar"][[leg["num"]]];

        res = leg["col"] logdbar
      ]

    LogXproddlbarcwtl[leg_?AssociationQ, obspar_?AssociationQ] :=
      Module[
        {logdlbar, res},

        logdlbar = obspar["logdlbar"][[leg["num"]]];

        res = leg["col"] logdlbar
      ]

    LogXproddabbarcwtab[dip_?AssociationQ, obspar_?AssociationQ] :=
      Module[
        {
          logd1bar, logd2bar,
          res
        },

        logd1bar = obspar["logdabbar"][dip["num"]][[1]];
        logd2bar = obspar["logdabbar"][dip["num"]][[2]];

        res = dip["col"]/2 (logd1bar + logd2bar)
      ]

  End[]

EndPackage[]