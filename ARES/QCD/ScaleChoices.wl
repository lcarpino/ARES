(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: ScaleChoices *)

(* :Author: *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)

BeginPackage["`ARES`QCD`ScaleChoices`"]

  muRConst::usage = ""
  muRGluonKT::usage = ""

  LogXConst::usage = ""

  Begin["`Private`"]

    muRConst[xq_?NumericQ, xqb_?NumericQ] := 1

    muRGluonKT[xq_?NumericQ, xqb_?NumericQ] := Sqrt[((1 - xq) (1 - xqb))/(xq + xqb - 1)]

    LogXConst[dip_?ListQ, leg_?ListQ, obspar_?AssociationQ] := 0

  End[]

EndPackage[]