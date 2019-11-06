(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: ScaleChoices *)

(* :Author: *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)

BeginPackage["ARES`QCD`ScaleChoices`"]

  muRconst::usage = ""
  muRgluonKT::usage = ""

  Begin["`Private`"]

    muRconst[eventConfig_List] := 1

    muRgluonKT[xq_?NumericQ, xqb_?NumericQ] := Sqrt[((1 - xq) (1 - xqb))/(xq + xqb - 1)]

  End[]

EndPackage[]