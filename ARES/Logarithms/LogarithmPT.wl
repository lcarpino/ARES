(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: LogarithmPT *)

(* :Author: *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)


BeginPackage["ARES`Logarithms`LogarithmPT`"]

  LtildePT::usage = ""

  Begin["`Private`"]

    (* Standard for most event shapes *)
    vmax = 1;
    pPT  = 1;

    Options[LtildePT] = {"vmax" -> vmax, "p" -> pPT}

    LtildePT[v_?NumericQ, xv_?NumericQ] :=
      Module[
        {
          vmax = OptionValue["vmax"],
          p = OptionValue["p"]
        }

        1/p Log[(xv/v)^p - (xv/vmax)^p + 1]
      ]

  End[]

EndPackage[]
