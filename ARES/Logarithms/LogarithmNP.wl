(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: LogarithmNP *)

(* :Author: *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)


BeginPackage["ARES`Logarithms`LogarithmNP`"]

  LtildeNP::usage = ""

  Begin["`Private`"]

    vmax = 1;
    pNP  = 1;

    qNP  = 2;

    Options[LtildeNP] = {"vmax" -> vmax, "p" -> pNP}

    LtildeNP[v_?NumericQ, deltav_?NumericQ, xv_?NumericQ, OptionsPattern[]] :=
      Module[
        {
          vmax = OptionValue["vmax"],
          p = OptionValue["p"]
        },

        1/p Log[(xv/(v-deltav))^p - (xv/(vmax-deltav))^p + 1]
      ]

    Options[deltavNP] = {"vmax" -> vmax, "pNP" -> qNP}

    deltavNP[deltav_?NumericQ, v_?NumericQ, OptionsPattern[]] := 
      Module[
        {
          vmax = OptionValue["vmax"],
          p
        },
        res = (1 - (v/vmax)^pNP) deltav
      ]

  End[]

EndPackage[]
