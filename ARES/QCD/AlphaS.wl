(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: AlphaS *)

(* :Author: *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)

BeginPackage["ARES`QCD`AlphaS`", {"ARES`QCD`Constants`"}]

  AlphaSFixedNF::usage = ""

  Begin["`Private`"]

    (* functions that provide an approximate solution to the running coupling *)

    f1[t_] := 1

    f2[t_] := -(beta1/beta0) Log[1 + t]

    f3[t_] := -(beta2/beta0) t + (beta1/beta0)^2 (t + (Log[1 + t] - 1) Log[1 + t])

    f4[t_] := 1/(2 beta0^3) (t beta1 (-t beta1^2 + 2 (1 + t) beta0 beta2) - \
                t (2 + t) beta0^2 beta3 + beta1 Log[1 + t] \
                (-4 t beta1^2 - 2 beta0 beta2 + 4 t beta0 beta2 \
                + beta1^2 (5 - 2 Log[1 + t]) Log[1 + t]))


     (* running of AlphaS *)

    Options[AlphaSFixedNF] =
      {
        "refalphas" -> AlphaSMZ,
        "refscale" -> MZ,
        "refNF" -> NF
      };

    AlphaSFixedNF[newscale_?NumericQ, nloops_?IntegerQ, OptionsPattern[]] :=
      Module[
        {
          refalphas = OptionValue["refalphas"],
          refscale = OptionValue["refscale"],
          refNF = OptionValue["refNF"],
          t,
          resloop1, resloop2, resloop3, resloop4, res
        },

        t = refalphas beta0 Log[newscale^2/refscale^2];

        If[nloops == 0,
          Return[refalphas, Module]
        ];

        If[nloops >= 1,
          resloop1 = refalphas/(1 + t) f1[t],
          resloop1 = 0
        ];

        If[nloops >= 2,
          resloop2 = (refalphas/(1 + t))^2 f2[t],
          resloop2 = 0
        ];

        If[nloops >= 3,
          resloop3 = (refalphas/(1 + t))^3 f3[t],
          resloop3 = 0
        ];

        If[nloops >= 4,
          resloop4 = (refalphas/(1 + t))^4 f4[t],
          resloop4 = 0
        ];

        res = resloop1 + resloop2 + resloop3 + resloop4
      ]

  End[]

EndPackage[]