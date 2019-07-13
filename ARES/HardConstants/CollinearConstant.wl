(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: CollinearConstant *)

(* :Author: *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)

BeginPackage["ARES`Radiator`HardConstants`", "ARES`QCD`Constants`"]

  C1hcl::usage = ""

  Begin["`Private`"]

    C1hcl[lambda_?NumericQ, xq_?NumericQ, xqb_?NumericQ, 
          leg_?AssociationQ, obspar_?AssociationQ] :=

      Module[
        {a, b, logdlbar, spow, res},
  
        a = obspar["ktpow"];
        b = obspar["etapow"][[leg["num"]]];
        logdlbar = obspar["logdlbar"][[leg["num"]]];
        spow = obspar["spow"];
  
        Which[
          leg["flav"] == "q" || leg["flav"] == "qb",
            {res = CF (7/2 b/(a + b) + 1/(a + b) 3 logdlbar + 1/2)},
          leg["flav"] == "g",
            {res = ((67/18 CA - 13/9 TF NF) b/(a + b)
                    + 1/(a + b) (11/3 CA - 4/3 TF NF) logdlbar + 1/3 TF NF
                    + 2/3 1/(a + b) spow/2 (xq + xqb - 1)/(xq^2 + xqb^2) CA
                    - 4/3 1/(a + b) spow/2 (xq + xqb - 1)/(xq^2 + xqb^2) TF NF)}
        ];

        1/(1 - (2 lambda)/(a + b)) res
      ]

  End[]
EndPackage[]