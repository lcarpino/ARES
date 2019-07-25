(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: CollinearConstant *)

(* :Author: *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)

BeginPackage["ARES`Expansion`CollinearConstant`"]

  C1hc10::usage = ""
  C1hc21::usage = ""

  Begin["`Private`"]

    Needs["ARES`QCD`Constants`"]

    C1hc10[legs_?ListQ, obsSC_?AssociationQ, xq_?NumericQ, xqb_?NumericQ] :=
      Total[Map[C1hc10l[#, obsSC, xq, xqb] &, legs]]

    C1hc21[legs_?ListQ, obsSC_?AssociationQ, xq_?NumericQ, xqb_?NumericQ] :=
      Total[Map[C1hc21l[#, obsSC, xq, xqb] &, legs]]


    C1hcl[leg_?AssociationQ, obsSC_?AssociationQ, xq_?NumericQ, xqb_?NumericQ] :=
      Module[
        {
          a, b, logdlbar, spow, res
        },
  
        a = obsSC["ktpow"];
        b = obsSC["etapow"][[leg["num"]]];
        logdlbar = obsSC["logdlbar"][[leg["num"]]];
        spow = obsSC["spow"](*[[leg["num"]]]*);

        Which[
          leg["flav"] == "q" || leg["flav"] == "qb",
            {res = CF (7/2 b/(a + b) + 1/(a + b) 3 logdlbar + 1/2)},
          leg["flav"] == "g",
            {res = ((67/18 CA - 13/9 TF NF) b/(a + b)
                    + 1/(a + b) (11/3 CA - 4/3 TF NF) logdlbar + 1/3 TF NF
                    + 2/3 1/(a + b) spow/2 (xq + xqb - 1)/(xq^2 + xqb^2) CA
                    - 4/3 1/(a + b) spow/2 (xq + xqb - 1)/(xq^2 + xqb^2) TF NF)}
        ];

        res
      ]

    C1hc10l[leg_?AssociationQ, obsSC_?AssociationQ, xq_?NumericQ, xqb_?NumericQ] :=
      C1hcl[leg, obsSC, xq, xqb]

    C1hc21l[leg_?AssociationQ, obsSC_?AssociationQ, xq_?NumericQ, xqb_?NumericQ] :=
      Module[
        {
          a, b
        },

        a = obsSC["ktpow"];
        b = obsSC["etapow"][[leg["num"]]];

        (4 Pi beta0)/(a + b) C1hcl[leg, obsSC, xq, xqb]
      ]

  End[]
EndPackage[]