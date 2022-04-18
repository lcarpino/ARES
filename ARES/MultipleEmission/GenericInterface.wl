(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: GenericInterface *)

(* :Author: *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)

BeginPackage["ARES`MultipleEmission`GenericInterface`"]

  FNLL::usage = ""
  dFsc::usage = ""
  dFrec::usage = ""
  dFhc::usage = ""
  dFwa::usage = ""
  dFcorrel::usage = ""
  dFclust::usage = ""

  Begin["`Private`"]

    (* FNLL *)
    FNLL[Rp_?NumericQ] :=
      Exp[-EulerGamma Rp]/Gamma[1 + Rp]


    (* soft-collinear correction *)
    dFscl[lambda_?NumericQ, RpNLL_?NumericQ, AlphaS_?NumericQ,
          leg_?AssociationQ, obspar_?AssociationQ, Iscl_] :=

      Module[
        {a, b, I, res},

        a = obspar["ktpow"];
        b = obspar["etapow"][[leg["num"]]];

        I = Iscl[lambda, RpNLL, AlphaS, leg, obspar];

        res = -FNLL[RpNLL] Pi/AlphaS I
      ]

    dFsc[lambda_?NumericQ, RpNLL_?NumericQ, AlphaS_?NumericQ,
         legs_?ListQ, obspar_?AssociationQ] :=
      Total[Map[dFscl[lambda, RpNLL, AlphaS, #, obspar] &, legs]]


    (* recoil correction *)
    dFrecl[lambda_?NumericQ, RpNLL_?NumericQ,
           leg_?AssociationQ, obspar_?AssociationQ] :=

      Module[
        {a, b, I, res},

        a = obspar["ktpow"];
        b = obspar["etapow"][[leg["num"]]];

        (* I = \[ScriptCapitalI]recl[leg, obspar]; *)

       res = FNLL[RpNLL] 1/(1 - (2 lambda)/(a + b)) 1/(a + b) I
      ]

    dFrec[lambda_?NumericQ, RpNLL_?NumericQ,
          legs_?ListQ, obspar_?AssociationQ] :=
      Total[Map[dFrecl[lambda, RpNLL, #, obspar] &, legs]]


    (* hard-collinear correction *)
    dFhcl[lambda_?NumericQ, RpNLL_?NumericQ,
          leg_?AssociationQ, obspar_?AssociationQ] :=

      Module[
        {a, b, I, res},

        a = obspar["ktpow"];
        b = obspar["etapow"][[leg["num"]]];

        (* I = \[ScriptCapitalI]hcl[RpNLL, leg, obspar]; *)

        res = FNLL[RpNLL] 1/(1 - (2 lambda)/(a + b)) 1/(a + b) I
      ]

    dFhc[lambda_?NumericQ, RpNLL_?NumericQ,
         legs_?ListQ, obspar_?AssociationQ] :=
      Total[Map[dFhcl[lambda, RpNLL, #, obspar] &, legs]]


    (* wide angle correction *)
    dFwaab[lambda_?NumericQ, RpNLL_?NumericQ,
           dip_?AssociationQ, obspar_?AssociationQ] :=

      Module[
        {a, xa, xb, I, res},

          a = obspar["ktpow"];
          xa = dip["legs"][[1]]["x"];
          xb = dip["legs"][[2]]["x"];

          (* I = \[ScriptCapitalI]waab[dip, obspar]; *)

          res = FNLL[RpNLL] dip["col"]/a 1/(1 - (2 lambda)/a) I
      ]

    dFwa[lambda_?NumericQ, RpNLL_?NumericQ,
         dips_?ListQ, obspar_?AssociationQ] :=
      Total[Map[dFwaab[lambda, RpNLL, #, obspar] &, dips]]


    (* correlated correction *)
    dFcorrell[lambda_?NumericQ, RpNLL_?NumericQ, AlphaS_?NumericQ,
              leg_?AssociationQ, obspar_?AssociationQ] :=

      Module[
        {a, b, col, I, RsNNLLl, res},

        a = obspar["ktpow"];
        b = obspar["etapow"][[leg["num"]]];
        col = leg["col"];

      (*I = \[ScriptCapitalI]correll[leg, obspar]; *)
        RsNNLLl = col RadsNNLLabl[lambda, AlphaS, a, b];

        res = -FNLL[RpNLL] (lambda RsNNLLl)/(2 a beta0 AlphaS) I
      ]

    dFcorrel[lambda_?NumericQ, RpNLL_?NumericQ, AlphaS_?NumericQ,
             legs_?ListQ, obspar_?AssociationQ] :=
      Total[Map[dFcorrell[lambda, RpNLL, AlphaS, #, obspar] &, legs]]

    (* clustering correction *)
    dFclust[lambda_?NumericQ, RpNLL_?NumericQ, AlphaS_?NumericQ,
            legs_?ListQ, obspar_?AssociationQ] := 0

  End[]

EndPackage[]