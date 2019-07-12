(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DerivativeRadiator *)

(* :Author: *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)

BeginPackage["ARES`Radiator`DerivativeRadiator`", {"ARES`QCD`Constants`"}]

  RadpNLLl::usage = ""
  RadpNLL::usage = ""

  RadpNNLLl::usage = ""
  RadpNNLL::usage = ""

  RadsNNLLl::usage = ""
  RadsNNLL::usage = ""

  Begin["`Private`"]

    RadpNLLl[lambda_?NumericQ, leg_?AssociationQ, obspar_?AssociationQ] :=
      Module[
        {a, b},

        a = obspar["ktpow"];
        b = obspar["etapow"][[leg["num"]]];

        (* lambda < a/2 *)
        If[lambda > a/2, Return[0, Module]];

        leg[["col"]] (Log[1 - (2 lambda)/(a + b)] - Log[1 - (2 lambda)/a])/(b Pi beta0)
      ]

    RadpNLL[lambda_?NumericQ, legs_?ListQ, obspar_?AssociationQ] :=
      Total[Map[RadpNLLl[lambda, #, obspar] &, legs]]


    RadsNNLLl[lambda_?NumericQ, alphas_?NumericQ,
              leg_?AssociationQ, obspar_?AssociationQ] :=

      Module[
        {a, b},

        a = obspar["ktpow"];
        b = obspar["etapow"][[leg["num"]]];

        (* lambda < a/2 *)
        If[lambda > a/2, Return[0, Module]];

        leg[["col"]] (alphas/Pi 2/((a - 2 lambda) (a + b - 2 lambda)))
      ]

    RadsNNLL[lambda_?NumericQ, alphas_?NumericQ,
             legs_?ListQ, obspar_?AssociationQ] :=
      Total[Map[RadsNNLLl[lambda, alphas, #, obspar] &, legs]]


    RadpNNLLl[lambda_?NumericQ, alphas_?NumericQ,
              leg_?AssociationQ, obspar_?AssociationQ] :=

      Module[
        {a, b},

        a = obspar["ktpow"];
        b = obspar["etapow"][[leg["num"]]];

        (* lambda < a/2 *)
        If[lambda > a/2, Return[0, Module]];

        leg[["col"]] (alphas/(b Pi^2 beta0^2 (a - 2 lambda) (a + b - 2 lambda))
                      (b K1 beta0 lambda - 2 Pi b beta1 lambda
                      - Pi a (a + b - 2 lambda) beta1 Log[1 - (2 lambda)/a]
                      + Pi (a + b) (a -2 lambda) beta1 Log[1 - (2 lambda)/(a + b)]))

      ]

    RadpNNLL[lambda_?NumericQ, alphas_?NumericQ,
             legs_?ListQ, obspar_?AssociationQ] :=
      Total[Map[RadpNNLLl[lambda, alphas, #, obspar] &, legs]]


  End[]

EndPackage[]