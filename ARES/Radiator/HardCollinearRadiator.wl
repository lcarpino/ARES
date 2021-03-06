(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: HardCollinearRadiator *)

(* :Author: *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)

BeginPackage["ARES`Radiator`HardCollinearRadiator`", "ARES`QCD`Constants`"]

  h2::usage = ""
  h3::usage = ""

  h2p::usage = ""

  Radhcl::usage = ""
  Radhc::usage = ""

  Begin["`Private`"]

    (* hard-collinear radiator *)

    h2[lambda_, Ga0_, a_, b_] :=
      Ga0 1/(2 Pi beta0) Log[1 - (2 lambda)/(a + b)]

    h3[lambda_, Ga0_, Ga1_, a_, b_] :=
      Ga0 (beta1 ((a + b) Log[1 - (2 lambda)/(a + b)] + 2 lambda))/(2 beta0^2 (a + b - 2 lambda)) \
        - Ga1 lambda/(2 Pi beta0 (a + b - 2 lambda))

    (*  *)

    h2p[lambda_, Ga0_, a_, b_] :=
      -Ga0/(Pi beta0 (a + b - 2 lambda))

    h2s[lambda_, Ga0_, a_, b_] :=
      -((2 Ga0)/(beta0 (a + b - 2 lambda)^2 Pi))

    (* full hard-collinear radiator *)

    (*
    Radhcl[lambda_, alphas_, xmuR_, logXV_,
           order_?IntegerQ, leg_?AssociationQ, obspar_?AssociationQ] :=

      Module[
        {
          a, b,
          ga0, ga1,
          resNLL = 0, resNNLL = 0, res = 0
        },
   
        a = obspar["ktpow"];
        b = obspar["etapow"][[leg["num"]]];
   
        (* lambda < a/2 *)
        If[lambda > a/2, Return[0, Module]];
   
        (* determine hard leg "colour factor" *)
        Which[
          leg["flav"] == "q" || leg["flav"] == "qb", {ga0 = Ga0q, ga1 = Ga1q},
          leg["flav"] == "g", {ga0 = Ga0g, ga1 = Ga1g}
        ];
   
        If[order >= 1,
          resNLL = ( -h2[lambda, ga0, a, b]
                     -alphas beta0 lambda h2p[lambda, ga0, a, b] Log[xmuR^2]
                     -alphas beta0 h2p[lambda, ga0, a, b] (-logXV)
                     -1/2 (alphas beta0)^2 h2s[lambda, ga0, a, b] (-logXV)^2
                     -(alphas beta0)^2 (h2p[lambda, ga0, a, b]
                       + lambda h2s[lambda, ga0, a, b]) Log[xmuR^2] (-logXV))
        ];
   
        If[order >= 2,
          resNNLL = -(alphas/Pi) h3[lambda, ga0, ga1, a, b] \
            + alphas/Pi ga0/(a + b - 2 lambda) logXV
        ];
   


        res = resNLL + resNNLL
    ];
    *)

    Radhcl[lambda_, alphas_, xmuR_, logXV_,
           order_?IntegerQ, leg_?AssociationQ, obspar_?AssociationQ] :=

      Module[
        {
          a, b,
          ga0, ga1,
          resNLL = 0, resNNLL = 0, res = 0
        },
   
        a = obspar["ktpow"];
        b = obspar["etapow"][[leg["num"]]];
   
        (* lambda < a/2 *)
        If[lambda > a/2, Return[0, Module]];
   
        (* determine hard leg "colour factor" *)
        Which[
          leg["flav"] == "q" || leg["flav"] == "qb", {ga0 = Ga0q, ga1 = Ga1q},
          leg["flav"] == "g", {ga0 = Ga0g, ga1 = Ga1g}
        ];
   
        If[order >= 1,
          resNLL = -h2[lambda, ga0, a, b]
        ];
   
        If[order >= 2,
          resNNLL = (
            (* normal contribution to hard-collinear radiator *)
            - (alphas/Pi) h3[lambda, ga0, ga1, a, b]

            (* muR scale variations of h *)
            - alphas beta0 lambda h2p[lambda, ga0, a, b] Log[xmuR^2]

            (* XV scale variations of h *)
            - alphas beta0 h2p[lambda, ga0, a, b] (-logXV));
        ];
   


        res = resNLL + resNNLL
    ];


    Radhc[lambda_?NumericQ, alphas_?NumericQ, xmuR_?NumericQ,
          logXV_?NumericQ, order_?IntegerQ, legs_?ListQ, obspar_?AssociationQ] := 
      Total[Map[Radhcl[lambda, alphas, xmuR, logXV, order, #, obspar] &, legs]]

  End[]

EndPackage[]