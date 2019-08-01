(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SoftRadiator *)

(* :Author: *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)

BeginPackage["ARES`Radiator`SoftRadiator`", "ARES`QCD`Constants`"]

  g1::usage = ""
  g2::usage = ""
  g3::usage = ""
  gm3::usage = ""

  g1p::usage = ""
  g1s::usage = ""
  g2p::usage = ""

  RadpNLLabl::usage = ""
  RadsNNLLabl::usage = ""
  RadpNNLLabl::usage = ""

  Radsab::usage = ""
  Rads::usage = ""

  Begin["`Private`"]

  (* massless radiator *)

  g1[lambda_Symbol, a_Symbol, b_Symbol] :=
    ((a + b - 2 lambda) Log[1 - 2 lambda/(a + b)] \
     - (a - 2 lambda) Log[1 - 2 lambda/a])/(2 Pi b beta0 lambda)

  g1[lambda_?NumericQ, a_?NumericQ, b_?NumericQ] :=
    If[lambda == 0,
      0,
      ((a + b - 2 lambda) Log[1 - 2 lambda/(a + b)] \
       - (a - 2 lambda) Log[1 - 2 lambda/a])/(2 Pi b beta0 lambda)]

  g2[lambda_, a_, b_] :=
    (K1 (a Log[1 - (2 lambda)/a] - (a + b) Log[1 - (2 lambda)/(a + b)])) \
      /(4 Pi^2 b beta0^2) \
    + (beta1 (a + b) Log[1 - (2 lambda)/(a + b)]^2) \
      /(4 Pi b beta0^3) \
    + (beta1 (a + b) Log[1 - (2 lambda)/(a + b)]) \
      /(2 Pi b beta0^3) \
    - (a beta1 Log[1 - (2 lambda)/a] (Log[1 - (2 lambda)/a] + 2)) \
      /(4 Pi b beta0^3)

  g3[lambda_, a_, b_] :=
    (beta1^2 (a + b)^2 (a - 2 lambda) Log[1 - (2 lambda)/(a + b)]^2 \
      - 4 b lambda^2 (beta0 beta2 + beta1^2)) \
      /(4 b beta0^4 (a - 2 lambda) (a + b - 2 lambda)) \
    - (a Log[1 - (2 lambda)/a] (2 beta0 beta2 (a - 2 lambda) \
      + a beta1^2 Log[1 - (2 lambda)/a] + 4 beta1^2 lambda)) \
      /(4 b beta0^4 (a - 2 lambda)) \
    + ((a + b) Log[1 - (2 lambda)/(a + b)] (beta0 beta2 (a + b - 2 lambda) \
      + 2 beta1^2 lambda)) \
      /(2 b beta0^4 (a + b - 2 lambda)) \
    + (K1 beta1 (a^2 (a + b - 2 lambda) Log[1 - (2 lambda)/a] \
      - (a + b)^2 (a - 2 lambda) Log[1 - (2 lambda)/(a + b)] + 6 b lambda^2)) \
      /(4 Pi b beta0^3 (a - 2 lambda) (a + b - 2 lambda)) \
    - (K2 2 lambda^2) \
      /(8 Pi^2 (a - 2 lambda) (a + b - 2 lambda) beta0^2)

  (* mass correction to radiator *)

  gm3[lambda_, a_, b_] :=
    - Zeta[2] lambda/(a + b - 2 lambda)

  (* derivatives of the g functions of the massless radiator *)

  g1p[lambda_Symbol, a_Symbol, b_Symbol] := 
    (a Log[1 - (2 lambda)/a] - (a + b) Log[1 - (2 lambda)/(a + b)]) \
      /(2 b Pi lambda^2 beta0)

  g1p[lambda_?NumericQ, a_?NumericQ, b_?NumericQ] := 
    If[lambda == 0,
      0,
      (a Log[1 - (2 lambda)/a] - (a + b) Log[1 - (2 lambda)/(a + b)]) \
        /(2 b Pi lambda^2 beta0)]

  g1s[lambda_Symbol, a_Symbol, b_Symbol] := 
    -(2/(Pi (a - 2 lambda) (a + b - 2 lambda) lambda beta0)) \
      - (a Log[1 - (2 lambda)/a] - (a + b) Log[1 - (2 lambda)/(a + b)]) \
      /(b Pi lambda^3 beta0)

  g1s[lambda_?NumericQ, a_?NumericQ, b_?NumericQ] := 
    If[lambda == 0, 
      0,
      -(2/(Pi (a - 2 lambda) (a + b - 2 lambda) \
        lambda beta0)) - (a Log[1 - (2 lambda)/a] \
        - (a + b) Log[1 - (2 lambda)/(a + b)])/(b Pi lambda^3 beta0)]

  g1t[lambda_?NumericQ, a_?NumericQ, b_?NumericQ] :=
    If[lambda == 0,
      0,
      ((2 (3 a (a + b) - 8 (2 a + b) lambda + 20 lambda^2))/(beta0 (a - 
        2 lambda)^2 (a + b - 2 lambda)^2 lambda^2 Pi) + (3 a Log[1 - (2 lambda)/a] - 
        3 (a + b) Log[1 - (2 lambda)/(a + b)])/(b beta0 lambda^4 Pi))]

  g2p[lambda_, a_, b_] :=
    (-b K1 lambda beta0 + Pi (2 b lambda + \
      a (a + b - 2 lambda) Log[1 - (2 lambda)/a] \
      - (a + b) (a - 2 lambda) Log[1 - (2 lambda)/(a + b)]) beta1) \
      /(b Pi^2 (a - 2 lambda) (a + b - 2 lambda) beta0^3)

  g2s[lambda_, a_, b_] :=
    ((-b beta0 K1 (a (a + b) - 4 lambda^2) + 
     2 a beta1 (a + b - 2 lambda)^2 Pi Log[1 - (2 lambda)/a] - 
     2 (a + b) beta1 (a - 2 lambda)^2 Pi Log[
     1 - (2 lambda)/(a + b)])/(b beta0^3 (a - 2 lambda)^2 (a + b - 
     2 lambda)^2 Pi^2))

  (* derivative of the massless radiator *)

  RadpNLLabl[lambda_, a_, b_] :=
    (Log[1 - (2 lambda)/(a + b)] - Log[1 - (2 lambda)/a])/(b Pi beta0)

  RadsNNLLabl[lambda_, alphas_, a_, b_] := 
    alphas/Pi 2/((a - 2 lambda) (a + b - 2 lambda))

  RadtNNNLLabl[lambda_, alphas_, a_, b_] := 
    (4 alphas^2 beta0 (2 a + b - 4 lambda))/((a - 2 lambda)^2 (a + b - 2 lambda)^2 Pi)

  RadpNNLLabl[lambda_, alphas_, a_, b_] := 
    alphas/(b Pi^2 beta0^2 (a - 2 lambda) (a + b - 2 lambda)) \
      (b K1 beta0 lambda - 2 Pi b beta1 lambda \
      - Pi a (a + b - 2 lambda) beta1 Log[1 - (2 lambda)/a] \
      + Pi (a + b) (a -2 lambda) beta1 Log[1 - (2 lambda)/(a + b)])


  (* Soft Radiator with parts for checks, need to find a way to compose this into blocks
     that can be easily called. now sure how to do this right now *)
  (*
  (* full soft radiator *)

  Radsab[lambda_, alphas_, xmuR_, logXV_, order_?IntegerQ,
         dip_?AssociationQ, obspar_?AssociationQ] :=

    Module[
      {
        a, b1, b2,
        xa, xb,
        logd1bar, logd2bar, log2d1bar, log2d2bar,
        leg1res = 0, leg2res = 0,
        resLL = 0, resNLL = 0, resNNLL = 0, res = 0
      },

      a = obspar["ktpow"];
      b1 = obspar["etapow"][[dip["legs"][[1]]["num"]]];
      b2 = obspar["etapow"][[dip["legs"][[2]]["num"]]];
      logd1bar = obspar["logdabbar"][dip["num"]][[1]];
      logd2bar = obspar["logdabbar"][dip["num"]][[2]];
      log2d1bar = obspar["log2dabbar"][dip["num"]][[1]];
      log2d2bar = obspar["log2dabbar"][dip["num"]][[2]];
 
      xa = dip["legs"][[1]]["x"];
      xb = dip["legs"][[2]]["x"];
 
      (* lambda < a/2 *)
      If[lambda > a/2, Return[0, Module]];
  
      (* LL contribution *)
      If[order >= 0,
        leg1res = (-lambda/(alphas beta0) g1[lambda, a, b1]
                   -lambda^2 g1p[lambda, a, b1] Log[xmuR^2/(xa + xb - 1)]
                   -alphas beta1/beta0 lambda^2 g1p[lambda, a, b1] Log[xmuR^2/(xa + xb - 1)]
                   -alphas (beta0 lambda^2 g1p[lambda, a, b1]
                   +beta0/2 lambda^3 g1s[lambda, a, b1]) Log[xmuR^2/(xa + xb - 1)]^2
                   -(g1[lambda, a, b1] + lambda g1p[lambda, a, b1]) (-logXV)
                   - (alphas beta0) (g1p[lambda, a, b1] + 1/2 lambda g1s[lambda, a, b1]) (-logXV)^2
                   - (alphas beta0)^2 (1/2 g1s[lambda, a, b1] + 1/6 lambda g1t[lambda, a, b1]) (-logXV)^3
                   - (alphas beta0) (2 lambda g1p[lambda, a, b1] + lambda^2 g1s[lambda, a, b1]) Log[xmuR^2/(xa + xb - 1)] (-logXV)
                   - (alphas beta0)^2 (g1p[lambda, a, b1] + 2 lambda g1s[lambda, a, b1] + 1/2 lambda^2 g1t[lambda, a, b1]) Log[xmuR^2/(xa + xb - 1)] (-logXV)^2);
        leg2res = (-lambda/(alphas beta0) g1[lambda, a, b2]
                   -lambda^2 g1p[lambda, a, b2] Log[xmuR^2/(xa + xb - 1)]
                   -alphas beta1/beta0 lambda^2 g1p[lambda, a, b2] Log[xmuR^2/(xa + xb - 1)]
                   -alphas (beta0 lambda^2 g1p[lambda, a, b2]
                   +beta0/2 lambda^3 g1s[lambda, a, b2]) Log[xmuR^2/(xa + xb - 1)]^2
                   -(g1[lambda, a, b2] + lambda g1p[lambda, a, b2]) (-logXV)
                   - (alphas beta0) (g1p[lambda, a, b2] + 1/2 lambda g1s[lambda, a, b2]) (-logXV)^2
                   - (alphas beta0)^2 (1/2 g1s[lambda, a, b2] + 1/6 lambda g1t[lambda, a, b2]) (-logXV)^3
                   - (alphas beta0) (2 lambda g1p[lambda, a, b2] + lambda^2 g1s[lambda, a, b2]) Log[xmuR^2/(xa + xb - 1)] (-logXV)
                   - (alphas beta0)^2 (g1p[lambda, a, b2] + 2 lambda g1s[lambda, a, b2] + 1/2 lambda^2 g1t[lambda, a, b2]) Log[xmuR^2/(xa + xb - 1)] (-logXV)^2);

        resLL = leg1res + leg2res
      ];
  
      (* NLL contribution *)
      If[order >= 1,
        leg1res = (-g2[lambda, a, b1]
                   +RadpNLLabl[lambda, a, b1] logd1bar
                   -alphas beta0 lambda g2p[lambda, a, b1] Log[xmuR^2/(xa + xb - 1)]
                   +lambda RadsNNLLabl[lambda, alphas, a, b1] logd1bar Log[xmuR^2/(xa + xb -1)]
                   -alphas beta0 g2p[lambda, a, b1] (-logXV)
                   -1/2 (alphas beta0)^2 g2s[lambda, a, b1] (-logXV)^2
                   +RadsNNLLabl[lambda, alphas, a, b1] logd1bar (-logXV)
                   +1/2 RadtNNNLLabl[lambda, alphas, a, b1] logd1bar (-logXV)^2
                   -alphas^2 beta0^2 (g2p[lambda, a, b1]
                     + lambda g2s[lambda, a, b1]) Log[xmuR^2/(xa + xb -1)] (-logXV)
                   +(alphas beta0 RadsNNLLabl[lambda, alphas, a, b1]
                     + lambda RadtNNNLLabl[lambda, alphas, a, b1]) logd1bar Log[xmuR^2/(xa + xb -1)] (-logXV));
        leg2res = (-g2[lambda, a, b2]
                   +RadpNLLabl[lambda, a, b2] logd2bar
                   -alphas beta0 lambda g2p[lambda, a, b2] Log[xmuR^2/(xa + xb - 1)]
                   +lambda RadsNNLLabl[lambda, alphas, a, b2] logd2bar Log[xmuR^2/(xa + xb -1)]
                   -alphas beta0 g2p[lambda, a, b2] (-logXV)
                   -1/2 (alphas beta0)^2 g2s[lambda, a, b2] (-logXV)^2
                   +RadsNNLLabl[lambda, alphas, a, b2] logd2bar (-logXV)
                   +1/2 RadtNNNLLabl[lambda, alphas, a, b2] logd2bar (-logXV)^2
                   -alphas^2 beta0^2 (g2p[lambda, a, b2]
                     + lambda g2s[lambda, a, b2]) Log[xmuR^2/(xa + xb -1)] (-logXV)
                   +(alphas beta0 RadsNNLLabl[lambda, alphas, a, b2]
                     + lambda RadtNNNLLabl[lambda, alphas, a, b2]) logd2bar Log[xmuR^2/(xa + xb -1)] (-logXV));

        resNLL = leg1res + leg2res
      ];
  
      (* NNLL contribution *)
      If[order >= 2,

        (*  massless and massive contributions to soft radiator *)
        leg1res = -(alphas/Pi) g3[lambda, a, b1] - alphas/Pi gm3[lambda, a, b1] \

        (* normalisation corrections *)
        + RadpNNLLabl[lambda, alphas, a, b1] logd1bar \
        + RadsNNLLabl[lambda, alphas, a, b1]/2 log2d1bar \

        (* XV scale variation *)
        - RadpNNLLabl[lambda, alphas, a, b1] logXV - RadsNNLLabl[lambda, alphas, a, b1]/2 logXV^2 \

        (* mixing of normalisation and XV scale terms *)
        - RadsNNLLabl[lambda, alphas, a, b1] logd1bar logXV \

        (* running of RpNLL XV scale term *)
        - lambda RadsNNLLabl[lambda, alphas, a, b1] logXV Log[xmuR^2/(xa + xb - 1)];



        (*  massless and massive contributions to soft radiator *)
        leg2res = -(alphas/Pi) g3[lambda, a, b2] - alphas/Pi gm3[lambda, a, b2] \

        (* normalisation corrections *)
        + RadpNNLLabl[lambda, alphas, a, b2] logd2bar \
        + RadsNNLLabl[lambda, alphas, a, b2]/2 log2d2bar \

        (* XV scale variation *)
        - RadpNNLLabl[lambda, alphas, a, b2] logXV - RadsNNLLabl[lambda, alphas, a, b2]/2 logXV^2 \

        (* mixing of normalisation and XV scale terms *)
        - RadsNNLLabl[lambda, alphas, a, b2] logd2bar logXV \

        (* running of RpNLL XV scale term *)
        - lambda RadsNNLLabl[lambda, alphas, a, b2] logXV Log[xmuR^2/(xa + xb - 1)];

        resNNLL = leg1res + leg2res
      ];



      res = dip["col"]/2 (resLL + resNLL + resNNLL)
    ]
    *)

  RadsOpts =
    {
      "RadiatorScheme" -> "Physical"
    };

  Options[Rads]   = RadsOpts;
  Options[Radsab] = RadsOpts;

  (* full soft radiator *)

  Radsab[lambda_, alphas_, xmuR_, logXV_, order_?IntegerQ,
         dip_?AssociationQ, obspar_?AssociationQ, OptionsPattern[]] :=

    Module[
      {
        RadiatorScheme = OptionValue["RadiatorScheme"],
        a, b1, b2,
        xa, xb,
        logd1bar, logd2bar, log2d1bar, log2d2bar,
        leg1res = 0, leg2res = 0,
        resLL = 0, resNLL = 0, resNNLL = 0, res = 0
      },

      a = obspar["ktpow"];
      b1 = obspar["etapow"][[dip["legs"][[1]]["num"]]];
      b2 = obspar["etapow"][[dip["legs"][[2]]["num"]]];
      logd1bar = obspar["logdabbar"][dip["num"]][[1]];
      logd2bar = obspar["logdabbar"][dip["num"]][[2]];
      log2d1bar = obspar["log2dabbar"][dip["num"]][[1]];
      log2d2bar = obspar["log2dabbar"][dip["num"]][[2]];
 
      xa = dip["legs"][[1]]["x"];
      xb = dip["legs"][[2]]["x"];
 
      (* lambda < a/2 *)
      If[lambda > a/2, Return[0, Module]];
  
      (* LL contribution *)
      If[order >= 0,
        leg1res = -lambda/(alphas beta0) g1[lambda, a, b1];
        leg2res = -lambda/(alphas beta0) g1[lambda, a, b2];

        resLL = leg1res + leg2res
      ];
  
      (* NLL contribution *)
      If[order >= 1,
        leg1res = (
          - g2[lambda, a, b1]
          + RadpNLLabl[lambda, a, b1] logd1bar
          - lambda^2 g1p[lambda, a, b1] Log[xmuR^2/(xa + xb - 1)]
          - (g1[lambda, a, b1] + lambda g1p[lambda, a, b1]) (-logXV));
        leg2res = (
          - g2[lambda, a, b2]
          + RadpNLLabl[lambda, a, b2] logd2bar
          - lambda^2 g1p[lambda, a, b2] Log[xmuR^2/(xa + xb - 1)]
          - (g1[lambda, a, b2] + lambda g1p[lambda, a, b2]) (-logXV));

        resNLL = leg1res + leg2res
      ];
  
      (* NNLL contribution *)
      If[order >= 2,

        leg1res = (
          (* massless and massive contributions to soft radiator *)
          -(alphas/Pi) g3[lambda, a, b1] - alphas/Pi gm3[lambda, a, b1]

          (* normalisation corrections *)
          + RadpNNLLabl[lambda, alphas, a, b1] logd1bar
          + RadsNNLLabl[lambda, alphas, a, b1]/2 log2d1bar

          (* muR scale variations of g *)
          - alphas (beta0 lambda g2p[lambda, a, b1] + beta1/beta0 lambda^2
                      g1p[lambda, a, b1]) Log[xmuR^2/(xa + xb - 1)]
          - alphas (beta0 lambda^2 g1p[lambda, a, b1] + beta0/2 lambda^3
                      g1s[lambda, a, b1]) Log[xmuR^2/(xa + xb - 1)]^2

          (* XV scale variations of g *)
          - alphas beta0 g2p[lambda, a, b1] (-logXV)
          - alphas beta0 (g1p[lambda, a, b1] + 1/2 lambda g1s[lambda, a, b1]) (-logXV)^2

          (* muR and XV scale variations of g *)
          - (alphas beta0) (2 lambda g1p[lambda, a, b1]
              + lambda^2 g1s[lambda, a, b1]) Log[xmuR^2/(xa + xb - 1)] (-logXV)

          (* muR scale variations of RpNLL *)
          + lambda RadsNNLLabl[lambda, alphas, a, b1] logd1bar Log[xmuR^2/(xa + xb -1)]

          (* XV scale variations of RpNLL *)
          + RadsNNLLabl[lambda, alphas, a, b1] logd1bar (-logXV));

        leg2res = (
          (* massless and massive contributions to soft radiator *)
          -(alphas/Pi) g3[lambda, a, b2] - alphas/Pi gm3[lambda, a, b2]

          (* normalisation corrections *)
          + RadpNNLLabl[lambda, alphas, a, b2] logd2bar
          + RadsNNLLabl[lambda, alphas, a, b2]/2 log2d2bar

          (* muR scale variations of g *)
          - alphas (beta0 lambda g2p[lambda, a, b2] + beta1/beta0 lambda^2
                      g1p[lambda, a, b2]) Log[xmuR^2/(xa + xb - 1)]
          - alphas (beta0 lambda^2 g1p[lambda, a, b2] + beta0/2 lambda^3
                      g1s[lambda, a, b2]) Log[xmuR^2/(xa + xb - 1)]^2

          (* XV scale variations of g *)
          - alphas beta0 g2p[lambda, a, b2] (-logXV)
          - (alphas beta0) (g1p[lambda, a, b2] + 1/2 lambda g1s[lambda, a, b2]) (-logXV)^2

          (* muR and XV scale variations of g *)
          - alphas beta0 (2 lambda g1p[lambda, a, b2]
              + lambda^2 g1s[lambda, a, b2]) Log[xmuR^2/(xa + xb - 1)] (-logXV)

          (* muR scale variations of RpNLL *)
          + lambda RadsNNLLabl[lambda, alphas, a, b2] logd2bar Log[xmuR^2/(xa + xb -1)]

          (* XV scale variations of RpNLL *)
          + RadsNNLLabl[lambda, alphas, a, b2] logd2bar (-logXV));


          (*
          (* Radiator scheme choice *)
          If[ RadiatorScheme == "ConstantFree",
            leg1res = (leg1res - RadsNNLLabl[0, alphas, a, b1]/2 log2d1bar
                               - RadsNNLLabl[0, alphas, a, b1] logd1bar (-logXV));
            leg2res = (leg2res - RadsNNLLabl[0, alphas, a, b2]/2 log2d2bar
                               - RadsNNLLabl[0, alphas, a, b2] logd2bar (-logXV));
          ];
          *)

        resNNLL = leg1res + leg2res
      ];


      res = dip["col"]/2 (resLL + resNLL + resNNLL)
    ]


  Rads[lambda_?NumericQ, alphas_?NumericQ, xmuR_?NumericQ, logXV_?NumericQ,
       order_?IntegerQ, dips_?ListQ, obspar_?AssociationQ, Opts: OptionsPattern[]] :=
    Total[Map[Radsab[lambda, alphas, xmuR, logXV, order, #, obspar, Opts] &, dips]];

  End[]
EndPackage[]