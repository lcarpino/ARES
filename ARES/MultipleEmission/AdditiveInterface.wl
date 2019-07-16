(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: AdditiveInterface *)

(* :Author: *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)

BeginPackage["ARES`MultipleEmission`AdditiveInterface`",
  {
    "ARES`QCD`Constants`",
    "ARES`Radiator`DerivativeRadiator`"
  }]

  FNLL::usage = ""
  dFsc::usage = ""
  dFrec::usage = ""
  dFhc::usage = ""
  dFwa::usage = ""
  dFcorrel::usage = ""
  dFclust::usage = ""

  Begin["`Private`"]

    (* Main Interfaces *)

    (* FNLL *)
    FNLL[RpNLL_?NumericQ] :=
      Exp[-EulerGamma RpNLL]/Gamma[1 + RpNLL]


    (* FNNLL *)

    (* soft-collinear correction *)
    dFsc[lambda_?NumericQ, RpNLL_?NumericQ, AlphaS_?NumericQ,
         legs_?ListQ, obspar_?AssociationQ] := 
      Total[Map[dFscl[lambda, RpNLL, AlphaS, #, obspar] &, legs]]

    dFsc[lambda_?NumericQ, RpNLL_?NumericQ, AlphaS_?NumericQ,
         legs_?ListQ, obspar_?AssociationQ, Isc_] := 
      Total[Map[dFscl[lambda, RpNLL, AlphaS, #, obspar, Isc] &, legs]]


    (* recoil correction *)
    dFrec[lambda_?NumericQ, RpNLL_?NumericQ, AlphaS_?NumericQ,
          legs_?ListQ, obspar_?AssociationQ, Irec_] := 
      Total[Map[dFrecl[lambda, RpNLL, AlphaS, #, obspar, Irec] &, legs]]


    (* hard-collinear correction *)
    dFhc[lambda_?NumericQ, RpNLL_?NumericQ, AlphaS_?NumericQ,
         legs_?ListQ, obspar_?AssociationQ] := 
      Total[Map[dFhcl[lambda, RpNLL, AlphaS, #, obspar] &, legs]]

    dFhc[lambda_?NumericQ, RpNLL_?NumericQ, AlphaS_?NumericQ,
         legs_?ListQ, obspar_?AssociationQ, Ihc_] := 
      Total[Map[dFhcl[lambda, RpNLL, AlphaS, #, obspar, Ihc] &, legs]]


    (* wide-angle correction *)
    dFwa[lambda_?NumericQ, RpNLL_?NumericQ, AlphaS_?NumericQ,
         dips_?ListQ, obspar_?AssociationQ, Iwa_] := 
      Total[Map[dFwaab[lambda, RpNLL, AlphaS, #, obspar, Iwa] &, dips]]


    (* correlated correction *)
    dFcorrel[lambda_?NumericQ, RpNLL_?NumericQ, AlphaS_?NumericQ, 
             legs_?ListQ, obspar_?AssociationQ, Icorrel_] := 
      Total[Map[dFcorrell[lambda, RpNLL, AlphaS, #, obspar, Icorrel] &, legs]]


    (* clustering correction *)
    dFclust[lambda_?NumericQ, RpNLL_?NumericQ, AlphaS_?NumericQ,
            legs_?ListQ, obspar_?AssociationQ, Iclust_] :=
      Total[Map[dFclustl[lambda, RpNLL, AlphaS, #, obspar, Iclust] &, legs]]



    (* Implementation of specific corrections in terms of legs/dipoles *)

    (* soft-collinear correction *)
    dFscl[lambda_?NumericQ, RpNLL_?NumericQ, AlphaS_?NumericQ,
          leg_?AssociationQ, obspar_?AssociationQ] :=

      Module[
        {
          a, b, logdlbar,
          RpNNLLl, RsNNLLl,
          I, res
        },
  
        a = obspar["ktpow"];
        b = obspar["etapow"][[leg["num"]]];
        logdlbar = obspar["logdlbar"][[leg["num"]]];
  
        RpNNLLl = RadpNNLLl[lambda, AlphaS, leg, obspar];
        RsNNLLl = RadsNNLLl[lambda, AlphaS, leg, obspar];
  
        I = ((RpNNLLl + RsNNLLl logdlbar) (PolyGamma[0, 1 + RpNLL] + EulerGamma)
             + RsNNLLl/2 ((PolyGamma[0, 1 + RpNLL] + EulerGamma)^2
             - PolyGamma[1, 1 + RpNLL] + Pi^2/6));

        res = -FNLL[RpNLL] Pi/AlphaS I
      ]


    (* recoil correction *)
    dFrecl[lambda_?NumericQ, RpNLL_?NumericQ, AlphaS_?NumericQ,
           leg_?AssociationQ, obspar_?AssociationQ, Irecl_] :=

      Module[
        {a, b, I, res},
  
        a = obspar["ktpow"];
        b = obspar["etapow"][[leg["num"]]];
  
        I = Irecl[leg, obspar];
  
       res = FNLL[RpNLL] 1/(1 - (2 lambda)/(a + b)) 1/(a + b) I
      ]


    (* hard-collinear correction *)
    dFhcl[lambda_?NumericQ, RpNLL_?NumericQ, AlphaS_?NumericQ,
          leg_?AssociationQ, obspar_?AssociationQ] :=

      Module[
        {a, b, g0, I, res},
  
        a = obspar["ktpow"];
        b = obspar["etapow"][[leg["num"]]];
  
        Which[
          leg["flav"] == "q" || leg["flav"] == "qb", {ga0 = Ga0q},
          leg["flav"] == "g", {ga0 = Ga0g}
        ];

        I = -ga0 (PolyGamma[0, 1 + RpNLL] + EulerGamma);

        res = FNLL[RpNLL] 1/(1 - (2 lambda)/(a + b)) 1/(a + b) I
      ]


    (* wide angle correction *)
    dFwaab[lambda_?NumericQ, RpNLL_?NumericQ, AlphaS_?NumericQ,
           dip_?AssociationQ, obspar_?AssociationQ, Iwaab_] :=

      Module[
        {a, xa, xb, I, res},
  
          a = obspar["ktpow"];
          xa = dip["legs"][[1]]["x"];
          xb = dip["legs"][[2]]["x"];
  
          I = Iwaab[dip, obspar];
  
          res = FNLL[RpNLL] dip["col"]/a 1/(1 - (2 lambda)/a) I
      ]


    (* correlated correction *)
    dFcorrell[lambda_?NumericQ, RpNLL_?NumericQ, AlphaS_?NumericQ, 
              leg_?AssociationQ, obspar_?AssociationQ, Icorrell_] :=

      Module[
        {a, b, col, I, RsNNLLl, res},
  
        a = obspar["ktpow"];
        b = obspar["etapow"][[leg["num"]]];
        col = leg["col"];
  
        I = Icorrell[leg, obspar];
        RsNNLLl = RadsNNLLl[lambda, AlphaS, leg, obspar];
  
        res = -FNLL[RpNLL] (lambda RsNNLLl)/(2 a beta0 AlphaS) I
      ]


    (* clustering correction *)
    dFclustl[lambda_?NumericQ, RpNLL_?NumericQ, AlphaS_?NumericQ, 
             leg_?AssociationQ, obspar_?AssociationQ, Iclustl_] := 0


  End[]

EndPackage[]