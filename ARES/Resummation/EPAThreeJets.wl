(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: EPAThreeJets *)

(* :Author: *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)


BeginPackage["ARES`Resummation`EPAThreeJets`",
  {
    "ARES`QCD`Constants`",
    "ARES`QCD`AlphaS`",
    "ARES`QCD`ScaleChoices`",
    "ARES`Logarithms`LogarithmPT`",
    "ARES`Logarithms`ScaleChoices`",
    "ARES`EPA`MatrixElements`",
    "ARES`Radiator`SoftRadiator`",
    "ARES`Radiator`HardCollinearRadiator`",
    "ARES`Radiator`DerivativeRadiator`",
    "ARES`MultipleEmission`AdditiveInterface`"
  }]

  LL::usage = ""
  NLL::usage = ""
  NNLL::usage = ""

  Begin["`Private`"]

    Options[LL] =
      {
        "Q" -> MZ,
        "muRstrategy" -> muRConst,  "muR0" -> 1, 
        "Xstrategy"   -> LogXConst, "X0"   -> 1,
        "RadiatorScheme" -> "Physical",
        "refscale" -> MZ, "refalphas" -> AlphaSMZ
      };

    LL[logV_?NumericQ, Event_?AssociationQ, obsSC_?AssociationQ,
       OptionsPattern[]] :=

      Module[
        {
          Q = OptionValue["Q"],
          muRstrategy = OptionValue["muRstrategy"], 
          muR0 = OptionValue["muR0"],
          Xstrategy = OptionValue["Xstrategy"], 
          logX0 = Log[OptionValue["X0"]],
          refscale = OptionValue["refscale"], 
          refalphas = OptionValue["refalphas"],
          legs, dipoles, xq, xqb,
          lambda, logXV, muR, alphas,
          Rs,
          res
        },
  
        xq      = Event["xq"];
        xqb     = Event["xqb"];
        legs    = Event["legs"];
        dipoles = Event["dipoles"];

        muR = muRstrategy[xq, xqb] muR0 Q;
        logXV = Xstrategy[dipoles, legs, obsSC] + logX0;
        alphas = AlphaSFixedNF[muR, 1];
        lambda = alphas beta0 LtildePT[Exp[-logV], Exp[logXV]];
  
        Rs = Rads[lambda, alphas, Q, muR, logXV, 0, dipoles, obsSC];
  
        res = M3sq[xq, xqb] Exp[-Rs]
      ]


    Options[NLL] =
      {
        "Q" -> MZ,
        "muRstrategy" -> muRConst,  "muR0" -> 1, 
        "Xstrategy"   -> LogXConst, "X0"   -> 1,
        "RadiatorScheme" -> "Physical",
        "refscale" -> MZ, "refalphas" -> AlphaSMZ
      };

    NLL[logV_?NumericQ, Event_?AssociationQ, obsSC_?AssociationQ, OptionsPattern[]] :=
      Module[
        {
          Q = OptionValue["Q"],
          muRstrategy = OptionValue["muRstrategy"], 
          muR0 = OptionValue["muR0"],
          Xstrategy = OptionValue["Xstrategy"], 
          logX0 = Log[OptionValue["X0"]],
          refscale = OptionValue["refscale"], 
          refalphas = OptionValue["refalphas"],
          legs, dipoles, xq, xqb,
          lambda, logXV, muR, alphas,
          Rs, Rhc, Rp,
          Fnll,
          res
        },
  
        xq      = Event["xq"];
        xqb     = Event["xqb"];
        legs    = Event["legs"];
        dipoles = Event["dipoles"];

        muR = muRstrategy[xq, xqb] muR0 Q;
        logXV = Xstrategy[dipoles, legs, obsSC] + logX0;
        alphas = AlphaSFixedNF[muR, 2];
  
        lambda = alphas beta0 LtildePT[Exp[-logV], Exp[logXV]];
  
        Rs = Rads[lambda, alphas, Q, muR, logXV, 1, dipoles, obsSC];
        Rhc = Radhc[lambda, alphas, Q, muR, logXV, 1, legs, obsSC];
        Rp = RadpNLL[lambda, legs, obsSC];
        Fnll = FNLL[Rp];
  
        res = M3sq[xq, xqb] Exp[-Rs-Rhc] Fnll
      ]


    Options[NNLL] =
      {
        "Q" -> MZ,
        "muRstrategy" -> muRconst,  "muR0" -> 1, 
        "Xstrategy"   -> logXconst, "X0"   -> 1,
        "RadiatorScheme" -> "Physical",
        "refscale" -> MZ, "refalphas" -> AlphaSMZ
      };

    NNLL[logV_?NumericQ, xq_?NumericQ, xqb_?NumericQ, OptionsPattern[]] :=
      Module[
        {
          Q = OptionValue["Q"],
          muRstrategy = OptionValue["muRstrategy"], 
          muR0 = OptionValue["muR0"],
          Xstrategy = OptionValue["Xstrategy"], 
          logX0 = Log[OptionValue["X0"]],
          refscale = OptionValue["refscale"], 
          refalphas = OptionValue["refalphas"],
          subEndPoint = OptionValue["subEndPoint"],
          obspar, legs, dipoles,
          lambda, logXV, muR, alphas,
          Rs, RsEndPoint, Rhc, RpNLL, RpNNLL, RsNNLL,
          FNLL,
          dFsc, dFrec, dFhc, dFwa, dFcorrel, dFclust, dFNNLL,
          H1, C1hc,
          res
        },
  
        legs = buildAssoclegs[xq, xqb];
        dipoles = buildAssocdips[legs];
        obspar = buildAssocdpar[dipoles, legs, xq, xqb];
  
        muR = muRstrategy[xq, xqb] muR0 Q;
        logXV = Xstrategy[dipoles, legs, obspar] + logX0;
  
        alphas = alphasFixednf[muR, 3, refalphas, refscale];
  
        lambda = alphas beta0 Ltilde[Exp[-logV], Exp[logXV], 1, 1];
  
        Rs = Rads[lambda, alphas, Q, muR, logXV, 2, dipoles, obspar];
        RsEndPoint = Rads[0, alphas, Q, muR, logXV, 2, dipoles, obspar];
        Rhc = Radhc[lambda, alphas, Q, muR, logXV, 2, legs, obspar];
        RpNLL = RadpNLL[lambda, legs, obspar];
        FNLL = FNLL[RpNLL];
        H1 = virt3[xq, xqb];
        C1hc = dC1hc[lambda, xq, xqb, legs, obspar];
  
        dFsc = dFsc[lambda, RpNLL, alphas, legs, obspar];
        dFrec = dFrec[lambda, RpNLL, legs, obspar];
        dFhc = dFhc[lambda, RpNLL, legs, obspar];
        dFwa = dFwa[lambda, RpNLL, dipoles, obspar];
        dFcorrel = dFcorrel[lambda, RpNLL, alphas, legs, obspar];
        dFclust = 0;
  
        dFNNLL = dFsc + dFrec + dFhc + dFwa + dFcorrel + dFclust;
  
        If[subEndPoint,
          res = (Exp[-(Rs - RsEndPoint) - Rhc]
                 (M3sq[xq, xqb] FNLL (1 + alphas/(2 Pi) C1hc - RsEndPoint)
                  + FNLL alphas/(2 Pi) H1 + M3sq[xq, xqb] alphas/Pi dFNNLL)),
          res = (Exp[-Rs-Rhc]
                 (M3sq[xq, xqb] FNLL (1 + alphas/(2 Pi) C1hc)
                  + FNLL alphas/(2 Pi) H1 + M3sq[xq, xqb] alphas/Pi dFNNLL))
        ];

        res
      ]

  End[]
EndPackage[]