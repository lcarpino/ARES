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
    "ARES`HardConstants`CollinearConstant`",
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
        "muRstrategy" -> muRConst,  "muR0" -> 1, 
        "Xstrategy"   -> LogXConst, "X0"   -> 1,
        "RadiatorScheme" -> "Physical",
        "refscale" -> MZ, "refalphas" -> AlphaSMZ
      };

    NNLL[logV_?NumericQ, Event_?AssociationQ, obs_?AssociationQ, OptionsPattern[]] :=
      Module[
        {
          Q = OptionValue["Q"],
          muRstrategy = OptionValue["muRstrategy"], 
          muR0 = OptionValue["muR0"],
          Xstrategy = OptionValue["Xstrategy"], 
          logX0 = Log[OptionValue["X0"]],
          refscale = OptionValue["refscale"], 
          refalphas = OptionValue["refalphas"],
          RadScheme = OptionValue["RadiatorScheme"],
          legs, dipoles, xq, xqb, obsSC,
          lambda, logXV, muR, alphas,
          Rs, RsEndPoint, Rhc, RpNLL, RpNNLL, RsNNLL,
          mFNLL,
          mIsc, mIrec, mIhc, mIwa, mIcorrel, mIclust,
          mdFsc, mdFrec, mdFhc, mdFwa, mdFcorrel, mdFclust, mdFNNLL,
          mH1, mC1hc,
          res
        },

        xq      = Event["xq"];
        xqb     = Event["xqb"];
        legs    = Event["legs"];
        dipoles = Event["dipoles"];
        obsSC   = obs["SCParametrisation"][Event];
 
        mIscl     = obs["TransferFunctions"]["Iscl"];
        mIrecl    = obs["TransferFunctions"]["Irecl"];
        mIhcl     = obs["TransferFunctions"]["Ihcl"];
        mIwaab    = obs["TransferFunctions"]["Iwaab"];
        mIcorrell = obs["TransferFunctions"]["Icorrell"];
        mIclustl  = obs["TransferFunctions"]["Iclustl"];

        muR = muRstrategy[xq, xqb] muR0 Q;
        logXV = Xstrategy[dipoles, legs, obsSC] + logX0;
        alphas = AlphaSFixedNF[muR, 3];
        lambda = alphas beta0 LtildePT[Exp[-logV], Exp[logXV]];
  
        Rs = Rads[lambda, alphas, Q, muR, logXV, 2, dipoles, obsSC];
        RsEndPoint = Rads[0, alphas, Q, muR, logXV, 2, dipoles, obsSC];
        Rhc = Radhc[lambda, alphas, Q, muR, logXV, 2, legs, obsSC];
        RpNLL = RadpNLL[lambda, legs, obsSC];
        mFNLL = FNLL[RpNLL];
        mH1 = Virt3[xq, xqb];
        mC1hc = C1hc[lambda, xq, xqb, legs, obsSC];
  
        mdFsc     = dFsc[lambda, RpNLL, alphas, legs, obsSC];
        mdFrec    = dFrec[lambda, RpNLL, alphas, legs, obsSC, mIrecl];
        mdFhc     = dFhc[lambda, RpNLL, alphas, legs, obsSC];
        mdFwa     = dFwa[lambda, RpNLL, alphas, dipoles, obsSC, mIwaab];
        mdFcorrel = dFcorrel[lambda, RpNLL, alphas, legs, obsSC, mIcorrell];
        mdFclust  = dFclust[lambda, RpNLL, alphas, legs, obsSC, mIclustl];
  
        mdFNNLL = mdFsc + mdFrec + mdFhc + mdFwa + mdFcorrel + mdFclust;
  
        res = (Exp[-Rs-Rhc]
               (M3sq[xq, xqb] mFNLL (1 + alphas/(2 Pi) mC1hc)
                + mFNLL alphas/(2 Pi) mH1 + M3sq[xq, xqb] alphas/Pi mdFNNLL))
      ]

  End[]
EndPackage[]