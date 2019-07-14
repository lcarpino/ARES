(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: EPAThreeJets *)

(* :Author: *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)


BeginPackage["ARES`Resummation`EPAThreeJets`",
  {
    "ARES`QCD`Constants`",
  }]

  Begin["`Private`"]

    Options[LL] = {"Q" -> 91.188, "refscale" -> 91.118, 
                   "refalphas" -> 0.118, "muRstrategy" -> muRconst, "muR0" -> 1, 
                   "Xstrategy" -> logXconst, "X0" -> 1};

    LL[logV_?NumericQ, xq_?NumericQ, xqb_?NumericQ, OptionsPattern[]] :=
      Module[
        {
          Q = OptionValue["Q"],
          muRstrategy = OptionValue["muRstrategy"], 
          muR0 = OptionValue["muR0"],
          Xstrategy = OptionValue["Xstrategy"], 
          logX0 = Log[OptionValue["X0"]],
          refscale = OptionValue["refscale"], 
          refalphas = OptionValue["refalphas"],
          obspar, legs, dipoles,
          lambda, logXV, muR, alphas,
          Rs,
          res
        },
  
        legs = buildAssoclegs[xq, xqb];
        dipoles = buildAssocdips[legs];
        obspar = buildAssocdpar[dipoles, legs, xq, xqb];
  
        logX0=Log[X0];
        muR = muRstrategy[xq, xqb] muR0 Q;
        logXV = Xstrategy[dipoles, legs, obspar] + logX0;
  
        alphas = alphasFixednf[muR, 1, refalphas, refscale];
  
        lambda = alphas beta0 Ltilde[Exp[-logV], Exp[logXV], 1, 1];
  
        Rs = Rads[lambda, alphas, Q, muR, logXV, 0, dipoles, obspar];
  
        res = M3sq[xq, xqb] Exp[-\[ScriptCapitalR]s]
      ]


    Options[NLL] = {"Q" -> 91.188, "refscale" -> 91.118, 
                    "refalphas" -> 0.118, "muRstrategy" -> muRconst, "muR0" -> 1, 
                    "Xstrategy" -> logXconst, "X0" -> 1};

    NLL[logV_?NumericQ, xq_?NumericQ, xqb_?NumericQ, OptionsPattern[]] :=
      Module[
        {
          Q = OptionValue["Q"],
          muRstrategy = OptionValue["muRstrategy"], 
          muR0 = OptionValue["muR0"],
          Xstrategy = OptionValue["Xstrategy"], 
          logX0 = Log[OptionValue["X0"]],
          refscale = OptionValue["refscale"], 
          refalphas = OptionValue["refalphas"],
          obspar, legs, dipoles,
          lambda, logXV, muR, alphas,
          Rs, Rhc, Rp,
          FNLL,
          res
        },
  
        legs = buildAssoclegs[xq, xqb];
        dipoles = buildAssocdips[legs];
        obspar = buildAssocdpar[dipoles, legs, xq, xqb];
  
        (*logX0=Log[X0];*)
        muR = muRstrategy[xq, xqb] muR0 Q;
        logXV = Xstrategy[dipoles, legs, obspar] + logX0;
  
        alphas = alphasFixednf[muR, 2, refalphas, refscale];
  
        lambda = alphas beta0 Ltilde[Exp[-logV], Exp[logXV], 1, 1];
  
        Rs = Rads[lambda, alphas, Q, muR, logXV, 1, dipoles, obspar];
        Rhc = Radhc[lambda, alphas, Q, muR, logXV, 1, legs, obspar];
        Rp = RadpNLL[lambda, legs, obspar];
        FNLL = FNLL[Rp];
  
        res = M3sq[xq, xqb] Exp[-Rs - Rhc] FNLL
      ]

  End[]
EndPackage[]