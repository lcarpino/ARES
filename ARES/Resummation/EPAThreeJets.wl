(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: EPAThreeJets *)

(* :Author: *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)


BeginPackage["ARES`Resummation`EPAThreeJets`"]

  ResumLLPT::usage = ""
  ResumNLLPT::usage = ""
  ResumNNLLPT::usage = ""

  ResumLLNP::usage = ""
  ResumNLLNP::usage = ""
  ResumNNLLNP::usage = ""

  Begin["`Private`"]

    Needs["ARES`QCD`Constants`"]
    Needs["ARES`QCD`AlphaS`"]
    Needs["ARES`QCD`ScaleChoices`"]
    Needs["ARES`Logarithms`LogarithmPT`"]
    Needs["ARES`NPShift`MilanFactor`"]
    Needs["ARES`Logarithms`LogarithmNP`"]
    Needs["ARES`Logarithms`ScaleChoices`"]
    Needs["ARES`EPA`MatrixElements`"]
    Needs["ARES`Radiator`SoftRadiator`"]
    Needs["ARES`Radiator`HardCollinearRadiator`"]
    Needs["ARES`Radiator`DerivativeRadiator`"]
    Needs["ARES`HardConstants`CollinearConstant`"]
    Needs["ARES`MultipleEmission`AdditiveInterface`"]

    (* PT resummation *)

    PTOpts = 
      {
        "xmuR"  -> 1,
        "logXV" -> 0,
        "TransferFunctions" -> Identity,
        "RadiatorScheme" -> "Physical"
      };
   

    Options[ResumLLPT] = PTOpts;

    ResumLLPT[alphaS_?NumericQ, logV_?NumericQ, Event_?AssociationQ, obsSC_?AssociationQ,
       OptionsPattern[]] :=

      Module[
        {
          xmuR  = OptionValue["xmuR"],
          logXV = OptionValue["logXV"],
          TransferFunctions = OptionValue["TransferFunctions"],
          RadiatorScheme = OptionValue["RadiatorScheme"],
          a,
          legs, dipoles, xq, xqb,
          lambda, Rs,
          res
        },
  
        a = obsSC["ktpow"];

        xq      = Event["xq"];
        xqb     = Event["xqb"];
        legs    = Event["legs"];
        dipoles = Event["dipoles"];

        lambda = alphaS beta0 LtildePT[Exp[-logV], Exp[logXV]];
  
        (* check Landau pole *)
        If[lambda > a/2, Return[0, Module]];

        Rs = Rads[lambda, alphaS, xmuR, logXV, 0, dipoles, obsSC];
  
        res = M3sq[xq, xqb] Exp[-Rs]
      ]


    Options[ResumNLLPT] = PTOpts;

    ResumNLLPT[alphaS_?NumericQ, logV_?NumericQ, Event_?AssociationQ, obsSC_?AssociationQ,
        OptionsPattern[]] :=
      Module[
        {
          xmuR  = OptionValue["xmuR"],
          logXV = OptionValue["logXV"],
          TransferFunctions = OptionValue["TransferFunctions"],
          RadiatorScheme = OptionValue["RadiatorScheme"],
          a,
          legs, dipoles, xq, xqb,
          lambda, Rs, Rhc, RpNLL, RsNNLL, mFNLL,
          res
        },
  
        a = obsSC["ktpow"];

        xq      = Event["xq"];
        xqb     = Event["xqb"];
        legs    = Event["legs"];
        dipoles = Event["dipoles"];

        lambda = alphaS beta0 LtildePT[Exp[-logV], Exp[logXV]];

        (* check Landau pole *)
        If[lambda > a/2, Return[0, Module]];

        Rs =  Rads[lambda, alphaS, xmuR, logXV, 1, dipoles, obsSC];
        Rhc = Radhc[lambda, alphaS, xmuR, logXV, 1, legs, obsSC];
        RpNLL =  RadpNLL[lambda, legs, obsSC];
        (* RsNNLL = RadsNNLL[lambda, alphaS, legs, obsSC]; *)
        (*
        mFNLL = (FNLL[RpNLL] + alphaS beta0 FpNNLL[RpNLL, RsNNLL, alphaS] (-logXV)
                 + 1/2 (alphaS beta0)^2 FsNNNLL[RpNLL, RsNNLL, alphaS] (-logXV)^2);
        *)
        mFNLL = FNLL[RpNLL];
  
        res = M3sq[xq, xqb] Exp[-Rs-Rhc] mFNLL
      ]


    Options[ResumNNLLPT] = PTOpts;

    ResumNNLLPT[alphaS_?NumericQ, logV_?NumericQ, Event_?AssociationQ, obsSC_?AssociationQ,
         OptionsPattern[]] :=
      Module[
        {
          xmuR  = OptionValue["xmuR"],
          logXV = OptionValue["logXV"],
          TransferFunctions = OptionValue["TransferFunctions"],
          RadiatorScheme = OptionValue["RadiatorScheme"],
          a,
          legs, dipoles, xq, xqb,
          lambda, Rs, RsConst, Rhc, RhcConst, RpNLL, RpNNLL, RsNNLL,
          mFNLL,
          mIsc, mIrec, mIhc, mIwa, mIcorrel, mIclust,
          mdFsc, mdFrec, mdFhc, mdFwa, mdFcorrel, mdFclust, mdFNNLL,
          mH1, mC1hc,
          res
        },

        a = obsSC["ktpow"];

        xq      = Event["xq"];
        xqb     = Event["xqb"];
        legs    = Event["legs"];
        dipoles = Event["dipoles"];
 
        mIscl     = TransferFunctions["Iscl"];
        mIrecl    = TransferFunctions["Irecl"];
        mIhcl     = TransferFunctions["Ihcl"];
        mIwaab    = TransferFunctions["Iwaab"];
        mIcorrell = TransferFunctions["Icorrell"];
        mIclustl  = TransferFunctions["Iclustl"];

        lambda = alphaS beta0 LtildePT[Exp[-logV], Exp[logXV]];

        (* check Landau pole *)
        If[lambda > a/2, Return[0, Module]];
 
        Rs  = Rads[lambda, alphaS, xmuR, logXV, 2, dipoles, obsSC];
        Rhc = Radhc[lambda, alphaS, xmuR, logXV, 2, legs, obsSC];
        RsConst  = Rads[0, alphaS, xmuR, logXV, 2, dipoles, obsSC];
        RhcConst = Radhc[0, alphaS, xmuR, logXV, 2, legs, obsSC];

        RpNLL = RadpNLL[lambda, legs, obsSC];
        RsNNLL = RadsNNLL[lambda, alphaS, legs, obsSC];
        mFNLL = (FNLL[RpNLL]
                  + alphaS beta0 lambda FpNNLL[RpNLL, RsNNLL, alphaS] Log[xmuR^2]
                  + alphaS beta0 FpNNLL[RpNLL, RsNNLL, alphaS] (-logXV));


        mH1 = Virt3[xq, xqb]/M3sq[xq, xqb];
        mC1hc = C1hc[lambda, xq, xqb, legs, obsSC];
  
        mdFsc     = dFsc[lambda, RpNLL, alphaS, legs, obsSC];
        mdFrec    = dFrec[lambda, RpNLL, alphaS, legs, obsSC, mIrecl];
        mdFhc     = dFhc[lambda, RpNLL, alphaS, legs, obsSC];
        mdFwa     = dFwa[lambda, RpNLL, alphaS, dipoles, obsSC, mIwaab];
        mdFcorrel = dFcorrel[lambda, RpNLL, alphaS, legs, obsSC, mIcorrell];
        mdFclust  = dFclust[lambda, RpNLL, alphaS, legs, obsSC, mIclustl];
  
        mdFNNLL = mdFsc + mdFrec + mdFhc + mdFwa + mdFcorrel + mdFclust;

        Which[
          RadiatorScheme == "Physical",
            res = (M3sq[xq, xqb] Exp[-Rs-Rhc] (mFNLL (1
                     + alphaS/(2 Pi) mC1hc + alphaS/(2 Pi) mH1)
                     + alphaS/Pi mdFNNLL)),
          RadiatorScheme == "ConstantFree",
            res = (M3sq[xq, xqb] Exp[-(Rs-RsConst)-(Rhc-RhcConst)] (mFNLL (1
                     + alphaS/(2 Pi) mC1hc + alphaS/(2 Pi) mH1 - RsConst - RhcConst)
                     + alphaS/Pi mdFNNLL))
        ];

        res
      ]

    (* NP resummation *)

    NPOpts =
      {
        "xmuR"  -> 1,
        "logXV" -> 0,
        "TransferFunctions" -> Identity,
        "RadiatorScheme" -> "Physical",
        "DeltaNP" -> Identity,
        "Q"      -> MZ,
        "alpha0" -> 0.52,
        "muI"    -> 2.0
      };

    Options[ResumLLNP] = NPOpts;

    ResumLLNP[alphaS_?NumericQ, logV_?NumericQ, Event_?AssociationQ, obsSC_?AssociationQ,
       OptionsPattern[]] :=

      Module[
        {
          xmuR  = OptionValue["xmuR"],
          logXV = OptionValue["logXV"],
          TransferFunctions = OptionValue["TransferFunctions"],
          RadiatorScheme = OptionValue["RadiatorScheme"],
          DeltaNP = OptionValue["DeltaNP"],
          Q = OptionValue["Q"],
          alpha0 = OptionValue["alpha0"],
          muI = OptionValue["muI"],
          a,
          legs, dipoles, xq, xqb,
          dv, lambda, Rs,
          res
        },
  
        a = obsSC["ktpow"];

        xq      = Event["xq"];
        xqb     = Event["xqb"];
        legs    = Event["legs"];
        dipoles = Event["dipoles"];

        dv = deltav[alphaS, Q, alpha0, muI, DeltaNP];

        (* check shift is possible *)
        If[Exp[-logV] - dv  < 0, Return[0, Module]];

        lambda = alphaS beta0 LtildeNP[Exp[-logV], dv, Exp[logXV]];
  
        (* check Landau pole *)
        If[lambda > a/2, Return[0, Module]];

        Rs = Rads[lambda, alphaS, xmuR, logXV, 0, dipoles, obsSC];
  
        res = M3sq[xq, xqb] Exp[-Rs]
      ]

    Options[ResumNLLNP] = NPOpts;

    ResumNLLNP[alphaS_?NumericQ, logV_?NumericQ, Event_?AssociationQ, obsSC_?AssociationQ,
        OptionsPattern[]] :=
      Module[
        {
          xmuR  = OptionValue["xmuR"],
          logXV = OptionValue["logXV"],
          TransferFunctions = OptionValue["TransferFunctions"],
          RadiatorScheme = OptionValue["RadiatorScheme"],
          DeltaNP = OptionValue["DeltaNP"],
          Q = OptionValue["Q"],
          alpha0 = OptionValue["alpha0"],
          muI = OptionValue["muI"],
          a,
          legs, dipoles, xq, xqb,
          dv, lambda, Rs, Rhc, RpNLL, RsNNLL, mFNLL,
          res
        },
  
        a = obsSC["ktpow"];

        xq      = Event["xq"];
        xqb     = Event["xqb"];
        legs    = Event["legs"];
        dipoles = Event["dipoles"];

        dv = deltav[alphaS, Q, alpha0, muI, DeltaNP];

        (* check shift is possible *)
        If[Exp[-logV] - dv  < 0, Return[0, Module]];

        lambda = alphaS beta0 LtildeNP[Exp[-logV], dv, Exp[logXV]];

        (* check Landau pole *)
        If[lambda > a/2, Return[0, Module]];

        Rs =  Rads[lambda, alphaS, xmuR, logXV, 1, dipoles, obsSC];
        Rhc = Radhc[lambda, alphaS, xmuR, logXV, 1, legs, obsSC];
        RpNLL =  RadpNLL[lambda, legs, obsSC];
        (* RsNNLL = RadsNNLL[lambda, alphaS, legs, obsSC]; *)
        (*
        mFNLL = (FNLL[RpNLL] + alphaS beta0 FpNNLL[RpNLL, RsNNLL, alphaS] (-logXV)
                 + 1/2 (alphaS beta0)^2 FsNNNLL[RpNLL, RsNNLL, alphaS] (-logXV)^2);
        *)
        mFNLL = FNLL[RpNLL];
  
        res = M3sq[xq, xqb] Exp[-Rs-Rhc] mFNLL
      ]


    Options[ResumNNLLNP] = NPOpts;

    ResumNNLLNP[alphaS_?NumericQ, logV_?NumericQ, Event_?AssociationQ, obsSC_?AssociationQ,
         OptionsPattern[]] :=
      Module[
        {
          xmuR  = OptionValue["xmuR"],
          logXV = OptionValue["logXV"],
          TransferFunctions = OptionValue["TransferFunctions"],
          RadiatorScheme = OptionValue["RadiatorScheme"],
          DeltaNP = OptionValue["DeltaNP"],
          Q = OptionValue["Q"],
          alpha0 = OptionValue["alpha0"],
          muI = OptionValue["muI"],
          a,
          legs, dipoles, xq, xqb,
          dv, lambda, Rs, RsConst, Rhc, RhcConst, RpNLL, RpNNLL, RsNNLL,
          mFNLL,
          mIsc, mIrec, mIhc, mIwa, mIcorrel, mIclust,
          mdFsc, mdFrec, mdFhc, mdFwa, mdFcorrel, mdFclust, mdFNNLL,
          mH1, mC1hc,
          res
        },

        a = obsSC["ktpow"];

        xq      = Event["xq"];
        xqb     = Event["xqb"];
        legs    = Event["legs"];
        dipoles = Event["dipoles"];
 
        mIscl     = TransferFunctions["Iscl"];
        mIrecl    = TransferFunctions["Irecl"];
        mIhcl     = TransferFunctions["Ihcl"];
        mIwaab    = TransferFunctions["Iwaab"];
        mIcorrell = TransferFunctions["Icorrell"];
        mIclustl  = TransferFunctions["Iclustl"];

        dv = deltav[alphaS, Q, alpha0, muI, DeltaNP];

        (* check shift is possible *)
        If[Exp[-logV] - dv  < 0, Return[0, Module]];

        lambda = alphaS beta0 LtildeNP[Exp[-logV], dv, Exp[logXV]];

        (* check Landau pole *)
        If[lambda > a/2, Return[0, Module]];
 
        Rs  = Rads[lambda, alphaS, xmuR, logXV, 2, dipoles, obsSC];
        Rhc = Radhc[lambda, alphaS, xmuR, logXV, 2, legs, obsSC];
        RsConst  = Rads[0., alphaS, xmuR, logXV, 2, dipoles, obsSC];
        RhcConst = Radhc[0, alphaS, xmuR, logXV, 2, legs, obsSC];

        RpNLL = RadpNLL[lambda, legs, obsSC];
        RsNNLL = RadsNNLL[lambda, alphaS, legs, obsSC];
        mFNLL = (FNLL[RpNLL]
                  + alphaS beta0 lambda FpNNLL[RpNLL, RsNNLL, alphaS] Log[xmuR^2]
                  + alphaS beta0 FpNNLL[RpNLL, RsNNLL, alphaS] (-logXV));


        mH1 = Virt3[xq, xqb]/M3sq[xq, xqb];
        mC1hc = C1hc[lambda, xq, xqb, legs, obsSC];
  
        mdFsc     = dFsc[lambda, RpNLL, alphaS, legs, obsSC];
        mdFrec    = dFrec[lambda, RpNLL, alphaS, legs, obsSC, mIrecl];
        mdFhc     = dFhc[lambda, RpNLL, alphaS, legs, obsSC];
        mdFwa     = dFwa[lambda, RpNLL, alphaS, dipoles, obsSC, mIwaab];
        mdFcorrel = dFcorrel[lambda, RpNLL, alphaS, legs, obsSC, mIcorrell];
        mdFclust  = dFclust[lambda, RpNLL, alphaS, legs, obsSC, mIclustl];
  
        mdFNNLL = mdFsc + mdFrec + mdFhc + mdFwa + mdFcorrel + mdFclust;

        Which[
          RadiatorScheme == "Physical",
            res = (M3sq[xq, xqb] Exp[-Rs-Rhc] (mFNLL (1
                     + alphaS/(2 Pi) mC1hc + alphaS/(2 Pi) mH1)
                     + alphaS/Pi mdFNNLL)),
          RadiatorScheme == "ConstantFree",
            res = (M3sq[xq, xqb] Exp[-(Rs-RsConst)-(Rhc-RhcConst)] (mFNLL (1
                     + alphaS/(2 Pi) mC1hc + alphaS/(2 Pi) mH1 - RsConst - RhcConst)
                     + alphaS/Pi mdFNNLL))
        ];

        res
      ]

  End[]
EndPackage[]