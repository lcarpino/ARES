(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: EPATwoJets *)

(* :Author: *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)


BeginPackage["ARES`Resummation`EPATwoJets`"]

  ResumLLPT::usage = ""
  ResumNLLPT::usage = ""
  ResumNNLLPT::usage = ""

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
          legs, dipoles,
          lambda, Rs,
          res
        },
  
        a = obsSC["ktpow"];

        legs    = Event["legs"];
        dipoles = Event["dipoles"];

        lambda = alphaS beta0 LtildePT[Exp[-logV], Exp[logXV]];
  
        (* check Landau pole *)
        If[lambda > a/2, Return[0, Module]];

        Rs = Rads[lambda, alphaS, xmuR, logXV, 0, dipoles, obsSC];
  
        res = Exp[-Rs]
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
          legs, dipoles,
          lambda, Rs, Rhc, RpNLL, RsNNLL, mFNLL,
          res
        },
  
        a = obsSC["ktpow"];

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
  
        res = Exp[-Rs-Rhc] mFNLL
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
          legs, dipoles,
          lambda, Rs, RsConst, Rhc, RhcConst, RpNLL, RpNNLL, RsNNLL,
          mFNLL,
          mIsc, mIrec, mIhc, mIwa, mIcorrel, mIclust,
          mdFsc, mdFrec, mdFhc, mdFwa, mdFcorrel, mdFclust, mdFNNLL,
          mH1, mC1hc,
          res
        },

        a = obsSC["ktpow"];

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


        mH1 = CF (Pi^2 - 19/2);
        mC1hc = C1hc[lambda, legs, obsSC];
  
        mdFsc     = dFsc[lambda, RpNLL, alphaS, legs, obsSC];
        mdFrec    = dFrec[lambda, RpNLL, alphaS, legs, obsSC, mIrecl];
        mdFhc     = dFhc[lambda, RpNLL, alphaS, legs, obsSC];
        mdFwa     = dFwa[lambda, RpNLL, alphaS, dipoles, obsSC, mIwaab];
        mdFcorrel = dFcorrel[lambda, RpNLL, alphaS, legs, obsSC, mIcorrell];
        mdFclust  = dFclust[lambda, RpNLL, alphaS, legs, obsSC, mIclustl];
  
        mdFNNLL = mdFsc + mdFrec + mdFhc + mdFwa + mdFcorrel + mdFclust;

        Which[
          RadiatorScheme == "Physical",
            res = (Exp[-Rs-Rhc] (mFNLL (1
                     + alphaS/(2 Pi) mC1hc + alphaS/(2 Pi) mH1)
                     + alphaS/Pi mdFNNLL)),
          RadiatorScheme == "ConstantFree",
            res = (Exp[-(Rs-RsConst)-(Rhc-RhcConst)] (mFNLL (1
                     + alphaS/(2 Pi) mC1hc + alphaS/(2 Pi) mH1 - RsConst - RhcConst)
                     + alphaS/Pi mdFNNLL))
        ];

        res
      ]

  End[]
EndPackage[]