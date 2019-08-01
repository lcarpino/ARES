(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: EPAThreeJets *)

(* :Author: *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)


BeginPackage["ARES`Resummation`EPAThreeJets`"]

  LL::usage = ""
  NLL::usage = ""
  NNLL::usage = ""

  Begin["`Private`"]

    Needs["ARES`QCD`Constants`"]
    Needs["ARES`QCD`AlphaS`"]
    Needs["ARES`QCD`ScaleChoices`"]
    Needs["ARES`Logarithms`LogarithmPT`"]
    Needs["ARES`Logarithms`ScaleChoices`"]
    Needs["ARES`EPA`MatrixElements`"]
    Needs["ARES`Radiator`SoftRadiator`"]
    Needs["ARES`Radiator`HardCollinearRadiator`"]
    Needs["ARES`Radiator`DerivativeRadiator`"]
    Needs["ARES`HardConstants`CollinearConstant`"]
    Needs["ARES`MultipleEmission`AdditiveInterface`"]

    Options[LL] =
      {
        "xmuR"  -> 1,
        "logXV" -> 0,
        "TransferFunctions" -> Identity,
        "RadiatorScheme" -> "Physical"
      };

    LL[alphaS_?NumericQ, logV_?NumericQ, Event_?AssociationQ, obsSC_?AssociationQ,
       OptionsPattern[]] :=

      Module[
        {
          xmuR  = OptionValue["xmuR"],
          logXV = OptionValue["logXV"],
          TransferFunctions = OptionValue["TransferFunctions"],
          RadiatorScheme = OptionValue["RadiatorScheme"],
          legs, dipoles, xq, xqb,
          lambda, Rs,
          res
        },
  
        xq      = Event["xq"];
        xqb     = Event["xqb"];
        legs    = Event["legs"];
        dipoles = Event["dipoles"];

        lambda = alphaS beta0 LtildePT[Exp[-logV], Exp[logXV]];
  
        Rs = Rads[lambda, alphaS, xmuR, logXV, 0, dipoles, obsSC];
  
        res = M3sq[xq, xqb] Exp[-Rs]
      ]


    Options[NLL] =
      {
        "xmuR"  -> 1,
        "logXV" -> 0,
        "TransferFunctions" -> Identity,
        "RadiatorScheme" -> "Physical"
      };

    NLL[alphaS_?NumericQ, logV_?NumericQ, Event_?AssociationQ, obsSC_?AssociationQ,
        OptionsPattern[]] :=
      Module[
        {
          xmuR  = OptionValue["xmuR"],
          logXV = OptionValue["logXV"],
          TransferFunctions = OptionValue["TransferFunctions"],
          RadiatorScheme = OptionValue["RadiatorScheme"],
          legs, dipoles, xq, xqb,
          lambda, Rs, Rhc, RpNLL, RsNNLL, mFNLL,
          res
        },
  
        xq      = Event["xq"];
        xqb     = Event["xqb"];
        legs    = Event["legs"];
        dipoles = Event["dipoles"];

        lambda = alphaS beta0 LtildePT[Exp[-logV], Exp[logXV]];
  
        Rs =  Rads[lambda, alphaS, xmuR, logXV, 1, dipoles, obsSC];
        Rhc = Radhc[lambda, alphaS, xmuR, logXV, 1, legs, obsSC];
        RpNLL =  RadpNLL[lambda, legs, obsSC];
        RsNNLL = RadsNNLL[lambda, alphaS, legs, obsSC];
        mFNLL = (FNLL[RpNLL] + alphaS beta0 FpNNLL[RpNLL, RsNNLL, alphaS] (-logXV)
                 + 1/2 (alphaS beta0)^2 FsNNNLL[RpNLL, RsNNLL, alphaS] (-logXV)^2);
  
        res = M3sq[xq, xqb] Exp[-Rs-Rhc] mFNLL
      ]


    Options[NNLL] =
      {
        "xmuR"  -> 1,
        "logXV" -> 0,
        "TransferFunctions" -> Identity,
        "RadiatorScheme" -> "Physical"
      };

    NNLL[alphaS_?NumericQ, logV_?NumericQ, Event_?AssociationQ, obsSC_?AssociationQ,
         OptionsPattern[]] :=
      Module[
        {
          xmuR  = OptionValue["xmuR"],
          logXV = OptionValue["logXV"],
          TransferFunctions = OptionValue["TransferFunctions"],
          RadiatorScheme = OptionValue["RadiatorScheme"],
          legs, dipoles, xq, xqb,
          lambda, Rs, RsConst, Rhc, RpNLL, RpNNLL, RsNNLL,
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
 
        mIscl     = TransferFunctions["Iscl"];
        mIrecl    = TransferFunctions["Irecl"];
        mIhcl     = TransferFunctions["Ihcl"];
        mIwaab    = TransferFunctions["Iwaab"];
        mIcorrell = TransferFunctions["Icorrell"];
        mIclustl  = TransferFunctions["Iclustl"];

        lambda = alphaS beta0 LtildePT[Exp[-logV], Exp[logXV]];
  
        Rs = Rads[lambda, alphaS, xmuR, logXV, 2, dipoles, obsSC];
        RsConst = Rads[0, alphaS, xmuR, logXV, 2, dipoles, obsSC];
        Rhc = Radhc[lambda, alphaS, xmuR, logXV, 2, legs, obsSC];
        RpNLL = RadpNLL[lambda, legs, obsSC];
        mFNLL = FNLL[RpNLL];
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
            res = (M3sq[xq, xqb] Exp[-(Rs-RsConst)-Rhc] (mFNLL (1
                     + alphaS/(2 Pi) mC1hc + alphaS/(2 Pi) mH1 - RsConst)
                     + alphaS/Pi mdFNNLL))
        ];

        res
      ]

  End[]
EndPackage[]