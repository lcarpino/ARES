(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Resummation *)

(* :Author: *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)


BeginPackage["ARES`Driver`Resummation`"]

  Resum::usage = ""

  Begin["`Private`"]

    Needs["ARES`Event`EventThreeJets`"]
    Needs["ARES`QCD`Constants`"]
    Needs["ARES`QCD`AlphaS`"]
    Needs["ARES`QCD`ScaleChoices`"]
    Needs["ARES`Logarithms`ScaleChoices`"]
    Needs["ARES`Resummation`EPAThreeJets`"]

    Options[Resum] =
      {
        "Contribution" -> "PT",
        "Order" -> "NNLL",
        "Q" -> MZ,
        "RadiatorScheme" -> "Physical",
        "muRstrategy"  -> muRconst,  "muR0" -> 1, 
        "LogXstrategy" -> LogXconst, "X0"   -> 1,
        "refscale" -> MZ, "refalphas" -> AlphaSMZ,
        "muI" -> 2.0, "alpha0" -> 0.52
      };

    Resum[xq_?NumericQ, xqb_?NumericQ, logV_?NumericQ, obs_?AssociationQ,
          OptionsPattern[]] :=

      Module[
        {
          Contribution = OptionValue["Contribution"],
          Order = OptionValue["Order"],
          Q = OptionValue["Q"],
          RadiatorScheme = OptionValue["RadiatorScheme"],
          muRstrategy = OptionValue["muRstrategy"], 
          muR0 = OptionValue["muR0"],
          LogXstrategy = OptionValue["LogXstrategy"], 
          X0 = OptionValue["X0"],
          refscale = OptionValue["refscale"], 
          refalphas = OptionValue["refalphas"],
          muI = OptionValue["muI"],
          alpha0 = OptionValue["alpha0"],
          event, dipoles, legs,
          obsSC,
          muR, xmuR, logX0, logXV,
          ResumPTOpts, ResumNPOpts, alphaSOpts, res
        },

        (* set up the event *)
        event = BuildEvent[xq, xqb];
        legs = event["legs"];
        dipoles = event["dipoles"];

        (* set up the observable parametrisation for this event geometry *)
        obsSC = obs["SCParametrisation"][event];

        (* set up renormalisation scale *)
        alphaSOpts =
          {
            "refscale"  -> refscale,
            "refalphas" -> refalphas
          };

        muR = muRstrategy[xq, xqb] muR0 Q;
        xmuR = muR/Q;
        Which[
          Order == "LL",
            alphaS = AlphaSFixedNF[muR, 1, alphaSOpts],
          Order == "NLL",
            alphaS = AlphaSFixedNF[muR, 2, alphaSOpts],
          Order == "NNLL",
            alphaS = AlphaSFixedNF[muR, 3, alphaSOpts]
        ];


        (* set up logarithm scales *)
        logX0 = Log[X0];
        logXV = LogXstrategy[dipoles, legs, obsSC] + logX0;

        Which[
          Contribution == "PT",

            ResumPTOpts =
              {
                "xmuR"  -> xmuR,
                "logXV" -> logXV,
                "TransferFunctions" -> obs["TransferFunctions"],
                "RadiatorScheme" -> RadiatorScheme
              };

            Which[
              Order == "LL",
                res = ResumLLPT[alphaS, logV, event, obsSC, ResumPTOpts],
              Order == "NLL",
                res = ResumNLLPT[alphaS, logV, event, obsSC, ResumPTOpts],
              Order == "NNLL",
                res = ResumNNLLPT[alphaS, logV, event, obsSC, ResumPTOpts]
            ],

          Contribution == "NP",

            ResumNPOpts =
              {
                "xmuR"  -> xmuR,
                "logXV" -> logXV,
                "TransferFunctions" -> obs["TransferFunctions"],
                "RadiatorScheme" -> RadiatorScheme,
                "DeltaNP" -> obs["DeltaNP"][event],
                "Q" -> Q,
                "alpha0" -> alpha0,
                "muI" -> muI
              };

            Which[
              Order == "LL",
                res = ResumLLNP[alphaS, logV, event, obsSC, ResumNPOpts],
              Order == "NLL",
                res = ResumNLLNP[alphaS, logV, event, obsSC, ResumNPOpts],
              Order == "NNLL",
                res = ResumNNLLNP[alphaS, logV, event, obsSC, ResumNPOpts]
            ]

          ];


        res
      ]

  End[]

EndPackage[]
