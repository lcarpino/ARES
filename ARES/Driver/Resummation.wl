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
        "Q" -> MZ,
        "RadiatorScheme" -> "Physical",
        "muRstrategy"  -> muRConst,  "muR0" -> 1, 
        "logXstrategy" -> LogXConst, "X0"   -> 1,
        "refscale" -> MZ, "refalphas" -> AlphaSMZ
      };

    Resum[xq_?NumericQ, xqb_?NumericQ, logV_?NumericQ, obs_?AssociationQ,
          OptionsPattern[]] :=

      Module[
        {
          Q = OptionValue["Q"],
          RadiatorScheme = OptionValue["RadiatorScheme"],
          muRstrategy = OptionValue["muRstrategy"], 
          muR0 = OptionValue["muR0"],
          logXstrategy = OptionValue["logXstrategy"], 
          X0 = OptionValue["X0"],
          refscale = OptionValue["refscale"], 
          refalphas = OptionValue["refalphas"],
          event, dipoles, legs,
          obsSC,
          muR, xmuR, logX0, logXV,
          ResumOpts
        },

        (* set up the event *)
        event = BuildEvent[xq, xqb];
        legs = event["legs"];
        dipoles = event["dipoles"];

        (* set up the observable parametrisation for this event geometry *)
        obsSC = obs["SCParametrisation"][event];

        (* set up renormalisation scale *)
        muR = muRstrategy[xq, xqb] muR0 Q;
        xmuR = muR/Q;
        alphaS = AlphaSFixedNF[muR, 1];

        (* set up logarithm scales *)
        logX0 = Log[X0];
        logXV = logXstrategy[dipoles, legs, obsSC] + logX0;

        ResumOpts =
          {
            "xmuR"  -> xmuR,
            "logXV" -> logXV,
            "TransferFunctions" -> obs["TransferFunctions"],
            "RadiatorScheme" -> RadiatorScheme
          };

        LL[alphaS, logV, event, obsSC, ResumOpts]
      ]

  End[]

EndPackage[]
