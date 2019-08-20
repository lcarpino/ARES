(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Expansion *)

(* :Author: *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)


BeginPackage["ARES`Driver`Expansion`"]

  Expansion::usage = ""
  ExpansionCoefficient::usage = ""

  Begin["`Private`"]

    Needs["ARES`Event`EventThreeJets`"]
    Needs["ARES`EPA`MatrixElements`"]
    Needs["ARES`QCD`Constants`"]
    Needs["ARES`QCD`AlphaS`"]
    Needs["ARES`QCD`ScaleChoices`"]
    Needs["ARES`Logarithms`ScaleChoices`"]
    Needs["ARES`Logarithms`LogarithmPT`"]
    Needs["ARES`Expansion`CombinedExpansion`"]

    Options[Expansion] =
      {
        "Order" -> "NNLL",
        "AlphaSOrder" -> "All",
        "Q" -> MZ,
        "RadiatorScheme" -> "Physical",
        "muRstrategy"  -> muRConst,  "muR0" -> 1,
        "LogXstrategy" -> LogXconst, "X0"   -> 1,
        "refscale" -> MZ, "refalphas" -> AlphaSMZ
      };

    Expansion[xq_?NumericQ, xqb_?NumericQ, logV_?NumericQ,
              obs_?AssociationQ, OptionsPattern[]] :=

      Module[
        {
          Order = OptionValue["Order"],
          asOrder = OptionValue["AlphaSOrder"],
          Q = OptionValue["Q"],
          RadiatorScheme = OptionValue["RadiatorScheme"],
          muRstrategy = OptionValue["muRstrategy"],
          muR0 = OptionValue["muR0"],
          LogXstrategy = OptionValue["LogXstrategy"],
          X0 = OptionValue["X0"],
          refscale = OptionValue["refscale"],
          refalphas = OptionValue["refalphas"],
          event, dipoles, legs,
          obsSC,
          muR, xmuR, logX0, logXV,
          OrderExpansion, Ltilde,
          ExpansionOpts, alphaSOpts,
          res0, res1, res2, res
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

        (*
        (* set up expansion options*)
        Which[
          Order == "LL",
            OrderExpansion = 0,
          Order == "NLL",
            OrderExpansion = 1,
          Order == "NNLL",
            OrderExpansion = 2
        ];
        *)

        ExpansionOpts =
          {
            "Order" -> Order,
            "xmuR"  -> xmuR,
            "logXV" -> logXV,
            "TransferFunctions" -> obs["TransferFunctions"],
            "RadiatorScheme" -> RadiatorScheme
          };

        (* set up the logarithm *)
        Ltilde = LtildePT[Exp[-logV], Exp[logXV]];

        (*
        (* LL contributions to expansion *)
        res0 = M3sq[xq, xqb];
        res1 = Hs12[event, obsSC, ExpansionOpts] Ltilde^2;
        res2 = (Hs24[event, obsSC, ExpansionOpts] Ltilde^4
               +Hs23[event, obsSC, ExpansionOpts] Ltilde^3);

        (* NLL contributions to expansion *)
        If[OrderExpansion >= 1,
          res1 = res1 + Hs11[event, obsSC, ExpansionOpts] Ltilde;
          res2 = res2 + Hs22[event, obsSC, ExpansionOpts] Ltilde^2;
        ];

        (* NNLL contributions to expansion *)
        If[OrderExpansion >= 2,
          res1 = res1 + Hs10[event, obsSC, ExpansionOpts];
          res2 = res2 + Hs21[event, obsSC, ExpansionOpts] Ltilde;
        ];
        *)

        res0 = M3sq[xq, xqb];
        res1 = (Hs12[event, obsSC, ExpansionOpts] Ltilde^2
               +Hs11[event, obsSC, ExpansionOpts] Ltilde
               +Hs10[event, obsSC, ExpansionOpts]);
        res2 = (Hs24[event, obsSC, ExpansionOpts] Ltilde^4
               +Hs23[event, obsSC, ExpansionOpts] Ltilde^3
               +Hs22[event, obsSC, ExpansionOpts] Ltilde^2
               +Hs21[event, obsSC, ExpansionOpts] Ltilde);

        Which[
          asOrder == 0,
            res = M3sq[xq, xqb],
          asOrder == 1,
            res = (alphaS/(2 Pi)) res1,
          asOrder == 2,
            res = (alphaS/(2 Pi))^2 res2,
          asOrder == "All",
            res = res0 + (alphaS/(2 Pi)) res1 + (alphaS/(2 Pi))^2 res2,
          True,
            res = 0
        ];

        res
      ]


    Options[ExpansionCoefficient] =
      {
        "Order" -> "NNLL",
        "Q" -> MZ,
        "RadiatorScheme" -> "Physical",
        "muRstrategy"  -> muRConst,  "muR0" -> 1,
        "LogXstrategy" -> LogXconst, "X0"   -> 1,
        "refscale" -> MZ, "refalphas" -> AlphaSMZ
      };

    ExpansionCoefficient[xq_?NumericQ, xqb_?NumericQ, aspow_?IntegerQ, lpow_?IntegerQ,
                         obs_?AssociationQ, OptionsPattern[]] :=

      Module[
        {
          Order = OptionValue["Order"],
          Q = OptionValue["Q"],
          RadiatorScheme = OptionValue["RadiatorScheme"],
          muRstrategy = OptionValue["muRstrategy"],
          muR0 = OptionValue["muR0"],
          LogXstrategy = OptionValue["LogXstrategy"],
          X0 = OptionValue["X0"],
          refscale = OptionValue["refscale"],
          refalphas = OptionValue["refalphas"],
          event, dipoles, legs,
          obsSC,
          muR, xmuR, logX0, logXV,
          OrderExpansion,
          ExpansionOpts, alphaSOpts, res
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

        (*
        (* set up expansion options*)
        Which[
          Order == "LL",
            OrderExpansion = 0,
          Order == "NLL",
            OrderExpansion = 1,
          Order == "NNLL",
            OrderExpansion = 2
        ];
        *)

        ExpansionOpts =
          {
            "Order" -> Order,
            "xmuR"  -> xmuR,
            "logXV" -> logXV,
            "TransferFunctions" -> obs["TransferFunctions"],
            "RadiatorScheme" -> RadiatorScheme
          };

        Which[
          aspow == 0 && lpow == 0,
            res = M3sq[xq, xqb],
          aspow == 1 && lpow == 2,
            res = Hs12[event, obsSC, ExpansionOpts],
          aspow == 1 && lpow == 1,
            res = Hs11[event, obsSC, ExpansionOpts],
          aspow == 1 && lpow == 0,
            res = Hs10[event, obsSC, ExpansionOpts],
          aspow == 2 && lpow == 4,
            res = Hs24[event, obsSC, ExpansionOpts],
          aspow == 2 && lpow == 3,
            res = Hs23[event, obsSC, ExpansionOpts],
          aspow == 2 && lpow == 2,
            res = Hs22[event, obsSC, ExpansionOpts],
          aspow == 2 && lpow == 1,
            res = Hs21[event, obsSC, ExpansionOpts],
          aspow == 2 && lpow == 0,
            res = Hs20[event, obsSC, ExpansionOpts],
          True,
            res = 0
        ];

        res
      ]

  End[]

EndPackage[]
