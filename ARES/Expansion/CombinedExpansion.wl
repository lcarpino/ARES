(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: CombinedExpansion *)

(* :Author: *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)

BeginPackage["ARES`Expansion`CombinedExpansion`"]

  Hs12::usage = ""
  Hs11::usage = ""
  Hs10::usage = ""

  Hs24::usage = ""
  Hs23::usage = ""
  Hs22::usage = ""
  Hs21::usage = ""
  Hs20::usage = ""

  Begin["`Private`"]

    Needs["ARES`EPA`MatrixElements`"]
    Needs["ARES`Expansion`SoftRadiatorExpansion`"]
    Needs["ARES`Expansion`HardCollinearRadiatorExpansion`"]
    Needs["ARES`Expansion`DerivativeRadiatorExpansion`"]
    Needs["ARES`Expansion`CollinearConstant`"]
    Needs["ARES`Expansion`AdditiveInterface`"]

    MasterExpOpts =
      {
        "Order"   -> "NNLL",
        "xmuRvar" ->True,
        "xmuR" -> 1,
        "logXVvar" -> True,
        "logXV" -> 0,
        "TransferFunctions" -> Identity,
        "RadiatorScheme" -> "Physical"
      };

    Options[Hs12] = MasterExpOpts;
    Options[Hs11] = MasterExpOpts;
    Options[Hs10] = MasterExpOpts;
    Options[Hs24] = MasterExpOpts;
    Options[Hs23] = MasterExpOpts;
    Options[Hs22] = MasterExpOpts;
    Options[Hs21] = MasterExpOpts;
    Options[Hs20] = MasterExpOpts;

    Hs12[Event_?AssociationQ, obsSC_?AssociationQ, OptionsPattern[]] :=
      Module[
        {
          Order    = OptionValue["Order"],
          xmuRvar  = OptionValue["xmuRvar"],
          xmuR     = OptionValue["xmuR"],
          logXVvar = OptionValue["logXVvar"],
          logXV    = OptionValue["logXV"],
          TransferFunctions = OptionValue["TransferFunctions"],
          RadiatorScheme = OptionValue["RadiatorScheme"],
          ExpOpts
        },

        ExpOpts =
          {
            "xmuRvar"  -> xmuRvar,
            "xmuR"     -> xmuR,
            "logXVvar" -> logXVvar,
            "logXV"    -> logXV,
            "TransferFunctions" -> Identity,
            "RadiatorScheme"    -> "Physical"
          };

        Which[
          Order == "LL",
            Hs12LL[Event, obsSC, ExpOpts],
          Order == "NLL",
            Hs12LL[Event, obsSC, ExpOpts],
          Order == "NNLL",
            Hs12LL[Event, obsSC, ExpOpts]
        ]
      ]

    Hs11[Event_?AssociationQ, obsSC_?AssociationQ, OptionsPattern[]] :=
      Module[
        {
          Order    = OptionValue["Order"],
          xmuRvar  = OptionValue["xmuRvar"],
          xmuR     = OptionValue["xmuR"],
          logXVvar = OptionValue["logXVvar"],
          logXV    = OptionValue["logXV"],
          TransferFunctions = OptionValue["TransferFunctions"],
          RadiatorScheme = OptionValue["RadiatorScheme"],
          ExpOpts
        },

        ExpOpts =
          {
            "xmuRvar"  -> xmuRvar,
            "xmuR"     -> xmuR,
            "logXVvar" -> logXVvar,
            "logXV"    -> logXV,
            "TransferFunctions" -> Identity,
            "RadiatorScheme"    -> "Physical"
          };

        Which[
          Order == "LL",
            0,
          Order == "NLL",
            Hs11NLL[Event, obsSC, ExpOpts],
          Order == "NNLL",
            Hs11NNLL[Event, obsSC, ExpOpts]
        ]
      ]

    Hs10[Event_?AssociationQ, obsSC_?AssociationQ, OptionsPattern[]] :=
      Module[
        {
          Order    = OptionValue["Order"],
          xmuRvar  = OptionValue["xmuRvar"],
          xmuR     = OptionValue["xmuR"],
          logXVvar = OptionValue["logXVvar"],
          logXV    = OptionValue["logXV"],
          TransferFunctions = OptionValue["TransferFunctions"],
          RadiatorScheme = OptionValue["RadiatorScheme"],
          ExpOpts
        },

        ExpOpts =
          {
            "xmuRvar"  -> xmuRvar,
            "xmuR"     -> xmuR,
            "logXVvar" -> logXVvar,
            "logXV"    -> logXV,
            "TransferFunctions" -> Identity,
            "RadiatorScheme"    -> "Physical"
          };

        Which[
          Order == "LL",
            0,
          Order == "NLL",
            0,
          Order == "NNLL",
            Hs10NNLL[Event, obsSC, ExpOpts]
        ]
      ]

    Hs24[Event_?AssociationQ, obsSC_?AssociationQ, OptionsPattern[]] :=
      Module[
        {
          Order    = OptionValue["Order"],
          xmuRvar  = OptionValue["xmuRvar"],
          xmuR     = OptionValue["xmuR"],
          logXVvar = OptionValue["logXVvar"],
          logXV    = OptionValue["logXV"],
          TransferFunctions = OptionValue["TransferFunctions"],
          RadiatorScheme = OptionValue["RadiatorScheme"],
          ExpOpts
        },

        ExpOpts =
          {
            "xmuRvar"  -> xmuRvar,
            "xmuR"     -> xmuR,
            "logXVvar" -> logXVvar,
            "logXV"    -> logXV,
            "TransferFunctions" -> Identity,
            "RadiatorScheme"    -> "Physical"
          };

        Which[
          Order == "LL",
            Hs24LL[Event, obsSC, ExpOpts],
          Order == "NLL",
            Hs24LL[Event, obsSC, ExpOpts],
          Order == "NNLL",
            Hs24LL[Event, obsSC, ExpOpts]
        ]
      ]

    Hs23[Event_?AssociationQ, obsSC_?AssociationQ, OptionsPattern[]] :=
      Module[
        {
          Order    = OptionValue["Order"],
          xmuRvar  = OptionValue["xmuRvar"],
          xmuR     = OptionValue["xmuR"],
          logXVvar = OptionValue["logXVvar"],
          logXV    = OptionValue["logXV"],
          TransferFunctions = OptionValue["TransferFunctions"],
          RadiatorScheme = OptionValue["RadiatorScheme"],
          ExpOpts
        },

        ExpOpts =
          {
            "xmuRvar"  -> xmuRvar,
            "xmuR"     -> xmuR,
            "logXVvar" -> logXVvar,
            "logXV"    -> logXV,
            "TransferFunctions" -> Identity,
            "RadiatorScheme"    -> "Physical"
          };

        Which[
          Order == "LL",
            Hs23LL[Event, obsSC, ExpOpts],
          Order == "NLL",
            Hs23NLL[Event, obsSC, ExpOpts],
          Order == "NNLL",
            Hs23NNLL[Event, obsSC, ExpOpts]
        ]
      ]

    Hs22[Event_?AssociationQ, obsSC_?AssociationQ, OptionsPattern[]] :=
      Module[
        {
          Order    = OptionValue["Order"],
          xmuRvar  = OptionValue["xmuRvar"],
          xmuR     = OptionValue["xmuR"],
          logXVvar = OptionValue["logXVvar"],
          logXV    = OptionValue["logXV"],
          TransferFunctions = OptionValue["TransferFunctions"],
          RadiatorScheme = OptionValue["RadiatorScheme"],
          ExpOpts
        },

        ExpOpts =
          {
            "xmuRvar"  -> xmuRvar,
            "xmuR"     -> xmuR,
            "logXVvar" -> logXVvar,
            "logXV"    -> logXV,
            "TransferFunctions" -> Identity,
            "RadiatorScheme"    -> "Physical"
          };

        Which[
          Order == "LL",
            0,
          Order == "NLL",
            Hs22NLL[Event, obsSC, ExpOpts],
          Order == "NNLL",
            Hs22NNLL[Event, obsSC, ExpOpts]
        ]
      ]

    Hs21[Event_?AssociationQ, obsSC_?AssociationQ, OptionsPattern[]] :=
      Module[
        {
          Order    = OptionValue["Order"],
          xmuRvar  = OptionValue["xmuRvar"],
          xmuR     = OptionValue["xmuR"],
          logXVvar = OptionValue["logXVvar"],
          logXV    = OptionValue["logXV"],
          TransferFunctions = OptionValue["TransferFunctions"],
          RadiatorScheme = OptionValue["RadiatorScheme"],
          ExpOpts
        },

        ExpOpts =
          {
            "xmuRvar"  -> xmuRvar,
            "xmuR"     -> xmuR,
            "logXVvar" -> logXVvar,
            "logXV"    -> logXV,
            "TransferFunctions" -> Identity,
            "RadiatorScheme"    -> "Physical"
          };

        Which[
          Order == "LL",
            0,
          Order == "NLL",
            0,
          Order == "NNLL",
            Hs21NNLL[Event, obsSC, ExpOpts]
        ]
      ]

    Hs20[Event_?AssociationQ, obsSC_?AssociationQ, OptionsPattern[]] :=
      Module[
        {
          Order    = OptionValue["Order"],
          xmuRvar  = OptionValue["xmuRvar"],
          xmuR     = OptionValue["xmuR"],
          logXVvar = OptionValue["logXVvar"],
          logXV    = OptionValue["logXV"],
          TransferFunctions = OptionValue["TransferFunctions"],
          RadiatorScheme = OptionValue["RadiatorScheme"],
          ExpOpts
        },

        ExpOpts =
          {
            "xmuRvar"  -> xmuRvar,
            "xmuR"     -> xmuR,
            "logXVvar" -> logXVvar,
            "logXV"    -> logXV,
            "TransferFunctions" -> Identity,
            "RadiatorScheme"    -> "Physical"
          };

        Which[
          Order == "LL",
            0,
          Order == "NLL",
            0,
          Order == "NNLL",
            0           
        ]
      ]


    ExpOpts =
      {
        "xmuRvar" ->True,
        "xmuR" -> 1,
        "logXVvar" -> True,
        "logXV" -> 0,
        "TransferFunctions" -> Identity,
        "RadiatorScheme" -> "Physical"
      };

    Options[Hs12LL]    = ExpOpts;
    Options[Hs12NLL]   = ExpOpts;
    Options[Hs12NNLL]  = ExpOpts;

    Options[Hs11NLL]   = ExpOpts;
    Options[Hs11NNLL]  = ExpOpts;

    Options[Hs10NNLL]  = ExpOpts;

    Options[Hs24LL]    = ExpOpts;
    Options[Hs24NLL]   = ExpOpts;
    Options[Hs24NNLL]  = ExpOpts;

    Options[Hs23LL]    = ExpOpts;
    Options[Hs23NLL]   = ExpOpts;
    Options[Hs23NNLL]  = ExpOpts;

    Options[Hs22NLL]   = ExpOpts;
    Options[Hs22NNLL]  = ExpOpts;

    Options[Hs21NNLL]  = ExpOpts;

    Options[Hs20NNNLL] = ExpOpts;

    Hs12LL[Event_?AssociationQ, obsSC_?AssociationQ, OptionsPattern[]] :=
      Module[
        {
          xmuRvar  = OptionValue["xmuRvar"],
          xmuR     = OptionValue["xmuR"],
          logXVvar = OptionValue["logXVvar"],
          logXV    = OptionValue["logXV"],
          TransferFunctions = OptionValue["TransferFunctions"],
          RadiatorScheme = OptionValue["RadiatorScheme"],
          legs, nlegs, dipoles, ndipoles, xq, xqb,
          mG12,
          mHs12, mHs12bar, mHs12hat, mHs12bh,
          mCoeff, res
        },

        xq       = Event["xq"];
        xqb      = Event["xqb"];
        legs     = Event["legs"];
        nlegs    = Length[legs];
        dipoles  = Event["dipoles"];
        ndipoles = Length[dipoles];
  
        mG12 = G12[dipoles, obsSC];
  
        (* Standard expansion *)
        mHs12 = Total[mG12];

        (* Barred expansion*)
        mHs12bar = mHs12;

        (* Hatted expansion *)
        mHs12hat = mHs12;

        (* Barred and Hatted expansion *)
        mHs12bh  = mHs12;
  

        Which[
          ((xmuRvar == True) && (logXVvar == True)),
            mCoeff = mHs12bh,
          ((xmuRvar == True) && (logXVvar == False)),
            mCoeff = mHs12bar,
          ((xmuRvar == False) && (logXVvar == True)),
            mCoeff = mHs12hat,
          ((xmuRvar == False) && (logXVvar == False)),
            mCoeff = mHs12
        ];

        res = M3sq[xq, xqb] mCoeff
      ]
 
    Hs12NLL[Event_?AssociationQ, obsSC_?AssociationQ, opts: OptionsPattern[]] :=
      Hs12LL[Event, obsSC, opts]

    Hs12NNLL[Event_?AssociationQ, obsSC_?AssociationQ, opts: OptionsPattern[]] :=
      Hs12LL[Event, obsSC, opts]


    Hs11NLL[Event_?AssociationQ, obsSC_?AssociationQ, OptionsPattern[]] :=
      Module[
        {
          xmuRvar  = OptionValue["xmuRvar"],
          xmuR     = OptionValue["xmuR"],
          logXVvar = OptionValue["logXVvar"],
          logXV    = OptionValue["logXV"],
          TransferFunctions = OptionValue["TransferFunctions"],
          RadiatorScheme = OptionValue["RadiatorScheme"],
          legs, nlegs, dipoles, ndipoles, xq, xqb,
          mG12, mG11, mH11,
          mHs12, mHs12bar, mHs12hat, mHs12bh,
          mHs11, mHs11bar, mHs11hat, mHs11bh,
          mCoeff, res
        },
  
        xq       = Event["xq"];
        xqb      = Event["xqb"];
        legs     = Event["legs"];
        nlegs    = Length[legs];
        dipoles  = Event["dipoles"];
        ndipoles = Length[dipoles];

        mG12 = G12[dipoles, obsSC];
        mG11 = G11[dipoles, obsSC];
        mH11 = H11[legs, obsSC];
  
        mHs12 = Total[mG12];
        mHs11 = Total[mG11] + Total[mH11];

        mHs12bar = mHs12;
        mHs11bar = mHs11;

        mHs12hat = mHs12;
        mHs11hat = mHs11 + 2 Total[mG12] (-logXV);

        mHs12bh  = mHs12;
        mHs11bh  = mHs11 + 2 Total[mG12] (-logXV);


        Which[
          ((xmuRvar == True) && (logXVvar == True)),
            mCoeff = mHs11bh,
          ((xmuRvar == True) && (logXVvar == False)),
            mCoeff = mHs11bar,
          ((xmuRvar == False) && (logXVvar == True)),
            mCoeff = mHs11hat,
          ((xmuRvar == False) && (logXVvar == False)),
            mCoeff = mHs11
        ];

        res = M3sq[xq, xqb] mCoeff
      ]

    Hs10NNLL[Event_?AssociationQ, obsSC_?AssociationQ, OptionsPattern[]] :=
      Module[
        {
          xmuRvar  = OptionValue["xmuRvar"],
          xmuR     = OptionValue["xmuR"],
          logXVvar = OptionValue["logXVvar"],
          logXV    = OptionValue["logXV"],
          TransferFunctions = OptionValue["TransferFunctions"],
          RadiatorScheme = OptionValue["RadiatorScheme"],
          legs, nlegs, dipoles, ndipoles, xq, xqb,
          mG12, mG11, mG10, mH11, mH10,
          mH1, mC1hc10, mRs10,
          mIsc, mIrec, mIhc, mIwa, mIcorrel, mIclust,
          RpNLL11, mFrec10, mFwa10, mF10,
          mHs12, mHs12bar, mHs12hat, mHs12bh,
          mHs11, mHs11bar, mHs11hat, mHs11bh,
          mHs10, mHs10bar, mHs10hat, mHs10bh,
          mCoeff, res
        },

        xq       = Event["xq"];
        xqb      = Event["xqb"];
        legs     = Event["legs"];
        nlegs    = Length[legs];
        dipoles  = Event["dipoles"];
        ndipoles = Length[dipoles];
 
        mIscl     = TransferFunctions["Iscl"];
        mIrecl    = TransferFunctions["Irecl"];
        mIhcl     = TransferFunctions["Ihcl"];
        mIwaab    = TransferFunctions["Iwaab"];
        mIcorrell = TransferFunctions["Icorrell"];
        mIclustl  = TransferFunctions["Iclustl"];

        RpNLL11 = RadpNLL11[legs, obsSC];

        mG12   = G12[dipoles, obsSC],
        mG11   = G11[dipoles, obsSC],
        mG10   = Which[
                    RadiatorScheme == "Physical",
                      G10[dipoles, obsSC, {"RadiatorScheme" -> "Physical"}],
                    RadiatorScheme == "ConstantFree",
                      G10[dipoles, obsSC, {"RadiatorScheme" -> "ConstantFree"}]
                  ],
        mH11   = H11[legs, obsSC],
        mH10   = ConstantArray[0, nlegs],
        mH1     = Virt3[xq, xqb]/M3sq[xq, xqb],
        mC1hc10 = C1hc10[legs, obsSC, xq, xqb],
        mRs10 = Which[
                  RadiatorScheme == "Physical",
                    0,
                  RadiatorScheme == "ConstantFree",
                    Total[G10[dipoles, obsSC]]
                ],
        mF10    = (Frec10[RpNLL11, legs, obsSC, mIrecl]
                    + Fwa10[RpNLL11, dipoles, obsSC, mIwaab])

        mHs12 = Total[mG12];
        mHs11 = Total[mG11] + Total[mH11];
        mHs10 = Total[mG10] + Total[mH10] + mC1hc10 + mF10 + mRs10 + mH1;

        mHs12bar = mHs12;
        mHs11bar = mHs11;
        mHs10bar = mHs10;

        mHs12hat = mHs12;
        mHs11hat = mHs11 + 2 Total[mG12] (-logXV);
        mHs10hat = mHs10 + (Total[mG11] + Total[mH11]) (-logXV) + Total[mG12] (-logXV)^2;
  
        mHs12bh  = mHs12;
        mHs11bh  = mHs11 + 2 Total[mG12] (-logXV);
        mHs10bh  = mHs10 + (Total[mG11] + Total[mH11]) (-logXV) + Total[mG12] (-logXV)^2;


        Which[
          ((xmuRvar == True) && (logXVvar == True)),
            mCoeff = mHs10bh,
          ((xmuRvar == True) && (logXVvar == False)),
            mCoeff = mHs10bar,
          ((xmuRvar == False) && (logXVvar == True)),
            mCoeff = mHs10hat,
          ((xmuRvar == False) && (logXVvar == False)),
            mCoeff = mHs10
        ];

        res = M3sq[xq, xqb] mCoeff
      ]

    Hs24LL[Event_?AssociationQ, obsSC_?AssociationQ, OptionsPattern[]] :=
      Module[
        {
          xmuRvar  = OptionValue["xmuRvar"],
          xmuR     = OptionValue["xmuR"],
          logXVvar = OptionValue["logXVvar"],
          logXV    = OptionValue["logXV"],
          TransferFunctions = OptionValue["TransferFunctions"],
          RadiatorScheme = OptionValue["RadiatorScheme"],
          legs, nlegs, dipoles, ndipoles, xq, xqb,
          mG12,
          mHs12, mHs12bar, mHs12hat, mHs12bh,
          mHs11, mHs11bar, mHs11hat, mHs11bh,
          mHs10, mHs10bar, mHs10hat, mHs10bh,
          mHs24, mHs24bar, mHs24hat, mHs24bh,
          mCoeff, res
        },
  
        xq       = Event["xq"];
        xqb      = Event["xqb"];
        legs     = Event["legs"];
        nlegs    = Length[legs];
        dipoles  = Event["dipoles"];
        ndipoles = Length[dipoles];

        mG12 = G12[dipoles, obsSC];
  
        (* Standard expansion *)
        mHs12 = Total[mG12];
        mHs24 = mHs12^2/2;

        mHs12bar = mHs12;
        mHs24bar = mHs24;

        mHs12hat = mHs12;
        mHs24hat = mHs24;

        mHs12bh  = mHs12;
        mHs24bh  = mHs24;


        Which[
          ((xmuRvar == True) && (logXVvar == True)),
            mCoeff = mHs24bh,
          ((xmuRvar == True) && (logXVvar == False)),
            mCoeff = mHs24bar,
          ((xmuRvar == False) && (logXVvar == True)),
            mCoeff = mHs24hat,
          ((xmuRvar == False) && (logXVvar == False)),
            mCoeff = mHs24
        ];

        res = M3sq[xq, xqb] mCoeff
      ]


    Hs23LL[Event_?AssociationQ, obsSC_?AssociationQ, OptionsPattern[]] :=
      Module[
        {
          xmuRvar  = OptionValue["xmuRvar"],
          xmuR     = OptionValue["xmuR"],
          logXVvar = OptionValue["logXVvar"],
          logXV    = OptionValue["logXV"],
          TransferFunctions = OptionValue["TransferFunctions"],
          RadiatorScheme = OptionValue["RadiatorScheme"],
          legs, nlegs, dipoles, ndipoles, xq, xqb,
          mHs23, mHs23bar, mHs23hat, mHs23bh,
          mCoeff, res
        },

        xq       = Event["xq"];
        xqb      = Event["xqb"];
        legs     = Event["legs"];
        nlegs    = Length[legs];
        dipoles  = Event["dipoles"];
        ndipoles = Length[dipoles];

        mG23 = G23[dipoles, obsSC];
  
        mHs23 = Total[mG23];

        mHs23bar = mHs23;
  
        mHs23hat = mHs23;

        mHs23bh  = mHs23;


        Which[
          ((xmuRvar == True) && (logXVvar == True)),
            mCoeff = mHs23bh,
          ((xmuRvar == True) && (logXVvar == False)),
            mCoeff = mHs23bar,
          ((xmuRvar == False) && (logXVvar == True)),
            mCoeff = mHs23hat,
          ((xmuRvar == False) && (logXVvar == False)),
            mCoeff = mHs23
        ];

        res = M3sq[xq, xqb] mCoeff
      ]

    Hs23NLL[Event_?AssociationQ, obsSC_?AssociationQ, OptionsPattern[]] :=
      Module[
        {
          xmuRvar  = OptionValue["xmuRvar"],
          xmuR     = OptionValue["xmuR"],
          logXVvar = OptionValue["logXVvar"],
          logXV    = OptionValue["logXV"],
          TransferFunctions = OptionValue["TransferFunctions"],
          RadiatorScheme = OptionValue["RadiatorScheme"],
          legs, nlegs, dipoles, ndipoles, xq, xqb,
          mG11, mG12, mG23, mH11,
          mHs12, mHs12bar, mHs12hat, mHs12bh,
          mHs11, mHs11bar, mHs11hat, mHs11bh,
          mHs10, mHs10bar, mHs10hat, mHs10bh,
          mHs24, mHs24bar, mHs24hat, mHs24bh,
          mHs23, mHs23bar, mHs23hat, mHs23bh,
          mCoeff, res
        },

        xq       = Event["xq"];
        xqb      = Event["xqb"];
        legs     = Event["legs"];
        nlegs    = Length[legs];
        dipoles  = Event["dipoles"];
        ndipoles = Length[dipoles];

        mG12 = G12[dipoles, obsSC];
        mG11 = G11[dipoles, obsSC];
        mG23 = G23[dipoles, obsSC];
        mH11 = H11[legs, obsSC];
  
        mHs12 = Total[mG12];
        mHs11 = Total[mG11] + Total[mH11];
        mHs24 = mHs12^2/2;
        mHs23 = Total[mG23] + Total[mG12] (Total[mG11] + Total[mH11]);

        mHs12bar = mHs12;
        mHs11bar = mHs11;
        mHs24bar = mHs24;
        mHs23bar = mHs23;
  
        mHs12hat = mHs12;
        mHs11hat = mHs11 + 2 Total[mG12] (-logXV);
        mHs24hat = mHs24;
        mHs23hat = mHs23 + 4 Total[mG12]^2/2 (-logXV);

        mHs12bh  = mHs12;
        mHs11bh  = mHs11 + 2 Total[mG12] (-logXV);
        mHs24bh  = mHs24;
        mHs23bh  = mHs23 + 4 Total[mG12]^2/2 (-logXV);


        Which[
          ((xmuRvar == True) && (logXVvar == True)),
            mCoeff = mHs23bh,
          ((xmuRvar == True) && (logXVvar == False)),
            mCoeff = mHs23bar,
          ((xmuRvar == False) && (logXVvar == True)),
            mCoeff = mHs23hat,
          ((xmuRvar == False) && (logXVvar == False)),
            mCoeff = mHs23
        ];

        res = M3sq[xq, xqb] mCoeff
      ]

    Hs22NLL[Event_?AssociationQ, obsSC_?AssociationQ, OptionsPattern[]] :=
      Module[
        {
          xmuRvar  = OptionValue["xmuRvar"],
          xmuR     = OptionValue["xmuR"],
          logXVvar = OptionValue["logXVvar"],
          logXV    = OptionValue["logXV"],
          TransferFunctions = OptionValue["TransferFunctions"],
          RadiatorScheme = OptionValue["RadiatorScheme"],
          legs, nlegs, dipoles, ndipoles, xq, xqb,
          mG10, mG11, mG12,
          mG21, mG22, mG23,
          mH10, mH11,
          mH21, mH22,
          mH1, mC1hc10, mRs10,
          RpNLL11,
          mF11, mF22,
          mIsc, mIrec, mIhc, mIwa, mIcorrel, mIclust,
          mFrec10, mFwa10, mF10,
          mFsc21, mFrec21, mFhc21, mFwa21, mFcorrel21, mF21,
          mlogXab, mlogXl,
          mHs12, mHs12bar, mHs12hat, mHs12bh,
          mHs11, mHs11bar, mHs11hat, mHs11bh,
          mHs10, mHs10bar, mHs10hat, mHs10bh,
          mHs24, mHs24bar, mHs24hat, mHs24bh,
          mHs23, mHs23bar, mHs23hat, mHs23bh,
          mHs22, mHs22bar, mHs22hat, mHs22bh,
          mCoeff, res
        },
  
        xq       = Event["xq"];
        xqb      = Event["xqb"];
        legs     = Event["legs"];
        nlegs    = Length[legs];
        dipoles  = Event["dipoles"];
        ndipoles = Length[dipoles];

        RpNLL11 = RadpNLL11[legs, obsSC];

        mG12   = G12[dipoles, obsSC];
        mG11   = G11[dipoles, obsSC];
        mG23   = G23[dipoles, obsSC];
        mG22   = G22[dipoles, obsSC];

        mH11   = H11[legs, obsSC];
        mH22   = H22[legs, obsSC];

        mF22    = FNLL22[RpNLL11];

        mlogXab = LogXdipoles[dipoles, xmuR];
        mlogXl  = LogXlegs[legs, xmuR];

        mHs12 = Total[mG12];
        mHs11 = Total[mG11] + Total[mH11];
        mHs24 = Total[mG12]^2/2;
        mHs23 = Total[mG23] + Total[mG12] (Total[mG11] + Total[mH11]);
        mHs22 = Total[mG22] + Total[mH22] + mF22 + (Total[mG11] + Total[mH11])^2/2;

        mHs12bar = mHs12;
        mHs11bar = mHs11;
        mHs24bar = mHs24;
        mHs23bar = mHs23;
        mHs22bar = mHs22 + 2 Pi beta0 mG12.mlogXab;

        mHs12hat = mHs12;
        mHs11hat = mHs11 + 2 mHs12 (-logXV);
        mHs24hat = mHs24;
        mHs23hat = mHs23 + 4 mHs24 (-logXV);
        mHs22hat = (mHs22 + 4 Total[mG12]^2/2 (-logXV)^2
                          + (3 Total[mG23] + 2 Total[mG12] (Total[mG11] + Total[mH11])) (-logXV));
 
        mHs12bh  = mHs12;
        mHs11bh  = mHs11 + 2 mHs12 (-logXV);
        mHs24bh  = mHs24;
        mHs23bh  = mHs23 + 4 mHs24 (-logXV);
        mHs22bh  = (mHs22 + 4 Total[mG12]^2/2 (-logXV)^2
                          + (3 Total[mG23] + 2 Total[mG12] (Total[mG11] + Total[mH11])) (-logXV)
                          + 2 Pi beta0 mG12.mlogXab);

        Which[
          ((xmuRvar == True) && (logXVvar == True)),
            mCoeff = mHs22bh,
          ((xmuRvar == True) && (logXVvar == False)),
            mCoeff = mHs22bar,
          ((xmuRvar == False) && (logXVvar == True)),
            mCoeff = mHs22hat,
          ((xmuRvar == False) && (logXVvar == False)),
            mCoeff = mHs22
        ];

        res = M3sq[xq, xqb] mCoeff
      ]

    Hs22NNLL[Event_?AssociationQ, obsSC_?AssociationQ, OptionsPattern[]] :=
      Module[
        {
          xmuRvar  = OptionValue["xmuRvar"],
          xmuR     = OptionValue["xmuR"],
          logXVvar = OptionValue["logXVvar"],
          logXV    = OptionValue["logXV"],
          TransferFunctions = OptionValue["TransferFunctions"],
          RadiatorScheme = OptionValue["RadiatorScheme"],
          legs, nlegs, dipoles, ndipoles, xq, xqb,
          mG10, mG11, mG12,
          mG21, mG22, mG23,
          mH10, mH11,
          mH21, mH22,
          mH1, mC1hc10, mRs10,
          RpNLL11,
          mF11, mF22,
          mIsc, mIrec, mIhc, mIwa, mIcorrel, mIclust,
          mFrec10, mFwa10, mF10,
          mFsc21, mFrec21, mFhc21, mFwa21, mFcorrel21, mF21,
          mlogXab, mlogXl,
          mHs12, mHs12bar, mHs12hat, mHs12bh,
          mHs11, mHs11bar, mHs11hat, mHs11bh,
          mHs10, mHs10bar, mHs10hat, mHs10bh,
          mHs24, mHs24bar, mHs24hat, mHs24bh,
          mHs23, mHs23bar, mHs23hat, mHs23bh,
          mHs22, mHs22bar, mHs22hat, mHs22bh,
          mCoeff, res
        },
  
        xq       = Event["xq"];
        xqb      = Event["xqb"];
        legs     = Event["legs"];
        nlegs    = Length[legs];
        dipoles  = Event["dipoles"];
        ndipoles = Length[dipoles];

        mIscl     = TransferFunctions["Iscl"];
        mIrecl    = TransferFunctions["Irecl"];
        mIhcl     = TransferFunctions["Ihcl"];
        mIwaab    = TransferFunctions["Iwaab"];
        mIcorrell = TransferFunctions["Icorrell"];
        mIclustl  = TransferFunctions["Iclustl"];

        RpNLL11 = RadpNLL11[legs, obsSC];

        mG12   = G12[dipoles, obsSC];
        mG11   = G11[dipoles, obsSC];
        mG10   = Which[
                    RadiatorScheme == "Physical",
                      G10[dipoles, obsSC, {"RadiatorScheme" -> "Physical"}],
                    RadiatorScheme == "ConstantFree",
                      G10[dipoles, obsSC, {"RadiatorScheme" -> "ConstantFree"}]
                  ];
        mG23   = G23[dipoles, obsSC];
        mG22   = G22[dipoles, obsSC];

        mH11   = H11[legs, obsSC];
        mH10   = ConstantArray[0, nlegs];
        mH22   = H22[legs, obsSC];

        mF22    = FNLL22[RpNLL11];

        mH1     = Virt3[xq, xqb]/M3sq[xq, xqb];
        mC1hc10 = C1hc10[legs, obsSC, xq, xqb];
        mRs10 = Which[
                  RadiatorScheme == "Physical",
                    0,
                  RadiatorScheme == "ConstantFree",
                    Total[G10[dipoles, obsSC]]
                ];
        mF10    = (Frec10[RpNLL11, legs, obsSC, mIrecl]
                    + Fwa10[RpNLL11, dipoles, obsSC, mIwaab]);

        mlogXab = LogXdipoles[dipoles, xmuR];
        mlogXl  = LogXlegs[legs, xmuR];

        mHs12 = Total[mG12];
        mHs11 = Total[mG11] + Total[mH11];
        mHs10 = Total[mG10] + Total[mH10] + mC1hc10 + mRs10 + mF10 + mH1;
        mHs24 = Total[mG12]^2/2;
        mHs23 = Total[mG23] + Total[mG12] (Total[mG11] + Total[mH11]);
        mHs22 = Total[mG22] + Total[mH22] + mF22 \
                + Total[mG12] (Total[mG10] + mC1hc10 + mRs10 + mF10 + mH1) \
                + (Total[mG11] + Total[mH11])^2/2;

        mHs12bar = mHs12;
        mHs11bar = mHs11;
        mHs10bar = mHs10;
        mHs24bar = mHs24;
        mHs23bar = mHs23;
        mHs22bar = mHs22 + 2 Pi beta0 mG12.mlogXab;

        mHs12hat = mHs12;
        mHs11hat = mHs11 + 2 mHs12 (-logXV);
        mHs10hat = mHs10 + 1 mHs11 (-logXV) + 1 mHs12 (-logXV)^2;
        mHs24hat = mHs24;
        mHs23hat = mHs23 + 4 mHs24 (-logXV);
        mHs22hat = mHs22 + 3 mHs23 (-logXV) + 6 mHs24 (-logXV)^2;
 
        mHs12bh  = mHs12;
        mHs11bh  = mHs11 + 2 mHs12 (-logXV);
        mHs10bh  = mHs10 + 1 mHs11 (-logXV) + 1 mHs12 (-logXV)^2;
        mHs24bh  = mHs24;
        mHs23bh  = mHs23 + 4 mHs24 (-logXV);
        mHs22bh  = mHs22 + 3 mHs23 (-logXV) + 6 mHs24 (-logXV)^2 + 2 Pi beta0 mG12.mlogXab;


        Which[
          ((xmuRvar == True) && (logXVvar == True)),
            mCoeff = mHs22bh,
          ((xmuRvar == True) && (logXVvar == False)),
            mCoeff = mHs22bar,
          ((xmuRvar == False) && (logXVvar == True)),
            mCoeff = mHs22hat,
          ((xmuRvar == False) && (logXVvar == False)),
            mCoeff = mHs22
        ];

        res = M3sq[xq, xqb] mCoeff
      ]

    Hs21NNLL[Event_?AssociationQ, obsSC_?AssociationQ, OptionsPattern[]] :=
      Module[
        {
          xmuRvar  = OptionValue["xmuRvar"],
          xmuR     = OptionValue["xmuR"],
          logXVvar = OptionValue["logXVvar"],
          logXV    = OptionValue["logXV"],
          TransferFunctions = OptionValue["TransferFunctions"],
          RadiatorScheme = OptionValue["RadiatorScheme"],
          legs, nlegs, dipoles, ndipoles, xq, xqb,
          mG10, mG11, mG12,
          mG21, mG22, mG23,
          mH11, mH10, mH21, mH22,
          mH1, mC1hc10, mRs10, mC1hc21, 
          RpNLL11,
          mIsc, mIrec, mIhc, mIwa, mIcorrel, mIclust,
          mFrec10, mFwa10, mF10,
          mFsc21, mFrec21, mFhc21, mFwa21, mFcorrel21, mF21, mF22,
          mlogXab, mlogXl,
          mHs12, mHs12bar, mHs12hat, mHs12bh,
          mHs11, mHs11bar, mHs11hat, mHs11bh,
          mHs10, mHs10bar, mHs10hat, mHs10bh,
          mHs24, mHs24bar, mHs24hat, mHs24bh,
          mHs23, mHs23bar, mHs23hat, mHs23bh,
          mHs22, mHs22bar, mHs22hat, mHs22bh,
          mHs21, mHs21bar, mHs21hat, mHs21bh,
          mCoeff, res
        },

        xq       = Event["xq"];
        xqb      = Event["xqb"];
        legs     = Event["legs"];
        nlegs    = Length[legs];
        dipoles  = Event["dipoles"];
        ndipoles = Length[dipoles];

        mIscl     = TransferFunctions["Iscl"];
        mIrecl    = TransferFunctions["Irecl"];
        mIhcl     = TransferFunctions["Ihcl"];
        mIwaab    = TransferFunctions["Iwaab"];
        mIcorrell = TransferFunctions["Icorrell"];
        mIclustl  = TransferFunctions["Iclustl"];

        RpNLL11 = RadpNLL11[legs, obsSC];
  
        mG12   = G12[dipoles, obsSC];
        mG11   = G11[dipoles, obsSC];
        mG10   = Which[
                    RadiatorScheme == "Physical",
                      G10[dipoles, obsSC, {"RadiatorScheme" -> "Physical"}],
                    RadiatorScheme == "ConstantFree",
                      G10[dipoles, obsSC, {"RadiatorScheme" -> "ConstantFree"}]
                  ];
        mG23   = G23[dipoles, obsSC];
        mG22   = G22[dipoles, obsSC];
        mG21   = G21[dipoles, obsSC];

        mH11   = H11[legs, obsSC];
        mH10   = ConstantArray[0, nlegs];
        mH22   = H22[legs, obsSC];
        mH21   = H21[legs, obsSC];

        mF22    = FNLL22[RpNLL11];

        mH1     = Virt3[xq, xqb]/M3sq[xq, xqb];
        mC1hc10 = C1hc10[legs, obsSC, xq, xqb];
        mRs10 = Which[
                  RadiatorScheme == "Physical",
                    0,
                  RadiatorScheme == "ConstantFree",
                    Total[G10[dipoles, obsSC]]
                ];
        mC1hc21 = C1hc21[legs, obsSC, xq, xqb];
        mF10    = (  Frec10[RpNLL11, legs, obsSC, mIrecl]
                    + Fwa10[RpNLL11, dipoles, obsSC, mIwaab]);
        mF21    = (  Fsc21[RpNLL11, legs, obsSC]
                    + Frec21[RpNLL11, legs, obsSC, mIrecl]
                    + Fhc21[RpNLL11, legs, obsSC]
                    + Fwa21[RpNLL11, dipoles, obsSC, mIwaab]
                    + Fcorrel21[RpNLL11, legs, obsSC, mIcorrell]
                    + Fclust21[RpNLL11, legs, obsSC, mIclustl]);

        mlogXab = LogXdipoles[dipoles, xmuR];
        mlogXl  = LogXlegs[legs, xmuR];

        mHs12 = Total[mG12];
        mHs11 = Total[mG11] + Total[mH11];
        mHs10 = Total[mG10] + Total[mH10] + mC1hc10 + mRs10 + mF10 + mH1;
        mHs24 = Total[mG12]^2/2;
        mHs23 = Total[mG23] + Total[mG12] (Total[mG11] + Total[mH11]);
        mHs22 = Total[mG22] + Total[mH22] + mF22 \
                + Total[mG12] (Total[mG10] + mC1hc10 + mRs10 + mF10 + mH1) \
                + (Total[mG11] + Total[mH11])^2/2;
        mHs21 = Total[mG21] + Total[mH21] + mC1hc21 + mF21 \
                + (Total[mG11] + Total[mH11]) (Total[mG10] + mC1hc10 + mRs10 + mF10 + mH1);

        mHs12bar = mHs12;
        mHs11bar = mHs11;
        mHs10bar = mHs10;
        mHs24bar = mHs24;
        mHs23bar = mHs23;
        mHs22bar = mHs22 + 2 Pi beta0 mG12.mlogXab;
        mHs21bar = mHs21 + 2 Pi beta0 (mG11.mlogXab + mH11.mlogXl);

        mHs12hat = mHs12;
        mHs11hat = mHs11 + 2 mHs12 (-logXV);
        mHs10hat = mHs10 + 1 mHs11 (-logXV) + 1 mHs12 (-logXV)^2;
        mHs24hat = mHs24;
        mHs23hat = mHs23;
        mHs22hat = mHs22 + 3 mHs23 (-logXV) + 6 mHs24 (-logXV)^2;
        mHs21hat = mHs21 + 2 mHs22 (-logXV) + 3 mHs23 (-logXV)^2 + 4 mHs24 (-logXV)^3;
  
        mHs12bh  = mHs12;
        mHs11bh  = mHs11 + 2 mHs12 (-logXV);
        mHs10bh  = mHs10 + 1 mHs11 (-logXV) + 1 mHs12 (-logXV)^2;
        mHs24bh  = mHs24;
        mHs23bh  = mHs23;
        mHs22bh  = mHs22 + 3 mHs23 (-logXV) + 6 mHs24 (-logXV)^2 + 2 Pi beta0 mG12.mlogXab;
        mHs21bh  = mHs21 + 2 mHs22 (-logXV) + 3 mHs23 (-logXV)^2 + 4 mHs24 (-logXV)^3 \
                         + 2 Pi beta0 (mG11.mlogXab + 2 mG12.mlogXab (-logXV) + mH11.mlogXl);


        Which[
          ((xmuRvar == True) && (logXVvar == True)),
            mCoeff = mHs21bh,
          ((xmuRvar == True) && (logXVvar == False)),
            mCoeff = mHs21bar,
          ((xmuRvar == False) && (logXVvar == True)),
            mCoeff = mHs21hat,
          ((xmuRvar == False) && (logXVvar == False)),
            mCoeff = mHs21
        ];

        res = M3sq[xq, xqb] mCoeff
      ]

    Hs20[Event_?AssociationQ, obsSC_?AssociationQ, OptionsPattern[]] :=
      Module[
        {
          Order    = OptionValue["Order"],
          xmuRvar  = OptionValue["xmuRvar"],
          xmuR     = OptionValue["xmuR"],
          logXVvar = OptionValue["logXVvar"],
          logXV    = OptionValue["logXV"],
          TransferFunctions = OptionValue["TransferFunctions"],
          RadiatorScheme = OptionValue["RadiatorScheme"],
          legs, nlegs, dipoles, ndipoles, xq, xqb,
          mG10, mG11, mG12,
          mG21, mG22, mG23,
          mH11, mH10, mH21, mH22,
          mH1, mC1hc10, mRs10, mC1hc21, 
          RpNLL11,
          mIsc, mIrec, mIhc, mIwa, mIcorrel, mIclust,
          mFrec10, mFwa10, mF10,
          mFsc21, mFrec21, mFhc21, mFwa21, mFcorrel21, mF21, mF22,
          mlogXab, mlogXl,
          mHs12, mHs12bar, mHs12hat, mHs12bh,
          mHs11, mHs11bar, mHs11hat, mHs11bh,
          mHs10, mHs10bar, mHs10hat, mHs10bh,
          mHs24, mHs24bar, mHs24hat, mHs24bh,
          mHs23, mHs23bar, mHs23hat, mHs23bh,
          mHs22, mHs22bar, mHs22hat, mHs22bh,
          mHs21, mHs21bar, mHs21hat, mHs21bh,
          mHs20, mHs20bar, mHs20hat, mHs20bh,
          mCoeff, res
        },

        xq       = Event["xq"];
        xqb      = Event["xqb"];
        legs     = Event["legs"];
        nlegs    = Length[legs];
        dipoles  = Event["dipoles"];
        ndipoles = Length[dipoles];

        mIscl     = TransferFunctions["Iscl"];
        mIrecl    = TransferFunctions["Irecl"];
        mIhcl     = TransferFunctions["Ihcl"];
        mIwaab    = TransferFunctions["Iwaab"];
        mIcorrell = TransferFunctions["Icorrell"];
        mIclustl  = TransferFunctions["Iclustl"];

        RpNLL11 = RadpNLL11[legs, obsSC];
  
        Which[
          Order == 0,
            {
              mG12   = G12[dipoles, obsSC],
              mG11   = ConstantArray[0, ndipoles],
              mG10   = ConstantArray[0, ndipoles],
              mG23   = G23[dipoles, obsSC],
              mG22   = ConstantArray[0, ndipoles],
              mG21   = ConstantArray[0, ndipoles],
    
              mH11   = ConstantArray[0, nlegs],
              mH10   = ConstantArray[0, nlegs],
              mH22   = ConstantArray[0, nlegs],
              mH21   = ConstantArray[0, nlegs],
    
              mF22    = 0,
    
              mH1     = 0,
              mC1hc10 = 0,
              mRs10   = 0,
              mC1hc21 = 0,
              mF10    = 0,
              mF21    = 0,

              mlogXab = LogXdipoles[dipoles, xmuR],
              mlogXl  = LogXlegs[legs, xmuR]
            },
          Order == 1,
            {
              mG12   = G12[dipoles, obsSC],
              mG11   = G11[dipoles, obsSC],
              mG10   = ConstantArray[0, nlegs],
              mG23   = G23[dipoles, obsSC],
              mG22   = G22[dipoles, obsSC],
              mG21   = ConstantArray[0, nlegs],
    
              mH11   = H11[legs, obsSC],
              mH10   = ConstantArray[0, nlegs],
              mH22   = H22[legs, obsSC],
              mH21   = ConstantArray[0, nlegs],
    
              mF22    = FNLL22[RpNLL11],
    
              mH1     = 0,
              mC1hc10 = 0,
              mRs10   = 0,
              mC1hc21 = 0,
              mF10    = 0,
              mF21    = 0,

              mlogXab = LogXdipoles[dipoles, xmuR],
              mlogXl  = LogXlegs[legs, xmuR]
            },
          Order >= 2,
            {
              mG12   = G12[dipoles, obsSC],
              mG11   = G11[dipoles, obsSC],
              mG10   = Which[
                         RadiatorScheme == "Physical",
                           G10[dipoles, obsSC, {"RadiatorScheme" -> "Physical"}],
                         RadiatorScheme == "ConstantFree",
                           G10[dipoles, obsSC, {"RadiatorScheme" -> "ConstantFree"}]
                       ],
              mG23   = G23[dipoles, obsSC],
              mG22   = G22[dipoles, obsSC],
              mG21   = G21[dipoles, obsSC],
    
              mH11   = H11[legs, obsSC],
              mH10   = ConstantArray[0, nlegs],
              mH22   = H22[legs, obsSC],
              mH21   = H21[legs, obsSC],
    
              mF22    = FNLL22[RpNLL11],
    
              mH1     = Virt3[xq, xqb],
              mC1hc10 = C1hc10[legs, obsSC, xq, xqb],
              mRs10 = Which[
                        RadiatorScheme == "Physical",
                          0,
                        RadiatorScheme == "ConstantFree",
                          Total[G10[dipoles, obsSC]]
                      ],
              mC1hc21 = C1hc21[legs, obsSC, xq, xqb],
              mF10    = (  Frec10[RpNLL11, legs, obsSC, mIrecl]
                         + Fwa10[RpNLL11, dipoles, obsSC, mIwaab]),
              mF21    = (  Fsc21[RpNLL11, legs, obsSC]
                         + Frec21[RpNLL11, legs, obsSC, mIrecl]
                         + Fhc21[RpNLL11, legs, obsSC]
                         + Fwa21[RpNLL11, dipoles, obsSC, mIwaab]
                         + Fcorrel21[RpNLL11, legs, obsSC, mIcorrell]
                         + Fclust21[RpNLL11, legs, obsSC, mIclustl]),

              mlogXab = LogXdipoles[dipoles, xmuR],
              mlogXl  = LogXlegs[legs, xmuR]
            }
        ];

        mHs12 = Total[mG12];
        mHs11 = Total[mG11] + Total[mH11];
        mHs10 = Total[mG10] + Total[mH10] + mC1hc10 + mRs10 + mF10 + mH1/M3sq[xq, xqb];
        mHs24 = Total[mG12]^2/2;
        mHs23 = Total[mG23] + Total[mG12] (Total[mG11] + Total[mH11]);
        mHs22 = Total[mG22] + Total[mH22] + mF22 \
                + Total[mG12] (Total[mG10] + mC1hc10 + mRs10 + mF10 + mH1/M3sq[xq, xqb]) \
                + (Total[mG11] + Total[mH11])^2/2;
        mHs21 = Total[mG21] + Total[mH21] + mC1hc21 + mF21 \
                + (Total[mG11] + Total[mH11]) (Total[mG10] + mC1hc10 + mRs10 + mF10 + mH1/M3sq[xq, xqb]);
        mHs20 = Total[mG10]^2/2 + Total[mG10] (Total[mG10] + mC1hc10 + mRs10 + mF10 + mH1/M3sq[xq, xqb]);

        mHs12bar = mHs12;
        mHs11bar = mHs11;
        mHs10bar = mHs10;
        mHs24bar = mHs24;
        mHs23bar = mHs23;
        mHs22bar = mHs22 + 2 Pi beta0 (mG12.mlogXab);
        mHs21bar = mHs21 + 2 Pi beta0 (mG11.mlogXab + mH11.mlogXl);
        mHs20bar = mHs20 + 2 Pi beta0 (mG10.mlogXab + mH10.mlogXl);

        mHs12hat = mHs12;
        mHs11hat = mHs11 + 2 mHs12 (-logXV);
        mHs10hat = mHs10 + 1 mHs11 (-logXV) + 1 mHs12 (-logXV)^2;
        mHs24hat = mHs24;
        mHs23hat = mHs23;
        mHs22hat = mHs22 + 3 mHs23 (-logXV) + 6 mHs24 (-logXV)^2;
        mHs21hat = mHs21 + 2 mHs22 (-logXV) + 3 mHs23 (-logXV)^2 + 4 mHs24 (-logXV)^3;
        mHs20hat = mHs20 + 1 mHs21 (-logXV) + 1 mHs22 (-logXV)^2 + 1 mHs23 (-logXV)^3 + 1 mHs24 (-logXV)^4;
  
        mHs12bh  = mHs12;
        mHs11bh  = mHs11 + 2 mHs12 (-logXV);
        mHs10bh  = mHs10 + 1 mHs11 (-logXV) + 1 mHs12 (-logXV)^2;
        mHs24bh  = mHs24;
        mHs23bh  = mHs23;
        mHs22bh  = mHs22 + 3 mHs23 (-logXV) + 6 mHs24 (-logXV)^2 + 2 Pi beta0 mG12.mlogXab;
        mHs21bh  = mHs21 + 2 mHs22 (-logXV) + 3 mHs23 (-logXV)^2 + 4 mHs24 (-logXV)^3 \
                         + 2 Pi beta0 (mG11.mlogXab + 2 mG12.mlogXab (-logXV) + mH11.mlogXl);
        mHs20bh  = mHs20 + 1 mHs21 (-logXV) + 1 mHs22 (-logXV)^2 + 1 mHs23 (-logXV)^3 + 1 mHs24 (-logXV)^4 \
                         + 2 Pi beta0 (mG10.mlogXab + 1 mG11.mlogXab (-logXV) + 1 mG12.mlogXab (-logXV)^2  \
                                      + mH10.mlogXl + 1 mH11.mlogXl (-logXV));


        Which[
          ((xmuRvar == True) && (logXVvar == True)),
            mCoeff = mHs20bh,
          ((xmuRvar == True) && (logXVvar == False)),
            mCoeff = mHs20bar,
          ((xmuRvar == False) && (logXVvar == True)),
            mCoeff = mHs20hat,
          ((xmuRvar == False) && (logXVvar == False)),
            mCoeff = mHs20
        ];

        res = M3sq[xq, xqb] mCoeff
      ]

  End[]

EndPackage[]