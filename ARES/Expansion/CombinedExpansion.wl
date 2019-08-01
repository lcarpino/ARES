(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: CombinedExpansion *)

(* :Author: *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)

BeginPackage["ARES`Expansion`CombinedExpansion`"]

  H12bar::usage = ""
  H11bar::usage = ""
  H10bar::usage = ""

  H24bar::usage = ""
  H23bar::usage = ""
  H22bar::usage = ""
  H21bar::usage = ""
  H20bar::usage = ""

  Begin["`Private`"]

    Needs["ARES`EPA`MatrixElements`"]
    Needs["ARES`Expansion`SoftRadiatorExpansion`"]
    Needs["ARES`Expansion`HardCollinearRadiatorExpansion`"]
    Needs["ARES`Expansion`DerivativeRadiatorExpansion`"]
    Needs["ARES`Expansion`CollinearConstant`"]
    Needs["ARES`Expansion`AdditiveInterface`"]

    ExpOpts =
      {
        "Order" -> 2,
        "xmuR" -> 1,
        "logXV" -> 0,
        "TransferFunctions" -> Identity,
        "RadiatorScheme" -> "Physical"
      };

    Options[H12bar] = ExpOpts;
    Options[H11bar] = ExpOpts;
    Options[H10bar] = ExpOpts;
    Options[H24bar] = ExpOpts;
    Options[H23bar] = ExpOpts;
    Options[H22bar] = ExpOpts;
    Options[H21bar] = ExpOpts;
    Options[H20bar] = ExpOpts;

    H12bar[Event_?AssociationQ, obsSC_?AssociationQ, OptionsPattern[]] :=
      Module[
        {
          Order = OptionValue["Order"],
          xmuR  = OptionValue["xmuR"],
          logXV = OptionValue["logXV"],
          TransferFunctions = OptionValue["TransferFunctions"],
          RadiatorScheme = OptionValue["RadiatorScheme"],
          legs, nlegs, dipoles, ndipoles, xq, xqb,
          mG12,
          mHs12, mHs12bar, mHs12hat, mHs12bh,
          res
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
  
        res = M3sq[xq, xqb] mHs12bh
      ]
 
    H11bar[Event_?AssociationQ, obsSC_?AssociationQ, OptionsPattern[]] :=
      Module[
        {
          Order = OptionValue["Order"],
          xmuR  = OptionValue["xmuR"],
          logXV = OptionValue["logXV"],
          TransferFunctions = OptionValue["TransferFunctions"],
          RadiatorScheme = OptionValue["RadiatorScheme"],
          legs, nlegs, dipoles, ndipoles, xq, xqb,
          mG12, mG11, mH11,
          mHs12, mHs12bar, mHs12hat, mHs12bh,
          mHs11, mHs11bar, mHs11hat, mHs11bh,
          res
        },
  
        xq       = Event["xq"];
        xqb      = Event["xqb"];
        legs     = Event["legs"];
        nlegs    = Length[legs];
        dipoles  = Event["dipoles"];
        ndipoles = Length[dipoles];

        Which[
          Order == 0,
            {
              mG12 = G12[dipoles, obsSC],
              mG11 = ConstantArray[0, ndipoles],
              mH11 = ConstantArray[0, nlegs]
            },
          Order >= 1,
            {
              mG12 = G12[dipoles, obsSC],
              mG11 = G11[dipoles, obsSC],
              mH11 = H11[legs, obsSC]
            }
        ];
  
        mHs12 = Total[mG12];
        mHs11 = Total[mG11] + Total[mH11];

        mHs12bar = mHs12;
        mHs11bar = mHs11;

        mHs12hat = mHs12;
        mHs11hat = mHs11 + 2 mHs12 (-logXV);

        mHs12bh  = mHs12;
        mHs11bh  = mHs11 + 2 mHs12 (-logXV);

        res = M3sq[xq, xqb] mHs11bh
      ]

    H10bar[Event_?AssociationQ, obsSC_?AssociationQ, OptionsPattern[]] :=
      Module[
        {
          Order = OptionValue["Order"],
          xmuR  = OptionValue["xmuR"],
          logXV = OptionValue["logXV"],
          TransferFunctions = OptionValue["TransferFunctions"],
          RadiatorScheme = OptionValue["RadiatorScheme"],
          legs, nlegs, dipoles, ndipoles, xq, xqb,
          mG12, mG11, mG10, mH11, mH10,
          mH1, mC1hc10,
          mIsc, mIrec, mIhc, mIwa, mIcorrel, mIclust,
          RpNLL11, mFrec10, mFwa10, mF10,
          mHs12, mHs12bar, mHs12hat, mHs12bh,
          mHs11, mHs11bar, mHs11hat, mHs11bh,
          mHs10, mHs10bar, mHs10hat, mHs10bh,
          res
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
              mH11   = ConstantArray[0, nlegs],
              mH10   = ConstantArray[0, nlegs],
              mH1     = 0,
              mF10    = 0,
              mC1hc10 = 0
            },
          Order == 1,
            {
              mG12   = G12[dipoles, obsSC],
              mG11   = G11[dipoles, obsSC],
              mG10   = ConstantArray[0, ndipoles],
              mH11   = H11[legs, obsSC],
              mH10   = ConstantArray[0, nlegs],
              mH1     = 0,
              mF10    = 0,
              mC1hc10 = 0
            },
          Order >= 2,
            {
              mG12   = G12[dipoles, obsSC],
              mG11   = G11[dipoles, obsSC],
              mG10   = G10[dipoles, obsSC],
              mH11   = H11[legs, obsSC],
              mH10   = ConstantArray[0, nlegs],
              mH1     = Virt3[xq, xqb],
              mF10    = (Frec10[RpNLL11, legs, obsSC, mIrecl]
                         + Fwa10[RpNLL11, dipoles, obsSC, mIwaab]),
              mC1hc10 = C1hc10[legs, obsSC, xq, xqb]
            }
        ];
  
        mHs12 = Total[mG12];
        mHs11 = Total[mG11] + Total[mH11];
        mHs10 = Total[mG10] + Total[mH10] + mC1hc10 + mF10 + mH1/M3sq[xq, xqb];

        mHs12bar = mHs12;
        mHs11bar = mHs11;
        mHs10bar = mHs10;

        mHs12hat = mHs12;
        mHs11hat = mHs11 + 2 mHs12 (-logXV);
        mHs10hat = mHs10 + mHs11 (-logXV) + mHs12 (-logXV)^2;
  
        mHs12bh  = mHs12;
        mHs11bh  = mHs11 + 2 mHs12 (-logXV);
        mHs10bh  = mHs10 + mHs11 (-logXV) + mHs12 (-logXV)^2;

        res = M3sq[xq, xqb] mHs10bh
      ]

    H24bar[Event_?AssociationQ, obsSC_?AssociationQ, OptionsPattern[]] :=
      Module[
        {
          Order = OptionValue["Order"],
          xmuR  = OptionValue["xmuR"],
          logXV = OptionValue["logXV"],
          TransferFunctions = OptionValue["TransferFunctions"],
          RadiatorScheme = OptionValue["RadiatorScheme"],
          legs, nlegs, dipoles, ndipoles, xq, xqb,
          mG12,
          mHs12, mHs12bar, mHs12hat, mHs12bh,
          mHs11, mHs11bar, mHs11hat, mHs11bh,
          mHs10, mHs10bar, mHs10hat, mHs10bh,
          mHs24, mHs24bar, mHs24hat, mHs24bh,
          res
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

        res = M3sq[xq, xqb] mHs24bh
      ]

    H23bar[Event_?AssociationQ, obsSC_?AssociationQ, OptionsPattern[]] :=
      Module[
        {
          Order = OptionValue["Order"],
          xmuR  = OptionValue["xmuR"],
          logXV = OptionValue["logXV"],
          TransferFunctions = OptionValue["TransferFunctions"],
          RadiatorScheme = OptionValue["RadiatorScheme"],
          legs, nlegs, dipoles, ndipoles, xq, xqb,
          mG11, mG12, mG23, mH11,
          mHs12, mHs12bar, mHs12hat, mHs12bh,
          mHs11, mHs11bar, mHs11hat, mHs11bh,
          mHs10, mHs10bar, mHs10hat, mHs10bh,
          mHs24, mHs24bar, mHs24hat, mHs24bh,
          mHs23, mHs23bar, mHs23hat, mHs23bh,
          res
        },

        xq       = Event["xq"];
        xqb      = Event["xqb"];
        legs     = Event["legs"];
        nlegs    = Length[legs];
        dipoles  = Event["dipoles"];
        ndipoles = Length[dipoles];

        Which[
          Order == 0,
            {
              mG12 = G12[dipoles, obsSC],
              mG11 = ConstantArray[0, ndipoles],
              mG23 = G23[dipoles, obsSC],
              mH11 = ConstantArray[0, nlegs]
            },
          Order >= 1,
            {
              mG12 = G12[dipoles, obsSC],
              mG11 = G11[dipoles, obsSC],
              mG23 = G23[dipoles, obsSC],
              mH11 = H11[legs, obsSC]
            }
        ];
  
        mHs12 = Total[mG12];
        mHs11 = Total[mG11] + Total[mH11];
        mHs24 = mHs12^2/2;
        mHs23 = Total[mG23] + Total[mG12] (Total[mG11] + Total[mH11]);

        mHs12bar = mHs12;
        mHs11bar = mHs11;
        mHs24bar = mHs24;
        mHs23bar = mHs23;
  
        mHs12hat = mHs12;
        mHs11hat = mHs11 + 2 mHs12 (-logXV);
        mHs24hat = mHs24;
        mHs23hat = mHs23 + 4 mHs24 (-logXV);

        mHs12bh  = mHs12;
        mHs11bh  = mHs11 + 2 mHs12 (-logXV);
        mHs24bh  = mHs24;
        mHs23bh  = mHs23 + 4 mHs24 (-logXV);

        res = M3sq[xq, xqb] mHs23bh
      ]

    H22bar[Event_?AssociationQ, obsSC_?AssociationQ, OptionsPattern[]] :=
      Module[
        {
          Order = OptionValue["Order"],
          xmuR  = OptionValue["xmuR"],
          logXV = OptionValue["logXV"],
          TransferFunctions = OptionValue["TransferFunctions"],
          RadiatorScheme = OptionValue["RadiatorScheme"],
          legs, nlegs, dipoles, ndipoles, xq, xqb,
          mG10, mG11, mG12,
          mG21, mG22, mG23,
          mH10, mH11,
          mH21, mH22,
          mH1, mC1hc10,
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
          res
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
    
              mH11   = ConstantArray[0, nlegs],
              mH10   = ConstantArray[0, nlegs],
              mH22   = ConstantArray[0, nlegs],
    
              mF22    = 0,
    
              mH1     = 0,
              mC1hc10 = 0,
              mF10    = 0,

              mlogXab = LogXdipoles[dipoles, xmuR],
              mlogXl  = LogXlegs[legs, xmuR]
            },
          Order == 1,
            {
              mG12   = G12[dipoles, obsSC],
              mG11   = G11[dipoles, obsSC],
              mG10   = ConstantArray[0, ndipoles],
              mG23   = G23[dipoles, obsSC],
              mG22   = G22[dipoles, obsSC],
    
              mH11   = H11[legs, obsSC],
              mH10   = ConstantArray[0, nlegs],
              mH22   = H22[legs, obsSC],
    
              mF22    = FNLL22[RpNLL11],
    
              mH1     = 0,
              mC1hc10 = 0,
              mF10    = 0,

              mlogXab = LogXdipoles[dipoles, xmuR],
              mlogXl  = LogXlegs[legs, xmuR]
            },
          Order >= 2,
            {
              mG12   = G12[dipoles, obsSC],
              mG11   = G11[dipoles, obsSC],
              mG10   = G10[dipoles, obsSC],
              mG23   = G23[dipoles, obsSC],
              mG22   = G22[dipoles, obsSC],
    
              mH11   = H11[legs, obsSC],
              mH10   = ConstantArray[0, nlegs],
              mH22   = H22[legs, obsSC],
    
              mF22    = FNLL22[RpNLL11],
    
              mH1     = Virt3[xq, xqb],
              mC1hc10 = C1hc10[legs, obsSC, xq, xqb],
              mF10    = (Frec10[RpNLL11, legs, obsSC, mIrecl]
                         + Fwa10[RpNLL11, dipoles, obsSC, mIwaab]),

              mlogXab = LogXdipoles[dipoles, xmuR],
              mlogXl  = LogXlegs[legs, xmuR]
            }
        ];

        mHs12 = Total[mG12];
        mHs11 = Total[mG11] + Total[mH11];
        mHs10 = Total[mG10] + Total[mH10] + mC1hc10 + mF10 + mH1/M3sq[xq, xqb];
        mHs24 = Total[mG12]^2/2;
        mHs23 = Total[mG23] + Total[mG12] (Total[mG11] + Total[mH11]);
        mHs22 = Total[mG22] + Total[mH22] + mF22 \
                + Total[mG12] (Total[mG10] + mC1hc10 + mF10 + mH1/M3sq[xq, xqb]) \
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

        res = M3sq[xq, xqb] mHs22bh
      ]

    H21bar[Event_?AssociationQ, obsSC_?AssociationQ, OptionsPattern[]] :=
      Module[
        {
          Order = OptionValue["Order"],
          xmuR  = OptionValue["xmuR"],
          logXV = OptionValue["logXV"],
          TransferFunctions = OptionValue["TransferFunctions"],
          RadiatorScheme = OptionValue["RadiatorScheme"],
          legs, nlegs, dipoles, ndipoles, xq, xqb,
          mG10, mG11, mG12,
          mG21, mG22, mG23,
          mH11, mH10, mH21, mH22,
          mH1, mC1hc10, mC1hc21, 
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
          res
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
              mG10   = ConstantArray[0, ndipoles],
              mG23   = G23[dipoles, obsSC],
              mG22   = G22[dipoles, obsSC],
              mG21   = ConstantArray[0, ndipoles],
    
              mH11   = H11[legs, obsSC],
              mH10   = ConstantArray[0, nlegs],
              mH22   = H22[legs, obsSC],
              mH21   = ConstantArray[0, nlegs],
    
              mF22    = FNLL22[RpNLL11],
    
              mH1     = 0,
              mC1hc10 = 0,
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
              mG10   = G10[dipoles, obsSC],
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
        mHs10 = Total[mG10] + Total[mH10] + mC1hc10 + mF10 + mH1/M3sq[xq, xqb];
        mHs24 = Total[mG12]^2/2;
        mHs23 = Total[mG23] + Total[mG12] (Total[mG11] + Total[mH11]);
        mHs22 = Total[mG22] + Total[mH22] + mF22 \
                + Total[mG12] (Total[mG10] + mC1hc10 + mF10 + mH1/M3sq[xq, xqb]) \
                + (Total[mG11] + Total[mH11])^2/2;
        mHs21 = Total[mG21] + Total[mH21] + mC1hc21 + mF21 \
                + (Total[mG11] + Total[mH11]) (Total[mG10] + mC1hc10 + mF10 + mH1/M3sq[xq, xqb]);

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

        res = M3sq[xq, xqb] mHs21bh
      ]

    H20bar[Event_?AssociationQ, obsSC_?AssociationQ, OptionsPattern[]] :=
      Module[
        {
          Order = OptionValue["Order"],
          xmuR  = OptionValue["xmuR"],
          logXV = OptionValue["logXV"],
          TransferFunctions = OptionValue["TransferFunctions"],
          RadiatorScheme = OptionValue["RadiatorScheme"],
          legs, nlegs, dipoles, ndipoles, xq, xqb,
          mG10, mG11, mG12,
          mG21, mG22, mG23,
          mH11, mH10, mH21, mH22,
          mH1, mC1hc10, mC1hc21, 
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
          res
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
              mG10   = G10[dipoles, obsSC],
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
        mHs10 = Total[mG10] + Total[mH10] + mC1hc10 + mF10 + mH1/M3sq[xq, xqb];
        mHs24 = Total[mG12]^2/2;
        mHs23 = Total[mG23] + Total[mG12] (Total[mG11] + Total[mH11]);
        mHs22 = Total[mG22] + Total[mH22] + mF22 \
                + Total[mG12] (Total[mG10] + mC1hc10 + mF10 + mH1/M3sq[xq, xqb]) \
                + (Total[mG11] + Total[mH11])^2/2;
        mHs21 = Total[mG21] + Total[mH21] + mC1hc21 + mF21 \
                + (Total[mG11] + Total[mH11]) (Total[mG10] + mC1hc10 + mF10 + mH1/M3sq[xq, xqb]);
        mHs20 = Total[mG10]^2/2 + Total[mG10] (Total[mG10] + mC1hc10 + mF10 + mH1/M3sq[xq, xqb]);

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

        res = M3sq[xq, xqb] mHs20bh
      ]

  End[]

EndPackage[]