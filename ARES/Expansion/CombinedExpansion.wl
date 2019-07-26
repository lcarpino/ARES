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
          legs, dipoles, xq, xqb,
          mG12, mH12,
          mH12bar, res
        },

        xq      = Event["xq"];
        xqb     = Event["xqb"];
        legs    = Event["legs"];
        dipoles = Event["dipoles"];
  
        rmG12 = G12[dipoles, obsSC];
  
        mH12 = Total[rmG12];
        mH12bar = mH12;
  
        res = M3sq[xq, xqb] mH12bar
      ]
 
    H11bar[Event_?AssociationQ, obsSC_?AssociationQ, OptionsPattern[]] :=
      Module[
        {
          Order = OptionValue["Order"],
          xmuR  = OptionValue["xmuR"],
          logXV = OptionValue["logXV"],
          TransferFunctions = OptionValue["TransferFunctions"],
          RadiatorScheme = OptionValue["RadiatorScheme"],
          legs, dipoles, xq, xqb,
          rmG12, rmG11, rmH11,
          mH12, mH11,
          mH11bar, res
        },
  
        xq      = Event["xq"];
        xqb     = Event["xqb"];
        legs    = Event["legs"];
        dipoles = Event["dipoles"];

        Which[
          Order == 0,
            {
              rmG12 = G12[dipoles, obsSC],
              rmG11 = {0},
              rmH11 = {0}
            },
          Order >= 1,
            {
              rmG12 = G12[dipoles, obsSC],
              rmG11 = G11[dipoles, obsSC],
              rmH11 = H11[legs, obsSC]
            }
        ];
  
        mH12 = Total[rmG12];
        mH11 = Total[rmG11] + Total[rmH11];

        mH11bar = mH11 + 2 mH12 (-logXV);

        res = M3sq[xq, xqb] mH11bar
      ]

    H10bar[Event_?AssociationQ, obsSC_?AssociationQ, OptionsPattern[]] :=
      Module[
        {
          Order = OptionValue["Order"],
          xmuR  = OptionValue["xmuR"],
          logXV = OptionValue["logXV"],
          TransferFunctions = OptionValue["TransferFunctions"],
          RadiatorScheme = OptionValue["RadiatorScheme"],
          legs, dipoles, xq, xqb,
          rmG12, rmG11, rmG10, rmH11, rmH10,
          mH1, mC1hc10,
          mIsc, mIrec, mIhc, mIwa, mIcorrel, mIclust,
          RpNLL11, mFrec10, mFwa10, mF10,
          mH12, mH11, mH10,
          mH10bar, res
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

        RpNLL11 = RadpNLL11[legs, obsSC];

        Which[
          Order == 0,
            {
              rmG12   = G12[dipoles, obsSC],
              rmG11   = {0},
              rmG10   = {0},
              rmH11   = {0},
              rmH10   = {0},
              mH1     = 0,
              mF10    = 0,
              mC1hc10 = 0
            },
          Order == 1,
            {
              rmG12   = G12[dipoles, obsSC],
              rmG11   = G11[dipoles, obsSC],
              rmG10   = {0},
              rmH11   = H11[legs, obsSC],
              rmH10   = {0},
              mH1     = 0,
              mF10    = 0,
              mC1hc10 = 0
            },
          Order >= 2,
            {
              rmG12   = G12[dipoles, obsSC],
              rmG11   = G11[dipoles, obsSC],
              rmG10   = G10[dipoles, obsSC],
              rmH11   = H11[legs, obsSC],
              rmH10   = {0},
              mH1     = Virt3[xq, xqb],
              mF10    = (Frec10[RpNLL11, legs, obsSC, mIrecl]
                         + Fwa10[RpNLL11, dipoles, obsSC, mIwaab]),
              mC1hc10 = C1hc10[legs, obsSC, xq, xqb]
            }
        ];
  
        mH12 = Total[rmG12];
        mH11 = Total[rmG11] + Total[rmH11];
        mH10 = Total[rmG10] + Total[rmH10] + mC1hc10 + mF10 + mH1/M3sq[xq, xqb];

        mH10bar = mH10 + mH11 (-logXV) + mH12 (-logXV)^2;
  
        res = M3sq[xq, xqb] mH10bar
      ]

    H24bar[Event_?AssociationQ, obsSC_?AssociationQ, OptionsPattern[]] :=
      Module[
        {
          Order = OptionValue["Order"],
          xmuR  = OptionValue["xmuR"],
          logXV = OptionValue["logXV"],
          TransferFunctions = OptionValue["TransferFunctions"],
          RadiatorScheme = OptionValue["RadiatorScheme"],
          legs, dipoles, xq, xqb,
          rmG12, mH12, mH12bar, res
        },
  
        xq      = Event["xq"];
        xqb     = Event["xqb"];
        legs    = Event["legs"];
        dipoles = Event["dipoles"];

        rmG12 = G12[dipoles, obsSC];
  
        mH12 = Total[rmG12];

        mH12bar = mH12;

        res = M3sq[xq, xqb] 1/2 mH12bar^2
      ]

    H23bar[Event_?AssociationQ, obsSC_?AssociationQ, OptionsPattern[]] :=
      Module[
        {
          Order = OptionValue["Order"],
          xmuR  = OptionValue["xmuR"],
          logXV = OptionValue["logXV"],
          TransferFunctions = OptionValue["TransferFunctions"],
          RadiatorScheme = OptionValue["RadiatorScheme"],
          legs, dipoles, xq, xqb,
          rmG11, rmG12, rmG23, rmH11,
          mH12, mH11, mH23, mH12bar, mH11bar, mH23bar, res
        },

        xq      = Event["xq"];
        xqb     = Event["xqb"];
        legs    = Event["legs"];
        dipoles = Event["dipoles"];

        Which[
          Order == 0,
            {
              rmG12 = G12[dipoles, obsSC],
              rmG11 = {0},
              rmG23 = G23[dipoles, obsSC],
              rmH11 = {0}
            },
          Order >= 1,
            {
              rmG12 = G12[dipoles, obsSC],
              rmG11 = G11[dipoles, obsSC],
              rmG23 = G23[dipoles, obsSC],
              rmH11 = H11[legs, obsSC]
            }
        ];
  
        mH12 = Total[rmG12];
        mH11 = Total[rmG11] + Total[rmH11];
        mH23 = Total[rmG23];

        mH12bar = mH12;
        mH11bar = mH11 + 2 mH12 (-logXV);
        mH23bar = mH23;
  
        res = M3sq[xq, xqb] (mH23bar + mH12bar mH11bar)
      ]

    H22bar[Event_?AssociationQ, obsSC_?AssociationQ, OptionsPattern[]] :=
      Module[
        {
          Order = OptionValue["Order"],
          xmuR  = OptionValue["xmuR"],
          logXV = OptionValue["logXV"],
          TransferFunctions = OptionValue["TransferFunctions"],
          RadiatorScheme = OptionValue["RadiatorScheme"],
          legs, dipoles, xq, xqb,
          rmG10, rmG11, rmG12,
          rmG21, rmG22, rmG23,
          rmH10, rmH11,
          rmH21, rmH22,
          mH1, mC1hc10,
          RpNLL11,
          mF11, mF22,
          mIsc, mIrec, mIhc, mIwa, mIcorrel, mIclust,
          mFrec10, mFwa10, mF10,
          mFsc21, mFrec21, mFhc21, mFwa21, mFcorrel21, mF21,
          mlogXab, mlogXl,
          mH23, mH22, mH12, mH11, mH10,
          mH12bar, mH11bar, mH10bar, mH23bar, mH22bar, res
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

        RpNLL11 = RadpNLL11[legs, obsSC];

        Which[
          Order == 0,
            {
              rmG12   = G12[dipoles, obsSC],
              rmG11   = {0},
              rmG10   = {0},
              rmG23   = G23[dipoles, obsSC],
              rmG22   = {0},
    
              rmH11   = {0},
              rmH10   = {0},
              rmH22   = {0},
    
              mF22    = 0,
    
              mH1     = 0,
              mC1hc10 = 0,
              mF10    = 0,

              mlogXab = LogXdipoles[dipoles, xmuR],
              mlogXl  = LogXlegs[legs, xmuR]
            },
          Order == 1,
            {
              rmG12   = G12[dipoles, obsSC],
              rmG11   = G11[dipoles, obsSC],
              rmG10   = {0},
              rmG23   = G23[dipoles, obsSC],
              rmG22   = G22[dipoles, obsSC],
    
              rmH11   = H11[legs, obsSC],
              rmH10   = {0},
              rmH22   = H22[legs, obsSC],
    
              mF22    = FNLL22[RpNLL11],
    
              mH1     = 0,
              mC1hc10 = 0,
              mF10    = 0,

              mlogXab = LogXdipoles[dipoles, xmuR],
              mlogXl  = LogXlegs[legs, xmuR]
            },
          Order >= 2,
            {
              rmG12   = G12[dipoles, obsSC],
              rmG11   = G11[dipoles, obsSC],
              rmG10   = G10[dipoles, obsSC],
              rmG23   = G23[dipoles, obsSC],
              rmG22   = G22[dipoles, obsSC],
    
              rmH11   = H11[legs, obsSC],
              rmH10   = {0},
              rmH22   = H22[legs, obsSC],
    
              mF22    = FNLL22[RpNLL11],
    
              mH1     = Virt3[xq, xqb],
              mC1hc10 = C1hc10[legs, obsSC, xq, xqb],
              mF10    = (Frec10[RpNLL11, legs, obsSC, mIrecl]
                         + Fwa10[RpNLL11, dipoles, obsSC, mIwaab]),

              mlogXab = LogXdipoles[dipoles, xmuR],
              mlogXl  = LogXlegs[legs, xmuR]
            }
        ];

        mH12 = Total[rmG12];
        mH11 = Total[rmG11] + Total[rmH11];
        mH10 = Total[rmG10] + Total[rmH10] + mC1hc10 + mF10 + mH1/M3sq[xq, xqb];
        mH23 = Total[rmG23];
        mH22 = Total[rmG22] + Total[rmH22] + mF22;

        mH12bar = mH12;
        mH11bar = mH11 + 2 mH12 (-logXV);
        mH10bar = mH10 + mH11 (-logXV) + mH12 (-logXV)^2;
        mH23bar = mH23;
        mH22bar = mH22 + 2 Pi beta0 Total[rmG12 mlogXab] + 3 mH23 (-logXV);
 
        res = M3sq[xq, xqb] (mH22bar + mH12bar mH10bar + 1/2 mH11bar^2)
      ]

    H21bar[Event_?AssociationQ, obsSC_?AssociationQ, OptionsPattern[]] :=
      Module[
        {
          Order = OptionValue["Order"],
          xmuR  = OptionValue["xmuR"],
          logXV = OptionValue["logXV"],
          TransferFunctions = OptionValue["TransferFunctions"],
          RadiatorScheme = OptionValue["RadiatorScheme"],
          legs, dipoles, xq, xqb,
          rmG10, rmG11, rmG12,
          rmG21, rmG22, rmG23,
          rmH11, rmH10, rmH21, rmH22,
          mH1, mC1hc10, mC1hc21, 
          RpNLL11,
          mIsc, mIrec, mIhc, mIwa, mIcorrel, mIclust,
          mFrec10, mFwa10, mF10,
          mFsc21, mFrec21, mFhc21, mFwa21, mFcorrel21, mF21, mF22,
          mlogXab, mlogXl,
          mH23, mH22, mH21, mH12, mH11, mH10,
          mH23bar, mH22bar, mH21bar, mH12bar, mH11bar, mH10bar, res
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

        RpNLL11 = RadpNLL11[legs, obsSC];
  
        Which[
          Order == 0,
            {
              rmG12   = G12[dipoles, obsSC],
              rmG11   = {0},
              rmG10   = {0},
              rmG23   = G23[dipoles, obsSC],
              rmG22   = {0},
              rmG21   = {0},
    
              rmH11   = {0},
              rmH10   = {0},
              rmH22   = {0},
              rmH21   = {0},
    
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
              rmG12   = G12[dipoles, obsSC],
              rmG11   = G11[dipoles, obsSC],
              rmG10   = {0},
              rmG23   = G23[dipoles, obsSC],
              rmG22   = G22[dipoles, obsSC],
              rmG21   = {0},
    
              rmH11   = H11[legs, obsSC],
              rmH10   = {0},
              rmH22   = H22[legs, obsSC],
              rmH21   = {0},
    
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
              rmG12   = G12[dipoles, obsSC],
              rmG11   = G11[dipoles, obsSC],
              rmG10   = G10[dipoles, obsSC],
              rmG23   = G23[dipoles, obsSC],
              rmG22   = G22[dipoles, obsSC],
              rmG21   = G21[dipoles, obsSC],
    
              rmH11   = H11[legs, obsSC],
              rmH10   = {0},
              rmH22   = H22[legs, obsSC],
              rmH21   = H21[legs, obsSC],
    
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

        mH12 = Total[rmG12];
        mH11 = Total[rmG11] + Total[rmH11];
        mH10 = Total[rmG10] + Total[rmH10] + mC1hc10 + mF10 + mH1/M3sq[xq, xqb];
        mH23 = Total[rmG23];
        mH22 = Total[rmG22] + Total[rmH22] + mF22;
        mH21 = Total[rmG21] + Total[rmH21] + mC1hc21 + mF21;

        mH12bar = mH12;
        mH11bar = mH11 + 2 mH12 (-logXV);
        mH10bar = mH10 + mH11 (-logXV) + mH12 (-logXV)^2;
        mH23bar = mH23;
        mH22bar = mH22 + 2 Pi beta0 rmG12.mlogXab + 3 mH23 (-logXV);
        mH21bar = (mH21 + 2 Pi beta0 (rmG11.mlogXab + rmH11.mlogXl + 2 rmG12.mlogXab.mlogXab (-logXV))
                   + 2 mH22 (-logXV) + 3 mH23 (-logXV)^2);
  
        res = M3sq[xq, xqb] (mH21bar + mH11 mH10)
      ]

    H20bar[Event_?AssociationQ, obsSC_?AssociationQ, OptionsPattern[]] :=
      Module[
        {
          Order = OptionValue["Order"],
          xmuR  = OptionValue["xmuR"],
          logXV = OptionValue["logXV"],
          TransferFunctions = OptionValue["TransferFunctions"],
          RadiatorScheme = OptionValue["RadiatorScheme"],
          legs, dipoles, xq, xqb,
          rmG10, rmG11, rmG12,
          rmG21, rmG22, rmG23,
          rmH11, rmH10, rmH21, rmH22,
          mH1, mC1hc10, mC1hc21, 
          RpNLL11,
          mIsc, mIrec, mIhc, mIwa, mIcorrel, mIclust,
          mFrec10, mFwa10, mF10,
          mFsc21, mFrec21, mFhc21, mFwa21, mFcorrel21, mF21, mF22,
          mH24, mH23, mH22, mH21, mH20,
          mH20bar
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

        RpNLL11 = RadpNLL11[legs, obsSC];
  
        Which[
          Order == 0,
            {
              rmG12   = G12[dipoles, obsSC],
              rmG11   = 0,
              rmG10   = 0,
              rmG23   = G23[dipoles, obsSC],
              rmG22   = 0,
              rmG21   = 0,
    
              rmH11   = 0,
              rmH10   = 0,
              rmH22   = 0,
              rmH21   = 0,
    
              mF22    = 0,
    
              mH1     = 0,
              mC1hc10 = 0,
              mC1hc21 = 0,
              mF10    = 0,
              mF21    = 0
            },
          Order == 1,
            {
              rmG12   = G12[dipoles, obsSC],
              rmG11   = G11[dipoles, obsSC],
              rmG10   = 0,
              rmG23   = G23[dipoles, obsSC],
              rmG22   = G22[dipoles, obsSC],
              rmG21   = 0,
    
              rmH11   = H11[legs, obsSC],
              rmH10   = 0,
              rmH22   = H22[legs, obsSC],
              rmH21   = 0,
    
              mF22    = FNLL22[RpNLL11],
    
              mH1     = 0,
              mC1hc10 = 0,
              mC1hc21 = 0,
              mF10    = 0,
              mF21    = 0
            },
          Order >= 2,
            {
              rmG12   = G12[dipoles, obsSC],
              rmG11   = G11[dipoles, obsSC],
              rmG10   = G10[dipoles, obsSC],
              rmG23   = G23[dipoles, obsSC],
              rmG22   = G22[dipoles, obsSC],
              rmG21   = G21[dipoles, obsSC],
    
              rmH11   = H11[legs, obsSC],
              rmH10   = 0,
              rmH22   = H22[legs, obsSC],
              rmH21   = H21[legs, obsSC],
    
              mF22    = FNLL22[legs, obsSC],
    
              mH1     = Virt3[xq, xqb],
              mC1hc10 = C1hc10[legs, obsSC, xq, xqb],
              mC1hc21 = C1hc21[legs, obsSC, xq, xqb],
              mF10    = (  Frec10[RpNLL11, legs, obsSC, Irecl]
                         + Fwa10[RpNLL11, dipoles, obsSC, Iwaab]),
              mF21    = (  Fsc21[RpNLL11, legs, obsSC]
                         + Frec21[RpNLL11, legs, obsSC, Irecl]
                         + Fhc21[RpNLL11, legs, obsSC]
                         + Fwa21[RpNLL11, dipoles, obsSC, Iwaab]
                         + Fcorrel21[RpNLL11, legs, obsSC, Icorrell]
                         + Fclust21[RpNLL11, legs, obsSC])
            }
        ];
  
        mH24 = M3sq[xq, xqb] 1/2 rmG12^2;
        mH23 = M3sq[xq, xqb] (rmG23 + rmG12 (rmG11 + rmH11));
        mH22 = M3sq[xq, xqb] (1/2 (rmG11 + rmH11)^2 + rmG12 (rmG10 + mC1hc10 + mF10)
                              + rmG22 + rmH22 + mF22) + rmG12 mH1;
        mH21 = M3sq[xq, xqb] ((rmG11 + rmH11) (rmG10 + mC1hc10 + mF10) + mC1hc21
                              + (rmG21 + rmH21) + mF21) + (rmG11 + rmH11) mH1;
  
        mH21bar = mH21 - 2 mH22 logXV + 3 mH23 logXV^2 - 4 mH24 logXV^3;
        mH20 = 0;

        (* this term is formally N3LL so set to zero, it's useful for some other checks *)
        (* mH20 = M3sq[xq,xqb] 1/2 mG10^2 + M3sq[xq,xqb] mG10 (mC1hc10 + mF10) + mG10 mH1; *)

        mH20bar = mH20 - mH21 logXV + mH22 logXV^2 - mH23 logXV^3 + mH24 logXV^4;

        mH20bar 
      ]

  End[]

EndPackage[]