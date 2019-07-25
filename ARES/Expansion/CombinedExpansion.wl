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
          mH12bar
        },

        xq      = Event["xq"];
        xqb     = Event["xqb"];
        legs    = Event["legs"];
        dipoles = Event["dipoles"];
  
        rmG12 = G12[dipoles, obsSC];
  
        mH12 = M3sq[xq, xqb] rmG12;
        mH12bar = mH12;
  
        mH12bar
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
          mH11bar
        },
  
        xq      = Event["xq"];
        xqb     = Event["xqb"];
        legs    = Event["legs"];
        dipoles = Event["dipoles"];

        Which[
          Order == 0,
            {
              rmG12 = G12[dipoles, obsSC],
              rmG11 = 0,
              rmH11 = 0
            },
          Order >= 1,
            {
              rmG12 = G12[dipoles, obsSC],
              rmG11 = G11[dipoles, obsSC],
              rmH11 = H11[legs, obsSC]
            }
        ];
  
        mH12 = M3sq[xq, xqb] rmG12;
        mH11 = M3sq[xq, xqb] (rmG11 + rmH11);
        mH11bar = mH11 - 2 mH12 logXV;
  
        mH11bar
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
          RpNLL11, mFrec10, mFwa10, mF10,
          mH12, mH11, mH10,
          mH10bar
        },

        xq      = Event["xq"];
        xqb     = Event["xqb"];
        legs    = Event["legs"];
        dipoles = Event["dipoles"];
 
        RpNLL11 = RadpNLL11[legs, obsSC];

        Which[
          Order == 0,
            {
              rmG12   = G12[dipoles, obsSC],
              rmG11   = 0,
              rmG10   = 0,
              rmH11   = 0,
              rmH10   = 0,
              mH1     = 0,
              mF10    = 0,
              mC1hc10 = 0
            },
          Order == 1,
            {
              rmG12   = G12[dipoles, obsSC],
              rmG11   = G11[dipoles, obsSC],
              rmG10   = 0,
              rmH11   = H11[legs, obsSC],
              rmH10   = 0,
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
              rmH10   = 0,
              mH1     = Virt3[xq, xqb],
              mF10    = (Frec10[RpNLL11, legs, obsSC, TransferFunctions["Irecl"]]
                         + Fwa10[RpNLL11, dipoles, obsSC, TransferFunctions["Iwaab"]]),
              mC1hc10 = C1hc10[legs, obsSC, xq, xqb]
            }
        ];
  
        mH12 = M3sq[xq, xqb] rmG12;
        mH11 = M3sq[xq, xqb] (rmG11 + rmH11);
        mH10 = M3sq[xq, xqb] (rmG10 + rmH10 + mC1hc10 + mF10) + mH1;
        mH10bar = mH10 - mH11 logXV + mH12 logXV^2;
  
        mH10bar
      ]

    H24bar[Event_?AssociationQ, obsSC_?AssociationQ, OptionsPattern[]] :=
      Module[
        {
          Order = OptionsValue["Order"],
          xmuR  = OptionValue["xmuR"],
          logXV = OptionValue["logXV"],
          TransferFunctions = OptionValue["TransferFunctions"],
          RadiatorScheme = OptionValue["RadiatorScheme"],
          legs, dipoles, xq, xqb,
          rmG12, mH24,
          mH24bar
        },
  
        xq      = Event["xq"];
        xqb     = Event["xqb"];
        legs    = Event["legs"];
        dipoles = Event["dipoles"];

        rmG12 = G12[dipoles, obsSC];
  
        mH24 = M3sq[xq, xqb] 1/2 rmG12^2;
        mH24bar = mH24;
  
        mH24bar
      ]

    H23bar[Event_?AssociationQ, obsSC_?AssociationQ, OptionsPattern[]] :=
      Module[
        {
          Order = OptionsValue["Order"],
          xmuR  = OptionValue["xmuR"],
          logXV = OptionValue["logXV"],
          TransferFunctions = OptionValue["TransferFunctions"],
          RadiatorScheme = OptionValue["RadiatorScheme"],
          legs, dipoles, xq, xqb,
          rmG11, rmG12, rmG23, rmH11,
          mH24, mH23, mH23bar
        },

        xq      = Event["xq"];
        xqb     = Event["xqb"];
        legs    = Event["legs"];
        dipoles = Event["dipoles"];

        Which[
          Order == 0,
            {
              rmG12 = G12[dipoles, obsSC],
              rmG11 = 0,
              rmG23 = G23[dipoles, obsSC],
              rmH11 = 0
            },
          Order >= 1,
            {
              rmG12 = G12[dipoles, obsSC],
              rmG11 = G11[dipoles, obsSC],
              rmG23 = G23[dipoles, obsSC],
              rmH11 = H11[legs, obsSC]
            }
        ];
  
        mH24 = M3sq[xq, xqb] 1/2 rmG12^2;
        mH23 = M3sq[xq, xqb] (rmG23 + rmG12 (rmG11 + rmH11));
        mH23bar = mH23 - 4 mH24 logXV;
  
        mH23bar
      ]

    H22bar[Event_?AssociationQ, obsSC_?AssociationQ, OptionsPattern[]] :=
      Module[
        {
          Order = OptionsValue["Order"],
          xmuR  = OptionValue["xmuR"],
          logXV = OptionValue["logXV"],
          TransferFunctions = OptionValue["TransferFunctions"],
          RadiatorScheme = OptionValue["RadiatorScheme"],
          legs, dipoles, xq, xqb,
          rmG10, rmG11, rmG12,
          rmG21, rmG22, rmG23,
          rmH10, rmH11,
          rmH21, rmH22,
          mH1, mC1hc,
          mF11, mF22,
          mFrec10, mFwa10, mF10,
          mFsc21, mFrec21, mFhc21, mFwa21, mFcorrel21, mF21,
          mH24, mH23,
          mH22, mH22bar
        },
  
        xq      = Event["xq"];
        xqb     = Event["xqb"];
        legs    = Event["legs"];
        dipoles = Event["dipoles"];

        Which[
          Order == 0,
            {
              rmG12 = G12[dipoles, obsSC],
              rmG11 = 0,
              rmG10 = 0,
              rmG23 = G23[dipoles, obsSC],
              rmG22 = 0,
    
              rmH11 = 0,
              rmH22 = 0,
    
              mF22  = 0,
    
              mH1   = 0,
              mC1hc = 0,
              mF10  = 0
            },
          Order == 1,
            {
              rmG12 = G12[dipoles, obsSC],
              rmG11 = G11[dipoles, obsSC],
              rmG10 = 0,
              rmG23 = G23[dipoles, obsSC],
              rmG22 = G22[dipoles, obsSC],
    
              rmH11 = H11[legs, obsSC],
              rmH22 = H22[legs, obsSC],
    
              mF22  = FNLL22[legs, obsSC],
    
              mH1   = 0,
              mC1hc = 0,
              mF10  = 0
            },
          Order >= 2,
            {
              rmG12 = G12[dipoles, obsSC],
              rmG11 = G11[dipoles, obsSC],
              rmG10 = G10[dipoles, obsSC],
              rmG23 = G23[dipoles, obsSC],
              rmG22 = G22[dipoles, obsSC],
    
              rmH11 = H11[legs, obsSC],
              rmH22 = H22[legs, obsSC],
    
              mF22  = FNLL22[legs, obsSC],
    
              mH1   = virt3[xq, xqb],
              mC1hc = C1hc[legs, obsSC, xq, xqb],
              mF10  = Frec10[legs, obsSC] + Fwa10[dipoles, obsSC]
            }
        ];
  
        mH24 = M3sq[xq, xqb] 1/2 rmG12^2;
        mH23 = M3sq[xq, xqb] (rmG23 + rmG12 (rmG11 + rmH11));
        mH22 = M3sq[xq, xqb] (1/2 (rmG11 + rmH11)^2 + rmG12 (rmG10 + mC1hc + mF10)
                              + rmG22 + rmH22 + mF22) + rmG12 mH1;
        mH22bar = mH22 - 3 mH23 logXV + 6 mH24 logXV^2;

        mH22bar
      ]

    H21bar[Event_?AssociationQ, obsSC_?AssociationQ, OptionsPattern[]] :=
      Module[
        {
          Order = OptionsValue["Order"],
          xmuR  = OptionValue["xmuR"],
          logXV = OptionValue["logXV"],
          TransferFunctions = OptionValue["TransferFunctions"],
          RadiatorScheme = OptionValue["RadiatorScheme"],
          legs, dipoles, xq, xqb,
          rmG10, rmG11, rmG12,
          rmG21, rmG22, rmG23,
          rmH11, rmH10, rmH21, rmH22,
          mH1, mC1hc, mC1hc21, 
          RpNLL,
          mFrec10, mFwa10, mF10,
          mFsc21, mFrec21, mFhc21, mFwa21, mFcorrel21, mF21, mF22,
          mH24, mH23, mH22, mH21,
          mH21bar
        },

        xq      = Event["xq"];
        xqb     = Event["xqb"];
        legs    = Event["legs"];
        dipoles = Event["dipoles"];
 
        RpNLL = RpNLL11[legs, obsSC];
  
        Which[
          Order == 0,
            {
              rmG12 = G12[dipoles, obsSC],
              rmG11 = 0,
              rmG10 = 0,
              rmG23 = G23[dipoles, obsSC],
              rmG22 = 0,
              rmG21 = 0,
    
              rmH11 = 0,
              rmH10 = 0,
              rmH22 = 0,
              rmH21 = 0,
    
              mF22 = 0,
    
              mH1 = 0,
              mC1hc = 0,
              mC1hc21 = 0,
              mF10 = 0,
              mF21 = 0
            },
          Order == 1,
            {
              rmG12 = G12[dipoles, obsSC],
              rmG11 = G11[dipoles, obsSC],
              rmG10 = 0,
              rmG23 = G23[dipoles, obsSC],
              rmG22 = G22[dipoles, obsSC],
              rmG21 = 0,
    
              rmH11 = H11[legs, obsSC],
              rmH10 = 0,
              rmH22 = H22[legs, obsSC],
              rmH21 = 0,
    
              mF22 = FNLL22[legs, obsSC],
    
              mH1 = 0,
              mC1hc = 0,
              mC1hc21 = 0,
              mF10 = 0,
              mF21 = 0
            },
          Order >= 2,
            {
              rmG12 = G12[dipoles, obsSC],
              rmG11 = G11[dipoles, obsSC],
              rmG10 = G10[dipoles, obsSC],
              rmG23 = G23[dipoles, obsSC],
              rmG22 = G22[dipoles, obsSC],
              rmG21 = G21[dipoles, obsSC],
    
              rmH11 = H11[legs, obsSC],
              rmH10 = 0,
              rmH22 = H22[legs, obsSC],
              rmH21 = H21[legs, obsSC],
    
              mF22 = FNLL22[legs, obsSC],
    
              mH1 = virt3[xq, xqb],
              mC1hc = C1hc[legs, obsSC, xq, xqb],
              mC1hc21 = C1hc21[legs, obsSC, xq, xqb],
              mF10 = Frec10[legs, obsSC] + Fwa10[dipoles, obsSC],
              mF21 = (Fsc21[legs, obsSC, RpNLL] + Frec21[legs, obsSC]
                      + Fhc21[legs, obsSC, RpNLL] + Fwa21[dipoles, obsSC]
                      + Fcorrel21[legs, obsSC])
            }
        ];
  
        mH24 = M3sq[xq, xqb] 1/2 rmG12^2;
        mH23 = M3sq[xq, xqb] (rmG23 + rmG12 (rmG11 + rmH11));
        mH22 = M3sq[xq, xqb] (1/2 (rmG11 + rmH11)^2 + rmG12 (rmG10 + mC1hc + mF10)
                              + rmG22 + rmH22 + mF22) + rmG12 mH1;
        mH21 = M3sq[xq, xqb] ((rmG11 + rmH11) (rmG10 + mC1hc + mF10) + mC1hc21
                              + (rmG21 + rmH21) + mF21) + (rmG11 + rmH11) mH1;
  
        mH21bar = mH21 - 2 mH22 logXV + 3 mH23 logXV^2 - 4 mH24 logXV^3;
  
        mH21bar
      ]

    H20bar[Event_?AssociationQ, obsSC_?AssociationQ, OptionsPattern[]] :=
      Module[
        {
          Order = OptionsValue["Order"],
          xmuR  = OptionValue["xmuR"],
          logXV = OptionValue["logXV"],
          TransferFunctions = OptionValue["TransferFunctions"],
          RadiatorScheme = OptionValue["RadiatorScheme"],
          legs, dipoles, xq, xqb,
          rmG10, rmG11, rmG12,
          rmG21, rmG22, rmG23,
          rmH11, rmH10, rmH21, rmH22,
          mH1, mC1hc, mC1hc21, 
          RpNLL,
          mFrec10, mFwa10, mF10,
          mFsc21, mFrec21, mFhc21, mFwa21, mFcorrel21, mF21, mF22,
          mH24, mH23, mH22, mH21, mH20,
          mH20bar
        },

        xq      = Event["xq"];
        xqb     = Event["xqb"];
        legs    = Event["legs"];
        dipoles = Event["dipoles"];
 
        RpNLL = RpNLL11[legs, obsSC];
  
        Which[
          Order == 0,
            {
              rmG12 = G12[dipoles, obsSC],
              rmG11 = 0,
              rmG10 = 0,
              rmG23 = G23[dipoles, obsSC],
              rmG22 = 0,
              rmG21 = 0,
    
              rmH11 = 0,
              rmH10 = 0,
              rmH22 = 0,
              rmH21 = 0,
    
              mF22 = 0,
    
              mH1 = 0,
              mC1hc = 0,
              mC1hc21 = 0,
              mF10 = 0,
              mF21 = 0
            },
          Order == 1,
            {
              rmG12 = G12[dipoles, obsSC],
              rmG11 = G11[dipoles, obsSC],
              rmG10 = 0,
              rmG23 = G23[dipoles, obsSC],
              rmG22 = G22[dipoles, obsSC],
              rmG21 = 0,
    
              rmH11 = H11[legs, obsSC],
              rmH10 = 0,
              rmH22 = H22[legs, obsSC],
              rmH21 = 0,
    
              mF22 = FNLL22[legs, obsSC],
    
              mH1 = 0,
              mC1hc = 0,
              mC1hc21 = 0,
              mF10 = 0,
              mF21 = 0
            },
          Order >= 2,
            {
              rmG12 = G12[dipoles, obsSC],
              rmG11 = G11[dipoles, obsSC],
              rmG10 = G10[dipoles, obsSC],
              rmG23 = G23[dipoles, obsSC],
              rmG22 = G22[dipoles, obsSC],
              rmG21 = G21[dipoles, obsSC],
    
              rmH11 = H11[legs, obsSC],
              rmH10 = 0,
              rmH22 = H22[legs, obsSC],
              rmH21 = H21[legs, obsSC],
    
              mF22 = FNLL22[legs, obsSC],
    
              mH1 = virt3[xq, xqb],
              mC1hc = C1hc[legs, obsSC, xq, xqb],
              mC1hc21 = C1hc21[legs, obsSC, xq, xqb],
              mF10 = Frec10[legs, obsSC] + Fwa10[dipoles, obsSC],
              mF21 = (Fsc21[legs, obsSC, RpNLL] + Frec21[legs, obsSC]
                      + Fhc21[legs, obsSC, RpNLL] + Fwa21[dipoles, obsSC]
                      + Fcorrel21[legs, obsSC])
            }
        ];
  
        mH24 = M3sq[xq, xqb] 1/2 rmG12^2;
        mH23 = M3sq[xq, xqb] (rmG23 + rmG12 (rmG11 + rmH11));
        mH22 = M3sq[xq, xqb] (1/2 (rmG11 + rmH11)^2 + rmG12 (rmG10 + mC1hc + mF10)
                              + rmG22 + rmH22 + mF22) + rmG12 mH1;
        mH21 = M3sq[xq, xqb] ((rmG11 + rmH11) (rmG10 + mC1hc + mF10) + mC1hc21
                              + (rmG21 + rmH21) + mF21) + (rmG11 + rmH11) mH1;
  
        mH21bar = mH21 - 2 mH22 logXV + 3 mH23 logXV^2 - 4 mH24 logXV^3;
        mH20 = 0;

        (* this term is formally N3LL so set to zero, it's useful for some other checks *)
        (* mH20 = M3sq[xq,xqb] 1/2 mG10^2 + M3sq[xq,xqb] mG10 (mC1hc+mF10) + mG10 mH1; *)

        mH20bar = mH20 - mH21 logXV + mH22 logXV^2 - mH23 logXV^3 + mH24 logXV^4;

        mH20bar 
      ]

  End[]

EndPackage[]