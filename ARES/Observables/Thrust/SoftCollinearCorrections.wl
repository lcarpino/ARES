(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SoftCollinearCorrections *)

(* :Author: *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)

BeginPackage["ARES`Observables`Thrust`SoftCollinearCorrections`",
  {
    "ARES`Config`",
    "ARES`QCD`Constants`",
    "ARES`Radiator`DerivativeRadiator`"
  }]

    InitialiseCorrections::usage = ""
    BuildMapICorrection::usage = "" 

    INLL::usage = ""
    Iscl::usage = ""
    Irecl::usage = ""
    Ihcl::usage = ""
    Iwaab::usage = ""
    Icorrell::usage = ""
    Iclustl::usage = ""
  Begin["`Private`"]

    (* variables to hold any grids *)
    Iwa12Interpolation;
    Iwa13Interpolation;
    Iwa23Interpolation;

    InitialiseCorrections[] :=
      0

    BuildMapICorrection[] :=
      Association[
        "INLL"     -> INLL,
        "Iscl"     -> Iscl,
        "Irecl"    -> Irecl,
        "Ihcl"     -> Ihcl,
        "Iwaab"    -> Iwaab,
        "Icorrell" -> Icorrell,
        "Iclustl"  -> Iclustl
      ]


    INLL[] := 0

    Iscl[lambda_?NumericQ, RpNLL_?NumericQ, as_?NumericQ,
         leg_?AssociationQ, obspar_?AssociationQ] :=

      Module[
        {
          a, b, logdlbar,
          RpNNLLl, RsNNLLl,
          res
        },
  
        a = obspar["ktpow"];
        b = obspar["etapow"][[leg["num"]]];
        logdlbar = obspar["logdlbar"][[leg["num"]]];
  
        RpNNLLl = RadpNNLLl[lambda, as, a, b];
        RsNNLLl = RadsNNLLl[lambda, as, a, b];
  
        res = (((RpNNLLl + RsNNLLl logdlbar) (PolyGamma[0, 1 + RpNLL] + EulerGamma)
               + RsNNLLl/2 ((PolyGamma[0, 1 + RpNLL] + EulerGamma)^2
               - PolyGamma[1, 1 + RpNLL] + Pi^2/6)))
      ]

    Irecl[leg_?AssociationQ, obspar_?AssociationQ] := 
      Module[
        {a, b, res},
  
        a = obspar["ktpow"];
        b = obspar["etapow"][[leg["num"]]];
  
        Which[
          leg["flav"] == "q" || leg["flav"] == "qb", {res = CF (5/4 - Pi^2/3)},
          leg["flav"] == "g", {res = CA (67/36 - Pi^2/3) - TF NF 13/18}
        ];
        res
      ]

    Ihcl[RpNLL_?NumericQ, leg_?AssociationQ, obspar_?AssociationQ] :=
      Module[
        {a, b, ga0, res},
  
        a = obspar["ktpow"];
        b = obspar["etapow"][[leg["num"]]];
  
        Which[
          leg["flav"] == "q" || leg["flav"] == "qb", {ga0 = Ga0q},
          leg["flav"] == "g", {ga0 = Ga0g}
        ];
        res = -ga0 (PolyGamma[0, 1 + RpNLL] + EulerGamma)
      ]

    Iwaab[dip_?AssociationQ, obspar_?AssociationQ] := 
      Module[
        {res},
  
        res = 0
      ]

    Icorrell[leg_?AssociationQ, obspar_?AssociationQ] := 
      Module[
        {a, b, caconst, nfconst, res},
  
        a = obspar["ktpow"];
        b = obspar["etapow"][[leg["num"]]];
  
        caconst = -(11/6) Zeta[2];
        nfconst = 1/3 Zeta[2];
  
        res = (Pi beta0 Zeta[2] + CA caconst + 2 TF NF nfconst)
      ]

    Iclustl[leg_?AssociationQ, obspar_?AssociationQ] := 0

  End[]

EndPackage[]