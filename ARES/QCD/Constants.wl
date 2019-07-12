(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Constants *)

(* :Author: *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)

BeginPackage["ARES`QCD`Constants`"]


  TF::usage = "TF is the index of the fundamental representation"
  TR::usage = "TR is the index of a general representation R, this is used interchangeably with TF"
  Nc::usage = "Nc is the number of colours"
  CF::usage = "CF is the quadratic Casimir of the fundamental representation"
  CA::usage = "CA is the quadratic Casimir of the adjoint representation"

  NU::usage = "nu is the number of up flavour quarks"
  ND::usage = "nd is the number of down flavour quarks"
  NF::usage = "nf is the total number of quarks"

  beta0::usage = "beta0 is "
  beta1::usage = "beta1 is "
  beta2::usage = "beta2 is "
  beta3::usage = "beta3 is "
  beta4::usage = "beta4 is "

  K1::usage = "K1 is "
  K2::usage = "K2 is "
  K3::usage = "K3 is "

  Ga0q::usage = "Ga0q is "
  Ga0g::usage = "Ga0g is "
  Ga1q::usage = "Ga1q is "
  Ga1g::usage = "Ga1g is "

  MZ::usage = ""
  AlphaSMZ::usage = ""

  Begin["`Private`"]

  (* SU(3) constants *)
  TF = 1/2;
  TR = 1/2;
  Nc = 3;
  CF = (Nc^2-1)/(2 Nc)
  CA = Nc;

  (* number of fermions *)
  NU = 2;
  ND = 3;
  NF = NU + ND;

  (* beta *)
  beta0 = (11 CA - 4 TF NF)/(12 Pi);
  beta1 = (17 CA^2 - TF NF (10 CA + 6 CF))/(24 Pi^2);
  beta2 = 1/(3456 Pi^3) (2857 CA^3 + 2 TF NF (54 CF^2 - 615 CF CA - 1415 CA^2) \
            + (2 TF NF)^2 (66 CF + 79 CA));
  beta3 = 0;
  beta4 = 0;

  (* K *)
  K1 = CA (67/18 - \[Pi]^2/6) - 10/9 TF NF;
  K2 = CA^2 (245/24 - 67/9 Zeta[2] + 11/6 Zeta[3] + 11/5 Zeta[2]^2) \
         + 2 CF TF NF (-(55/24) + 2 Zeta[3]) \
         + 2 CA TF NF (-(209/108) + 10/9 Zeta[2] - 7 /3 Zeta[3]) \
         - 1/27 (2 TF NF)^2 + (\[Pi] beta0)/2 (CA (808/27 - 28 Zeta[3]) \
         - 224/54 2 TF NF);
  K3 = 0;

  (* (minus) coefficient of delta(1-x) in AP splitting functions *)
  Ga0q = (-3 CF)/2;
  Ga0g = (-11 CA + 4 TF NF)/6;
  Ga1q = -CF/2 (CF (3/4 - Pi^2 + 12 Zeta[3]) + CA (17/12 + (11 Pi^2)/9 - 6 Zeta[3]) \
           - 2 TF NF (1/6 + (2 Pi^2)/9));
  Ga1g = CF TF NF + 4/3 CA TF NF - CA^2 (8/3 + 3 Zeta[3]);

  (* AlphaS in MSbar scheme *)
  MZ = 91.188;
  AlphaSMZ = 0.118;

  End[]
EndPackage[]