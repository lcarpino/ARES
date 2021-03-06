(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: MilanFactor *)

(* :Author: *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)

BeginPackage["ARES`NPShift`MilanFactor`"]

  MilanFactor::usage = ""
  aNP::usage = ""
  deltav::usage = ""

  Begin["`Private`"]

    Needs["ARES`QCD`Constants`"]

    NFL = 3;

    MilanFactor =
      (3/64 ((128 Pi + 128 Pi Log[2] - 35 Pi^2) CA - 10 Pi^2 TR NFL)/(11 CA - 4 TR NFL))

    aNP[alphaS_, Q_, alpha0_, muI_] :=
      (4 muI)/Pi^2 MilanFactor (alpha0 - alphaS - (4 Pi beta0) alphaS^2/(2 Pi)
                                 (Log[Q/muI] + K1/(4 Pi beta0) + 1))

    deltav[alphaS_, Q_, alpha0_, muI_, Delta_] :=
      aNP[alphaS, Q, alpha0, muI]/Q Delta

  End[]

EndPackage[]