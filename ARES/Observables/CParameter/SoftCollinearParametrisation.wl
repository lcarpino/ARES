(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SoftCollinearParametrisation *)

(* :Author: *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)

BeginPackage["ARES`Observables`CParameter`SoftCollinearParametrisation`"]

  BuildMapCParameter::usage = ""

  Begin["`Private`"]

    (* Constant parameters of the D-parameter *)
    ktpow = 1;
    etapow = {1, 1};
    spow = 0;

    BuildMapCParameter[] :=
      Module[
        {},
        0
      ]

    dl[] :=
      Module[
        {},
        0
      ]

    dab[] :=
      Module[
        {},
        0
      ]

    logdlbar[] := 0

    log2dlbar[] := 0

    logdabbar[] := 0

    log2dabbar[] := 0

  End[]

EndPackage[]