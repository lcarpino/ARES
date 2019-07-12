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
      ]

    dl[] :=
      Module[
        {},
      ]

    dab[] :=
      Module[
        {},
      ]

    logdlbar[]   := []

    log2dlbar[]  := []

    logdabbar[]  := []

    log2dabbar[] := []

  End[]

EndPackage[]