(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: NPCorrections *)

(* :Author: *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)


BeginPackage["ARES`Observables`DParameter`NPCorrections`"]

  InitialiseNPCorrections::usage = ""
  DeltaNP::usage = ""

  Begin["`Private`"]

    Needs["ARES`Config`"]

    (* variables to hold any grids *)
    InitialiseNPCorrections[] :=
      Module[
        {gNP12Grid},

        gNP12Grid = Import["DParameter-gNP12.mx", Path -> $ARESGrids];

        gNP12Interpolation = Interpolation[
                              Re[
                              Flatten[
                                gNP12Grid, 1]], InterpolationOrder -> 1];

        gNP13Interpolation = gNP12Interpolation;
        gNP23Interpolation = gNP12Interpolation;

      ];


    DeltaNP[event_?AssociationQ] :=
      Module[
        {xq, xqb, xg, lambda12},

        xq  = event["xq"];
        xqb = event["xqb"];
        xg  = 2 - xq - xqb;
        lambda12 = (2 (1 - xq) (1 - xqb) (1 - xg))/(xq xqb xg);

        27 lambda12 Total[Map[Deltaab[#] &, event["dipoles"]]]
      ]

    Deltaab[dip_?AssociationQ] :=
      Module[
       {xa, xb, res},
  
        xa = dip["legs"][[1]]["x"];
        xb = dip["legs"][[2]]["x"];
  
        res = dip["col"]/2 gNP12Interpolation[xa, xb]
      ]

  End[]

EndPackage[]
