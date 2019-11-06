(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SoftCollinearParametrisation *)

(* :Author: *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)

BeginPackage["ARES`Observables`CParameter`SoftCollinearParametrisation`"]

  InitialiseSoftCollinearParametrisation::usage = ""

  BuildMapCParameter::usage =
    ""

  Begin["`Private`"]

    (* Constant parameters of the C-parameter *)
    ktpow = 1;
    etapow = {1, 1};
    spow = 0;

    InitialiseSoftCollinearParametrisation[] := 0;

    BuildMapCParameter[Event_?AssociationQ] :=
      BuildMapCParameter[Event["dipoles"], Event["legs"]]

    BuildMapCParameter[dipoles_?ListQ, legs_?ListQ] :=
      Module[
        {
          diplogdabbar, diplog2dabbar,
          diplogdbar, diplog2dbar,
          leglogdlbar, leglog2dlbar,
          mlogd, mlogdbar
        },

        mlogd = Map[logd[] &, legs];
        mlogdbar = Map[logdbar[] &, legs];

        diplogdabbar = Map[logdabbar[] &, dipoles];
        diplog2dabbar = Map[log2dabbar[] &, dipoles];
  
        diplogdbar =
          Association[
            Table[
              dipoles[[i]]["num"] -> diplogdabbar[[i]],
            {i, Length[dipoles]}
            ]
          ];
  
        diplog2dbar =
          Association[
            Table[
              dipoles[[i]]["num"] -> diplog2dabbar[[i]],
              {i, Length[dipoles]}
            ]
          ];
  
        leglogdlbar = Map[logdlbar[] &, legs];
        leglog2dlbar = Map[log2dlbar[] &, legs];
   
        Association[
          "ktpow" -> ktpow,
          "etapow" -> etapow,
          "spow" -> spow,
          "logd" -> mlogd,
          "logdbar" -> mlogdbar,
          "logdabbar" -> diplogdbar,
          "log2dabbar" -> diplog2dbar,
          "logdlbar" -> leglogdlbar,
          "log2dlbar" -> leglog2dlbar
        ]
      ]

    d[] :=
      Module[
        {},
        6
      ]

    dl[] :=
      Module[
        {},
        6
      ]

    dab[] :=
      Module[
        {},
        {6, 6}
      ]

    logd[] :=
      Log[d[]]

    logdbar[] :=
      Log[d[]] - spow Log[2]

    logdlbar[] :=
      Log[dl[]] - spow Log[2]

    log2dlbar[] :=
      (Log[dl[]] - spow Log[2])^2 + spow^2/2 Zeta[2]

    logdabbar[] :=
      Log[dab[]] - spow Log[2]

    log2dabbar[] :=
      (Log[dab[]] - spow Log[2])^2 + spow^2/2 Zeta[2]

  End[]

EndPackage[]