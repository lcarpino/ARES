(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SoftCollinearParametrisation *)

(* :Author: *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)

BeginPackage["ARES`Observables`Thrust`SoftCollinearParametrisation`"]

  BuildMapThrust::usage = ""

  Begin["`Private`"]

    (* Constant parameters of the D-parameter *)
    ktpow = 1;
    etapow = {1, 1};
    spow = 0;

    BuildMapThrust[dipoles_?ListQ, legs_?ListQ] :=
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
        1
      ]

    dl[] :=
      Module[
        {},
        1
      ]

    dab[] :=
      Module[
        {},
        {1, 1}
      ]

    logd[] :=
      Log[d[]]

    logdbar[] :=
      Log[d[]]

    logdlbar[] :=
      Log[dl[]]

    log2dlbar[] :=
      Log[dl[]]^2

    logdabbar[] :=
      Log[dab[]]

    log2dabbar[] :=
      Log[dab[]]^2

  End[]

EndPackage[]