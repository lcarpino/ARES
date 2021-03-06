(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SoftCollinearParametrisation *)

(* :Author: *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)

BeginPackage["ARES`Observables`DParameter`SoftCollinearParametrisation`"]

  InitialiseSoftCollinearParametrisation::usage = ""

  BuildMapDParameter::usage =
    "BuildMapDParameter[dipoles_?ListQ, legs_?ListQ]"

  Begin["`Private`"]

    (* Constant parameters of the D-parameter *)
    ktpow = 1;
    etapow = {1, 1, 1};
    spow = 2;

    InitialiseSoftCollinearParametrisation[] := 0;

    BuildMapDParameter[Event_?AssociationQ] :=
      BuildMapDParameter[Event["dipoles"], Event["legs"]]

    BuildMapDParameter[dipoles_?ListQ, legs_?ListQ] :=
      Module[
        {
          xq, xqb,
          diplogdabbar, diplog2dabbar,
          diplogdbar, diplog2dbar,
          leglogdlbar, leglog2dlbar,
          mlogd, mlogdbar
        },
  
        (* find q flavour leg *)
        Which[
          legs[[1]]["flav"] == "q", xq = legs[[1]]["x"],
          legs[[2]]["flav"] == "q", xq = legs[[2]]["x"],
          legs[[3]]["flav"] == "q", xq = legs[[3]]["x"]
        ];

        (* find qb flavour leg *)
        Which[
          legs[[1]]["flav"] == "qb", xqb = legs[[1]]["x"],
          legs[[2]]["flav"] == "qb", xqb = legs[[2]]["x"],
          legs[[3]]["flav"] == "qb", xqb = legs[[3]]["x"]
        ];

        mlogd = Map[logd[xq, xqb] &, legs];
        mlogdbar = Map[logdbar[xq, xqb] &, legs];

        diplogdabbar = Map[logdabbar[#] &, dipoles];
        diplog2dabbar = Map[log2dabbar[#] &, dipoles];
  
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
  
        leglogdlbar = Map[logdlbar[#, xq, xqb] &, legs];
        leglog2dlbar = Map[log2dlbar[#, xq, xqb] &, legs];
   
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

    d[xq_?NumericQ, xqb_?NumericQ] :=
      Module[
        {lambda12, xl, xg = 2 - xq - xqb},

        lambda12 = (2 (1 - xq) (1 - xqb) (1 - xg))/(xq xqb xg);

        54 lambda12
      ]

    dl[leg_?AssociationQ, xq_?NumericQ, xqb_?NumericQ] :=
      Module[
        {lambda12, xl, xg = 2 - xq - xqb},
  
        xl = leg["x"];
  
        lambda12 = (2 (1 - xq) (1 - xqb) (1 - xg))/(xq xqb xg);
  
        (54 lambda12)/xl
      ]

    dab[dip_?AssociationQ] :=
      Module[
        {lambda12, xa, xb, daba, dabb},
  
        xa = dip["legs"][[1]]["x"];
        xb = dip["legs"][[2]]["x"];
  
        lambda12 = (2 (1 - xa) (1 - xb) (1 - (2 - xa - xb)))/(xa xb (2 - xa - xb));
 
        daba = 54 lambda12 (xa + xb - 1)/xa;
        dabb = 54 lambda12 (xa + xb - 1)/xb;
  
        {daba, dabb}
      ]

    logd[xq_?NumericQ, xqb_?NumericQ] :=
      Log[d[xq, xqb]]

    logdbar[xq_?NumericQ, xqb_?NumericQ] :=
      Log[d[xq, xqb]] - spow Log[2]

    logdlbar[leg_?AssociationQ, xq_?NumericQ, xqb_?NumericQ] := 
      Log[dl[leg, xq, xqb]] - spow Log[2]

    log2dlbar[leg_?AssociationQ, xq_?NumericQ, xqb_?NumericQ] :=
      (Log[dl[leg, xq, xqb]] - spow Log[2])^2 + spow^2/2 Zeta[2]

    logdabbar[dipole_?AssociationQ] :=
      Log[dab[dipole]] - spow Log[2]

    log2dabbar[dipole_?AssociationQ] :=
      (Log[dab[dipole]] - spow Log[2])^2 + spow^2/2 Zeta[2]

  End[]

EndPackage[]