(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: EventEPATwoJet *)

(* :Author: *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)

BeginPackage["ARES`Event`EventEPATwoJet`",
  {
    "ARES`QCD`Constants`"
  }]

  BuildEventEPATwoJet::usage = ""

  Begin["`Private`"]

    BuildEventEPATwoJet[eventConfig_List] :=
      Module[
        {legs, dipoles},

        legs = BuildMapTwoLegs[];
        dipoles = BuildMapDipoles[legs];

        Association[
          "legs" -> legs,
          "dipoles" -> dipoles,
          "njets" -> 2,
          "type" -> "EPA"
        ]
      ]


    BuildMapTwoLegs[] :=
      Module[
        {
          x1, x2,
          xflav, xnum,
          legmap, colmap,
          legs
        },
  
        x1 = 1;
        x2 = 1;
        legmap = Association["q" -> 1, "qb" -> 2, 1 -> "q", 2 -> "qb"];
  
        colmap = Association["q" -> CF, "qb" -> CF];
        xflav = {xq, xqb};
        xnum = {x1, x2};
  
        legs =
          Table[
            Association[
              "num"  -> i,
              "flav" -> legmap[i],
              "col"  -> colmap[legmap[i]],
              "x"    -> xnum[[i]]
            ],
            {i, 1, 2}
          ]
      ]


    BuildMapDipoles[legs_?ListQ] :=
      Module[
        {
          leggroup, dipnames, dipflavs, dipcols, dipcsq, dipssq
        },
  
        leggroup = Subsets[legs, {2}];
  
        dipnames =
          Table[
            StringJoin[ToString /@ (#["num"] & /@ leggroup[[i]])],
            {i, Length[leggroup]}
          ];
  
        dipflavs =
          Table[
            StringJoin[ToString /@ (#["flav"] & /@ leggroup[[i]])],
            {i, Length[leggroup]}
          ];
  
        dipcols =
          Table[
            Which[
              dipflavs[[i]] == "qqb" || dipflavs[[i]] == "qbq", 2 CF
            ],
            {i, Length[leggroup]}
          ];
  
        dipcsq =
          Table[
            Module[
              {xa, xb},
              xa = leggroup[[i]][[1]]["x"];
              xb = leggroup[[i]][[2]]["x"];
     
              ((1 - xa) (1 - xb))/(xa xb)
            ],
            {i, Length[leggroup]}
          ];
  
        dipssq =
          Table[
            Module[
              {xa, xb},
              xa = leggroup[[i]][[1]]["x"];
              xb = leggroup[[i]][[2]]["x"];
     
              (xa + xb - 1)/(xa xb)
            ],
            {i, Length[leggroup]}
          ];
  
        Table[
          Association[
            "num" -> dipnames[[i]],
            "flav" -> dipflavs[[i]],
            "col" -> dipcols[[i]],
            "legs" -> leggroup[[i]],
            "csq" -> dipcsq[[i]],
            "ssq" -> dipssq[[i]]
          ],
          {i, 1, Length[leggroup]}
        ]
      ]

  End[]

EndPackage[]