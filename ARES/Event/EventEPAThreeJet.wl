(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: EventEPAThreeJet *)

(* :Author: *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)

BeginPackage["ARES`Event`EventEPAThreeJet`",
  {
    "ARES`QCD`Constants`"
  }]

  BuildEventEPAThreeJet::usage = ""

  Begin["`Private`"]

    BuildEventEPAThreeJet[eventConfig_List] :=
      Module[
        {xq, xqb, legs, dipoles},

        xq  = eventConfig[[1]];
        xqb = eventConfig[[2]];
        legs = BuildMapThreeLegs[xq, xqb];
        dipoles = BuildMapDipoles[legs];

        Association[
          "xq"  -> xq,
          "xqb" -> xqb,
          "legs" -> legs,
          "dipoles" -> dipoles,
          "njets" -> 3,
          "type" -> "EPA"
        ]
      ]


   BuildMapThreeLegs[xq_?NumericQ, xqb_?NumericQ] :=
      Module[
        {
          xg = 2 - xq - xqb,
          x1, x2, x3,
          xflav, xnum,
          legmap, colmap,
          legs
        },
  
        Which[
          xq >= xqb >= xg, {x1 = xq, x2 = xqb, x3 = xg, 
            legmap = Association["q" -> 1, "qb" -> 2, "g" -> 3, 1 -> "q", 2 -> "qb", 3 -> "g"]},
          xq >= xg >= xqb, {x1 = xq, x2 = xg, x3 = xqb, 
            legmap = Association["q" -> 1, "qb" -> 3, "g" -> 2, 1 -> "q", 2 -> "g", 3 -> "qb"]},
          xqb >= xq >= xg, {x1 = xqb, x2 = xq, x3 = xg, 
            legmap = Association["q" -> 2, "qb" -> 1, "g" -> 3, 1 -> "qb", 2 -> "q", 3 -> "g"]},
          xqb >= xg >= xq, {x1 = xqb, x2 = xg, x3 = xq, 
            legmap = Association["q" -> 3, "qb" -> 1, "g" -> 2, 1 -> "qb", 2 -> "g", 3 -> "q"]},
          xg >= xq >= xqb, {x1 = xg, x2 = xq, x3 = xqb, 
            legmap = Association["q" -> 2, "qb" -> 3, "g" -> 1, 1 -> "g", 2 -> "q", 3 -> "qb"]},
          xg >= xqb >= xq, {x1 = xg, x2 = xqb, x3 = xq, 
            legmap = Association["q" -> 3, "qb" -> 2, "g" -> 1, 1 -> "g", 2 -> "qb", 3 -> "q"]}
        ];
  
        colmap = Association["q" -> CF, "qb" -> CF, "g" -> CA];
        xflav = {xq, xqb, xg};
        xnum = {x1, x2, x3};
  
        legs =
          Table[
            Association[
              "num"  -> i,
              "flav" -> legmap[i],
              "col"  -> colmap[legmap[i]],
              "x"    -> xnum[[i]]
            ],
            {i, 1, 3}
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
              dipflavs[[i]] == "qqb" || dipflavs[[i]] == "qbq", 2 CF - CA,
              dipflavs[[i]] == "qg" || dipflavs[[i]] == "gq", CA,
              dipflavs[[i]] == "qbg" || dipflavs[[i]] == "gqb", CA
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