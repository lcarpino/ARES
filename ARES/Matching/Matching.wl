(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Matching *)

(* :Author: *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)


BeginPackage["ARES`Matching`Matching`"]

  MatchingLogR::usage = ""
  MatchingModR::usage = ""

  Begin["`Private`"]

    MatchingLogR[sigma_?ListQ, SigmaBar_?ListQ, ResumExpanded_?ListQ, Resum_?ListQ] :=
      Module[
        {
          order, matching, res
        },

        order = Length[SigmaBar];
        (* order = Length[ResumExpanded]; *)

        (* perform the matching *)
        matching = Exp[(SigmaBar[[1]]-ResumExpanded[[1]])/sigma[[1]]];

        If[order >= 2,
          matching = (matching Exp[(SigmaBar[[2]]-ResumExpanded[[2]])/sigma[[1]]]
                      Exp[-(SigmaBar[[1]]^2 - ResumExpanded[[1]]^2)/(2 sigma[[1]]^2)]);
        ];

        matching = Resum matching;

        (* normalise distribution to one at end point *)
        res = matching/matching[[-1]]
      ]

    MatchingModR[v_?ListQ, sigma_?ListQ, SigmaBar_?ListQ, ResumExpanded_?ListQ, Resum_?ListQ] :=
      Module[
        {
          order, Z, matching, res
        },

        order = Length[SigmaBar];
        (* order = Length[ResumExpanded]; *)

        Z = (1 - v/v[[-1]]);

        (* perform the matching *)
        matching = sigma[[1]];

        If[order >= 1,
          matching = matching + SigmaBar[[1]] - Z ResumExpanded[[1]];
        ];

        If[order >= 2,
          matching = (matching + SigmaBar[[2]] - Z ResumExpanded[[2]]
                      - Z ResumExpanded[[1]]/sigma[[1]] (SigmaBar[[1]] - (Z+1)/2 ResumExpanded[[1]]));
        ];

        matching = (Resum/sigma[[1]])^Z matching;

        (* normalise distribution to one at end point *)
        res = matching/matching[[-1]]
      ]

  End[]

EndPackage[]
