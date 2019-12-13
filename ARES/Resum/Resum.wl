(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Resum *)

(* :Author: *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)


BeginPackage["ARES`Resum`Resum`"]

  ResumLLPT::usage = ""
  ResumNLLPT::usage = ""
  ResumNNLLPT::usage = ""

  ResumLLNP::usage = ""
  ResumNLLNP::usage = ""
  ResumNNLLNP::usage = ""

  Begin["`Private`"]

    Needs["ARES`Resum`ResumEPATwoJet`"]
    Needs["ARES`Resum`ResumEPAThreeJet`"]

    (* EPA *)

    (* Two Jet *)

    ResumLLPT[alphaS_?NumericQ, logV_?NumericQ,
              Event_Association?(MatchQ[KeyValuePattern[{"njets" -> 2}]]),
              obsSC_?AssociationQ, opt: OptionsPattern[]] :=
      ResumLLPTEPATwoJet[alphaS, logV, Event, obsSC, opt]
              
    ResumNLLPT[alphaS_?NumericQ, logV_?NumericQ,
               Event_Association?(MatchQ[KeyValuePattern[{"njets" -> 2}]]),
               obsSC_?AssociationQ, opt: OptionsPattern[]] :=
      ResumNLLPTEPATwoJet[alphaS, logV, Event, obsSC, opt]

    ResumNNLLPT[alphaS_?NumericQ, logV_?NumericQ,
                Event_Association?(MatchQ[KeyValuePattern[{"njets" -> 2}]]),
                obsSC_?AssociationQ, opt: OptionsPattern[]] :=
      ResumNNLLPTEPATwoJet[alphaS, logV, Event, obsSC, opt]

    (* Three Jet *)

    ResumLLPT[alphaS_?NumericQ, logV_?NumericQ,
              Event_Association?(MatchQ[KeyValuePattern[{"njets" -> 3}]]),
              obsSC_?AssociationQ, opt: OptionsPattern[]] :=
      ResumLLPTEPAThreeJet[alphaS, logV, Event, obsSC, opt]

    ResumNLLPT[alphaS_?NumericQ, logV_?NumericQ,
               Event_Association?(MatchQ[KeyValuePattern[{"njets" -> 3}]]),
               obsSC_?AssociationQ, opt: OptionsPattern[]] :=
      ResumNLLPTEPAThreeJet[alphaS, logV, Event, obsSC, opt]

    ResumNNLLPT[alphaS_?NumericQ, logV_?NumericQ,
                Event_Association?(MatchQ[KeyValuePattern[{"njets" -> 3}]]),
                obsSC_?AssociationQ, opt: OptionsPattern[]] :=
      ResumNNLLPTEPAThreeJet[alphaS, logV, Event, obsSC, opt]


    ResumLLNP[alphaS_?NumericQ, logV_?NumericQ,
              Event_Association?(MatchQ[KeyValuePattern[{"njets" -> 3}]]),
              obsSC_?AssociationQ, opt: OptionsPattern[]] :=
      ResumLLNPEPAThreeJet[alphaS, logV, Event, obsSC, opt]

    ResumNLLNP[alphaS_?NumericQ, logV_?NumericQ,
               Event_Association?(MatchQ[KeyValuePattern[{"njets" -> 3}]]),
               obsSC_?AssociationQ, opt: OptionsPattern[]] :=
      ResumNLLNPEPAThreeJet[alphaS, logV, Event, obsSC, opt]

    ResumNNLLNP[alphaS_?NumericQ, logV_?NumericQ,
                Event_Association?(MatchQ[KeyValuePattern[{"njets" -> 3}]]),
                obsSC_?AssociationQ, opt: OptionsPattern[]] :=
      ResumNNLLNPEPAThreeJet[alphaS, logV, Event, obsSC, opt]


  End[]
EndPackage[]