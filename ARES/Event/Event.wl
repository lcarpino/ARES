(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Event *)

(* :Author: *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)

BeginPackage["ARES`Event`Event`"]

  BuildEvent::usage = ""

  Begin["`Private`"]

  Needs["ARES`Event`EventEPATwoJet`"]
  Needs["ARES`Event`EventEPAThreeJet`"]

  BuildEvent[event_List?(MatchQ[lst_ /; Length[lst] == 0])] := BuildEventEPATwoJet[event]
  BuildEvent[event_List?(MatchQ[lst_ /; Length[lst] == 2])] := BuildEventEPAThreeJet[event]

  End[]

EndPackage[]