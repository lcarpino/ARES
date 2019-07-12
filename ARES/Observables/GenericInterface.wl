(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: GenericInterface *)

(* :Author: *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)

BeginPackage["ARES`Observables`GenericInterface`",
  {
    "ARES`Observables`Generic`"
  }]

  InitialiseObservable::usage = ""

  Begin["`Private`"]

    InitialiseObservable[] :=
      Module[
        {testlegs, testdips, testobs},

        testlegs = BuildMapThreeLegs[0.3, 0.9];
        testdips = BuildMapDipoles[testlegs];
        testobs  = ARES`Observables`DParameter`SoftCollinearParametrisation`BuildMapDParameter[testdips, testlegs];
        {testlegs, testdips, testobs};

        ARES`Observables`DParameter`SoftCollinearCorrections`InitialiseCorrections[]

      ]

      (* {ARES`Observables`DParameter`Initialise[], ARES`Observables`CParameter`Initialise[]} *)

  End[]

EndPackage[]
