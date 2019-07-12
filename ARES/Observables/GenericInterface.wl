(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: GenericInterface *)

(* :Author: *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)

BeginPackage["ARES`Observable`GenericInterface`",
  {
    "ARES`Observables`Generic`",
    "ARES`Observables`DParameter`SoftCollinearParametrisation`",
    "ARES`Observables`CParameter`SoftCollinearParametrisation`"
  }]

  InitialiseObservable::usage = ""

  Begin["`Private`"]

    InitialiseObservable[] :=
      Module[
        {testlegs, testdips, testobs},

        testlegs = BuildMapThreeLegs[0.3, 0.9];
        testdips = BuildMapDipoles[testlegs];
        testobs  = ARES`Observables`DParameter`SoftCollinearParametrisation`BuildMapDParameter[testdips, testlegs];
        {testlegs, testdips, testobs}

      ]

      (* {ARES`Observable`DParameter`Initialise[], ARES`Observable`CParameter`Initialise[]} *)

  End[]

EndPackage[]
