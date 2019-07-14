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

    InitialiseObservable[Observable_] :=

      Module[
        {},

        Which[
          Observable == "DParameter",
            ARES`Observables`DParameter`Initialise`InitialiseDParameter[];
            Association[
              "Additive"          -> ARES`Observables`DParameter`Initialise`Additive[],
              "SCParametrisation" -> ARES`Observables`DParameter`Initialise`SoftCollinearMap,
              "TransferFunctions" -> ARES`Observables`DParameter`Initialise`ICorrectionMap[]
            ],

          Observable == "CParameter",
            {}
        ]
      ]

  End[]

EndPackage[]
