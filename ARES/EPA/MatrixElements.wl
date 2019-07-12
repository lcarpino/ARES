(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: MatrixElements *)

(* :Author: *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)

BeginPackage["ARES`EPA`MatrixElements`", {"ARES`QCD`Constants`"}]

  M3sq::usage = ""
  Virt3::usage = ""

  Begin["`Private`"]

    M3sq[xq_?NumericQ, xqb_?NumericQ] :=
      CF (xq^2 + xqb^2)/((1 - xq) (1 - xqb))

    R[x_?NumericQ, y_?NumericQ] :=
      Log[x] Log[y] - Log[x] Log[1 - x] - Log[y] Log[1 - y] + Pi^2/6 \
        - PolyLog[2, x] - PolyLog[2, y]

    ERTV[xq_?NumericQ, xqb_?NumericQ] :=
      Module[
        {
          yqqb, yqg, yqbg,
          Lqqb, Lqg, Lqbg,
          cfsub, casub, tfsub,
          ertv
        },
        yqqb = xq + xqb - 1;
        yqg = 1 - xqb;
        yqbg = 1 - xq;
  
        Lqqb = Log[yqqb];
        Lqg = Log[yqg];
        Lqbg = Log[yqbg];
 
        cfsub = (yqqb/(yqqb + yqg) + yqqb/(yqqb + yqbg) + (yqqb + yqbg)/yqg + (yqqb + yqg)/yqbg
                 + Lqg (4 yqqb^2 + 2 yqqb yqg + 4 yqqb yqbg + yqg yqbg)/(yqqb + yqbg)^2
                 + Lqbg (4 yqqb^2 + 2 yqqb yqbg + 4 yqqb yqg + yqg yqbg)/(yqqb + yqg)^2
                 - 2 ((yqqb^2 + (yqqb + yqg)^2)/(yqg yqbg) R[yqqb, yqbg]
                 + (yqqb^2 + (yqqb + yqbg)^2)/(yqg yqbg) R[yqqb, yqg]
                 + (yqg^2 + yqbg^2)/(yqg yqbg (yqg + yqbg))
                 - 2 Lqqb (yqqb^2/(yqg + yqbg)^2 + (2 yqqb)/(yqg + yqbg))));
  
        casub = (Lqg yqg/(yqqb + yqbg) + Lqbg yqbg/(yqqb + yqg)
                 + ((yqqb^2 + (yqqb + yqg)^2)/(yqg yqbg) R[yqqb, yqbg]
                 + (yqqb^2 + (yqqb + yqbg)^2)/(yqg yqbg) R[yqqb, yqg]
                 + (yqg^2 + yqbg^2)/(yqg yqbg (yqg + yqbg))
                 - 2 Lqqb (yqqb^2/(yqg + yqbg)^2 + (2 yqqb)/(yqg + yqbg)))
                 - R[yqg, yqbg] (yqg/yqbg + yqbg/yqg + (2 yqqb)/(yqg yqbg)));
  
        tfsub = 0;
  
        CF {cfsub, casub, tfsub}
      ]

    Virt3[xq_?NumericQ, xqb_?NumericQ] :=
      Module[
        {
          ertv,
          cftot, catot, tftot,
          cfsub, casub, tfsub
        },
  
        cftot = M3sq[xq, xqb] (Pi^2 - 8);
        catot = M3sq[xq, xqb] (Pi^2/2);
        tftot = 0;
   
        ertv = ERTV[xq, xqb];
        cftot = cftot + ertv[[1]];
        catot = catot + ertv[[2]];
        tftot = tftot + ertv[[3]];
   
        CF cftot + CA catot + TF NF tftot
      ]

  End[]
EndPackage[]