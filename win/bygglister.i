/************************************************************
    Program:  bygglister.i
    Created:  TN    8 Jul 99
Description:  Bygger kommaseparerte lister.

              {&Variabel}  - Navn på variabel som skal initieres.
              {&Tekst}     - Tekst som skal settes inn i teksttreng.
              {&Fil}       - Databasefil
              {&Felt}      - Databasefelt.

Last change:  TN    8 Jul 99   11:05 am
************************************************************/

{&Variabel}  = "".
FI-Info = "Initiering av {&Tekst} pågår...".
display FI-Info with frame {&FRAME-NAME}.
{sww.i}
for each {&Fil} no-lock:
  assign
    {&Variabel} = {&Variabel} +
                  (if {&Variabel} = ""
                    then ""
                    else ",") +
                  string({&Fil}.{&Felt}).
end.
{swn.i}
FI-Info = "".
display FI-Info with frame {&FRAME-NAME}.

