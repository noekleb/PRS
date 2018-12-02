/************************************************************
    Program:  plukkliste.i
    Created:  TN   21 Dec 99
Description:

Last change:  TN   21 Dec 99    7:49 pm
************************************************************/

DEF {&New} shared TEMP-TABLE PlukkListe
  FIELD Butik      LIKE Butiker.Butik
  FIELD Vg         LIKE ArtBas.Vg
  FIELD LopNr      LIKE ArtBas.LopNr
  FIELD Storl      LIKE ArtLag.Storl
  FIELD Hg         LIKE ArtBas.Hg
  FIELD Antall     as   INT format "-zz9"
  FIELD RabKr      as   DEC FORMAT "-zzzz9.99"
  field Pris       as   dec format "-zzzzz9.99".


