/************************************************************
    Program:  etikettlogg.i
    Created:  TN   10 Feb 99
Description:  Definisjon av etikettlogg.

Last change:  TN   10 Feb 99    9:31 pm
************************************************************/

def {&new} shared temp-table EtikettLogg NO-UNDO
  field Vg        like ArtBas.Vg
  field LopNr     like ArtBas.LopNr
  field Ant       as INTEGER FORMAT "->>>>>>>9"
  field Storl     as char
  field bongtekst as char
  field pris      as dec format "-zzz,zz9.99"
  field pris2     as dec format "-zzz,zz9.99"
  FIELD individ   AS DEC DECIMALS 0
  field butik     as int
  field SeqNr     as int.

