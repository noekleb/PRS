/************************************************************
    Program:  tmpfilliste.i
    Created:  TN    17 Jul 99
Description:  Definisjon av temp-file som benyttes for †
              bygge en liste over filer som skal leses inn.

Last change:  TN   18 Jul 99   10:43 am
************************************************************/

DEF {&New} shared TEMP-TABLE tmpFilListe
  FIELD FilNavn   as CHAR FORMAT "x(20)"
  FIELD FullPath  as CHAR FORMAT "x(40)"
  field Ekstent   as char FORMAT "x(3)"
  field File-Size as INT
  FIELD FilDato   as date
  FIELD FilTid    as int
  .

