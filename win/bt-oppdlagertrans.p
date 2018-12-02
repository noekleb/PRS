/************************************************************
    Program:  bt-oppdlagertrans.p
    Created:  TN   11 Jan 1999
Description:  Oppdatering av lagertransaksjoner.

   TN   13/3-99  Lagt inn håndtering av Batch nummer.
   TN   29/3-99  Nullstiller wBatchListe for den bygges pr. dag.
   TN   22/4-99  Leser ferdige batcher med status = 2.
   TN   22/4-99  - Debug niv† innfort.
                 - Forenklet logikken i loopen.


Last change:  TN   22 Dec 99   11:59 am
************************************************************/

RUN bt-2oppdlagertrans.p (?).

