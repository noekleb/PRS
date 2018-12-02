/* Kopier valuta fra prisprofil 1 til en annen profil
   Parameter:  
   Opprettet: 28.01.10 av BHa              
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEFINE VARIABLE hQuery   AS HANDLE NO-UNDO.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME).
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END TRANSACTION:
  FIND FIRST KasValuta EXCLUSIVE-LOCK
       WHERE KasValuta.ValKod   = string(ihBuffer:BUFFER-FIELD("ValKod"):BUFFER-VALUE)
         AND KasValuta.ProfilNr = INTEGER(icParam)
       NO-WAIT NO-ERROR.
  IF NOT AVAIL KasValuta AND NOT LOCKED(KasValuta) THEN DO:
    CREATE KasValuta.
    BUFFER KasValuta:HANDLE:BUFFER-COPY(ihBuffer,"ValDatum,Profilnr").
    ASSIGN KasValuta.ProfilNr = INTEGER(icParam)
           KasValuta.ValAktiv = YES
           .
  END.
  IF AVAIL KasValuta THEN
    ASSIGN 
    KasValuta.ValKurs  = ihBuffer:BUFFER-FIELD("ValKurs"):BUFFER-VALUE
    KasValuta.Retur    = ihBuffer:BUFFER-FIELD("Retur"):BUFFER-VALUE.

  hQuery:GET-NEXT().
END. 

DELETE OBJECT hQuery NO-ERROR.

obOK = ocReturn = "".
