/* Aktivering av varer til etikettkø
   Parameter:  
   Opprettet: 28.01.10 av BHa              
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEFINE VARIABLE hQuery   AS HANDLE NO-UNDO.
DEFINE VARIABLE h_Prisko AS HANDLE NO-UNDO.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME).
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END TRANSACTION:
    /* Henter databaserecorden. */
    FIND PrisKo EXCLUSIVE-LOCK WHERE
        PrisKo.ArtikkelNr    = DECI(ihBuffer:BUFFER-FIELD("Artikkelnr"):BUFFER-VALUE) AND
        Prisko.ProfilNr      = INT(ihBuffer:BUFFER-FIELD("ProfilNr"):BUFFER-VALUE) AND 
        PrisKo.AktiveresDato = DATE(string(ihBuffer:BUFFER-FIELD("AktiveresDato"):BUFFER-VALUE)) AND 
        PrisKo.AktiveresTid  = INT(ihBuffer:BUFFER-FIELD("AktiveresTid"):BUFFER-VALUE) AND
        PrisKo.Tilbud        = LOGICAL(ihBuffer:BUFFER-FIELD("Tilbud"):BUFFER-VALUE) NO-ERROR.

    IF AVAILABLE PrisKo THEN 
      ASSIGN PrisKo.EtikettStatus = 1.
      
  /* Neste post. */
  hQuery:GET-NEXT().
END. /* BLOKKEN */

DELETE OBJECT hQuery NO-ERROR.

obOK = ocReturn = "".

