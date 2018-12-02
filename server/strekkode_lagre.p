/* Lagre strekkoder 
   Parametere:temp-tabell med strekkoder for artikkel
   
   Opprettet: 17.11.04 av BHa                  
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR hQuery        AS HANDLE NO-UNDO.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " WHERE bBlank AND Kode NE '' NO-LOCK").
hQuery:QUERY-OPEN().


IF hQuery:IS-OPEN THEN DO TRANSACTION ON ERROR UNDO, LEAVE:

  hQuery:GET-FIRST().
  REPEAT WHILE NOT hQuery:QUERY-OFF-END:

    FIND Strekkode EXCLUSIVE-LOCK WHERE
      Strekkode.Kode = ihBuffer:BUFFER-FIELD("Kode"):BUFFER-VALUE NO-ERROR.
    IF NOT AVAILABLE Strekkode THEN 
    DO:
        CREATE Strekkode.
        ASSIGN Strekkode.kode       = ihBuffer:BUFFER-FIELD("Kode"):BUFFER-VALUE
               Strekkode.kodetype   = ihBuffer:BUFFER-FIELD("Kodetype"):BUFFER-VALUE
               Strekkode.Artikkelnr = ihBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE
               Strekkode.StrKode    = ihBuffer:BUFFER-FIELD("StrKode"):BUFFER-VALUE.
    END.
    ELSE ASSIGN Strekkode.Artikkelnr = ihBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE
                Strekkode.StrKode    = ihBuffer:BUFFER-FIELD("StrKode"):BUFFER-VALUE.
    hQuery:GET-NEXT().
  END.
END.

DELETE OBJECT hQuery.

IF ocReturn = "" THEN 
  obOk = TRUE.

