/* varebok_slett.p: Sletter en vareboklinjer og tilhørende artikler i messebok dersom det ikke fins bestilling
   Opprettet 23.02.07 av BHa     
-----------------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR hQuery       AS HANDLE NO-UNDO.
DEF VAR ix           AS INT    NO-UNDO.

DEF BUFFER bVarebokLinje FOR VareBokLinje.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME).

hQuery:QUERY-OPEN().
hQuery:GET-FIRST().

FIND FIRST VareBehHode NO-LOCK
     WHERE VareBehHode.Kilde = DECIMAL(ihBuffer:BUFFER-FIELD("VarebokNr"):BUFFER-VALUE)
     NO-ERROR.

SletteLoop:
REPEAT WHILE NOT hQuery:QUERY-OFF-END TRANSACTION:
  FIND bVarebokLinje EXCLUSIVE-LOCK 
       WHERE bVarebokLinje.VareBokNr  = DECIMAL(hQuery:GET-BUFFER-HANDLE(1):BUFFER-FIELD("VarebokNr"):BUFFER-VALUE)
         AND bVarebokLinje.ArtikkelNr = DECIMAL(hQuery:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE)
       NO-ERROR.
  IF AVAIL VareBehHode THEN DO:
    FIND FIRST VareBehLinje EXCLUSIVE-LOCK 
         WHERE VareBehLinje.VareBehNr  = VareBehHode.VareBehNr
           AND VareBehLinje.ArtikkelNr = DECIMAL(ihBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE)
         NO-ERROR.
    IF AVAIL VareBehLinje THEN DO:
      FOR EACH varebehlinjetrans EXCLUSIVE-LOCK 
          WHERE varebehlinjetrans.VareBehNr  = VareBehHode.VareBehNr
            AND varebehlinjetrans.ArtikkelNr = VareBehLinje.ArtikkelNr
          :
        IF varebehlinjetrans.RegistrertBestilling THEN DO:
          IF ocReturn = "" THEN
            ocReturn = "Advarsel: Det fins artikler med bestilling i utvalget. Disse artiklene ble ikke slettet".  
          hQuery:GET-NEXT().
          NEXT SletteLoop.
        END.
        ELSE DELETE varebehlinjetrans.
      END.
      DELETE VareBehLinje.
    END.
    DELETE bVarebokLinje.
  END.
  ELSE IF AVAIL bVarebokLinje THEN DELETE bVarebokLinje.
  hQuery:GET-NEXT().
END.

obOk = ocReturn = "".
