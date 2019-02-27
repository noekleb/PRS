/* Sjekk om merknad kan slettes eller SLETT merknadskode fra messe-register
   
   Opprettet: 17.09.07 av BHa                  
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR fMesseNr     AS DEC  NO-UNDO.
DEF VAR bDelete      AS LOG  NO-UNDO.
DEF VAR ix           AS INT  NO-UNDO.
DEF VAR bIbruk       AS LOG  NO-UNDO.
DEF VAR cNyMerknad   AS CHAR NO-UNDO.
DEF VAR cNyFargeKode AS CHAR NO-UNDO.

DEF TEMP-TABLE ttIkkeIbruk
    FIELD cIkkeIbruk AS CHAR.

fMesseNr = DEC(ENTRY(1,icParam)).

IF NUM-ENTRIES(icParam) > 1 AND ENTRY(2,icParam) = "delete" THEN
  bDelete = YES.

FIND FIRST Messe NO-LOCK
     WHERE Messe.MesseNr = fMesseNr
     NO-ERROR.
IF NOT AVAIL Messe THEN DO:
  ocReturn = "Finner ikke Messen".  
  RETURN.
END.

DO ix = 1 TO NUM-ENTRIES(Messe.oppmerking,"¤"):
  bIbruk = NO.
  FOR EACH VareBokHode NO-LOCK
      WHERE VareBokHode.MesseNr = fMesseNr
      :
    FIND FIRST VareBokLinje NO-LOCK
         OF VareBokHode
         WHERE VareBokLinje.LinjeMerknad = ENTRY(ix,Messe.Oppmerking,"¤")
         NO-ERROR.
    IF AVAIL VareBokLinje THEN
      bIbruk = YES.
  END.
  IF NOT bIbruk THEN DO:
    CREATE ttIkkeIbruk.  
    ttIkkeIbruk.cIkkeIbruk = ENTRY(ix,Messe.Oppmerking,"¤").
  END.
END.

IF NOT bDelete AND ocReturn = "" THEN DO:
  FOR EACH ttIkkeIbruk
      BY ttIkkeIbruk.cIkkeIbruk:
    ocReturn = ocReturn + ttIkkeIbruk.cIkkeIbruk + CHR(10).
  END.
  obOk = TRUE.
END.
ELSE IF ocReturn = "" THEN DO:
  FIND CURRENT Messe EXCLUSIVE-LOCK 
       NO-ERROR.
  IF NOT AVAIL Messe THEN DO:
    ocReturn = "Messen er ikke tilgjengelig for oppdatering".  
    RETURN.      
  END.

  DO ix = 1 TO NUM-ENTRIES(Messe.oppmerking,"¤"):
    FIND FIRST ttIkkeIbruk
         WHERE ttIkkeIbruk.cIkkeIbruk = ENTRY(ix,Messe.Oppmerking,"¤")
         NO-ERROR.
    IF NOT AVAIL ttIkkeIbruk THEN DO:
      cNyMerknad = cNyMerknad + ENTRY(ix,Messe.Oppmerking,"¤") + "¤".
      IF NUM-ENTRIES(Messe.Fargekoder) GE ix THEN
        cNyFargeKode = cNyFargeKode + ENTRY(ix,Messe.Fargekoder) + ",".
    END.
  END.
  ASSIGN Messe.Oppmerking = TRIM(cNyMerknad,"¤")
         Messe.Fargekoder = TRIM(cNyFargeKode,",")
         obOk             = YES.
END.

