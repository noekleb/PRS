DEF INPUT  PARAM icVarebehRowid AS CHAR NO-UNDO.
DEF INPUT  PARAM icFields            AS CHAR NO-UNDO.
DEF INPUT  PARAM icValues            AS CHAR NO-UNDO.
DEF INPUT  PARAM icSessionId         AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn            AS CHAR NO-UNDO.

DEF VAR bOK            AS LOG    NO-UNDO.
DEF VAR ix             AS INT    NO-UNDO.
DEF VAR iNyProfil      AS INT    NO-UNDO.
DEF VAR cNyButikkListe AS CHAR   NO-UNDO.
DEF VAR iNyMesse       AS INT    NO-UNDO.
DEF VAR iButikkNr      AS INT    NO-UNDO.

FIND VareBehHode NO-LOCK
     WHERE ROWID(VareBehHode) = TO-ROWID(icVarebehRowid) 
     NO-ERROR.

IF NOT AVAIL VareBehHode THEN RETURN.

ASSIGN iNyProfil      = INT(DYNAMIC-FUNCTION("getValue" IN SOURCE-PROCEDURE,"ProfilNr"))
       cNyButikkListe = DYNAMIC-FUNCTION("getValue" IN SOURCE-PROCEDURE,"ButikkListe")
       iNyMesse       = DYNAMIC-FUNCTION("getValue" IN SOURCE-PROCEDURE,"MesseNr")
       .

IF iNyProfil NE VareBehHode.ProfilNr THEN DO:
  IF CAN-FIND(FIRST VareBehLinje OF VareBehHode) AND 
     DYNAMIC-FUNCTION("getCurrentAction" IN SOURCE-PROCEDURE) NE "create" THEN
    ocReturn = "Prisprofil kan ikke byttes etter at det er lagt inn artikler".
  ELSE DO ix = 1 TO NUM-ENTRIES(cNyButikkListe):
    iButikkNr = INT(ENTRY(ix,cNyButikkListe)) NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN DO:
      FIND Butiker NO-LOCK
           WHERE Butiker.Butik = iButikkNr  
           NO-ERROR.
      IF AVAIL Butiker AND Butiker.ProfilNr NE iNyProfil THEN DO:
        ocReturn = "Kan ikke sette profil pga at det inngår butikker i butikklista som ikke har aktuell profil".
        RETURN.
      END.
    END.
  END.
END.

IF iNyMesse NE 0 THEN DO:    
  FIND Messe NO-LOCK
       WHERE Messe.MesseNr = iNyMesse
       NO-ERROR. 
  IF NOT AVAIL Messe THEN
    ocReturn = "Ugyldig messenr".
END.
