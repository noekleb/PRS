CURRENT-WINDOW:WIDTH = 250.
    
/*
Opprinnelig formel fra excelarket
HØYRE
(90 - 
  (
    HVIS(
      DELTEKST(A3;6;1) * 2 > 9;
      DELTEKST(A3;6;1) * 2 - 9; 
      DELTEKST(A3;6;1) * 2
         ) 
  + HVIS(
      DELTEKST(A3;4;1) * 2 > 9;
      DELTEKST(A3;4;1) * 2 - 9;
      DELTEKST(A3;4;1) * 2
         ) 
  + HVIS(
      DELTEKST(A3;2;1) * 2 > 9;
      DELTEKST(A3;2;1) * 2 - 9;
      DELTEKST(A3;2;1) * 2
         ) 
  + DELTEKST(A3;1;1) 
  + DELTEKST(A3;3;1) 
  + DELTEKST(A3;5;1)
  );1
)

*/

DEF VAR lDec   AS DEC                        NO-UNDO.
DEF VAR lChkSiffer AS DEC FORMAT "->>>>>>>>>9" NO-UNDO.
DEF VAR cLevKod    AS CHAR NO-UNDO.
DEF VAR cBestNr    AS CHAR NO-UNDO.
DEF VAR cStar AS CHAR FORMAT 'xx' NO-UNDO.


GURRE:
FOR EACH ArtBas, EACH Strekkode OF ArtBas:
    IF Strekkode.bestillingsnummer = ? THEN
        Strekkode.Bestillingsnummer = ArtBas.LevKod.
    IF Strekkode.Bestillingsnummer  = '' THEN
        Strekkode.Bestillingsnummer = ArtBas.LevKod.

    IF ArtBas.LevKod = '' THEN
        NEXT.
    IF LENGTH(ArtBas.LevKod) < 6 OR 
        LENGTH(ArtBas.LevKod) > 7 THEN
        NEXT.

    ASSIGN
        lDec = DEC(ArtBas.LevKod) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        NEXT.

    /* LevKod feltet skal bare ha 6 siffer. */
    ASSIGN 
        cLevKod = SUBSTRING(ArtBas.LevKod,1,6)
        cBestNr = Strekkode.Bestillingsnummer.

    IF LENGTH(cBestNr) = 6 THEN
    DO:
        lChkSiffer = DEC(cBestNr).
        RUN bibl_getchk(INPUT-OUTPUT lChkSiffer).
        cBestNr = cBestNr + STRING(lChkSiffer,"9").
        cStar = '*'.
    END.
    ELSE DO: 
        cStar = ''.
        lChkSiffer = DEC(SUBSTRING(cBestNr,1,6)).
        RUN bibl_getchk(INPUT-OUTPUT lChkSiffer).
    END.

    /*IF cStar <> '' THEN*/
    DO:
       DISPLAY
           ArtBas.ArtikkelNr
           ArtBas.Beskr
           ArtBas.LevKod
           (IF LENGTH(ArtBas.LevKod) > 6 THEN '*' ELSE '') FORMAT "x"
           Strekkode.Bestillingsnummer
           (IF LENGTH(Strekkode.Bestillingsnummer) > 6 THEN '*' ELSE '') FORMAT "x"
           cLevKod
           cBestNr
           lChkSiffer
           cStar
           WITH WIDTH 250.
        
    END.

    ASSIGN 
        ArtBas.LevKod               = cLevKod
        Strekkode.BestillingsNummer = cBestNr
        .

END.
