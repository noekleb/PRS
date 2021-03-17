/* kampanje_antallpatilbud.p
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEFINE VARIABLE iAnt AS INTEGER NO-UNDO.
DEFINE VARIABLE iAntMLager AS INTEGER NO-UNDO.

FOR EACH ArtPris NO-LOCK WHERE 
  ArtPris.ProfilNr = 16 AND 
  ArtPris.tilbud = TRUE:
  iAnt = iant + 1.  
END.

FOR EACH ArtPris NO-LOCK WHERE 
  ArtPris.ProfilNr = 16 AND 
  ArtPris.tilbud = FALSE:
  IF CAN-FIND(FIRST Lager NO-LOCK WHERE 
              Lager.ArtikkelNr = ArtPris.ArtikkelNr AND 
              Lager.butik      = 16 AND 
              Lager.Lagant > 0) THEN 
    iAntMLager = iAntMLager + 1.  
END.

obOK     = YES.
ocReturn = STRING(iAnt) + '|' + STRING(iAntMLager).

RETURN ocReturn.


