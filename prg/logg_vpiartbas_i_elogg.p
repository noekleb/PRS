DEF INPUT PARAMETER iVPIArtBasRecid AS RECID NO-UNDO.
DEF INPUT PARAMETER cEDBSystem      AS CHAR  NO-UNDO.

FIND VPIArtBas NO-LOCK WHERE
    RECID(VPIArtBas) = iVPIArtBasRecid NO-ERROR.
IF NOT AVAILABLE VPIArtBas THEN
    RETURN.

IF VPIArtBas.KorrArtikkelNr > 0 THEN 
LOGG_I_ELOGG:
DO:
  FIND ELogg WHERE 
       ELogg.TabellNavn     = "VPIArtBas"   AND
       ELogg.EksterntSystem = cEDBSystem    AND
       ELogg.Verdier        = STRING(VPIArtBas.EkstVPILevNr) + "|" + 
                              STRING(VPIArtBas.VareNr) + "|" + 
                              STRING(VPIArtBas.KorrArtikkelNr)
                              NO-ERROR.
  IF NOT AVAIL Elogg THEN DO:
      CREATE Elogg.
      ASSIGN ELogg.TabellNavn     = "VPIArtBas"
             ELogg.EksterntSystem = cEDBSystem   
             ELogg.Verdier        = STRING(VPIArtBas.EkstVPILevNr) + "|" + 
                                    STRING(VPIArtBas.VareNr) + "|" + 
                                    STRING(VPIArtBas.KorrArtikkelNr).
  END.
  ASSIGN ELogg.EndringsType = 1
         ELogg.Behandlet    = FALSE.
  RELEASE ELogg.

END. /* LOGG_I_ELOGG */
