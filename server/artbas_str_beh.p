DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO. 
DEF INPUT  PARAM icParam      AS CHAR NO-UNDO.
DEF OUTPUT PARAM TABLE-HANDLE hTempTable.
DEF OUTPUT PARAM ocReturn     AS CHAR NO-UNDO.

DEF VAR ix               AS INT    NO-UNDO.
DEF VAR bOK              AS LOG    NO-UNDO.
DEF VAR hBuffMenu        AS HANDLE NO-UNDO.
DEF VAR httBuffer        AS HANDLE NO-UNDO.
DEF VAR hQuery           AS HANDLE NO-UNDO.
DEF VAR bArtLag          AS LOG    NO-UNDO.
DEF VAR fArtikkelNr      AS DEC    NO-UNDO.
DEF VAR iButikkNr        AS INT    NO-UNDO.
DEF VAR bEgenButikk      AS LOG    NO-UNDO.
DEFINE VARIABLE i2ButNr AS INTEGER NO-UNDO.

CREATE TEMP-TABLE hTempTable.
hTempTable:ADD-NEW-FIELD("Storl","CHARACTER").
hTempTable:ADD-NEW-FIELD("StrKode","INTEGER").
hTempTable:ADD-NEW-FIELD("ButikkNr","INTEGER").
hTempTable:ADD-NEW-FIELD("Lagant","DECIMAL").
hTempTable:ADD-NEW-FIELD("ArtikkelNr","DECIMAL").
hTempTable:ADD-NEW-FIELD("SeqNr","INTEGER").

hTempTable:TEMP-TABLE-PREPARE("ArtStr").
httBuffer = hTempTable:DEFAULT-BUFFER-HANDLE.

IF icParam NE "" THEN DO:
  ASSIGN fArtikkelNr = DEC(ENTRY(1,icParam))
         iButikkNr   = INT(ENTRY(2,icParam))
         bEgenButikk = INT(ENTRY(3,icParam)) = 1
         .
  IF CAN-FIND(ArtBas WHERE ArtBas.ArtikkelNr = fArtikkelNr) THEN 
    RUN brukArtBas.
  ELSE IF CAN-FIND(FIRST VPIArtBas WHERE VPIArtBas.ArtikkelNr = fArtikkelNr) THEN
    RUN brukVPIArtBas.        
END.




/* **********************  Internal Procedures  *********************** */

PROCEDURE brukArtBas:
/*------------------------------------------------------------------------------
		Purpose:  																	  
		Notes:  																	  
------------------------------------------------------------------------------*/

  FOR FIRST ArtBas NO-LOCK
      WHERE ArtBas.ArtikkelNr = fArtikkelNr
      ,FIRST StrType OF ArtBas NO-LOCK:
    
    DO ix = 1 TO NUM-ENTRIES(StrType.Fordeling):
      IF CAN-FIND(FIRST Strekkode WHERE Strekkode.ArtikkelNr = ArtBas.ArtikkelNr
                                    AND Strekkode.kode > ""
                                    AND Strekkode.StrKode = INT(ENTRY(ix,StrType.Fordeling))) THEN DO:

        bArtLag = FALSE.
        FOR EACH ArtLag NO-LOCK
           WHERE ArtLag.ArtikkelNr = ArtBas.ArtikkelNr
             AND ArtLag.StrKode    = INT(ENTRY(ix,StrType.Fordeling))
             AND (IF bEgenButikk THEN ArtLag.butik = iButikkNr ELSE TRUE)
             :

          httBuffer:BUFFER-CREATE().
          ASSIGN httBuffer:BUFFER-FIELD("Storl"):BUFFER-VALUE      = TRIM(ENTRY(ix,StrType.AlfaFordeling))
                 httBuffer:BUFFER-FIELD("StrKode"):BUFFER-VALUE    = INT(ENTRY(ix,StrType.Fordeling))
                 httBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE = ArtBas.ArtikkelNr
                 httBuffer:BUFFER-FIELD("SeqNr"):BUFFER-VALUE      = ix
                 httBuffer:BUFFER-FIELD("ButikkNr"):BUFFER-VALUE   = ArtLag.butik
                 httBuffer:BUFFER-FIELD("Lagant"):BUFFER-VALUE     = ArtLag.lagant
                 bArtLag = TRUE
                 .
        END.
        IF NOT bArtLag THEN DO:
          httBuffer:BUFFER-CREATE().
          ASSIGN httBuffer:BUFFER-FIELD("Storl"):BUFFER-VALUE      = TRIM(ENTRY(ix,StrType.AlfaFordeling))
                 httBuffer:BUFFER-FIELD("StrKode"):BUFFER-VALUE    = INT(ENTRY(ix,StrType.Fordeling))
                 httBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE = ArtBas.ArtikkelNr
                 httBuffer:BUFFER-FIELD("SeqNr"):BUFFER-VALUE      = ix
                 .
        END.
      END.
      ELSE DO:
        httBuffer:BUFFER-CREATE().
        ASSIGN httBuffer:BUFFER-FIELD("Storl"):BUFFER-VALUE      = TRIM(ENTRY(ix,StrType.AlfaFordeling))
               httBuffer:BUFFER-FIELD("StrKode"):BUFFER-VALUE    = INT(ENTRY(ix,StrType.Fordeling))
               httBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE = ArtBas.ArtikkelNr
               httBuffer:BUFFER-FIELD("SeqNr"):BUFFER-VALUE      = ix
               .
      END.
    END.
  END.
  
  FOR FIRST ArtBas NO-LOCK
      WHERE ArtBas.ArtikkelNr = fArtikkelNr
     ,EACH STrekkode OF ArtBas NO-LOCK:

    bOk = NOT httBuffer:FIND-FIRST("WHERE StrKode = " + STRING(Strekkode.StrKode),NO-LOCK) NO-ERROR.
    IF bOk = ? THEN bOk = FALSE.
    IF bOk THEN
    BLOKKEN: 
    DO:
      FIND StrKonv NO-LOCK WHERE
        StrKonv.StrKode = Strekkode.StrKode NO-ERROR. 
      IF NOT AVAILABLE StrKonv THEN LEAVE BLOKKEN.
      
      httBuffer:BUFFER-CREATE().
      ASSIGN httBuffer:BUFFER-FIELD("Storl"):BUFFER-VALUE      = StrKonv.Storl
             httBuffer:BUFFER-FIELD("StrKode"):BUFFER-VALUE    = Strekkode.StrKode
             httBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE = ArtBas.ArtikkelNr
             httBuffer:BUFFER-FIELD("SeqNr"):BUFFER-VALUE      = ix + 1.
             .
    END. /* BLOKKEN */
  END.

END PROCEDURE.

PROCEDURE brukVPIArtBas:
/*------------------------------------------------------------------------------
		Purpose:  																	  
		Notes:  																	  
------------------------------------------------------------------------------*/

  FIND FIRST VPIArtBas NO-LOCK
      WHERE VPIArtBas.ArtikkelNr = fArtikkelNr NO-ERROR.
  IF NOT AVAILABLE VPIArtBAs THEN 
    RETURN.

  FIND STrType NO-LOCK WHERE
    StrType.StrTypeId = VPIArtBas.STrTypeId NO-ERROR.
  IF NOT AVAILABLE StrType THEN 
    RETURN.

  ARTBAS_BLOKK:
  DO:
    
    DO ix = 1 TO NUM-ENTRIES(StrType.Fordeling):
      IF CAN-FIND(FIRST VPIStrekkode WHERE VPIStrekkode.EkstVPILevNr = VPIArtBas.EkstVPILevNr 
                                    AND VPIStrekkode.VareNr = VPIArtBas.VareNr
                                    AND VPIStrekkode.kode > ""
                                    AND VPIStrekkode.StrKode = INT(ENTRY(ix,StrType.Fordeling))) THEN DO:
        httBuffer:BUFFER-CREATE().
        ASSIGN httBuffer:BUFFER-FIELD("Storl"):BUFFER-VALUE      = TRIM(ENTRY(ix,StrType.AlfaFordeling))
               httBuffer:BUFFER-FIELD("StrKode"):BUFFER-VALUE    = INT(ENTRY(ix,StrType.Fordeling))
               httBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE = VPIArtBas.ArtikkelNr
               httBuffer:BUFFER-FIELD("SeqNr"):BUFFER-VALUE      = ix
               .
      END.
    END.
    
    FOR EACH VPISTrekkode OF VPIArtBas NO-LOCK:
        bOk = NOT httBuffer:FIND-FIRST("WHERE StrKode = " + STRING(VPIStrekkode.StrKode),NO-LOCK) NO-ERROR.
        IF bOk = ? THEN bOk = FALSE.
        IF bOk THEN
        BLOKKEN: 
        DO:
          FIND StrKonv NO-LOCK WHERE
            StrKonv.StrKode = VPIStrekkode.StrKode NO-ERROR. 
          IF NOT AVAILABLE StrKonv THEN LEAVE BLOKKEN.
          
          httBuffer:BUFFER-CREATE().
          ASSIGN httBuffer:BUFFER-FIELD("Storl"):BUFFER-VALUE      = StrKonv.Storl
                 httBuffer:BUFFER-FIELD("StrKode"):BUFFER-VALUE    = VPIStrekkode.StrKode
                 httBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE = VPIArtBas.ArtikkelNr
                 httBuffer:BUFFER-FIELD("SeqNr"):BUFFER-VALUE      = ix + 1.
                 .
        END. /* BLOKKEN */
    END.
    
  END. /* ARTBAS_BLOKK */
END PROCEDURE.
