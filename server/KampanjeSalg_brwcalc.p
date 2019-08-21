/* KampanjeSalg_brwcalc.p
*/

DEFINE VARIABLE cButLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTTIdSalgLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE iLoop1 AS INTEGER NO-UNDO.
DEFINE VARIABLE iLoop2 AS INTEGER NO-UNDO.
DEFINE VARIABLE iButNr AS INTEGER NO-UNDO.
DEFINE VARIABLE dDato AS DATE NO-UNDO.
DEFINE VARIABLE iTTId AS INTEGER NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.

ASSIGN 
    cLogg = 'KampanjeSalg_brwcalc'
    .

PROCEDURE artpris_Varegruppe:
  DEFINE INPUT  PARAMETER rRowId AS ROWID  NO-UNDO.
  DEFINE INPUT  PARAMETER icParam      AS CHARACTER  NO-UNDO.  
  DEFINE INPUT  PARAMETER icSessionId  AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER ocValue      AS CHARACTER NO-UNDO. 
    
END PROCEDURE.

PROCEDURE artpris_Hovedgruppe:
  DEFINE INPUT  PARAMETER rRowId AS ROWID  NO-UNDO.
  DEFINE INPUT  PARAMETER icParam      AS CHARACTER  NO-UNDO.  
  DEFINE INPUT  PARAMETER icSessionId  AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER ocValue      AS CHARACTER NO-UNDO. 
    
END PROCEDURE.

PROCEDURE artpris_Sesong:
  DEFINE INPUT  PARAMETER rRowId AS ROWID  NO-UNDO.
  DEFINE INPUT  PARAMETER icParam      AS CHARACTER  NO-UNDO.  
  DEFINE INPUT  PARAMETER icSessionId  AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER ocValue      AS CHARACTER NO-UNDO. 
    
END PROCEDURE.

PROCEDURE artpris_Produsent:
  DEFINE INPUT  PARAMETER rRowId AS ROWID  NO-UNDO.
  DEFINE INPUT  PARAMETER icParam      AS CHARACTER  NO-UNDO.  
  DEFINE INPUT  PARAMETER icSessionId  AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER ocValue      AS CHARACTER NO-UNDO. 
    
END PROCEDURE.

PROCEDURE artpris_Varemerke:
  DEFINE INPUT  PARAMETER rRowId AS ROWID  NO-UNDO.
  DEFINE INPUT  PARAMETER icParam      AS CHARACTER  NO-UNDO.  
  DEFINE INPUT  PARAMETER icSessionId  AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER ocValue      AS CHARACTER NO-UNDO. 
    
END PROCEDURE.

PROCEDURE artpris_Dummy:
  DEFINE INPUT  PARAMETER rRowId AS ROWID  NO-UNDO.
  DEFINE INPUT  PARAMETER icParam      AS CHARACTER  NO-UNDO.  
  DEFINE INPUT  PARAMETER icSessionId  AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER ocValue      AS CHARACTER NO-UNDO.
  
  DEFINE VARIABLE lLagVerdi AS DECIMAL FORMAT "->>,>>>,>>>9.99" NO-UNDO.
  
  FIND FIRST ArtPris NO-LOCK WHERE 
    ROWID(ArtPris) = rROWID NO-ERROR.
    
    
END PROCEDURE.

PROCEDURE artpris_LagVerdi:
  DEFINE INPUT  PARAMETER rRowId AS ROWID  NO-UNDO.
  DEFINE INPUT  PARAMETER icParam      AS CHARACTER  NO-UNDO.  
  DEFINE INPUT  PARAMETER icSessionId  AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER ocValue      AS CHARACTER NO-UNDO.
  
  DEFINE VARIABLE lLagVerdi AS DECIMAL FORMAT "->>,>>>,>>>9.99" NO-UNDO.
  
  FIND FIRST ArtPris NO-LOCK WHERE 
    ROWID(ArtPris) = rROWID NO-ERROR.
    
    
END PROCEDURE.

PROCEDURE artpris_LagAnt:
  DEFINE INPUT  PARAMETER rRowId AS ROWID  NO-UNDO.
  DEFINE INPUT  PARAMETER icParam      AS CHARACTER  NO-UNDO.  
  DEFINE INPUT  PARAMETER icSessionId  AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER ocValue      AS CHARACTER NO-UNDO.
  
  DEFINE VARIABLE lLagAnt AS DECIMAL FORMAT "->>>,>>>9" NO-UNDO.
  
  FIND FIRST ArtPris NO-LOCK WHERE 
    ROWID(ArtPris) = rROWID NO-ERROR.
    
    
END PROCEDURE.

PROCEDURE artpris_Solgt%:
  DEFINE INPUT  PARAMETER rRowId AS ROWID  NO-UNDO.
  DEFINE INPUT  PARAMETER icParam      AS CHARACTER  NO-UNDO.  
  DEFINE INPUT  PARAMETER icSessionId  AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER ocValue      AS CHARACTER NO-UNDO.
  
  DEFINE VARIABLE lSolgt% AS DECIMAL FORMAT "->>>9.99" NO-UNDO.
  
  FIND FIRST ArtPris NO-LOCK WHERE 
    ROWID(ArtPris) = rROWID NO-ERROR.
    
    
END PROCEDURE.

PROCEDURE artpris_AntSolgt:
  DEFINE INPUT  PARAMETER rRowId AS ROWID  NO-UNDO.
  DEFINE INPUT  PARAMETER icParam      AS CHARACTER  NO-UNDO.  
  DEFINE INPUT  PARAMETER icSessionId  AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER ocValue      AS CHARACTER NO-UNDO.
  
  DEFINE VARIABLE lAntSolgt AS DECIMAL FORMAT "->>,>>>,>>9" NO-UNDO.
  
  FIND FIRST ArtPris NO-LOCK WHERE 
    ROWID(ArtPris) = rROWID NO-ERROR.
/*  IF AVAILABLE ArtPris AND ArtPris.Tilbud = TRUE THEN*/
/*  DO:                                                */
/*    ASSIGN                                           */
/*      cButLst        = '15,16'                       */
/*      cTTIdSalgLst   = '1,3,10'                      */
/*      .                                              */
/*                                                     */
/*    DO iLoop1 = 1 TO NUM-ENTRIES(cButLst):                               */
/*        iButNr = INT(ENTRY(iLoop1,cButLst)).                             */
/*        IF ArtPris.TilbudFraDato <> ? AND ArtPris.TilbudTilDato <> ? THEN*/
/*        DO dDato = ArtPris.TilbudFraDato TO ArtPris.TilbudTilDato:       */
/*            DO iLoop2 = 1 TO NUM-ENTRIES(cTTIdSalgLst):                  */
/*                iTTId = INT(ENTRY(iLoop2,cTTIdSalgLst)).                 */
/*                FOR EACH TransLogg NO-LOCK WHERE                         */
/*                  TransLogg.ArtikkelNr = ArtPris.ArtikkelNr AND          */
/*                  TransLogg.Dato  = dDato AND                            */
/*                  TransLogg.Tid >= 0 AND                                 */
/*                  TransLogg.Butik = iButNr AND                           */
/*                  TransLogg.TTId  = iTTId:                               */
/*                  ASSIGN                                                 */
/*                    lantSolgt = lantSolgt + TransLogg.Antall             */
/*                    .                                                    */
/*                END. /* TRANSLOGG */                                     */
/*            END.                                                         */
/*        END.                                                             */
/*    END.                                                                 */
/*    ocValue = STRING(lAntSolgt).                                         */
/*  END.                                                                   */
/*  ELSE                                                                   */
/*    ocValue = "".                                                        */
END.

PROCEDURE artpris_VerdiSolgt:
  DEFINE INPUT  PARAMETER rRowId AS ROWID  NO-UNDO.
  DEFINE INPUT  PARAMETER icParam      AS CHARACTER  NO-UNDO.  
  DEFINE INPUT  PARAMETER icSessionId  AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER ocValue      AS CHARACTER NO-UNDO.
  
  DEFINE VARIABLE lVerdiSolgt AS DECIMAL FORMAT "->>,>>>,>>9.99" NO-UNDO.
  
  FIND FIRST ArtPris NO-LOCK WHERE 
    ROWID(ArtPris) = rROWID NO-ERROR.
/*  IF AVAILABLE ArtPris AND ArtPris.Tilbud = TRUE THEN*/
/*  DO:                                                */
/*    ASSIGN                                           */
/*      cButLst        = '15,16'                       */
/*      cTTIdSalgLst   = '1,3,10'                      */
/*      .                                              */
/*                                                     */
/*    DO iLoop1 = 1 TO NUM-ENTRIES(cButLst):                                         */
/*        iButNr = INT(ENTRY(iLoop1,cButLst)).                                       */
/*        DO dDato = ArtPris.TilbudFraDato TO ArtPris.TilbudTilDato:                 */
/*            DO iLoop2 = 1 TO NUM-ENTRIES(cTTIdSalgLst):                            */
/*                iTTId = INT(ENTRY(iLoop2,cTTIdSalgLst)).                           */
/*                FOR EACH TransLogg NO-LOCK WHERE                                   */
/*                  TransLogg.ArtikkelNr = ArtPris.ArtikkelNr AND                    */
/*                  TransLogg.Dato  = dDato AND                                      */
/*                  TransLogg.Tid >= 0 AND                                           */
/*                  TransLogg.Butik = iButNr AND                                     */
/*                  TransLogg.TTId  = iTTId:                                         */
/*                  ASSIGN                                                           */
/*                    lVerdiSolgt = lVerdiSolgt + (TransLogg.Pris * Translogg.antall)*/
/*                    .                                                              */
/*                END. /* TRANSLOGG */                                               */
/*            END.                                                                   */
/*        END.                                                                       */
/*    END.                                                                           */
/*    ocValue = STRING(lVerdiSolgt).*/
/*  END.                            */
/*  ELSE                            */
/*    ocValue = "".                 */
END.

PROCEDURE artpris_TilbudFraDato:
  DEFINE INPUT  PARAMETER rRowId       AS ROWID  NO-UNDO.
  DEFINE INPUT  PARAMETER icParam      AS CHARACTER  NO-UNDO.  
  DEFINE INPUT  PARAMETER icSessionId  AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER ocValue      AS CHARACTER NO-UNDO.
  
  FIND FIRST ArtPris NO-LOCK WHERE 
    ROWID(ArtPris) = rROWID NO-ERROR.
  IF AVAILABLE ArtPris THEN 
    ASSIGN 
    ocValue = STRING(
                    DATETIME(ArtPris.TilbudFraDato, ArtPris.TilbudFraTid * 1000),
                    "99/99/9999 HH:MM:SS") .
  ELSE 
    ocValue = "".   
    
/*  RUN bibl_loggDbFri.p (cLogg,STRING(AVAILABLE ArtPris)).*/
END.

PROCEDURE artpris_TilbudTilDato:
  DEFINE INPUT  PARAMETER rRowId       AS ROWID  NO-UNDO.
  DEFINE INPUT  PARAMETER icParam      AS CHARACTER  NO-UNDO.  
  DEFINE INPUT  PARAMETER icSessionId  AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER ocValue      AS CHARACTER NO-UNDO.
  
  FIND FIRST ArtPris NO-LOCK WHERE 
    ROWID(ArtPris) = rROWID NO-ERROR.
  IF AVAILABLE ArtPris THEN 
    ocValue = STRING(
                    DATETIME(ArtPris.TilbudTilDato, ArtPris.TilbudTilTid * 1000),
                    "99/99/9999 HH:MM:SS") .
  ELSE 
    ocValue = "". 
END.

PROCEDURE artpris_Beskr:
  DEFINE INPUT  PARAMETER ifArtikkelNr AS DECIMAL  NO-UNDO.
  DEFINE INPUT  PARAMETER icSessionId  AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER ocValue      AS CHARACTER NO-UNDO.
  
  FIND ArtBas NO-LOCK WHERE 
    ArtBas.ArtikkelNr = ifArtikkelNr NO-ERROR.
  IF AVAILABLE ArtBas THEN 
    ocValue =  ArtBas.Beskr.
  ELSE 
    ocValue = "".  
    
  MESSAGE 'TEST2 - ' + ocValue.  
    
END.

PROCEDURE artpris_LevKod:
  DEFINE INPUT  PARAMETER ifArtikkelNr AS DECIMAL  NO-UNDO.
  DEFINE INPUT  PARAMETER icSessionId  AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER ocValue      AS CHARACTER NO-UNDO.
  
  FIND ArtBas NO-LOCK WHERE 
    ArtBas.ArtikkelNr = ifArtikkelNr NO-ERROR.
  IF AVAILABLE ArtBas THEN 
    ocValue = ArtBas.LevKod.
  ELSE 
    ocValue = "".  
END.

PROCEDURE artpris_LevFargKod:
  DEFINE INPUT  PARAMETER ifArtikkelNr AS DECIMAL  NO-UNDO.
  DEFINE INPUT  PARAMETER icSessionId  AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER ocValue      AS CHARACTER NO-UNDO.
  
  FIND ArtBas NO-LOCK WHERE 
    ArtBas.ArtikkelNr = ifArtikkelNr NO-ERROR.
  IF AVAILABLE ArtBas THEN 
    ocValue = ArtBas.LevFargKod.
  ELSE 
    ocValue = "".  
END.




























