&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/


/* Temp-Table and Buffer definitions                                    */
DEFINE SHARED TEMP-TABLE TT_OvBuffer NO-UNDO LIKE OvBuffer.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure  
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :
                  Fraoverføringsordre           1
                  Fra bestilling - full innlev  2
                  Fra Bestilling                4
                  Fra kasse                     5
                  Fra suppleringsordre          6
                  Fra plukklager/plukkbutikk    7
                  Fra plukkliste/PDA            8
                  Fra varetelling               9

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE INPUT-OUTPUT  PARAMETER iBuntNr      AS INTEGER    NO-UNDO.
DEFINE INPUT  PARAMETER iArtikkelNr  LIKE ArtBas.ArtikkelNr  NO-UNDO.
DEFINE INPUT  PARAMETER cBuntMerknad AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER wEDB-System  AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER wTabell      AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER iOpphav      AS INTEGER    NO-UNDO.

DEFINE VARIABLE         iLinjeNr     AS INTEGER    NO-UNDO.
DEF    VAR              lOppdDirekte AS LOG        NO-UNDO.
DEF    VAR              bPlukklager  AS LOG        NO-UNDO.
DEFINE VARIABLE cReturnValue AS CHARACTER NO-UNDO.
DEFINE VARIABLE bFraKassen   AS LOG       NO-UNDO.

DEF VAR piBatchNr AS INT  NO-UNDO.
DEF VAR pcStatus  AS CHAR NO-UNDO.

/* Sikrer at Plukklageroverføringer blir fakturert. */
/*
IF iBuntNr = -2 THEN
    bPlukklager = TRUE.
*/

/* Sjekker parameter som styrer direkte oppdatering */
IF NUM-ENTRIES(cBuntMerknad,CHR(1)) > 1 THEN
DO:
    IF NUM-ENTRIES(cBuntMerknad,CHR(1)) >= 3 THEN 
       ASSIGN bFraKassen = ENTRY(3,cBuntMerknad,CHR(1)) = "J".
    ASSIGN lOppdDirekte = ENTRY(1,cBuntMerknad,CHR(1)) = "J"
           cBuntMerknad = ENTRY(2,cBuntMerknad,CHR(1)).
END.

{overforing.i
    &NEW    = "New"
    &SHARED = "Shared"
}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
   Temp-Tables and Buffers:
      TABLE: TT_OvBuffer T "SHARED" NO-UNDO skotex OvBuffer
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
RUN LagraTT_OvBuffer.

IF CAN-FIND(FIRST tmpOverfor) AND lOppdDirekte THEN
DO:
    FIND FIRST tmpOverfor NO-LOCK.

    DO TRANSACTION:
        RUN opprettbatchoverfor.p (OUTPUT piBatchNr, OUTPUT pcStatus).
        IF piBatchNr <> ? THEN 
          cReturnValue = STRING(piBatchNr). 
        /* Setter batchnummer på bunten. */
        IF piBatchNr <> 0 THEN
        DO:
            FIND OvBunt EXCLUSIVE-LOCK WHERE
                OvBunt.BuntNr = tmpOverfor.BuntNr.
            ASSIGN
              OvBunt.BatchNr       = piBatchNr
              .
            IF Ovbunt.DatoOppdatert = ? THEN
                ASSIGN
                OvBunt.OppdatertAv   = USERID("skotex")
                OvBunt.DatoOppdatert = TODAY
                OvBunt.TidOppdatert  = TIME
                Ovbunt.OppdatertAv   = USERID("skotex")
                OvBunt.BatchNr       = piBatchNr
                .
            FIND CURRENT OvBunt NO-LOCK.
        END.
    END.
    /* Poster faktura - hvis internsalget skal faktureres. Skal ikke kjøres når faktura kommer fra kassen. */
    IF NOT bFraKassen THEN RUN opprettfakturaoverfor.p (OUTPUT piBatchNr, OUTPUT pcStatus).
END.
ELSE IF bPlukklager THEN
DO:
    /* Poster faktura - hvis internsalget skal faktureres. Skal ikke kjøres når faktura kommer fra kassen.  */
    IF NOT bFraKassen THEN RUN opprettfakturaoverfor.p (OUTPUT piBatchNr, OUTPUT pcStatus).
END.

/* Sender tilbake BatchNr */
RETURN cReturnValue.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-LagraTT_OvBuffer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LagraTT_OvBuffer Procedure 
PROCEDURE LagraTT_OvBuffer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE         iLinjeNr AS INTEGER    NO-UNDO.
    DEFINE VARIABLE        pdDato    AS DATE       NO-UNDO.

    /* Tømmer temp-tabell */
    FOR EACH tmpOverfor:
        DELETE tmpOverfor.
    END.
    IF (iBuntNr = ? OR iBuntNr = 0) THEN 
    BUNT:
    REPEAT:
        /* Sikrer at vi finner en som ikke er i bruk */
        FIND LAST ovBunt NO-LOCK NO-WAIT NO-ERROR.
        ASSIGN
            iBuntNr = IF AVAILABLE OvBunt
                        THEN OvBunt.BuntNr + 1
                        ELSE 1.
        IF iBuntNr > 9999999 THEN
            RETURN "AVBRYT".

        CREATE ovBunt.
        ASSIGN ovBunt.BuntNr  = iBuntNr
               ovBunt.Merknad = cBuntMerknad
               ovBunt.Opphav  = iOpphav
               iBuntNr        = OvBunt.BuntNr.
        LEAVE BUNT.
    END. /* BUNT */

    ELSE IF iBuntNr = -1 THEN 
    KASSEBUNT:
    REPEAT:
        /* Finner siste dag i måneden.         */
        /* Samme dato på alle linjer i bongen. */
        FIND FIRST TT_OvBuffer.
        IF MONTH(TT_OvBuffer.RegistrertDato) = 12 THEN
            ASSIGN
                pdDato = DATE(12, 31, YEAR(TT_Ovbuffer.RegistrertDato))
                .
        ELSE
            ASSIGN
                pdDato = DATE(MONTH(TT_Ovbuffer.RegistrertDato) + 1, 1, YEAR(TT_Ovbuffer.RegistrertDato)) - 1
                .

        /* Sikrer at vi finner en som ikke er i bruk */
        FIND LAST ovBunt EXCLUSIVE-LOCK WHERE
            ovBunt.DatoOppdatert  = pdDato AND
            OvBunt.Opphav         = iOpphav
            NO-WAIT NO-ERROR.
        IF NOT AVAILABLE OvBunt THEN
        DO:
            FIND LAST ovBunt NO-LOCK NO-ERROR.
            IF AVAILABLE ovBunt THEN
                iBuntNr = ovBunt.BuntNr + 1.
            ELSE
                iBuntNr = 1.
            IF iBuntNr > 9999999 THEN
                RETURN "AVBRYT".

            CREATE ovBunt.
            ASSIGN ovBunt.BuntNr         = iBuntNr
                   ovBunt.Merknad        = cBuntMerknad
                   ovBunt.Opphav         = iOpphav
                   ovbunt.DatoOppdatert  = pdDato
                   ovBunt.TidOppdatert   = (24 * 60 * 60) - 30
                   ovBunt.OppdatertAv    = TT_OvBuffer.RegistrertAv
                   iBuntNr               = OvBunt.BuntNr
                   .
        END.
        ELSE DO:
            ASSIGN
                iBuntNr = OvBunt.BuntNr
                .
        END.
        LEAVE KASSEBUNT.
    END. /* KASSEBUNT */
    ELSE IF iBuntNr = -2 THEN 
    SALGBUNT:
    DO:
        /* Overføringsbunt for bongens dato */
        FIND FIRST TT_OvBuffer.
        ASSIGN
          pdDato = TT_Ovbuffer.RegistrertDato
          .
        FIND LAST ovBunt NO-LOCK USE-INDEX BuntNR NO-ERROR.
        IF AVAILABLE ovBunt THEN
            iBuntNr = ovBunt.BuntNr + 1.
        ELSE
            iBuntNr = 1.
        IF iBuntNr > 9999999 THEN
            RETURN "AVBRYT".

        CREATE ovBunt.
        ASSIGN ovBunt.BuntNr         = iBuntNr
               ovBunt.Merknad        = cBuntMerknad
               ovBunt.Opphav         = iOpphav
               OvBunt.RegistrertDato = pdDato
               ovbunt.DatoOppdatert  = IF iOpphav = 7
                                         THEN TT_Ovbuffer.RegistrertDato
                                         ELSE ?
               ovBunt.TidOppdatert   = IF iOpphav = 7
                                         THEN TT_Ovbuffer.RegistrertTid
                                         ELSE 0
               ovBunt.OppdatertAv    = IF iOpphav = 7
                                         THEN TT_Ovbuffer.RegistrertAv
                                         ELSE ""
               iBuntNr               = OvBunt.BuntNr
               .
    END. /* SALGBUNT */
    /* Henter og fyller på valgt OvBunt. */
    ELSE DO:
        FIND ovBunt WHERE ovBunt.BuntNr = iBuntnr EXCLUSIVE NO-WAIT NO-ERROR.
        IF LOCKED OvBunt THEN
            RETURN "AVBRYT".
    END.
    
    FIND LAST ovBuffer OF ovBunt USE-INDEX BuntLinjeNr NO-LOCK NO-ERROR.
    ASSIGN iLinjeNr = IF AVAIL ovBuffer THEN ovBuffer.LinjeNr ELSE 0.
    FOR EACH TT_OvBuffer WHERE
        TT_OvBuffer.Antall <> 0:
        CREATE ovBuffer.
        ASSIGN OvBuffer.BuntNr      = iBuntNr     
               OvBuffer.LinjeNr     = TT_OvBuffer.LinjeNr + iLinjeNr
               OvBuffer.ButikkNrFra = TT_OvBuffer.ButikkNrFra
               OvBuffer.ButikkNrTil = TT_OvBuffer.ButikkNrTil        
               OvBuffer.ArtikkelNr  = TT_OvBuffer.ArtikkelNr 
               OvBuffer.Vg          = TT_OvBuffer.Vg         
               OvBuffer.LopNr       = TT_OvBuffer.LopNr      
               OvBuffer.Antall      = TT_OvBuffer.Antall     
               OvBuffer.Merknad     = TT_OvBuffer.Merknad    
               OvBuffer.Storl       = TT_OvBuffer.Storl
               OvBuffer.TilStorl    = TT_OvBuffer.TilStorl
               OvBuffer.Varekost    = TT_OvBuffer.Varekost.
        /* Logger for fakturering */

        CREATE tmpOverfor.
        ASSIGN
            tmpOverfor.ArtikkelNr = TT_OvBuffer.ArtikkelNr
            tmpOverfor.Vg         = TT_OvBuffer.Vg
            tmpOverfor.LopNr      = TT_OvBuffer.LopNr
            tmpOverfor.FraBut     = TT_OvBuffer.ButikkNrFra
            tmpOverfor.TilBut     = TT_OvBuffer.ButikkNrTil
            tmpOverfor.FraStorl   = TT_OvBuffer.Storl
            tmpOverfor.TilStorl   = TT_OvBuffer.TilStorl
            tmpOverfor.Antall     = TT_OvBuffer.Antall
            tmpOverfor.BuntNr     = iBuntNr
            .
    END.
    /* Kommer vi fra kasse, er iArtikkelNr = 0, og det skal ikke logges. */
    IF iArtikkelNr <> 0 THEN
    DO:
        FIND FIRST KonvReg NO-LOCK WHERE
          KonvReg.EDB-System = wEDB-System AND
          KonvReg.Tabell     = wTabell   AND
          KonvReg.EkstId     = string(iArtikkelNr) NO-ERROR.
        IF NOT AVAILABLE KonvReg THEN
          DO:
            CREATE KonvReg.
            ASSIGN
              KonvReg.EDB-System = wEDB-System 
              KonvReg.Tabell     = wTabell   
              KonvReg.EkstId     = STRING(iArtikkelNr)
              KonvReg.InterntID  = STRING(iArtikkelNr).
          END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

