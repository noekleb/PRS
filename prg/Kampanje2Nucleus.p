&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE INPUT  PARAMETER cOutputDir   AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER iAntKampanjeMixMatch AS INTEGER    NO-UNDO.

DEFINE TEMP-TABLE TT_Elogg NO-UNDO LIKE ELogg
    FIELD lNy AS LOGICAL.

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

RUN KopierElogg.

RUN ExporteraKampanj.
/*                                                                                                                          */
RUN SlettElogg. /* Vi uppdaterar æven KampanjeMixMatch 'KampanjeMixMatch.KampSendtDato KampanjeMixMatch.KampSendtTid' */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ExporteraKampanj) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExporteraKampanj Procedure 
PROCEDURE ExporteraKampanj :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE cVareFiler AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cMixFiler  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iAntVarer  AS INTEGER    NO-UNDO.
DEFINE VARIABLE iAntPakker AS INTEGER    NO-UNDO.


    FOR EACH TT_Elogg:

    /*  Parameter till ArtBas2Nucleus:
        1: ?,         Används när vi kommer från Elogg->"ArtBAs"
        2: "",        Inte aktuell vid kampanj
        3:            sendes-dir
        4: nnn,       KAMPANJEID
        5: TRUE/FALSE Vid första anropet skall vi sända artiklar som ingår i kampanjen (TRUE)
        6: POS        Sende med POS som EDB system
        6: OUTPUT cVareFiler     6,7,8,9 Har ingen betydelse här
        7: OUTPUT cMixFiler
        8: OUTPUT FI-AntVarer
        9: OUTPUT FI-AntPakker        
        
    */
        RUN ArtBas2Nucleus.p (?,"",INPUT cOutputDir,DECI(TT_ELogg.Verdier),TRUE,"POS",OUTPUT cVareFiler,OUTPUT cMixFiler,OUTPUT iAntVarer,OUTPUT iAntPakker).
        RUN ArtBas2Nucleus.p (?,"",INPUT cOutputDir,DECI(TT_ELogg.Verdier),FALSE,"POS",OUTPUT cVareFiler,OUTPUT cMixFiler,OUTPUT iAntVarer,OUTPUT iAntPakker).
        iAntKampanjeMixMatch = iAntKampanjeMixMatch + 1.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-KopierElogg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KopierElogg Procedure 
PROCEDURE KopierElogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH Elogg WHERE Elogg.Tabellnavn = "KampanjeMixMatch" AND ELogg.EksterntSystem = "POS" AND
                         Elogg.behandlet = FALSE.
        FIND KampanjeMixMatch WHERE KampanjeMixMatch.KampId = DECI(Elogg.Verdier) NO-ERROR.
        IF AVAIL KampanjeMixMatch THEN DO:
            CREATE TT_Elogg.
            BUFFER-COPY Elogg TO TT_Elogg.
            TT_Elogg.lNy = KampanjeMixMatch.KampSendtDato = ?.
            iAntKampanjeMixMatch = iAntKampanjeMixMatch + 1.
        END.
        Elogg.Behandlet = TRUE.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SlettElogg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettElogg Procedure 
PROCEDURE SlettElogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH Elogg WHERE Elogg.Tabellnavn = "KampanjeMixMatch" AND ELogg.EksterntSystem = "POS" AND
                         Elogg.behandlet = TRUE.
        DELETE Elogg.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

