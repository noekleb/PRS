&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       : RUN asFinansrapporter.p (ButikkNr, Dato, Rapporttype).
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER ipiButikkNr AS INTEGER   FORMAT ">>>>>9" NO-UNDO.
DEFINE INPUT PARAMETER ipdDato     AS DATE      FORMAT "99/99/99" NO-UNDO.
DEFINE INPUT PARAMETER ipiRappType AS INTEGER   NO-UNDO.
DEFINE INPUT PARAMETER ipcSkriver  AS CHARACTER NO-UNDO.

DEFINE VARIABLE hRapport1 AS HANDLE NO-UNDO.
DEFINE VARIABLE hRapport2 AS HANDLE NO-UNDO.
DEFINE VARIABLE cFilNavn AS CHARACTER NO-UNDO.

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
SKRIV_FINRAPP:
DO:
  FIND Butiker NO-LOCK WHERE
    Butiker.Butik = ipiButikkNr NO-ERROR.
    
  IF NOT AVAILABLE Butiker THEN 
    LEAVE SKRIV_FINRAPP.

  CASE ipiRappType:
    WHEN 10 THEN 
      DO:
/*        RUN w-rkassarapportx.w PERSISTENT SET hRapport1.            */
/*        RUN AutoInit IN hRapport1 (ipiRappType,ipiButikkNr,ipdDato).*/
        RUN dagsrapp_utskrift.p ("1", ipiButikkNr, ipdDato, ipdDato, TRUE, OUTPUT cFilnavn).
      END.  
    WHEN 11 THEN 
      DO:
/*        RUN w-rkassarapportx.w PERSISTENT SET hRapport2.            */
/*        RUN AutoInit IN hRapport2 (ipiRappType,ipiButikkNr,ipdDato).*/
        RUN dagsrapp_utskrift.p ("2", ipiButikkNr, ipdDato, ipdDato, TRUE, OUTPUT cFilnavn).
      END.  
    WHEN 12 THEN 
      DO:
        RUN skrivbongrap.p (ipiButikkNr,ipdDato,TRUE,TRUE).
      END.  
  END.
END. /* SKRIV_FINRAPP */
     
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


