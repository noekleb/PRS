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

DEFINE VARIABLE dDatum AS DATE       NO-UNDO.

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

FOR EACH butiker WHERE butiker.eodrapport = TRUE NO-LOCK:
    DISP "Borttag av data før butik" butiker.butik NO-LABEL   WITH FRAME a.
    PAUSE 0.
/*     IF NOT butiker.butik = 86970 THEN */
/*         NEXT.                         */
    DO dDatum = DATE(6,1,2007) TO TODAY:
        RUN SlettData (butiker.butik,dDatum).
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-SlettData) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettData Procedure 
PROCEDURE SlettData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
     DEFINE INPUT  PARAMETER iButik AS INTEGER    NO-UNDO.
     DEFINE INPUT  PARAMETER dDato  AS DATE       NO-UNDO.

     FOR EACH bokforingsdag WHERE bokforingsdag.butikknr = iButik AND bokforingsdag.dato = dDato:
         FOR EACH skift WHERE skift.bokforingsid = bokforingsdag.bokforingsid:
             FOR EACH bonghode  WHERE Bonghode.skiftid = skift.skiftid:
                 FOR EACH Bonglinje WHERE bonglinje.b_id = bonghode.b_id:
                     DELETE bonglinje.
                 END.
                 DELETE bonghode.
             END.
             DELETE skift.
         END.
         DELETE bokforingsdag.
     END.
     FOR EACH prBMData WHERE prBMdata.butikknr = iButik AND prBMdata.dato = dDato:
         DELETE prBMdata.
     END.
     FOR EACH prPGData WHERE prPGdata.butikknr = iButik AND prPGdata.dato = dDato:
         DELETE prPGdata.
     END.
     FOR EACH prKDData WHERE prKDdata.butikknr = iButik AND prKDdata.dato = dDato:
         DELETE prKDdata.
     END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

