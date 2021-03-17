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

DEF VAR wMesseNr LIKE messe.MesseNr NO-UNDO.

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

ASSIGN
    wMesseNr = 100008
    .
FIND messe NO-LOCK WHERE
    Messe.MesseNr = wMesseNr NO-ERROR.
IF NOT AVAILABLE Messe THEN
DO :
    MESSAGE "Ukjent messe " wMesseNr
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
END.

RUN SettMesse.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-SettMesse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettMesse Procedure 
PROCEDURE SettMesse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
CURRENT-WINDOW:WIDTH = 200.

FOR EACH VareBokHode OF Messe NO-LOCK:
  PAUSE 0 BEFORE-HIDE.
  DISPLAY
      VarebokHode.MesseNr
      VareBokHode.VareBokNr
      WITH WIDTH 200 TITLE "Varebok".

  FOR EACH VareBokLinje OF VareBokHode EXCLUSIVE-LOCK:
      FIND ArtBas NO-LOCK OF VareBokLinje.
      PAUSE 0.
      ASSIGN
          VareBokLinje.KjedeVare = ArtBas.KjedeVare
          VareBokLinje.Gjennomfaktureres = ArtBas.Gjennomfaktureres
          .
      /*
      DISPLAY
          VareBokLinje.ArtikkelNr
          VareBokLinje.Beskr
          VareBokLinje.KjedeVare
          VareBokLinje.Gjennomfaktureres
          ArtBas.KjedeVare
          ArtBas.Gjennomfaktureres
          WITH WIDTH 200 TITLE "VareBokLinje".
      */
  END.
END.

FOR EACH VareBehHode OF Messe NO-LOCK:
    PAUSE 0 BEFORE-HIDE.
    DISPLAY
        VarebehHode.MesseNr
        VareBehHode.VareBehNr
        WITH WIDTH 200 TITLE "Varebehandlingsbok".

    FOR EACH VareBehLinje OF VareBehHode EXCLUSIVE-LOCK:
        FIND ArtBas NO-LOCK OF VareBehLinje.
        PAUSE 0.
        ASSIGN
            VareBehLinje.KjedeVare = ArtBas.KjedeVare
            VareBehLinje.Gjennomfaktureres = ArtBas.Gjennomfaktureres
            .
        /*
        DISPLAY
            VareBehLinje.ArtikkelNr
            VareBehLinje.Beskr
            VareBehLinje.KjedeVare
            VareBehLinje.Gjennomfaktureres
            ArtBas.KjedeVare
            ArtBas.Gjennomfaktureres
            WITH WIDTH 200 TITLE "Vaerbehandlignsboklinje".
        */

    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

