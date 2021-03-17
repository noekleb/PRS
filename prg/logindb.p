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
def var wAppDBpf       as char no-undo.
def var wRappDBpf      as char no-undo.
def var wDataDBpf      as char no-undo.
DEF VAR wVpiDBpf       AS CHAR NO-UNDO.
DEF VAR wOk            AS CHAR INITIAL 'OK' NO-UNDO.

DEF INPUT PARAMETER wProsjektListe AS CHAR NO-UNDO.

/* Parameters Definitions ---                                           */
&Scoped-define APP-DB  SkoTex
&Scoped-define RAPP-DB WR
&Scoped-define DATA-DB Data
&Scoped-define VPI-DB  VPI

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
    wAppDBpf  = entry(1,wProsjektListe,";")
    wRappDBpf = entry(2,wProsjektListe,";")
    wDataDBpf = entry(3,wProsjektListe,";")
    wVpiDBpf  = entry(4,wProsjektListe,";")
    .  

RUN Oppkobling.

RETURN wOk.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-Oppkobling) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Oppkobling Procedure 
PROCEDURE Oppkobling :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* Kobler opp applikasjonsdatabasen databasen */
IF NOT CONNECTED("{&APP-DB}") then
  do:
    connect -pf value(wAppDBpf) no-error.
    /* Sjekker at databasen er koblet opp. */
    IF NOT CONNECTED("{&APP-DB}") then
      DO:
        MESSAGE "** Klarte ikke å koble opp (Applik) - {&APP-DB} databasen! **" skip
                "Applikasjonen avsluttes." VIEW-AS ALERT-BOX.
        QUIT.
      END.
end.

/* Kobler opp rapportdatabasen */
IF NOT CONNECTED("{&RAPP-DB}") then
  do:
    connect -pf value(wRappDBpf) no-error.
    /* Sjekker at databasen er koblet opp. */
    IF NOT CONNECTED("{&RAPP-DB}") then
      DO:
        MESSAGE "** Klarte ikke å koble opp (Rapport) - {&RAPP-DB} databasen! **" skip
                "Applikasjonen avsluttes." VIEW-AS ALERT-BOX.
        QUIT.
      END.
end.

/* Kobler opp Data databasen */
IF wDataDBpf <> "" THEN
DO:
  IF NOT CONNECTED("{&DATA-DB}") then
  do:
    connect -pf value(wDataDBpf) no-error.
    /* Sjekker at databasen er koblet opp. */
    IF NOT CONNECTED("{&DATA-DB}") then
      DO:
        MESSAGE "** Klarte ikke å koble opp (Data) - {&DATA-DB} databasen! **" skip
                "Applikasjonen avsluttes." VIEW-AS ALERT-BOX.
        QUIT.
      END.
  END.
end.

/* Kobler opp VPI databasen */
IF wVPIDBpf <> "" THEN
DO:
  IF NOT CONNECTED("{&VPI-DB}") then
  do:
    connect -pf value(wVPIDBpf) no-error.

    /* Sjekker at databasen er koblet opp. */
    IF NOT CONNECTED("{&VPI-DB}") then
      DO:
        MESSAGE "** Klarte ikke å koble opp (VPI) - {&VPI-DB} databasen! **" skip
                "Pf fil: " wVPIDBpf
                "Applikasjonen avsluttes." VIEW-AS ALERT-BOX.
        QUIT.
      END.
  END.
end.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

