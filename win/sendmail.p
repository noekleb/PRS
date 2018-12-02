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
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
  DEFINE VARIABLE cSubject      AS CHARACTER INIT "Test" NO-UNDO.
  DEFINE VARIABLE cTo           AS CHARACTER INIT "ken1@polygonsoftware.no" NO-UNDO.
  DEFINE VARIABLE cMessage      AS CHARACTER INIT "Testmeddelande" NO-UNDO.
  DEFINE VARIABLE cVedleggListe AS CHARACTER INIT "C:\appdir\skotex\skotex.ini" NO-UNDO.
&ELSE
  DEFINE INPUT  PARAMETER cSubject      AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER cTo           AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER cMessage      AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER cVedleggListe AS CHARACTER  NO-UNDO.
&ENDIF

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

RUN SkickaMail.
RETURN RETURN-VALUE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-SkickaMail) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkickaMail Procedure 
PROCEDURE SkickaMail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR chOutlook   AS COM-HANDLE  NO-UNDO.
  DEF VAR chNameSpace AS COM-HANDLE  NO-UNDO.
  DEF VAR chFolder    AS COM-HANDLE  NO-UNDO.
  DEF VAR chMailItem  AS COM-HANDLE  NO-UNDO.
  DEF VAR chAttachment AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE iCount AS INTEGER  NO-UNDO.

  CREATE "Outlook.application" chOutlook NO-ERROR.
  IF NOT VALID-HANDLE(chOutlook) THEN
     RETURN "Kunde inte koppla till mailsystem.".
  IF NUM-ENTRIES(cTo,"@") <> 2 THEN
     RETURN "Felaktig mailadress".
  ASSIGN chNameSpace = chOutlook:GetNameSpace("MAPI")  /* get namespace */
         chFolder    = chNameSpace:GetDefaultFolder(6). /* get email folder */
         chMailItem         = chFolder:Items:Add().
  ASSIGN chMailItem:To      = cTo
         chMailItem:Subject = cSubject
         chMailItem:Body    = cMessage.         
  IF cVedleggListe <> "" THEN DO iCount = 1 TO NUM-ENTRIES(cVedleggListe):
      chAttachment = chMailItem:Attachments:Add(ENTRY(iCount,cVedleggListe)).
  END.
  chMailItem:Send().
  RELEASE OBJECT chAttachment NO-ERROR.
  RELEASE OBJECT chMailItem   NO-ERROR.
  RELEASE OBJECT chFolder     NO-ERROR.
  RELEASE OBJECT chNameSpace  NO-ERROR.
  RELEASE OBJECT chOutlook    NO-ERROR.
  ASSIGN chAttachment = ?
         chMailItem   = ?
         chFolder     = ?
         chNameSpace  = ?
         chOutlook    = ?.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

