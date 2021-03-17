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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-FixStorl) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FixStorl Procedure 
PROCEDURE FixStorl :
/*------------------------------------------------------------------------------
  Purpose:     Konverterer størrelse til SkoTex standard.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  def INPUT-output parameter wStorl as char NO-UNDO.

  DEF VAR wDecimaler as CHAR NO-UNDO.

  {syspara.i 1 1 16 wDecimaler}
   IF wDecimaler = ""
      THEN wDecimaler = "5".

  assign
     wStorl     = trim(wStorl)
     wStorl     = caps(wStorl)
     wStorl     = if (length(wStorl) = 1 or
                      length(wStorl) = 3
                     )
                    then " " + wStorl
                 else wStorl
     .

  /* Sjekker om det er benyttet gyldige tegn i halvnummer. */
  /* Er det ikke det, tas halvnummeret bort.               */
  IF DEC(wStorl) <= 999 THEN
  DO:
    ASSIGN wStorl = STRING(DEC(wStorl) / 10).

    /* Bytter ut eventuelle comma med punkt. */
    if index(wStorl,",") <> 0 then
      OVERLAY(wStorl, index(wStorl,","), 1, "CHARACTER") = ".".

    /*
    if NUM-ENTRIES(wStorl,".") = 2 then
      DO:
        if NOT CAN-DO(wDecimaler,ENTRY(2,wStorl,".")) then
          wStorl = ENTRY(1,wStorl,".").
      END.
    */
  END.

  RETURN wStorl.   /* Function return value. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetTempFileName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetTempFileName Procedure 
PROCEDURE GetTempFileName :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER ipPrefix      as CHAR NO-UNDO.
  DEF INPUT  PARAMETER ipExtent      as CHAR NO-UNDO.
  def output parameter iptmpFileName as char no-undo.

  /* Dette gir et windows tempfile name. Men da virker ikke macroen i Excel??
  RUN gettmpfile.p (INPUT ipPrefix).
  assign
    iptmpFileName = RETURN-VALUE.
  OVERLAY(iptmpFileName, R-INDEX(iptmpFileName,".") + 1, 3) = ipExtent.
  */

  def var iptmpDirName as char no-undo.
  
  assign
    iptmpDirName = session:TEMP-DIRECTORY.
  
  LET_LOOP:
  do while true:
    assign
      iptmpFileName = iptmpDirName +
                      ipPrefix + 
                      /*string(random(1,9999),"9999") +*/
                      REPLACE(STRING(TODAY),'/','') + 
                      REPLACE(STRING(TIME,"HH:MM:SS"),':','') +
                      "." + ipExtent.
    if search(iptmpFileName) = ? then
      leave LET_LOOP.
    ELSE 
        OS-DELETE VALUE(iptmpFileName) NO-ERROR.
  end. /* LET_LOOP */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WinTempFileName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WinTempFileName Procedure 
PROCEDURE WinTempFileName :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

