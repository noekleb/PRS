&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :  RUN koblevpifil.p (INPUT VPIFilHode.VPIFilType, OUTPUT pcInnlesning).


    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEF INPUT  PARAMETER piVPIFilTypeNr AS INT  NO-UNDO.
DEF INPUT  PARAMETER pcFilNavn      AS CHAR NO-UNDO.
DEF INPUT  PARAMETER piAction       AS INT  NO-UNDO.
DEF OUTPUT PARAMETER pcProgram      AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER piEkstVPILevNr AS INT  NO-UNDO.

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

IF NUM-ENTRIES(pcFilNavn,'|') = 2 THEN 
  ASSIGN
    piEkstVPILevNr = INTEGER(ENTRY(2,pcFilNavn,'|'))
    pcFilNavn      = ENTRY(1,pcFilNavn,'|').

RUN KobleVPIFil.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-KobleVPIFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KobleVPIFil Procedure 
PROCEDURE KobleVPIFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR pcTekst AS CHAR NO-UNDO.

  DEF BUFFER lokEkstVPILev FOR EkstVPILev.

  /* Leser alle filbeskrivelser av filtypen.             */
  /* Intill videre tar vi første unike match på filnavn. */
  FOR EACH EkstVPIFil NO-LOCK WHERE
      (IF piEkstVPILevNr <> 0 THEN EkstVPIFil.EkstVPILevNr = piEkstVPILevNr ELSE TRUE) AND 
      EkstVPIFil.VPIFilTypeNr = piVPIFilTypeNr AND
      EkstVPIFil.VPIFilAktiv = TRUE:

      FIND lokEkstVPILev NO-LOCK WHERE 
          lokEkstVPILev.EkstVPILevNr = piEkstVPILevNr NO-ERROR.
      IF AVAILABLE lokEkstVPILev AND
          lokEkstVPILev.AktivLev = FALSE THEN
          NEXT.

      /* Sjekker om det er begins, equal, matches som skal benyttes */
      IF EkstVpiFil.VPIOperator = 1          /* "<Navn>"   */
        THEN pcTekst = EkstVpiFil.VPIFilNavn.           
      ELSE IF EkstVpiFil.VPIOperator = 2     /* "<Navn>*"  */
        THEN pcTekst = EkstVpiFil.VPIFilNavn + "*".
      ELSE IF EkstVpiFil.VPIOperator = 3     /* "*<Navn>"  */
        THEN pcTekst = "*" + EkstVpiFil.VPIFilNavn. 
      ELSE IF EkstVpiFil.VPIOperator = 4     /* "*<Navn>*" */
        THEN pcTekst = "*" + EkstVpiFil.VPIFilNavn + "*".
      CASE piAction:
          WHEN 1 THEN
          DO:
              IF pcFilNavn MATCHES pcTekst THEN
                  ASSIGN
                  pcProgram      = EkstVPIFil.VPIInnlesningsrutine
                  piEkstVPILevNr = EkstVPIFil.EkstVPILevNr
                  .
          END.
          OTHERWISE
              pcProgram = "".
      END CASE.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

