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
    
    12/3-99  TN   Endret filnavn. filen legges nå på kom\ut katalogen.
    
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEF VAR wFilNavn    AS CHAR NO-UNDO.
DEF VAR wKatalog    AS CHAR NO-UNDO.
DEF VAR wMaksButikk AS INT NO-UNDO.

DEF STREAM UtFil.

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


IF AVAILABLE SysPara THEN
  RELEASE SysPara.
{syspara.i 3 1 2 wMaksButikk INT}
/* Filnavn */
{syspara.i 3 1 1 wFilNavn}
IF wFilNavn = "" THEN
  ASSIGN
    wFilNavn = "lager.dat".

/* Katalognavn */
IF AVAILABLE SysPara THEN
  RELEASE SysPara.
IF OPSYS = "unix" 
  THEN {syspar2.i 50 1 11 wKatalog}
ELSE {syspara.i 50 1 11 wKatalog}
IF wKatalog = "" THEN
  wKatalog = IF OPSYS = "unix" THEN "." ELSE ".".
IF SUBSTRING(wKatalog,LENGTH(wKatalog),1) = "/" OR
   SUBSTRING(wKatalog,LENGTH(wKatalog),1) = "\" THEN
 wKatalog = SUBSTRING(wKatalog,1,LENGTH(wKatalog) - 1).
    
/* Bygger full path til fil */
ASSIGN
  wFilNavn = wKatalog +
               (IF OPSYS = "unix" 
                  THEN "/"
                  ELSE "\") +
               wFilNavn.

RUN LesUtLager.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-LesUtLager) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LesUtLager Procedure 
PROCEDURE LesUtLager :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR wStreng AS CHAR NO-UNDO.
DEF VAR wLagant AS DECIMAL FORMAT "->>>>>9" EXTENT 99.
DEF VAR wLoop AS INT NO-UNDO.
DEF VAR wTeller AS INT NO-UNDO.
DEF VAR wStorl LIKE artlag.storl.
DEF VAR wStop   AS LOG NO-UNDO.

/* Åpner stream til fil. */
OUTPUT stream UtFil to value(wFilNavn) no-echo.

/* ARTIKKEL-LOOP */
FOR EACH ArtBas NO-LOCK WHERE
  ArtBas.Vg    >  0   AND
  ArtBas.Vg    <= 999 AND
  ArtBas.LopNr >  0   AND
  ArtBas.LopNr <= 9999 
  BY ArtBas.Vg
  BY ArtBas.LopNr:

  MAINLOOP: 
  FOR EACH artlag NO-LOCK WHERE
    ArtLag.Artikkelnr = ArtBas.artikkelnr AND
    artlag.storl  > ''            AND
    ArtLag.Butik  > 0             AND
    artlag.lagant > 0
    BREAK 
    BY ArtLag.artikkelnr
    BY ArtLag.storl 
    BY ArtLag.butik:

    /* Antall leste poster. */
    ASSIGN
      wTeller = wTeller + 1.

    /* Første linje i filen */
    IF FIRST-OF(Artlag.Storl) THEN
      DO:
        wStorl = artlag.storl.
        IF INDEX(wStorl,",") <> 0 THEN
        DO:
            IF SUBSTRING(wStorl,1,1) = "," THEN
              wStorl = "." + substring(wstorl,2,3).
          IF SUBSTRING(wStorl,2,1) = "," THEN
            wStorl = SUBSTRING(wStorl,1,1) + "." + substring(wstorl,3,2).
          IF SUBSTRING(wStorl,3,1) = "," THEN
            wStorl = SUBSTRING(wstorl,1,2) + "." + substring(wstorl,4,1).
          IF SUBSTRING(wStorl,4,1) = "," THEN
            wStorl = SUBSTRING(wStorl,1,3) + ".".
        END.
        ASSIGN
           wLagant = 0
          wStreng = TRIM(STRING(Artlag.Vg,"zz9")) + "," +
                     trim(STRING(Artlag.LopNr,"zzz9")) + "," +
                     trim(wStorl) + ",".
      END.
    
    /* Sumerer opp */
    IF artlag.butik <= wMaksButikk THEN
    ASSIGN
      wLagant[Artlag.Butik] = wLagant[Artlag.Butik] +
                               Artlag.Lagant.

    /* Siste post - ut skal den. */
    IF LAST-OF(Artlag.Storl) THEN
      LEGG-UT:
      DO:
        /* Undertrykker linjer med alfanumeriske størrelserl */
        wStop = FALSE.
        RUN SjekkStorl (INPUT wStorl, OUTPUT wStop).

        /* Legger ut linjen */
        DO wLoop = 1 TO wMaksButikk:
          ASSIGN
            wStreng = wStreng +
                       (IF wLagant[wLoop] > 0
                           THEN STRING(wLagant[wLoop])
                         ELSE '') + 
                      (IF wLoop < wMaksButikk
                             THEN ","
                             ELSE '').
        END.
        IF wStop = FALSE THEN
          DO:
            PUT STREAM UtFil UNFORMATTED wStreng.
            PUT STREAM UtFil SKIP.
          END.
      END. /* LEGG-UT */
  END. /* MAINLOOP */
END. /* ARTIKKEL-LOOP */

OUTPUT close.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SjekkStorl) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SjekkStorl Procedure 
PROCEDURE SjekkStorl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER wStorl AS CHAR NO-UNDO.
  DEF OUTPUT PARAMETER wStop  AS LOG NO-UNDO.
  
  DEF VAR wTekst AS CHAR NO-UNDO.  
  DEF VAR wLoop  AS INT NO-UNDO.
  
  ASSIGN
    wStop  = FALSE
    wTekst = wStorl.
  
  LOOPEN:
  DO wLoop = 1 TO LENGTH(wStorl):
    IF SUBSTRING(wStorl,wloop,1) = "." THEN.
    ELSE IF NOT CAN-DO(" ,0,1,2,3,4,5,6,7,8,9",SUBSTRING(wStorl,wLoop,1)) THEN
      DO:
        wStop = TRUE.
        LEAVE LOOPEN.
      END.
  END. /* LOOPEN */

  /* Sjekker om tallverdien er for stor. */
  IF wStop = FALSE THEN
  DO:
    IF DEC("10.0") = 100 THEN
      DO:
        IF INDEX(wTekst,".") <> 0 THEN
          OVERLAY(wTekst,INDEX(wTekst,".")) = ",".
      END.
    IF DEC(wTekst) > 99 THEN
      wStop = TRUE.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

