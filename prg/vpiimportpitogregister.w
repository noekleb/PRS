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

DEF VAR cKataloger    AS CHAR NO-UNDO.
DEF VAR cFilNavnListe AS CHAR NO-UNDO.

DEF VAR h_dvpifilhode AS HANDLE NO-UNDO.
DEF VAR h_dvpifillogg AS HANDLE NO-UNDO.

DEF VAR cTekst AS CHAR NO-UNDO.
DEF VAR lFilId AS DEC  NO-UNDO.

DEF VAR bPBR   AS LOG  NO-UNDO.
DEF VAR bFiler AS LOG  NO-UNDO.

DEF BUFFER bufVPIFilHode FOR VPIFilHode.

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

RUN dvpifillogg.w PERSISTENT SET h_dvpifillogg.                                               
RUN dvpifilhode.w PERSISTENT SET h_dvpifilhode.

SUBSCRIBE "VPIFillogg"   ANYWHERE.
SUBSCRIBE "GetFilIdLogg" ANYWHERE.

{syspara.i 50 200 1 cTekst}
IF CAN-DO("1,yes,true,ja",cTekst) THEN
    bPBR = TRUE.
ELSE
    bPBR = FALSE.

{sww.i}
PUBLISH "loggEditorBatchServerInn" ("VPIScannKataloger").
RUN ScannKataloger.
PUBLISH "loggEditorBatchServerInn" ("VPILeser inn").
RUN LesInn.
PUBLISH "loggEditorBatchServerInn" ("VPIPakker ut").
RUN PakkUt.
IF bPBR = TRUE AND cFilNavnListe <> "" THEN
DO:
    PUBLISH "loggEditorBatchServerInn" ("Overfør PBR").
    RUN pbr_register.
END.
{swn.i}

IF VALID-HANDLE(h_dvpifillogg) THEN
    DELETE PROCEDURE h_dvpifillogg.
IF VALID-HANDLE(h_dvpifilhode) THEN
    DELETE PROCEDURE h_dvpifilhode.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-GetFilIdLogg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetFilIdLogg Procedure 
PROCEDURE GetFilIdLogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAMETER plFilId AS DEC NO-UNDO.

  ASSIGN
      plFilId = lFilId
      .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LesInn) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LesInn Procedure 
PROCEDURE LesInn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DEF VAR pcValgteFiler    AS CHAR NO-UNDO.
  DEF VAR piLoop1          AS INT  NO-UNDO.
  DEF VAR pcTekst          AS CHAR NO-UNDO.
  DEF VAR pbOk             AS LOG  NO-UNDO.
  DEF VAR pbMore           AS LOG  NO-UNDO.
  DEF VAR piAntLinjer      AS INT  NO-UNDO.
  DEF VAR pcKeyValues      AS char NO-UNDO.
  DEF VAR piFokusRad       AS INT  NO-UNDO.

  /* Leser X og X filer av gangen, til det er tomt. */
  /* Sikrer at ikke variabel sprekker.              */
  ASSIGN pbMore = TRUE.
  DO WHILE pbMore = TRUE:

      IF pcValgteFiler = "" THEN
          RUN GetAlleIkkeInnleste IN h_dvpifilhode (INPUT 100, OUTPUT pcValgteFiler, OUTPUT pbMore).
      ELSE
          pbMore = FALSE.

      /* Tar vare på den første posten for å reposisjonere */
      ASSIGN
          pcKeyValues = ENTRY(1,pcValgteFiler,CHR(1))
          .

      /* Leser inn de valgte filene */
      IF pcValgteFiler <> "" THEN
      DO piLoop1 = 1 TO num-entries(pcValgteFiler,chr(1)):

          FIND bufVPIFilHode NO-LOCK WHERE
              bufVPIFilHode.FilId = DEC(ENTRY(piLoop1,pcValgteFiler,CHR(1))) NO-ERROR.
          IF AVAILABLE bufVPIFilHode THEN
              PUBLISH "SkrivTilDataMottaksLogg" (";Leser inn:;" +   
                                                 bufVPIFilHode.FilNavn + " " + 
                                                 string(bufVPIFilHode.Dato) + " " + 
                                                 bufVPIFilHode.Kl + " " + 
                                                 string(bufVPIFilHode.Storrelse) + " " + 
                                                 bufVPIFilHode.Katalog).

        ASSIGN
            lFilId = DEC(ENTRY(piLoop1,pcValgteFiler,CHR(1)))
            .
        /* Leser inn fil. */
        RUN LesInnFil IN h_dvpifilhode (INPUT ENTRY(piLoop1,pcValgteFiler,CHR(1)), 
                                   OUTPUT pbOk, 
                                   OUTPUT piAntLinjer).
        /* Viser eventuell feilmelding. */
        /* PUBLISH .... */
      END.
      ASSIGN
          pcValgteFiler = ""
          .
  END.

  IF pcKeyValues <> "" THEN
  DO:
      DYNAMIC-FUNCTION('openQuery':U IN h_dvpifilhode).
      DYNAMIC-FUNCTION('findRow':U IN h_dvpifilhode,
         INPUT pcKeyValues /* CHARACTER */).
      RUN dataAvailable IN h_dvpifilhode
          ( INPUT "SAME" /* CHARACTER */).
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PakkUt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PakkUt Procedure 
PROCEDURE PakkUt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DEF VAR pcValgteFiler    AS CHAR NO-UNDO.
  DEF VAR piLoop1          AS INT  NO-UNDO.
  DEF VAR pcTekst          AS CHAR NO-UNDO.
  DEF VAR pbOk             AS LOG  NO-UNDO.
  DEF VAR pbMore           AS LOG  NO-UNDO.
  DEF VAR piAntLinjer      AS INT  NO-UNDO.
  DEF VAR pcKeyValues      AS char NO-UNDO.
  DEF VAR piFokusRad       AS INT  NO-UNDO.
  DEF VAR pbVPIFilStatus   AS INT  NO-UNDO.

  /* Leser X og X filer av gangen, til det er tomt. */
  /* Sikrer at ikke variabel sprekker.              */
  ASSIGN pbMore = TRUE.
  DO WHILE pbMore = TRUE:

      IF pcValgteFiler = "" THEN
          RUN GetAlleIkkeUtpakkede IN h_dvpifilhode (INPUT 100, OUTPUT pcValgteFiler, OUTPUT pbMore).
      ELSE
          pbMore = FALSE.

      /* Tar vare på den første posten for å reposisjonere */
      ASSIGN
          pcKeyValues = ENTRY(1,pcValgteFiler,CHR(1))
          .

      /* Starter utpakkingsrutine for de filer som skal pakkes ut */
      IF pcValgteFiler <> "" THEN
      FILBEHANDLING:
      DO piLoop1 = 1 TO num-entries(pcValgteFiler,chr(1)):

          FIND bufVPIFilHode NO-LOCK WHERE
              bufVPIFilHode.FilId = DEC(ENTRY(piLoop1,pcValgteFiler,CHR(1))) NO-ERROR.
          IF AVAILABLE bufVPIFilHode THEN
              PUBLISH "SkrivTilDataMottaksLogg" (";Pakk ut:;" +   
                                                 bufVPIFilHode.FilNavn + " " + 
                                                 string(bufVPIFilHode.Dato) + " " + 
                                                 bufVPIFilHode.Kl + " " + 
                                                 string(bufVPIFilHode.Storrelse) + " " + 
                                                 bufVPIFilHode.Katalog).

          ASSIGN
              lFilId = DEC(ENTRY(piLoop1,pcValgteFiler,CHR(1)))
              .
          RUN GetVPIFilStatus IN h_dvpifilhode (INPUT entry(piLoop1,pcValgteFiler,CHR(1)),OUTPUT pbVPIFilStatus).
          IF pbVPIFilStatus >= 3 AND 
             pbVPIFilStatus <= 5 THEN
              RUN PakkUtFil IN h_dvpifilhode (INPUT entry(piLoop1,pcValgteFiler,CHR(1))).
      END. /* FILBEHANDLING */
      ASSIGN
          pcValgteFiler = ""
          .
  END.

  IF pcKeyValues <> "" THEN
  DO:
      DYNAMIC-FUNCTION('openQuery':U IN h_dvpifilhode).
      DYNAMIC-FUNCTION('findRow':U IN h_dvpifilhode,
         INPUT pcKeyValues /* CHARACTER */).
      RUN dataAvailable IN h_dvpifilhode
          ( INPUT "SAME" /* CHARACTER */).
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pbr_register) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pbr_register Procedure 
PROCEDURE pbr_register :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
      ASSIGN
          cTekst = "PBR - Oppdaterer vare og registerinfo.".
      PUBLISH "VPIFilLogg" (cTekst + chr(1) + "5").
      RUN pfxoppdatfastereg.p.
      ASSIGN
          cTekst = "PBR - Oppdatering ferdig.".
      PUBLISH "VPIFilLogg" (cTekst + chr(1) + "5").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ScannKataloger) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ScannKataloger Procedure 
PROCEDURE ScannKataloger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Scanner katalog og åpner query med ny liste. 
------------------------------------------------------------------------------*/
  /* 1. Bygg en liste over alle kataloger som skal kontrolleres. */
  PUBLISH "loggEditorBatchServerInn" ("VPIScann Katalog").  
  RUN GetKatalogListe IN h_dvpifilhode (OUTPUT cKataloger).
  
  /* 2. Bygg en liste med alle filnavn + ekstent som skal kontrolleres. */
  PUBLISH "loggEditorBatchServerInn" ("VPIScann Filnavn").
  RUN GetFilNavnListe IN h_dvpifilhode (OUTPUT cFilNavnListe).
  
  /* 3. Opprett en post i fillisten for alle filer som ikke finnes der. */
  PUBLISH "loggEditorBatchServerInn" ("VPIScann OpprettPoster").
  RUN OpprettPoster IN h_dvpifilhode (INPUT cKataloger, INPUT cFilNavnListe). 

 /* 5. OpenQuery for fillisten. */ 
  DYNAMIC-FUNCTION('openQuery':U IN h_dvpifilhode).
  PUBLISH "loggEditorBatchServerInn" ("VPIFerdig ScannKataloger").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-VPIFillogg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VPIFillogg Procedure 
PROCEDURE VPIFillogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER pcTekst AS CHAR NO-UNDO.

  RUN VPIFilLogg IN h_dvpifillogg (INPUT pcTekst).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

