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

DEF VAR cTekst AS CHAR NO-UNDO.
DEF VAR lFilId AS DEC  NO-UNDO.

DEF VAR bPBR   AS LOG  NO-UNDO.
DEF VAR bFiler AS LOG  NO-UNDO.

DEF BUFFER bufVPIFilHode FOR VPIFilHode.

DEF STREAM InnFil.

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
         HEIGHT             = 24.67
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */


SUBSCRIBE "VPIFillogg"   ANYWHERE.
SUBSCRIBE "GetFilIdLogg" ANYWHERE.

/* Starte oppdatering mot ProfilBase. */
{syspara.i 50 200 1 cTekst}
IF CAN-DO("1,yes,true,ja",cTekst) THEN
    bPBR = TRUE.
ELSE
    bPBR = FALSE.

/*{sww.i}*/
/* Bygger en liste over alle filer som skal leses inn. */
PUBLISH "loggEditorBatchServerInn" ("VPIScannKataloger").
RUN ScannKataloger.

/* Leser inn alle filer i VPIFilHode og VPIFillinje og logger dem i importlisten som innlest (VPIFilMottak). */
PUBLISH "loggEditorBatchServerInn" ("VPILeser inn").
RUN LesInn.

/* Pakker ut filer alle filer som ligger merket som innlest, men ikke utpakket i importlisten (VPIFilMottak). */
PUBLISH "loggEditorBatchServerInn" ("VPIPakker ut").
RUN PakkUt.

/* Eksport til ProfilBase */
IF bPBR = TRUE AND cFilNavnListe <> "" THEN
DO:
    PUBLISH "loggEditorBatchServerInn" ("Overfør PBR").
    RUN pbr_register.
END.
/*{swn.i} */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-GetAlleIkkeInnleste) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetAlleIkkeInnleste Procedure 
PROCEDURE GetAlleIkkeInnleste :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:      

 
------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER piAntVPIFilHode AS INT  NO-UNDO.
  DEF OUTPUT PARAMETER pcListe         AS CHAR NO-UNDO.
  DEF OUTPUT PARAMETER pbMore          AS LOG  NO-UNDO.

  DEF BUFFER bufVPIFilHode FOR VPIFilHode.

  ASSIGN
      pbMore = FALSE
      .

  LOOPEN:
  FOR EACH bufVPIFilHode NO-LOCK WHERE
      bufVPIFilHode.VPIFilStatus   >= 1 AND
      bufVPIFilHode.VPIFilStatus   <= 2
      BY bufVPIFilHode.FilId:

      ASSIGN
          pcListe = pcListe + 
                    (IF pcListe = ""
                       THEN ""
                       ELSE CHR(1)) + 
                    string(bufVPIFilHode.FilId).
      IF NUM-ENTRIES(pcListe,CHR(1)) > piAntVPIFilHode THEN
      DO:
          ASSIGN
              pbMore = TRUE
              .
          LEAVE LOOPEN.
      END.
  END. /* LOOPEN */

              
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetAlleIkkeUtpakkede) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetAlleIkkeUtpakkede Procedure 
PROCEDURE GetAlleIkkeUtpakkede :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:      

 
------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER piAntVPIFilHode AS INT  NO-UNDO.
  DEF OUTPUT PARAMETER pcListe         AS CHAR NO-UNDO.
  DEF OUTPUT PARAMETER pbMore          AS LOG  NO-UNDO.

  ASSIGN
      pbMore = FALSE
      .

  LOOPEN:
  FOR EACH VPIFilHode NO-LOCK WHERE
      VPIFilHode.VPIFilStatus   >=3 AND
      VPIFilHode.VPIFilStatus   <=4
      BY VPIFilHode.FilId:

      ASSIGN
          pcListe = pcListe + 
                    (IF pcListe = ""
                       THEN ""
                       ELSE CHR(1)) + 
                    string(VPIFilHode.FilId).
      IF NUM-ENTRIES(pcListe,CHR(1)) > piAntVPIFilHode THEN
      DO:
          ASSIGN
              pbMore = TRUE
              .
          LEAVE LOOPEN.
      END.
  END. /* LOOPEN */

              
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

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

&IF DEFINED(EXCLUDE-GetFilNavnListe) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetFilNavnListe Procedure 
PROCEDURE GetFilNavnListe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAMETER pcFilNavn AS CHAR NO-UNDO.
  
  DEFINE VARIABLE pcLst AS CHARACTER NO-UNDO.

  ASSIGN
    pcFilNavn = ""
    pcLst     = 'SBA,SPK,AE,APK'
    .
  FOR EACH EkstVPILev NO-LOCK WHERE
      EkstVPILev.AktivLev = TRUE:
      VPIFIL:
      FOR EACH EkstVPIFil OF EkstVPILev NO-LOCK WHERE
          EkstVPIFil.VPIFilAktiv = TRUE:

          /* VPI filer skal ikke lenger leses inn her. Det gjøres manuelt fra VPIMottakskontrollen */
          /* TN 28/7-08.                                                                           */
          /* VPI filer med filnavnprefix som ligger i denne listen, skal leses inn automatisk.     */
          /* TN 19/9-10 pcLst                                                                      */
          IF (EkstVPIFil.VPIFilType = 1  AND
              NOT CAN-DO(pcLst,EkstVPIFil.VPIFilNavn) AND 
              EkstVPIFil.VPIUtpakkingsrutin = "xsport1vpiutpakk") THEN
              NEXT VPIFIL.

          IF NOT CAN-DO(pcFilNavn,EkstVPIFil.VPIFilNavn + "|" + EkstVPIFil.VPIEkst + "|" + "1|1") THEN
          ASSIGN
            pcFilNavn = pcFilNavn + 
                          (IF pcFilNavn = ""
                             THEN ""
                             ELSE ",") + 
                          EkstVPIFil.VPIFilNavn + "|" + 
                          EkstVPIFil.VPIEkst + "|" + 
                          STRING(EkstVpiFil.VPIOperator) + 
                          "|" + STRING(EkstVpiFil.VPIFilTypeNr) + "|" + 
                          string(EkstVPIFil.EkstVPILevNr)
                          .
      END. /* VPIFIL */
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetKatalogListe) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetKatalogListe Procedure 
PROCEDURE GetKatalogListe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAMETER pcKataloger AS CHAR NO-UNDO.

  ASSIGN
    pcKataloger = ""
    .
  FOR EACH EkstVPILev NO-LOCK WHERE
      EkstVPILev.Aktiv = TRUE:
      VPIFIL:
      FOR EACH EkstVPIFil OF EkstVPILev NO-LOCK WHERE
          EkstVPIFil.VPIFilAktiv = TRUE:

          /* VPI filer skal ikke lenger leses inn her. Det gjøres manuelt fra VPIMottakskontrollen */
          /* TN 28/7-08.                                                                           */
          IF (EkstVPIFil.VPIFilType = 1  AND
              EkstVPIFil.VPIUtpakkingsrutin = "xsport1vpiutpakk") THEN
              NEXT VPIFIL.

          IF NOT CAN-DO(pcKataloger,EkstVPIFil.VPIKatalog) THEN
          ASSIGN
            pcKataloger = pcKataloger + 
                          (IF pcKataloger = ""
                             THEN ""
                             ELSE ",") + 
                          EkstVPIFil.VPIKatalog
                          .
      END. /* VPIFIL */
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetVPIFilStatus) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetVPIFilStatus Procedure 
PROCEDURE GetVPIFilStatus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER pdFilId        AS DEC NO-UNDO.
  DEF OUTPUT PARAMETER pbVPIFilStatus AS INT NO-UNDO.

  FIND VPIFilHode NO-LOCK WHERE
      VPIFilHode.FilId = pdFilId NO-ERROR.
  IF AVAILABLE VPIFilHode THEN
      pbVPIFilStatus = VPIFilHode.VPIFilStatus.
  ELSE
      pbVPIFilStatus = 0.

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
  DEF VAR pcKeyValues      AS CHAR NO-UNDO.
  DEF VAR piFokusRad       AS INT  NO-UNDO.

  PUBLISH "loggEditorBatchServerInn - VPILeser inn" ("Før loop").

  /* Leser X og X filer av gangen, til det er tomt. */
  /* Sikrer at ikke variabel sprekker.              */
  ASSIGN pbMore = TRUE.
  DO WHILE pbMore = TRUE:

      IF pcValgteFiler = "" THEN
          RUN GetAlleIkkeInnleste (INPUT 100, OUTPUT pcValgteFiler, OUTPUT pbMore).
      ELSE
          pbMore = FALSE.

      PUBLISH "loggEditorBatchServerInn - VPILeser inn" ("Flagg - " + string(pbMore)).
      PUBLISH "loggEditorBatchServerInn - VPILeser inn" ("Filliste - " + pcValgteFiler).

      /* Tar vare på den første posten for å reposisjonere */
      ASSIGN
          pcKeyValues = ENTRY(1,pcValgteFiler,CHR(1))
          .

      /* Leser inn de valgte filene */
      IF pcValgteFiler <> "" THEN
      DO piLoop1 = 1 TO NUM-ENTRIES(pcValgteFiler,CHR(1)):

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
        PUBLISH "loggEditorBatchServerInn - VPILeser inn" ("Leser fil - " + ENTRY(piLoop1,pcValgteFiler,CHR(1))).
        /* Leser inn fil. */
        RUN LesInnFil(INPUT ENTRY(piLoop1,pcValgteFiler,CHR(1)), 
                                   OUTPUT pbOk, 
                                   OUTPUT piAntLinjer).
        /* Viser eventuell feilmelding. */
        PUBLISH "loggEditorBatchServerInn - VPILeser inn" ("Ferdig fil fil - " + ENTRY(piLoop1,pcValgteFiler,CHR(1))).
      END.
      ASSIGN
          pcValgteFiler = ""
          .
  END.

  PUBLISH "loggEditorBatchServerInn - VPILeser inn" ("Etter loop").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LesInnFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LesInnFil Procedure 
PROCEDURE LesInnFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER plFilId     LIKE VPIFilHode.FilId NO-UNDO.
  DEF OUTPUT PARAMETER pbOk        AS   LOG         NO-UNDO.
  DEF OUTPUT PARAMETER piAntLinjer AS   INT         NO-UNDO.
  
  DEF VAR pcKvitteringId  AS CHAR NO-UNDO.
  DEF VAR pcError         AS CHAR NO-UNDO.
  DEF VAR pcInnlesning AS CHAR NO-UNDO.
  DEF VAR pcLinje         AS CHAR NO-UNDO.
  DEF VAR pcBkuFil        AS CHAR NO-UNDO.

  DEF VAR piButikkNr      AS INT  NO-UNDO.
  DEF VAR piGruppeNr      AS INT  NO-UNDO.
  DEF VAR piKasseNr       AS INT  NO-UNDO.
  DEF VAR piTid           AS INT  NO-UNDO.
  DEF VAR pcReturn-Value  AS CHAR NO-UNDO.
  DEF VAR piEkstVPILevNr  AS INT  NO-UNDO.

  ASSIGN
      piTid          = TIME
      pcReturn-Value = ""
      .

  FIND VPIFilHode NO-LOCK WHERE
      VPIFilHode.FilId = plFilId NO-ERROR.
  IF NOT AVAILABLE VPIFilHode THEN
      RETURN "** VPIFilHode posten finnes ikke.".
  IF SEARCH(VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn) = ? THEN
  DO:
      ASSIGN
          cTekst = "* Finner ikke filen " + VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn + ". Innlesning avbrutt.".

      /* Er filen borte fra katalogen, skal Filer posten bare slettes */
      /* Det skal ikke logges noe.                                    */
      /* Loggen vil bli slettet av databasetrigger.                   */
      DO TRANSACTION:
          FIND CURRENT VPIFilHode EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
          IF AVAILABLE VPIFilhode THEN
              DELETE VPIFilHode.
      END. /* TRANSACTION */
      RETURN cTekst.
  END.
  ELSE DO:
      ASSIGN
          cTekst = "Leser inn filen " + VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn + ".".
      PUBLISH "VPIFilLogg" (cTekst + chr(1) + "5").
  END.

  IF VPIFilHode.Storrelse > 0 THEN
  FILINN:
  DO ON ERROR UNDO, LEAVE:
    STATUS DEFAULT "".

    /* Koble filen til Ekstern VPI leverandør og filtype for å finne */
    /* programnavn for innlesning og oppdatering av filen.            */
    ASSIGN
        pcInnlesning = "".
    RUN koblevpifil.p 
        (INPUT VPIFilHode.VPIFilType, 
         INPUT ENTRY(1,VPIFilHode.FilNavn,"."),
         1, /* Innlesningsprogram skal hentes */
         OUTPUT pcInnlesning,
         OUTPUT piEkstVPILevNr).

    /* Sjekker at programmet finnes. */
    IF SEARCH(pcInnlesning + ".r") = ? THEN
    DO:
      IF pcInnlesning = "" THEN
      DO:
          ASSIGN
              cTekst = "* Det er ikke satt opp innlesningsprogram for denne filtypen. Innlesning avbrutt.".
          PUBLISH "VPIFilLogg" (cTekst + chr(1) + "40").
          /*RUN SetFilInnlest.*/
          RETURN "Det er ikke satt opp innlesningsprogram for denne filtypen.".
      END.
      ELSE DO:
          ASSIGN
              cTekst = "* Ukjent innlesningsprogram: " + pcInnlesning + ".r. innlesning avbrutt.".
          PUBLISH "VPIFilLogg" (cTekst + chr(1) + "40").
          /*RUN SetFilInnlest. */
          RETURN "Ukjent innlesningsprogram: " + pcInnlesning + ".r".
      END.
    END.

    /* Blanker RETURN-VALUE */
    RUN RensReturn.

    /* Leser inn fillinjene */
    RUN VALUE(pcInnlesning + ".p") 
        (INPUT  plFilId,
         INPUT  THIS-PROCEDURE,
         OUTPUT piAntLinjer
        ).

    IF RETURN-VALUE <> "" AND RETURN-VALUE <> 'OK' THEN
    DO:
        pcError = RETURN-VALUE.
        
        /* Innleste filer som returnerer dette, skal ikke slettes fra ankommet katalogne. */
        IF pcError = 'OK Ikke slett' THEN 
            pbOk = TRUE.
    END.
    ELSE
      ASSIGN
          pbOk = TRUE
          .

    STATUS DEFAULT "".
  END. /* FILINN */
  ELSE
      pbOk = TRUE. /* NullByte filer skal ferdigmerkes. */

  IF pbOk = FALSE THEN
  DO:
      DO TRANSACTION:
          FIND CURRENT VPIFilHode EXCLUSIVE-LOCK.
          ASSIGN
              VPIFilHode.VPIFilStatus = 9 /* Misslykket innlesning */
              VPIFilHode.AntLinjer    = piantLinjer
              .
      END. /* TRANSACTION */
      FIND CURRENT VPIFilHode NO-LOCK.

      ASSIGN
          cTekst = "* Det er oppdaget feil på filen. " + 
                   "Fil innlesning avbrutt. (Lest antall linjer = " + STRING(piAntLinjer) + ").".
      PUBLISH "VPIFilLogg" (cTekst + chr(1) + "40").
      RETURN cTekst.
  END.
  ELSE DO:
      DO TRANSACTION:
          FIND CURRENT VPIFilHode EXCLUSIVE-LOCK.
          ASSIGN
              pcBkuFil = VPIFilHode.FilNavn + ".bku" + string(TODAY,"99-99-99") + "-" + string(TIME)   
              VPIFilHode.VPIFilStatus = IF VPIFilHode.Storrelse > 0 
                                          THEN 3 /* Vellykket innlesning */
                                          ELSE 5 /* Ferdigstatus.        */
              VPIFilHode.AntLinjer    = piantLinjer
              VPIFilHode.EkstVPILevNr = piEkstVPILevNr
              .
      END. /* TRANSACTION */
      FIND CURRENT VPIFilHode NO-LOCK.

      /* Sikrer at backup katalog finnes. */
      OS-CREATE-DIR value(VPIFilHode.Katalog + "~\bku").
      /* Flytter filen til backup katalog. */
      OS-COPY value(VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn) value(VPIFilHode.Katalog + "~\bku~\" + pcBkuFil).

      /* Renser bort fil */
      IF SEARCH(VPIFilHode.Katalog + "~\bku~\" + pcBkuFil) <> ? AND pcError <> 'OK Ikke slett' THEN
      DO:
          /* Filen tas bort fra katalogen. */
          IF SEARCH(VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn) <> ? THEN
              OS-DELETE VALUE(VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn).
      END.
      ELSE /* */.
     
      ASSIGN
          cTekst = "Fil innlesning ferdig. (Tid brukt: " + 
                    string(TIME - piTid,"HH:MM:SS") + " Lest antall linjer = " + STRING(piAntLinjer) + ").".           
      PUBLISH "VPIFilLogg" (cTekst + chr(1) + "5").
      RETURN cTekst.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OpprettPoster) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpprettPoster Procedure 
PROCEDURE OpprettPoster :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF INPUT PARAMETER pcKataloger AS CHAR NO-UNDO.
  DEF INPUT PARAMETER pcFiler     AS CHAR NO-UNDO.

  DEF VAR piLoop1        AS INT  NO-UNDO.
  DEF VAR piLoop2        AS INT  NO-UNDO.
  DEF VAR pcKatalog      AS CHAR NO-UNDO.
  DEF VAR pcEkstent      AS CHAR NO-UNDO.
  DEF VAR pcKatFil       AS CHAR NO-UNDO.
  DEF VAR plFilId        AS DEC  NO-UNDO.
  DEF VAR pcBegins       AS CHAR NO-UNDO.
  DEF VAR piEntries      AS INT  NO-UNDO.
  DEF VAR piFilType      AS INT  NO-UNDO.
  DEF VAR piEkstVPILevNr AS INT  NO-UNDO.

  DEF VAR pcFileName    AS CHAR FORMAT "x(100)" NO-UNDO.
  DEF VAR pcFilePath    AS CHAR FORMAT "x(100)" NO-UNDO.
  DEF VAR pcFileAttrib  AS CHAR FORMAT "x(15)" NO-UNDO.

  /* Ingenting å sjekke */
  IF pcKataloger = "" THEN
    RETURN "Ingen kataloger som skal sjekkes.".

  /* Behandler alle filer. */
  DO piLoop1 = 1 TO NUM-ENTRIES(pcFiler):
    /*
      1. --> _ og _
      2. --> _ og *
      3. --> * og _
      4. --> * og *
    */
    ASSIGN
      piEkstVPILevNr = INT(ENTRY(5,ENTRY(piLoop1,pcFiler),"|"))
      pcBegins  = /* Legger på stjerne først, hvis det er "Inneholder" */
                  (IF (ENTRY(3,entry(piLoop1,pcFiler),"|") = "3" OR ENTRY(3,entry(piLoop1,pcFiler),"|") = "4")
                    THEN "*"
                    ELSE "") + 
                  /* Legger på filnavnet */
                  ENTRY(1,
                          ENTRY(piLoop1,pcFiler),"|"
                       ) +
                  /* Legger på stjerne hvis det er starter med, eller inneholder */  
                  (IF (ENTRY(3,entry(piLoop1,pcFiler),"|") = "2" OR ENTRY(3,entry(piLoop1,pcFiler),"|") = "4")
                    THEN "*"
                    ELSE "")
      pcEkstent = ENTRY(2,
                          ENTRY(piLoop1,pcFiler),"|"
                       )
      piFilType = INT(ENTRY(4,
                          ENTRY(piLoop1,pcFiler),"|"
                       ))
      .

    /* For hver ekstent i katalog leses og behandles fillisten i katalogen. */
    DO piLoop2 = 1 TO NUM-ENTRIES(pcKataloger):
      ASSIGN
        pcKatFil  = ENTRY(piLoop2,pcKataloger)
        .
       /* Skaper katalogen hvis den ikke finnes */
      OS-CREATE-DIR value(pcKatFil).

      /* Leser fillisten fra katalogen. */
      INPUT STREAM InnFil FROM OS-DIR (pcKatFil) NO-ECHO.
      FILINPUT:
      REPEAT:

        IMPORT STREAM Innfil   
          pcFileName  
          pcFilePath  
          pcFileAttrib
          NO-ERROR.
        /*
        SET STREAM InnFil
          pcFileName  
          pcFilePath  
          pcFileAttrib
          WITH WIDTH 248.
        */
        
        /* Bare filer skal opprettes */
        IF LOOKUP("F",pcFileAttrib) <> 0 THEN
        DO:
          /* Åpner for filinformasjonen */
          ASSIGN
            piEntries           = NUM-ENTRIES(pcFileName,".")
            FILE-INFO:FILE-NAME = pcFilePath
            . 

          /* Hopper over tomme filer.                                                   */
          /* TN 13/11-08 Dette har gitt problem i pakseddel plukklisteimport.           */
          /* Tomme filer skal ikke slettes, bare ignoreres.                             */
          /* Antagelig slettes filer når avsender ikke har rukket å bygge filen ferdig. */
          IF FILE-INFO:FILE-SIZE = 0 THEN DO:
              /* OS-DELETE VALUE(FILE-INFO:FILE-NAME). */
              NEXT FILINPUT.
          END.

          /* Kun filer som oppfyller masken på filnavn skal inn. */
          IF ENTRY(1,pcFileName,".") MATCHES pcBegins THEN. /* Gjør ingenting. */
          ELSE
            NEXT FILINPUT. /* Hopp over denne */

          /* Kun filer som oppfyller extent masken skal inn */
          IF piEntries > 1 THEN
          DO:
            IF ENTRY(piEntries,pcfileName,".") MATCHES pcEkstent THEN. /* gjør ingenting. */
            ELSE
              NEXT FILINPUT. /* Hopp over denne */
          END.
          /* Oppretter posten i filen. */
          IF NOT CAN-FIND(FIRST 
                          VPIFilHode WHERE
                          VPIFilHode.FilNavn   = pcFileName AND
                          VPIFilHode.Dato      = FILE-INFO:FILE-MOD-DATE AND
                          VPIFilHode.Kl        = string(FILE-INFO:FILE-MOD-TIME,"HH:MM:SS") AND
                          VPIFilHode.Storrelse = FILE-INFO:FILE-SIZE AND
                          VPIFilHode.Katalog   = pcKatFil
                         ) THEN
          DO:
            /* Finner FilId */
            FIND LAST VPIFilHode NO-LOCK NO-ERROR.
            IF AVAILABLE VPIFilHode THEN
              plFilId = VPIFilHode.FilId + 1.
            ELSE
              plFilId = 1.
            CREATE VPIFilHode.
            ASSIGN
              VPIFilHode.FilId        = plFilId
              VPIFilHode.FilNavn      = pcFileName
              VPIFilHode.Katalog      = pcKatFil
              VPIFilHode.Dato         = FILE-INFO:FILE-MOD-DATE
              VPIFilHode.Kl           = STRING(FILE-INFO:FILE-MOD-TIME,"HH:MM:SS")
              VPIFilHode.Storrelse    = FILE-INFO:FILE-SIZE
              VPIFilHode.AntLinjer    = 0
              VPIFilHode.VPIFilType   = piFilType
              VPIFilHode.VPIFilStatus = 1
              VPIFilHode.EkstVPILevNr = piEkstVPILevNr
              .
          END.
        END.

      END. /* FILINPUT */
      INPUT STREAM InnFil CLOSE.
    END.
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
  DEF VAR pcKeyValues      AS CHAR NO-UNDO.
  DEF VAR piFokusRad       AS INT  NO-UNDO.
  DEF VAR pbVPIFilStatus   AS INT  NO-UNDO.

  /* Leser X og X filer av gangen, til det er tomt. */
  /* Sikrer at ikke variabel sprekker.              */
  ASSIGN pbMore = TRUE.
  DO WHILE pbMore = TRUE:

      IF pcValgteFiler = "" THEN
          RUN GetAlleIkkeUtpakkede (INPUT 100, OUTPUT pcValgteFiler, OUTPUT pbMore).
      ELSE
          pbMore = FALSE.

      /* Tar vare på den første posten for å reposisjonere */
      ASSIGN
          pcKeyValues = ENTRY(1,pcValgteFiler,CHR(1))
          .

      /* Starter utpakkingsrutine for de filer som skal pakkes ut */
      IF pcValgteFiler <> "" THEN
      FILBEHANDLING:
      DO piLoop1 = 1 TO NUM-ENTRIES(pcValgteFiler,CHR(1)):

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
          RUN GetVPIFilStatus (INPUT ENTRY(piLoop1,pcValgteFiler,CHR(1)),OUTPUT pbVPIFilStatus).
          IF pbVPIFilStatus >= 3 AND 
             pbVPIFilStatus <= 5 THEN
              RUN PakkUtFil (INPUT ENTRY(piLoop1,pcValgteFiler,CHR(1))).
      END. /* FILBEHANDLING */
      ASSIGN
          pcValgteFiler = ""
          .
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PakkUtFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PakkUtFil Procedure 
PROCEDURE PakkUtFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  
  
------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER pcFilId AS  CHAR NO-UNDO.

  FIND VPIFilHode NO-LOCK WHERE
      VPIFilHode.FilId = dec(pcFilId) NO-ERROR.
  IF NOT AVAILABLE VPIFilHode THEN
      RETURN.
  
  FIND FIRST EkstVPIFil NO-LOCK WHERE
      EkstVPIFil.EkstVPILevNr = VPIFilHode.EkstVPILevNr AND
      EkstVPIFil.VPIFilTypeNr = VPIFilHode.VPIFilType
      NO-ERROR.
  IF NOT AVAILABLE EkstVPIFil THEN
      RETURN.

  PAKKUT:
  DO ON ERROR UNDO, LEAVE:
    IF SEARCH(EkstVPIFil.VPIUtpakkingsrutine + ".r") <> ? THEN
        RUN VALUE(EkstVPIFil.VPIUtpakkingsrutine)
            (INPUT VPIFilHode.FilId).
            
  END. /* PAKKUT */

  RETURN "Utpakking av fil ferdig.".

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

&IF DEFINED(EXCLUDE-RensReturn) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RensReturn Procedure 
PROCEDURE RensReturn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RETURN "".
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
  RUN GetKatalogListe (OUTPUT cKataloger).
  
  /* 2. Bygg en liste med alle filnavn + ekstent som skal kontrolleres. */
  PUBLISH "loggEditorBatchServerInn" ("VPIScann Filnavn").
  RUN GetFilNavnListe (OUTPUT cFilNavnListe).
  
  /* 3. Opprett en post i fillisten for alle filer som ikke finnes der. */
  PUBLISH "loggEditorBatchServerInn" ("VPIScann OpprettPoster").
  RUN OpprettPoster (INPUT cKataloger, INPUT cFilNavnListe). 

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
  DEF INPUT PARAMETER pcTekst AS   CHAR        NO-UNDO.

  DEF VAR plFilId     AS DEC NO-UNDO.
  DEF VAR piLinjeNr   AS INT NO-UNDO.
  DEF VAR piGradering AS INT NO-UNDO.

  DEF BUFFER bVPIFilLogg FOR VPIFilLogg.

  ASSIGN
      plFilId     = 0 /*dec(DYNAMIC-FUNCTION('getForeignValues':U))*/
      piGradering = 0
      .
  IF plFilId = 0 OR plFilId = ? THEN
      PUBLISH 'GetFilIdLogg' (OUTPUT plFilId).

  IF NUM-ENTRIES(pcTekst,CHR(1)) >= 2 THEN
    ASSIGN
      piGradering = INT(ENTRY(2,pcTekst,CHR(1)))
      NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
      piGradering = 0.

  DO FOR bVPIFilLogg TRANSACTION:
    FIND LAST bVPIFilLogg NO-LOCK WHERE
        bVPIFilLogg.FilId = plFilId NO-ERROR.
    IF AVAILABLE bVPIFilLogg THEN
        piLinjeNr = bVPIFilLogg.LinjeNr + 1.
    ELSE
        piLinjeNr = 1.
    CREATE bVPIFilLogg.
    ASSIGN
        bVPIFilLogg.FilId     = plFilId
        bVPIFilLogg.LinjeNr   = piLinjeNr
        bVPIFilLogg.Tekst     = STRING(TODAY) + " " + STRING(TIME,"HH:MM:SS") + " " + 
                               entry(1,pcTekst,CHR(1))
        bVPIFilLogg.Gradering = piGradering
        .
    RELEASE bVPIFilLogg.
  END. /* TRANSACTION */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

