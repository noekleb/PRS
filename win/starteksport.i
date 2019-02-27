
  DEF VAR iCount          AS INTE  NO-UNDO.
  DEF VAR cHgrFiler       AS CHAR  NO-UNDO.
  DEF VAR cKKampFiler     AS CHAR  NO-UNDO.
  DEF VAR cKundeFiler     AS CHAR  NO-UNDO.
  DEF VAR cKasValutaFiler AS CHAR  NO-UNDO.
  DEF VAR cVareFiler      AS CHAR  NO-UNDO.
  DEF VAR cPRSVareFiler   AS CHAR  NO-UNDO.
  DEF VAR cMixFiler       AS CHAR  NO-UNDO.
  DEF VAR cPRSMixFiler    AS CHAR  NO-UNDO. 
  DEF VAR cKasserereFiler AS CHAR  NO-UNDO.
  DEF VAR cSelgerFiler    AS CHAR  NO-UNDO.
  DEF VAR cFargFiler      AS CHAR  NO-UNDO.
  DEF VAR cStrKonvFiler   AS CHAR  NO-UNDO.
  DEF VAR cStrTypeFiler   AS CHAR  NO-UNDO.
  DEF VAR cTeksterFiler   AS CHAR  NO-UNDO.
  DEF VAR cGarantiFiler   AS CHAR  NO-UNDO.
  DEF VAR cButikerFiler   AS CHAR  NO-UNDO.
  DEF VAR lOverfort       AS LOGI  NO-UNDO.
  DEF VAR cLoadedJpg      AS CHAR  NO-UNDO.
  DEF VAR cLoadedIcon     AS CHAR  NO-UNDO.
  DEF VAR cToolTip        AS CHAR  NO-UNDO.
  DEF VAR lSendError      AS LOGI  NO-UNDO.
  DEF VAR cSkjul          AS CHARACTER  NO-UNDO.
  DEF VAR cDummyFiler     AS CHAR  NO-UNDO.
  DEF VAR iDummyAnt1      AS INT   NO-UNDO.
  DEF VAR iDummyAnt2      AS INT   NO-UNDO.
  


  DEFINE VARIABLE cExportFil      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE piLoop          AS INTEGER   NO-UNDO.
  DEFINE VARIABLE cEksportKatalog AS CHARACTER NO-UNDO.
  DEFINE BUFFER   bfKasse FOR kasse. 
  DEFINE BUFFER   DistributeFile FOR DistributeFile. 
  DEFINE BUFFER   DistributeFileReceiver FOR DistributeFileReceiver. 
  DEFINE VARIABLE glPOSKasseExport AS LOGICAL INIT TRUE NO-UNDO. 
  DEFINE VARIABLE ToFile AS CHAR NO-UNDO. 
  DEFINE VARIABLE cBTekst AS CHARACTER NO-UNDO.
  
  /* Står det 0 i parameter, skal parameter settes blank. */
  {syspara.i 1 1 62 cBTekst}
  glPOSKasseExport = CAN-DO('1,J,Ja,Y,Yes,True',cBTekst).

  {syspara.i 1 1 56 cEksportKatalog}
  IF cEksportKatalog = '' THEN 
      cEksportKatalog = "c:\home\lindbak\sendes".
  
  ASSIGN lTimerOff  = NOT chPSTimer:ENABLED
         cExportFil = "_PRSPos" + 
                      STRING(YEAR(TODAY),'9999') + 
                      STRING(MONTH(TODAY),'99') +
                      STRING(DAY(TODAY),'99') +
                      REPLACE(STRING(TIME,"HH:MM:SS"),':','') + '.'.
  
  DO WITH FRAME FRAME-Para:
    ASSIGN cSkjul = IF TG-Skjul:CHECKED THEN "-q " ELSE "".
    DO WITH FRAME fMain:
        ASSIGN cLoadedJpg = IMAGE-1:IMAGE.
        IMAGE-1:LOAD-IMAGE(".\icon\yellowlight.jpg").
        IF lIconExist THEN DO:
             cLoadedIcon = {&WINDOW-NAME}:ICON.
            {&WINDOW-NAME}:LOAD-ICON(cYellowIcon) NO-ERROR.
        END.
    END.
  
    {sww.i}
    ASSIGN cDatoTid = STRING(TIME,"HH:MM").
    IF (lVare OR TG-VareAuto:CHECKED) AND (canfindElogg("ArtBas","POS") OR canfindElogg("ArtPris","POS") OR 
                                           canfindElogg("PakkeLinje","POS") OR canfindElogg("MixMatch","POS") OR canfindElogg("RedeleteArtBas","POS")) THEN 
    VARE: 
    DO:
      IF lVare = FALSE AND lTimerOff THEN LEAVE VARE.
/*       RUN eksportArtBas.p (INPUT cLanButiker,INPUT cFtpButiker,INPUT cPRSFtpButiker,OUTPUT cVareFiler,OUTPUT cMixFiler,OUTPUT FI-AntVarer,OUTPUT FI-AntKKamp). */
      RUN eksportPRSArtBas.p (INPUT cPRSFtpButiker,INPUT cExportFil,OUTPUT cPRSVareFiler,OUTPUT cDummyFiler,OUTPUT iDummyAnt1,OUTPUT iDummyAnt2).
      ASSIGN FI-VareFiler:SCREEN-VALUE IN FRAME FRAME-Filinfo = cVareFiler
             FI-MixFiler:SCREEN-VALUE = cMixFiler
             FI-VareTilExp:BGCOLOR = ?
             lVare                 = FALSE
             B-SendVare:SENSITIVE  = FALSE
             FI-AntVarer:SCREEN-VALUE  = STRING(FI-AntVarer).
             FI-AntKKamp:SCREEN-VALUE = STRING(FI-AntKKamp).
      ASSIGN cToolTip   = ""
             lSendError = FALSE.
      IF cVareFiler <> "" THEN DO:
        DO iCount = 1 TO NUM-ENTRIES(cVareFiler):
          IF NOT ENTRY(iCount,cVareFiler) MATCHES("*.txt") THEN NEXT.
          OS-COMMAND SILENT VALUE("c:\home\lindbak\util\kasse -s " + cSkjul + ENTRY(iCount,cVareFiler)).
          IF lSendError = FALSE AND SEARCH(ENTRY(iCount,cVareFiler)) <> ? THEN
            ASSIGN lSendError = TRUE.
          ASSIGN cToolTip = cToolTip + (IF cToolTip = "" THEN "" ELSE ",") + DYNAMIC-FUNCTION('getFiltxt':U,INPUT cULOGfil)
                 lOverfort = TRUE
                 FI-VareDatoTid:SCREEN-VALUE = cDatoTid.
        END.
        setError(FI-VareError:HANDLE,INPUT-OUTPUT lSendError,INPUT-OUTPUT cToolTip).
      END.
      IF cMixFiler <> "" THEN DO:
        DO iCount = 1 TO NUM-ENTRIES(cMixFiler):
          IF NOT ENTRY(iCount,cMixFiler) MATCHES("*.txt") THEN NEXT.
          OS-COMMAND SILENT VALUE("c:\home\lindbak\util\kasse -s " + cSkjul + ENTRY(iCount,cMixFiler)).
          IF lSendError = FALSE AND SEARCH(ENTRY(iCount,cMixFiler)) <> ? THEN
            ASSIGN lSendError = TRUE.
          ASSIGN cToolTip = cToolTip + (IF cToolTip = "" THEN "" ELSE ",") + DYNAMIC-FUNCTION('getFiltxt':U,INPUT cULOGfil)
                 lOverfort = TRUE
                 FI-KKampDatoTid:SCREEN-VALUE = cDatoTid.
        END.
        setError(FI-KKampError:HANDLE,INPUT-OUTPUT lSendError,INPUT-OUTPUT cToolTip).
      END.
    END. /* VARE*/
    
    IF (lVarGr OR TG-VarGrAuto:CHECKED) AND canfindElogg("VarGr","POS") THEN VARGR: DO:
      IF lVarGr = FALSE AND lTimerOff THEN
        LEAVE VARGR.
      RUN eksportVarGr.p (INPUT cLanButiker,INPUT cFtpButiker,INPUT cPRSFtpButiker,OUTPUT cHgrFiler,OUTPUT FI-AntVarGr).
      RUN eksportprsVarGr.p (INPUT cPRSFtpButiker,INPUT cExportFil,OUTPUT cDummyFiler,OUTPUT iDummyAnt1).
      ASSIGN FI-HgrFiler:SCREEN-VALUE = cHgrFiler
             FI-VarGrTilExp:BGCOLOR = ?
             lVarGr                 = FALSE
             B-SendVarGr:SENSITIVE  = FALSE
             FI-AntVarGr:SCREEN-VALUE = STRING(FI-AntVarGr).
      IF cHgrFiler <> "" THEN DO:
        DO iCount = 1 TO NUM-ENTRIES(cHgrFiler):
          IF NOT ENTRY(iCount,cHgrFiler) MATCHES("*.txt") THEN NEXT.
          OS-COMMAND SILENT VALUE("c:\home\lindbak\util\kasse -g " + cSkjul + ENTRY(iCount,cHgrFiler)).
          IF lSendError = FALSE AND SEARCH(ENTRY(iCount,cHgrFiler)) <> ? THEN
            ASSIGN lSendError = TRUE.
          ASSIGN cToolTip = cToolTip + (IF cToolTip = "" THEN "" ELSE ",") + DYNAMIC-FUNCTION('getFiltxt':U,INPUT cULOGfil)
                 lOverfort = TRUE
                 FI-VarGrDatoTid:SCREEN-VALUE = cDatoTid.
        END.
        setError(FI-VarGrError:HANDLE,INPUT-OUTPUT lSendError,INPUT-OUTPUT cToolTip).
      END.
    END.
    /* Kombinasjonskampanje */
    IF (lKKAmp OR TG-KKampAuto:CHECKED) AND canfindElogg("KampanjeMixMatch","POS") THEN 
    KKAMP: 
    DO:
      IF lKKamp = FALSE AND lTimerOff THEN
        LEAVE KKAMP.
      RUN eksportKampanjeMixMatch.p (INPUT cLanButiker,INPUT cFtpButiker,INPUT cPRSFtpButiker,OUTPUT cHgrFiler,OUTPUT FI-AntKKamp).
      RUN eksportprsKampanjeMixMatch.p (INPUT cPRSFtpButiker,INPUT cExportFil,OUTPUT cDummyFiler,OUTPUT iDummyAnt1).
      ASSIGN FI-KKampFiler:SCREEN-VALUE = cKKampFiler
             FI-KKampTilExp:BGCOLOR = ?
             lKKamp                 = FALSE
             B-SendKKamp:SENSITIVE  = FALSE
             FI-AntKKamp:SCREEN-VALUE = STRING(FI-AntKKamp).
      IF cKKampFiler <> "" THEN DO:
      DO iCount = 1 TO NUM-ENTRIES(cKKampFiler):
          IF NOT ENTRY(iCount,cKKampFiler) MATCHES("*.txt") THEN NEXT.
          OS-COMMAND SILENT VALUE("c:\home\lindbak\util\kasse -s " + cSkjul + ENTRY(iCount,cKKampFiler)).
          IF lSendError = FALSE AND SEARCH(ENTRY(iCount,cKKampFiler)) <> ? THEN
          ASSIGN lSendError = TRUE.
          ASSIGN cToolTip = cToolTip + (IF cToolTip = "" THEN "" ELSE ",") + DYNAMIC-FUNCTION('getFiltxt':U,INPUT cULOGfil)
                 lOverfort = TRUE
                 FI-KKampDatoTid:SCREEN-VALUE = cDatoTid.
      END.
      setError(FI-KKampError:HANDLE,INPUT-OUTPUT lSendError,INPUT-OUTPUT cToolTip).
    END. /* KKAMP */
  END.
    /* Valuta */
    IF (lKasValuta OR TG-KasValutaAuto:CHECKED) AND canfindElogg("KasValuta","POS") THEN KASVALUTA: DO:
      IF lKasValuta = FALSE AND lTimerOff THEN
        LEAVE KASVALUTA.
      RUN eksportValuta.p (INPUT cLanButiker,INPUT cFtpButiker,INPUT cPRSFtpButiker,OUTPUT cKasValutaFiler,OUTPUT FI-AntKasValuta). /* Här läggs alltid alla ut */
      RUN eksportprsValuta.p (INPUT cPRSFtpButiker,INPUT cExportFil,OUTPUT cDummyFiler,OUTPUT iDummyAnt1). /* Här läggs alltid alla ut */
      ASSIGN FI-KasValutaFiler:SCREEN-VALUE = cKasValutaFiler
             FI-KasValutaTilExp:BGCOLOR = ?
             lKasValuta                 = FALSE
             B-SendKasValuta:SENSITIVE  = FALSE
             FI-AntKasValuta:SCREEN-VALUE = STRING(FI-AntKasValuta).
      IF cKasValutaFiler <> "" THEN DO:
        DO iCount = 1 TO NUM-ENTRIES(cKasValutaFiler):
          IF NOT ENTRY(iCount,cKasValutaFiler) MATCHES("*.txt") THEN NEXT.
          OS-COMMAND SILENT VALUE("c:\home\lindbak\util\kasse -u " + cSkjul + ENTRY(iCount,cKasValutaFiler)).
          IF lSendError = FALSE AND SEARCH(ENTRY(iCount,cKasValutaFiler)) <> ? THEN
            ASSIGN lSendError = TRUE.
          ASSIGN cToolTip = cToolTip + (IF cToolTip = "" THEN "" ELSE ",") + DYNAMIC-FUNCTION('getFiltxt':U,INPUT cULOGfil)
                 lOverfort = TRUE
                 FI-KasValutaDatoTid:SCREEN-VALUE = cDatoTid.
        END.
        setError(FI-KasValutaError:HANDLE,INPUT-OUTPUT lSendError,INPUT-OUTPUT cToolTip).
      END.
    END.
    IF (lKasserere OR TG-KasserereAuto:CHECKED) AND (canfindElogg("ButikkForsalj","POS") OR 
                                                     canfindElogg("Forsalj","POS")) THEN KASSERER: DO:
      IF lKasserere = FALSE AND lTimerOff THEN
        LEAVE KASSERER.
      RUN eksportButikkForsalj.p (INPUT cLanButiker,INPUT cFtpButiker,INPUT cPRSFtpButiker,OUTPUT cKasserereFiler,OUTPUT FI-AntKasserer).
      RUN eksportprsButikkForsalj.p (INPUT cPRSFtpButiker,INPUT cExportFil,OUTPUT cDummyFiler,OUTPUT iDummyAnt1).
      ASSIGN FI-KassererFiler:SCREEN-VALUE = cKasserereFiler
             FI-KassererTilExp:BGCOLOR = ?
             lKasserere                = FALSE
             B-SendKasserere:SENSITIVE = FALSE
             FI-AntKasserer:SCREEN-VALUE = STRING(FI-AntKasserer).
      IF cKasserereFiler <> "" THEN DO:
        DO iCount = 1 TO NUM-ENTRIES(cKasserereFiler):
          IF NOT ENTRY(iCount,cKasserereFiler) MATCHES("*.txt") THEN NEXT.
          OS-COMMAND SILENT VALUE("c:\home\lindbak\util\kasse -o " + cSkjul + ENTRY(iCount,cKasserereFiler)).
          IF lSendError = FALSE AND SEARCH(ENTRY(iCount,cKasserereFiler)) <> ? THEN
            ASSIGN lSendError = TRUE.
          ASSIGN cToolTip = cToolTip + (IF cToolTip = "" THEN "" ELSE ",") + DYNAMIC-FUNCTION('getFiltxt':U,INPUT cULOGfil)
                 lOverfort = TRUE
                 FI-KassererDatoTid:SCREEN-VALUE = cDatoTid.
        END.
        setError(FI-KassererError:HANDLE,INPUT-OUTPUT lSendError,INPUT-OUTPUT cToolTip).
      END.
    END.
    IF (lSelgere OR TG-SelgereAuto:CHECKED) AND (canfindElogg("ButikkSelger","POS") OR 
                                                 canfindElogg("Selger","POS")) THEN SELGER: DO:
      IF lSelgere = FALSE AND lTimerOff THEN
        LEAVE SELGER.
/*       RUN eksportButikkSelger.p (INPUT cLanButiker,INPUT cFtpButiker,INPUT cPRSFtpButiker,OUTPUT cSelgerFiler,OUTPUT FI-AntSelger). */
      RUN eksportprsButikkSelger.p (INPUT cPRSFtpButiker,INPUT cExportFil,OUTPUT cDummyFiler,OUTPUT iDummyAnt1).
      ASSIGN FI-SelgerFiler:SCREEN-VALUE = cSelgerFiler
             FI-SelgerTilExp:BGCOLOR = ?
             lSelgere                = FALSE
             B-SendSelgere:SENSITIVE = FALSE
             FI-AntSelger:SCREEN-VALUE = STRING(FI-AntSelger).
      IF cSelgerFiler <> "" THEN DO:
        DO iCount = 1 TO NUM-ENTRIES(cSelgerFiler):
          IF NOT ENTRY(iCount,cSelgerFiler) MATCHES("*.txt") THEN NEXT.
          OS-COMMAND SILENT VALUE("c:\home\lindbak\util\kasse -c " + cSkjul + ENTRY(iCount,cSelgerFiler)).
          IF lSendError = FALSE AND SEARCH(ENTRY(iCount,cSelgerFiler)) <> ? THEN
            ASSIGN lSendError = TRUE.
          ASSIGN cToolTip = cToolTip + (IF cToolTip = "" THEN "" ELSE ",") + DYNAMIC-FUNCTION('getFiltxt':U,INPUT cULOGfil)
                 lOverfort = TRUE
                 FI-SelgerDatoTid:SCREEN-VALUE = cDatoTid.
        END.
        setError(FI-SelgerError:HANDLE,INPUT-OUTPUT lSendError,INPUT-OUTPUT cToolTip).
      END.
    END.

    IF (lKunde OR TG-KundeAuto:CHECKED) AND (canfindElogg("Kunde","POS") OR canfindElogg("KundeKort","POS")) THEN KUNDE: DO:
      IF lKunde = FALSE AND lTimerOff THEN
        LEAVE KUNDE.
      RUN eksportKunde.p (INPUT cLanButiker,INPUT cFtpButiker,INPUT cPRSFtpButiker,OUTPUT cKundeFiler,OUTPUT FI-AntKunder).
      RUN eksportprsKunde.p (INPUT cPRSFtpButiker,INPUT cExportFil,OUTPUT cDummyFiler,OUTPUT iDummyAnt2).
      ASSIGN FI-KundeFiler:SCREEN-VALUE = cKundeFiler
             FI-KundeTilExp:BGCOLOR  = ?
             lKunde                = FALSE
             B-SendKunde:SENSITIVE = FALSE
             FI-AntKunder:SCREEN-VALUE = STRING(FI-AntKunder).
      IF cKundeFiler <> "" THEN DO:
        DO iCount = 1 TO NUM-ENTRIES(cKundeFiler):
          IF NOT ENTRY(iCount,cKundeFiler) MATCHES("*.txt") THEN NEXT.
          OS-COMMAND SILENT VALUE("c:\home\lindbak\util\kasse -k " + cSkjul + ENTRY(iCount,cKundeFiler)).
          IF lSendError = FALSE AND SEARCH(ENTRY(iCount,cKundeFiler)) <> ? THEN
            ASSIGN lSendError = TRUE.
          ASSIGN cToolTip = cToolTip + (IF cToolTip = "" THEN "" ELSE ",") + DYNAMIC-FUNCTION('getFiltxt':U,INPUT cULOGfil)
                 lOverfort = TRUE
                 FI-KundeDatoTid:SCREEN-VALUE = cDatoTid.
        END.
        setError(FI-KundeError:HANDLE,INPUT-OUTPUT lSendError,INPUT-OUTPUT cToolTip).
      END.
    END.

    /* NB: Det må gjøres plass i GUI for medlemsdata. */
    IF /*(lKunde OR TG-KundeAuto:CHECKED) AND */ (canfindElogg("Medlem","POS") OR canfindElogg("MedlemsKort","POS")) THEN MEDLEM: DO:
      /*
      IF lKunde = FALSE AND lTimerOff THEN
        LEAVE MEDLEM.
      */
      RUN eksportprsmedlem.p (INPUT cPRSFtpButiker,INPUT cExportFil,OUTPUT cDummyFiler,OUTPUT iDummyAnt1).
      /*
      ASSIGN FI-KundeFiler:SCREEN-VALUE = cKundeFiler
             FI-KundeTilExp:BGCOLOR  = ?
             lKunde                = FALSE
             B-SendKunde:SENSITIVE = FALSE
             FI-AntKunder:SCREEN-VALUE = STRING(FI-AntKunder).
      IF cKundeFiler <> "" THEN DO:
        DO iCount = 1 TO NUM-ENTRIES(cKundeFiler):
          IF NOT ENTRY(iCount,cKundeFiler) MATCHES("*.txt") THEN NEXT.
          OS-COMMAND SILENT VALUE("c:\home\lindbak\util\kasse -k " + cSkjul + ENTRY(iCount,cKundeFiler)).
          IF lSendError = FALSE AND SEARCH(ENTRY(iCount,cKundeFiler)) <> ? THEN
            ASSIGN lSendError = TRUE.
          ASSIGN cToolTip = cToolTip + (IF cToolTip = "" THEN "" ELSE ",") + DYNAMIC-FUNCTION('getFiltxt':U,INPUT cULOGfil)
                 lOverfort = TRUE
                 FI-KundeDatoTid:SCREEN-VALUE = cDatoTid.
        END.
        setError(FI-KundeError:HANDLE,INPUT-OUTPUT lSendError,INPUT-OUTPUT cToolTip).
      END.
      */
    END.

    /* NB: Det må gjøres plass i GUI for produktfamilie. */
    IF /*(lKunde OR TG-KundeAuto:CHECKED) AND */ (canfindElogg("ProduktFamilie","POS")) THEN PRODUKFAMILIE: DO:
      /*
      IF lKunde = FALSE AND lTimerOff THEN
        LEAVE MEDLEM.
      */
      RUN eksportprsproduktfamilie.p (INPUT cPRSFtpButiker,INPUT cExportFil,OUTPUT cDummyFiler,OUTPUT iDummyAnt1).
      /*
      ASSIGN FI-KundeFiler:SCREEN-VALUE = cKundeFiler
             FI-KundeTilExp:BGCOLOR  = ?
             lKunde                = FALSE
             B-SendKunde:SENSITIVE = FALSE
             FI-AntKunder:SCREEN-VALUE = STRING(FI-AntKunder).
      IF cKundeFiler <> "" THEN DO:
        DO iCount = 1 TO NUM-ENTRIES(cKundeFiler):
          IF NOT ENTRY(iCount,cKundeFiler) MATCHES("*.txt") THEN NEXT.
          OS-COMMAND SILENT VALUE("c:\home\lindbak\util\kasse -k " + cSkjul + ENTRY(iCount,cKundeFiler)).
          IF lSendError = FALSE AND SEARCH(ENTRY(iCount,cKundeFiler)) <> ? THEN
            ASSIGN lSendError = TRUE.
          ASSIGN cToolTip = cToolTip + (IF cToolTip = "" THEN "" ELSE ",") + DYNAMIC-FUNCTION('getFiltxt':U,INPUT cULOGfil)
                 lOverfort = TRUE
                 FI-KundeDatoTid:SCREEN-VALUE = cDatoTid.
        END.
        setError(FI-KundeError:HANDLE,INPUT-OUTPUT lSendError,INPUT-OUTPUT cToolTip).
      END.
      */
    END.

    IF (lFarger OR TG-FargerAuto:CHECKED) AND canfindElogg("Farg","POS") THEN FARGER: DO:
      IF lFarger = FALSE AND lTimerOff THEN
        LEAVE FARGER.
      RUN eksportFarg.p (INPUT cLanButiker,INPUT cFtpButiker,INPUT cPRSFtpButiker,OUTPUT cFargFiler,OUTPUT FI-AntFarger).
      RUN eksportprsFarg.p (INPUT cPRSFtpButiker,INPUT cExportFil,OUTPUT cDummyFiler,OUTPUT iDummyAnt1).
      ASSIGN FI-FargFiler:SCREEN-VALUE = cFargFiler
             FI-FargerTilExp:BGCOLOR = ?
             lFarger                 = FALSE
             B-SendFarger:SENSITIVE  = FALSE
             FI-AntFarger:SCREEN-VALUE = STRING(FI-AntFarger).
      IF cFargFiler <> "" THEN DO:
        DO iCount = 1 TO NUM-ENTRIES(cFargFiler):
          IF NOT ENTRY(iCount,cFargFiler) MATCHES("*.txt") THEN NEXT.
          OS-COMMAND SILENT VALUE("c:\home\lindbak\util\kasse -f " + cSkjul + ENTRY(iCount,cFargFiler)).
          IF lSendError = FALSE AND SEARCH(ENTRY(iCount,cFargFiler)) <> ? THEN
            ASSIGN lSendError = TRUE.
          ASSIGN cToolTip = cToolTip + (IF cToolTip = "" THEN "" ELSE ",") + DYNAMIC-FUNCTION('getFiltxt':U,INPUT cULOGfil)
                 lOverfort = TRUE
                 FI-FargerDatoTid:SCREEN-VALUE = cDatoTid.
        END.
        setError(FI-FargerError:HANDLE,INPUT-OUTPUT lSendError,INPUT-OUTPUT cToolTip).
      END.
    END.
    /* NB: Det må på GUI siden gjøres plass til visning av moms */
    IF /*(lFarger OR TG-FargerAuto:CHECKED) AND*/ canfindElogg("Moms","POS") THEN MOMS: DO:
      /*
      IF lFarger = FALSE AND lTimerOff THEN
        LEAVE FARGER.
     */
      RUN eksportprsmoms.p (INPUT cPRSFtpButiker,INPUT cExportFil,OUTPUT cDummyFiler,OUTPUT iDummyAnt1).
      /*
      ASSIGN FI-FargFiler:SCREEN-VALUE = cFargFiler
             FI-FargerTilExp:BGCOLOR = ?
             lFarger                 = FALSE
             B-SendFarger:SENSITIVE  = FALSE
             FI-AntFarger:SCREEN-VALUE = STRING(FI-AntFarger).
      IF cFargFiler <> "" THEN DO:
        DO iCount = 1 TO NUM-ENTRIES(cFargFiler):
          IF NOT ENTRY(iCount,cFargFiler) MATCHES("*.txt") THEN NEXT.
          OS-COMMAND SILENT VALUE("c:\home\lindbak\util\kasse -f " + cSkjul + ENTRY(iCount,cFargFiler)).
          IF lSendError = FALSE AND SEARCH(ENTRY(iCount,cFargFiler)) <> ? THEN
            ASSIGN lSendError = TRUE.
          ASSIGN cToolTip = cToolTip + (IF cToolTip = "" THEN "" ELSE ",") + DYNAMIC-FUNCTION('getFiltxt':U,INPUT cULOGfil)
                 lOverfort = TRUE
                 FI-FargerDatoTid:SCREEN-VALUE = cDatoTid.
        END.
        setError(FI-FargerError:HANDLE,INPUT-OUTPUT lSendError,INPUT-OUTPUT cToolTip).
      END.
      */
    END.
    
    IF (lStrKonv OR TG-StrKonvAuto:CHECKED) AND canfindElogg("StrKonv","POS") THEN STRKONV: DO:
      IF lStrKonv = FALSE AND lTimerOff THEN
          LEAVE STRKONV.
      RUN eksportStrKonv.p (INPUT cLanButiker,INPUT cFtpButiker,OUTPUT cStrKonvFiler,OUTPUT FI-AntStrKonv).
      ASSIGN FI-StrKonvFiler:SCREEN-VALUE = cStrKonvFiler
             FI-StrKonvTilExp:BGCOLOR = ?
             lStrKonv                 = FALSE
             B-SendStrKonv:SENSITIVE  = FALSE
             FI-AntStrKonv:SCREEN-VALUE = STRING(FI-AntStrKonv).
      IF cStrKonvFiler <> "" THEN DO:
        DO iCount = 1 TO NUM-ENTRIES(cStrKonvFiler):
          IF NOT ENTRY(iCount,cStrKonvFiler) MATCHES("*.txt") THEN NEXT.
          OS-COMMAND SILENT VALUE("c:\home\lindbak\util\kasse -z " + cSkjul + ENTRY(iCount,cStrKonvFiler)).
          IF lSendError = FALSE AND SEARCH(ENTRY(iCount,cStrKonvFiler)) <> ? THEN
            ASSIGN lSendError = TRUE.
          ASSIGN cToolTip = cToolTip + (IF cToolTip = "" THEN "" ELSE ",") + DYNAMIC-FUNCTION('getFiltxt':U,INPUT cULOGfil)
                 lOverfort = TRUE
                 FI-StrKonvDatoTid:SCREEN-VALUE = cDatoTid.
        END.
        setError(FI-StrKonvError:HANDLE,INPUT-OUTPUT lSendError,INPUT-OUTPUT cToolTip).
      END.
    END.
    IF (lStrType OR TG-StrTypeAuto:CHECKED) AND canfindElogg("StrType","POS") THEN STRTYPE: DO:
      IF lStrType = FALSE AND lTimerOff THEN
        LEAVE STRTYPE.
      RUN eksportStrType.p (INPUT cLanButiker,INPUT cFtpButiker,OUTPUT cStrTypeFiler,OUTPUT FI-AntStrType).
      ASSIGN FI-StrTypeFiler:SCREEN-VALUE = cStrTypeFiler
             FI-StrTypeTilExp:BGCOLOR = ?
             lStrType                 = FALSE
             B-SendStrType:SENSITIVE  = FALSE
             FI-AntStrType:SCREEN-VALUE = STRING(FI-AntStrType).
      IF cStrTypeFiler <> "" THEN DO:
        DO iCount = 1 TO NUM-ENTRIES(cStrTypeFiler):
          IF NOT ENTRY(iCount,cStrTypeFiler) MATCHES("*.txt") THEN NEXT.
          OS-COMMAND SILENT VALUE("c:\home\lindbak\util\kasse -t " + cSkjul + ENTRY(iCount,cStrTypeFiler)).
          PAUSE 1 NO-MESSAGE.
          IF lSendError = FALSE AND SEARCH(ENTRY(iCount,cStrTypeFiler)) <> ? THEN
              ASSIGN lSendError = TRUE.
          ASSIGN cToolTip = cToolTip + (IF cToolTip = "" THEN "" ELSE ",") + DYNAMIC-FUNCTION('getFiltxt':U,INPUT cULOGfil)
                 lOverfort = TRUE
                 FI-StrTypeDatoTid:SCREEN-VALUE = cDatoTid.
        END.
        setError(FI-StrTypeError:HANDLE,INPUT-OUTPUT lSendError,INPUT-OUTPUT cToolTip).
      END.
    END.

    IF (lTekster OR TG-TeksterAuto:CHECKED) AND (canfindElogg("FeilKode","POS") OR 
                                                 canfindElogg("KravKode","POS") OR
                                                 canfindElogg("GaveKType","POS") OR
                                                 canfindElogg("UtbetType","POS") OR
                                                 canfindElogg("InnBetType","POS")) THEN TEKSTER: DO:
      IF lTekster = FALSE AND lTimerOff THEN
        LEAVE TEKSTER.
      RUN eksportTekster.p (INPUT cLanButiker,INPUT cFtpButiker,INPUT cPRSFtpButiker,OUTPUT cTeksterFiler,OUTPUT FI-AntTekster).
      RUN eksportprsTekster.p (INPUT cPRSFtpButiker,INPUT cExportFil,OUTPUT cDummyFiler,OUTPUT iDummyAnt1).
      ASSIGN FI-TeksterFiler:SCREEN-VALUE = cTeksterFiler
             FI-TeksterTilExp:BGCOLOR = ?
             lTekster                 = FALSE
             B-SendTekster:SENSITIVE  = FALSE
             FI-AntTekster:SCREEN-VALUE = STRING(FI-AntTekster).
      IF cTeksterFiler <> "" THEN DO iCount = 1 TO NUM-ENTRIES(cTeksterFiler):
        IF NOT ENTRY(iCount,cTeksterFiler) MATCHES("*.txt") THEN NEXT.
        OS-COMMAND SILENT VALUE("c:\home\lindbak\util\kasse -a " + cSkjul + ENTRY(iCount,cTeksterFiler)).
        IF lSendError = FALSE AND SEARCH(ENTRY(iCount,cTeksterFiler)) <> ? THEN
            ASSIGN lSendError = TRUE.
        ASSIGN cToolTip = cToolTip + (IF cToolTip = "" THEN "" ELSE ",") + DYNAMIC-FUNCTION('getFiltxt':U,INPUT cULOGfil)
               lOverfort = TRUE.
      END.
      setError(FI-TeksterError:HANDLE,INPUT-OUTPUT lSendError,INPUT-OUTPUT cToolTip).
      ASSIGN FI-TeksterDatoTid:SCREEN-VALUE = cDatoTid.
    END.

    IF (lGaranti OR TG-GarantiAuto:CHECKED) AND canfindElogg("Garanti","POS") THEN GARANTI: DO:
      IF lGaranti = FALSE AND lTimerOff THEN
          LEAVE GARANTI.
      RUN eksportGaranti.p (INPUT cLanButiker,INPUT cFtpButiker,INPUT cPRSFtpButiker,OUTPUT cGarantiFiler,OUTPUT FI-AntGaranti).
      RUN eksportprsGaranti.p (INPUT cPRSFtpButiker,INPUT cExportFil,OUTPUT cDummyFiler,OUTPUT iDummyAnt1).
      ASSIGN FI-GarantiFiler:SCREEN-VALUE = cGarantiFiler
             FI-GarantiTilExp:BGCOLOR = ?
             lGaranti                 = FALSE
             B-SendGaranti:SENSITIVE  = FALSE
             FI-AntGaranti:SCREEN-VALUE = STRING(FI-AntGaranti).
      IF cGarantiFiler <> "" THEN DO iCount = 1 TO NUM-ENTRIES(cGarantiFiler):
        IF NOT ENTRY(iCount,cGarantiFiler) MATCHES("*.txt") THEN NEXT.
        OS-COMMAND SILENT VALUE("c:\home\lindbak\util\kasse -m " + cSkjul + ENTRY(iCount,cGarantiFiler)).
        IF lSendError = FALSE AND SEARCH(ENTRY(iCount,cGarantiFiler)) <> ? THEN
            ASSIGN lSendError = TRUE.
        ASSIGN cToolTip = cToolTip + (IF cToolTip = "" THEN "" ELSE ",") + DYNAMIC-FUNCTION('getFiltxt':U,INPUT cULOGfil)
               lOverfort = TRUE
               FI-GarantiDatoTid:SCREEN-VALUE = cDatoTid.
      END.
    END.
    IF (lButiker OR TG-ButikerAuto:CHECKED) AND (canfindElogg("Butiker","POS") OR 
                                                 canfindElogg("ekstbutiker","POS")) THEN BUTIKER: DO:
      IF lButiker = FALSE AND lTimerOff THEN
        LEAVE BUTIKER.
      RUN eksportButiker.p (INPUT cLanButiker,INPUT cFtpButiker,INPUT cPRSFtpButiker,OUTPUT cButikerFiler,OUTPUT FI-AntButiker).
      RUN eksportprsButiker.p (INPUT cPRSFtpButiker,INPUT cExportFil,OUTPUT cDummyFiler,OUTPUT iDummyAnt1).
      ASSIGN FI-ButikerFiler:SCREEN-VALUE = cButikerFiler
             FI-ButikerTilExp:BGCOLOR = ?
             lButiker                 = FALSE
             B-SendButiker:SENSITIVE  = FALSE
             FI-AntButiker:SCREEN-VALUE = STRING(FI-AntButiker).
      IF cButikerFiler <> "" THEN DO iCount = 1 TO NUM-ENTRIES(cButikerFiler):
        IF NOT ENTRY(iCount,cButikerFiler) MATCHES("*.txt") THEN NEXT.
        OS-COMMAND SILENT VALUE("c:\home\lindbak\util\kasse -e " + cSkjul + ENTRY(iCount,cButikerFiler)).
        IF lSendError = FALSE AND SEARCH(ENTRY(iCount,cButikerFiler)) <> ? THEN
            ASSIGN lSendError = TRUE.
        ASSIGN cToolTip = cToolTip + (IF cToolTip = "" THEN "" ELSE ",") + DYNAMIC-FUNCTION('getFiltxt':U,INPUT cULOGfil)
               lOverfort = TRUE.
      END.
      setError(FI-ButikerError:HANDLE,INPUT-OUTPUT lSendError,INPUT-OUTPUT cToolTip).
      ASSIGN FI-ButikerDatoTid:SCREEN-VALUE = cDatoTid.
    END.

    DEFINE VARIABLE iFileBnr AS INT NO-UNDO.

    /* 'Rename' av temporære filer */
    IF TRIM(cPRSFtpButiker) <> '' THEN 
    DO piLoop = 1 TO NUM-ENTRIES(cPRSFtpButiker):
      IF SEARCH(cEksportKatalog + '\' + cExportFil + TRIM(ENTRY(piLoop,cPRSFtpButiker),'.')) <> ? THEN
 
        DO:

        ToFile = cEksportKatalog + '\' + LEFT-TRIM(cExportFil,'_') + TRIM(ENTRY(piLoop,cPRSFtpButiker),'.').

        /* Gir filen dens riktige navn og tar bort den temporære filen. */
        OS-RENAME VALUE(cEksportKatalog + '\' + cExportFil + TRIM(ENTRY(piLoop,cPRSFtpButiker),'.')) 
                  VALUE(ToFile).

        IF glPOSKasseExport THEN 
        DO: 
        kasse:
        FOR EACH bfkasse NO-LOCK  
            WHERE bfKasse.Butikknr = INTEGER(TRIM(ENTRY(piLoop,cPRSFtpButiker),'.')) AND 
                  bfKasse.Aktiv AND 
                  bfKasse.Modellnr = 5 AND 
                  bfKasse.Kassenr NE 99 TRANSACTION: /* pos kasse*/ 

             IF TRIM(ToFile) = "" THEN NEXT kasse.
             /* Dirty override */
             FIND butiker WHERE butiker.butik = bfkasse.butikknr NO-LOCK NO-ERROR.
             IF AVAIL butiker AND butiker.FalckMedlNr = "2222" THEN
                 NEXT.
             ASSIGN 
               iFileBnr = INT(SUBSTRING(ToFile,R-INDEX(ToFile,".") + 1)) NO-ERROR.
             IF ERROR-STATUS:ERROR THEN NEXT kasse.     
         
             IF bfKasse.Butikknr NE iFileBnr THEN NEXT kasse. 

             FIND FIRST DistributeFile WHERE 
                        DistributeFile.Butikk = bfKasse.Butikknr AND
                        DistributeFile.FILENAME = ToFile NO-LOCK NO-ERROR.

             IF NOT AVAIL DistributeFile THEN
             DO:
                 CREATE DistributeFile. 
                 ASSIGN 
                 DistributeFile.id     = GUID(GENERATE-UUID)
                 DistributeFile.DATE   = NOW
                 DistributeFile.Butikk = bfKasse.Butikknr
                 DistributeFile.FILENAME = ToFile. 
                 COPY-LOB FILE ToFile TO DistributeFile.FileObject NO-CONVERT. 
             END. 

             FIND FIRST DistributeFileReceiver WHERE 
                        DistributeFileReceiver.DistributeFileId =  DistributeFile.id AND
                        DistributeFileReceiver.kassenr = bfKasse.kassenr NO-LOCK NO-ERROR. 

             IF NOT AVAIL DistributeFileReceiver THEN
             DO:
                 CREATE DistributeFileReceiver. 
                 ASSIGN 
                 DistributeFileReceiver.id     = GUID(GENERATE-UUID)
                 DistributeFileReceiver.DistributeFileid =  DistributeFile.id 
                 DistributeFileReceiver.KasseNr = bfKasse.KasseNr.
             END. 
        END.
        RELEASE DistributeFileReceiver NO-ERROR. 
        RELEASE DistributeFile NO-ERROR. 
    END. 
    END.
    END. 
    
    RUN FlyttFiler.
    IF lOverfort THEN
      RUN SetDatoTidColor (cDatoTid).
    RUN FinnesIkkeOverforte.
    {swn.i}
    IMAGE-1:LOAD-IMAGE(cLoadedJpg).
    IF lIconExist THEN
        {&WINDOW-NAME}:LOAD-ICON(cLoadedIcon) NO-ERROR.
  END.

{syspara.i 200 10 1 cTekst}
IF CAN-DO('1,j,y,true,ja',cTekst) THEN
    lHost = TRUE.
ELSE
    lHost = FALSE.
{syspara.i 200 10 2 ipcHost}
{syspara.i 200 10 3 ipcPort}
  
/* Starter ftp overføring for PRS Pos kassene. */
IF TG-SendPRSPos:CHECKED IN FRAME FRAME-Para THEN DO:
  IF TRIM(ipcHost + ipcPort) <> '' THEN 
    RUN runPRSPosFileDistrib.p (ipcHost, ipcPort).
END.
