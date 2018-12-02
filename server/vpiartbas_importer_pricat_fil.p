/* Import av pricat fil.
   Parameter:  
   Opprettet: 18/7-08             
*/

DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR iEkstVPILevNr AS INT  NO-UNDO.
DEF VAR cFileName     AS CHAR NO-UNDO.
DEF VAR iInnlest      AS INT  NO-UNDO.
DEF VAR cPrgInnles    AS CHAR NO-UNDO.
DEF VAR cPrgUtpakk    AS CHAR NO-UNDO.
DEF VAR lFilId        AS DEC  NO-UNDO.
DEF VAR iAntLinjer    AS INT  NO-UNDO.
DEF VAR cVPIKatalog   AS CHAR NO-UNDO.

/* for TEST */
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEFINE VARIABLE bTest AS LOG       NO-UNDO.

DEF BUFFER bVPIFilHode FOR VPIFilHode.

ASSIGN 
    bTest         = TRUE 
    cLogg         = 'vpiartbas_importer_pricat_fil' + REPLACE(STRING(TODAY),'/','')
    iEkstVPILevNr = INTEGER(ENTRY(1,icParam,";"))
    cFileName     = ENTRY(2,icParam,";")
    .

/* Henter navn på oppdateringsrutine */
IF cFileName <> '' THEN 
FOR EACH EkstVPIFil NO-LOCK WHERE
    EkstVPIFil.EkstVPILevNr = iEkstVPILevNr:
    IF cFileName MATCHES "*" + EkstVPIFil.VPIFilNavn + "*" THEN
        ASSIGN
        cVPIKatalog = EkstVPIFil.VPIKatalog
        cPrgInnles  = EkstVPIFil.VPIInnlesningsrutine + (IF NUM-ENTRIES(EkstVPIFil.VPIInnlesningsrutine,".") = 1
                                                         THEN ".p" 
                                                         ELSE "")
        cPrgUtpakk  = EkstVPIFil.VPIUtpakkingsrutine + (IF NUM-ENTRIES(EkstVPIFil.VPIUtpakkingsrutine,".") = 1
                                                         THEN ".p" 
                                                         ELSE "")
        .
END.
ELSE ASSIGN 
         cFileName   = ENTRY(3,icParam,";")
         cVPIKatalog = ENTRY(4,icParam,";")
         cPrgInnles  = ENTRY(5,icParam,";")
         cPrgUtpakk  = ENTRY(6,icParam,";")
         .

IF bTest THEN DO:
    RUN bibl_loggDbFri.p (cLogg, 'Start vpiartbas_importer_pricat_fil.p').
    RUN bibl_loggDbFri.p (cLogg, '  iEkstVPILevNr' + STRING(iEkstVPILevNr)).
    RUN bibl_loggDbFri.p (cLogg, '  cFileName    ' + cFileName).
    RUN bibl_loggDbFri.p (cLogg, '  cVPIKatalog  ' + cVPIKatalog).
    RUN bibl_loggDbFri.p (cLogg, '  cPrgInnles   ' + cPrgInnles).
    RUN bibl_loggDbFri.p (cLogg, '  cPrgUtpakk   ' + cPrgUtpakk).
END.

/* oppretter VPIFilHode og starter programmene */
IF cPrgInnles <> "" THEN
KJOR_FILER:
DO:
    IF bTest THEN 
        RUN bibl_loggDbFri.p (cLogg, '  Oppretter VPIFilhode').
    /* Oppretter VPIFilHode */
    DO TRANSACTION:
        FILE-INFO:FILE-NAME = cFileName.
        FIND LAST bVPIFilHode NO-LOCK NO-ERROR.
        CREATE VPIFilHode.
        ASSIGN
            VPIFilHode.FilId        = IF AVAILABLE bVPIFilHode
                                        THEN bVPIFilHode.FilId + 1
                                        ELSE 1
            VPIFilHode.FilNavn      = ENTRY(NUM-ENTRIES(cFileName,"\")
                                            ,cFileName,"\")
            VPIFilHode.Katalog      = RIGHT-TRIM(REPLACE(cFileName,VPIFilHode.FilNavn,""),"\")
            VPIFilHode.Dato         = FILE-INFO:FILE-CREATE-DATE
            VPIFilHode.Kl           = STRING(FILE-INFO:FILE-CREATE-TIME,"HH:MM:SS")
            VPIFilHode.Storrelse    = FILE-INFO:FILE-SIZE
            VPIFilHode.VPIFilType   = 1
            VPIFilHode.VPIFilStatus = 7
            VPIFilHode.EkstVPILevNr = iEkstVPILevNr
            lFilId                  = VPIFilHode.FilId
            NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
        DO:
            DELETE VPIFilHode.
            ocReturn = 'Feil ved lesing av filinformasjon.'.
            obOk = FALSE.
            IF bTest THEN 
                RUN bibl_loggDbFri.p (cLogg, '  Feil ved lesing av filinformasjon. forlater KJOR_FILER.').
            LEAVE KJOR_FILER.
        END.        
        IF AVAILABLE VPIFilHode THEN
            RELEASE VPIFilHode.
    END. /* TRANSACTION */

    IF bTest THEN 
        RUN bibl_loggDbFri.p (cLogg, '  Kjører program ' + cPrgInnles + ' med parameter lFilId = ' + STRING(lFilId) + '.').
    /* Kjører innlesning og utpakking av fil (Setter status til 8)*/
    RUN value(cPrgInnles) (lfilId, ?, OUTPUT iAntLinjer).
    IF bTest THEN 
        RUN bibl_loggDbFri.p (cLogg, '  Ferdig med program ' + cPrgInnles + ' antall linjer lest ' + STRING(iantLinjer) + '.').

    FIND VPIFilHode NO-LOCK WHERE
        VPIFilHode.FilId = lFilId NO-ERROR.

    IF AVAILABLE VPIFilHode THEN
    DO:
        /* Er filen ferdig innlest, pakkes den ut(Setter status til 5 - ok eller 9 - feil. */
        IF VPIFilHode.VPIFilStatus = 8 THEN
        DO:
            IF bTest THEN 
                RUN bibl_loggDbFri.p (cLogg, '  Kjører program ' + cPrgUtpakk + '.').
            
            RUN value(cPrgUtpakk) (lFilId).
            IF RETURN-VALUE = "OK" 
                THEN obOk = TRUE.
                ELSE obOk = FALSE.
            IF bTest THEN 
                RUN bibl_loggDbFri.p (cLogg, '  Ferdig program ' + cPrgUtpakk + ' RETURN-VALUE = ' + RETURN-VALUE + '.').
        END.
    END.
END. /* KJOR_FILER */

IF obOk THEN
DO TRANSACTION:
  FIND VPIDataSett EXCLUSIVE-LOCK WHERE
      VPIDataSett.EkstVPILevNr = iEkstVPILevNr NO-ERROR.
  IF AVAILABLE VPIDataSett THEN
  DO:
      ASSIGN
          VPIDataSett.Beskrivelse = "OK: Innlest fil " + cFileName
          VPIDataSett.ImportDato  = TODAY
          VPIDataSett.ImportKl    = TIME
          .
      RELEASE VPIDataSett.
      IF bTest THEN 
          RUN bibl_loggDbFri.p (cLogg, '  OK: Innlest fil.').
  END.

END. /* TRANSACTION */
ELSE DO TRANSACTION:
    FIND VPIDataSett EXCLUSIVE-LOCK WHERE
        VPIDataSett.EkstVPILevNr = iEkstVPILevNr NO-ERROR.
    IF AVAILABLE VPIDataSett THEN
    DO:
        ASSIGN
            VPIDataSett.Beskrivelse = "Feil: Innlest fil " + cFileName
            VPIDataSett.ImportDato  = TODAY
            VPIDataSett.ImportKl    = TIME
            .
        RELEASE VPIDataSett.
        IF bTest THEN 
            RUN bibl_loggDbFri.p (cLogg, '  Feil: Innlest fil.').
    END.

END. /* TRANSACTION */

/* Flytter filen til backup katalogen. */
IF obOk AND AVAILABLE VPIFilHode THEN
DO:
    /* Sikrer at backup katalog finnes. */
    OS-CREATE-DIR value(VPIFilHode.Katalog + "~\bku").
    /* Flytter filen til backup katalog. */
    OS-COPY value(VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn) value(VPIFilHode.Katalog + "~\bku~\" + VPIFilHode.FilNavn +
                  '_' + replace(STRING(TODAY),'/','-') +
                   '_' + replace(STRING(TIME,"HH:MM:SS"),':','')).

    /* Renser bort fil */
    IF SEARCH(VPIFilHode.Katalog + "~\bku~\" + VPIFilHode.FilNavn +
                  '_' + replace(STRING(TODAY),'/','-') +
                   '_' + replace(STRING(TIME,"HH:MM:SS"),':','')) <> ? THEN
    DO:
        /* Filen tas bort fra katalogen. */
        IF SEARCH(VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn) <> ? THEN
            OS-DELETE VALUE(VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn).
    END.
    ELSE /* */.
END.
/* HVis filen feiles ved innlesning, skal den slettes fra fillisten igjen. */
/* Da kan man lese inn samme filen uten å endre den.                       */
ELSE DO TRANSACTION:
    FOR EACH VPIFilLinje OF VPIFilHode EXCLUSIVE-LOCK:
        DELETE VPIFilLinje.
    END.
    FIND CURRENT VPIFilHode NO-ERROR.
    IF AVAILABLE VPIFilHode THEN DELETE VPIFilHode.
END. /* TRANSACTION */

IF ocReturn = "" THEN
    ocReturn = IF iInnlest > 0
                 THEN 'Det ble opprettet ' + STRING(iInnlest) + ' poster'
                 ELSE 'Ingen poster ble innlest'.
ASSIGN
    obOk     = TRUE
    .

IF bTest THEN DO:
    RUN bibl_loggDbFri.p (cLogg, '  ocReturn: ' + ocReturn).
    RUN bibl_loggDbFri.p (cLogg, 'Ferdig vpiartbas_importer_pricat_fil.p').
END.
