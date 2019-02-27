/************************************************************
    Program:  bt-oppdlagertrans.p
    Created:  TN   11 Jan 1999
Description:  Oppdatering av lagertransaksjoner.

   TN   13/3-99  Lagt inn håndtering av Batch nummer.
   TN   29/3-99  Nullstiller wBatchListe for den bygges pr. dag.
   TN   22/4-99  Leser ferdige batcher med status = 2.
   TN   22/4-99  - Debug niv† innfort.
                 - Forenklet logikken i loopen.


Last change:  TN   22 Dec 99   12:32 pm
************************************************************/

DEF INPUT PARAMETER wParentHandle AS HANDLE NO-UNDO.


DEF VAR wPause        AS INT  NO-UNDO.
DEF VAR wStopp        AS CHAR NO-UNDO.
DEF VAR wBatchNr      AS INT  NO-UNDO.
DEF VAR wBatchListe   AS CHAR NO-UNDO.
DEF VAR wLoop         AS INT  NO-UNDO.
DEF VAR wStart        AS LOG  NO-UNDO.
DEF VAR wOldDato      AS DATE NO-UNDO.
DEFINE VARIABLE wEksportDato AS DATE NO-UNDO.
DEFINE VARIABLE wLoop2        AS INT  NO-UNDO.
DEF VAR wMsgStopp     AS LOG  NO-UNDO.
DEF VAR piLoop        AS INT  NO-UNDO.
DEFINE VARIABLE cStLager AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTekst   AS CHARACTER NO-UNDO.
DEF VAR cStatTilHK    AS CHAR NO-UNDO.
DEF VAR cStatKontroll AS CHAR NO-UNDO.
DEFINE VARIABLE iTime AS INTEGER NO-UNDO.
        
{syspara.i 3 4 3 cStatKontroll}
{syspara.i 3 4 1 cStatTilHK}
IF cStatKontroll = "" THEN
    cStatTilHK = "1".

RUN RunBatch.

/* --------------------- Subrutiner -------------------------------------- */

/* Ligger det en rutine over som skal ha display, s† flagges det ved  */
/* † sette wParentHandle til en gyldig verdi.                         */
PROCEDURE SettParentHandle:
  DEF INPUT PARAMETER ipParentHandle AS HANDLE.
  ASSIGN
    wParentHandle = ipParentHandle.
END.

/* Kj›rer batchen */
PROCEDURE RunBatch:

DEF BUFFER bufBatchLogg FOR BatchLogg.

ASSIGN
  wStart       = TRUE
  wOldDato     = ? /* Tar alltid alle ved første kjøring. */
  wEksportDato = ? /* Eksport av statistikkdata etter døgnskifte. */
  wBatchListe  = "".

/* Gir beskjed om at vi starter */
IF VALID-HANDLE(wParentHandle) THEN
  RUN Melding1 IN wParentHandle ("Batch kjøring startet " + STRING(TODAY) + " " + STRING(TIME,"HH:MM:SS")).

/* Utfører jobben */
LOOPEN:
DO WHILE TRUE:
    IF VALID-HANDLE(wParentHandle) THEN
      RUN VisIcon IN wParentHandle (2) NO-ERROR.
  /* Initierer en ny kjøring av alle ikke ferdig oppdaterte batcher. */
  IF wOldDato <> TODAY THEN
    ASSIGN
      wOldDato    = TODAY
      wStart      = TRUE.

  /* Henter data fra kassene og leser inn transaksjonene */
/*   IF CAN-DO("Ja,True,Yes",wSmash) and wCmdListe <> "" then                        */
/*     DO:                                                                           */
/*       IF VALID-HANDLE(wParentHandle) then                                         */
/*         RUN Melding1 IN wParentHandle ("Samler inn data fra kassene...").         */
/*       /* Henter data fra kassene */                                               */
/*       DO wLoop2 = 1 to NUM-ENTRIES(wCmdListe):                                    */
/*         OS-COMMAND SILENT VALUE(ENTRY(wLoop2,wCmdListe)).                         */
/*       END.                                                                        */
/*                                                                                   */
/*       IF VALID-HANDLE(wParentHandle) then                                         */
/*         RUN Melding1 IN wParentHandle ("Leser inn transaksjoner fra kassene..."). */
/*       /* Leser inn transaksjonene */                                              */
/*       RUN bt-innlesdata.p.                                                        */
/*     END.                                                                          */

  /* Blanker meldingsraden */
  IF VALID-HANDLE(wParentHandle) THEN
    RUN Melding1 IN wParentHandle ("Ferdig med behandle transaksjonsfiler.. ").

  /* Blanker meldingsraden */
  IF cStatTilHK = "1" AND VALID-HANDLE(wParentHandle) AND (wEksportDato <> TODAY) THEN 
  DO:
    /* Skal bare eksporteres en gang pr. time. */
    /*IF iTime <> INT(ENTRY(2,STRING(TIME,"HH:MM:SS"),':')) THEN*/ 
    IF wEksportDato <> TODAY THEN 
    DO:
      wEksportDato = TODAY.
      iTime = INT(ENTRY(2,STRING(TIME,"HH:MM:SS"),':')).
      RUN Melding1 IN wParentHandle ("Eksporterer statistikk til hk. ").
      
      RUN eksportButikk.p.
      
      STATLOOPEN:
      REPEAT:
        IF CAN-FIND(FIRST ELogg WHERE
        ELogg.TabellNAvn = 'StLinje' AND
        ELogg.EksterntSystem = 'HK') 
          THEN 
          RUN eksportbutikk.p.
        ELSE 
          LEAVE STATLOOPEN.
      END. /* STATLOOPEN */
    END.
  END.

  /* Blanker meldingsraden */
  IF VALID-HANDLE(wParentHandle) THEN
    RUN Melding1 IN wParentHandle (" ").

  /* Ved oppstart av programmet tar vi alle ikke ferdig oppddaterte batcher. */
  /* Samme blokk h†ndterer datoskifte.                                       */
  IF wStart THEN
    DO:
      ASSIGN
        wBatchNr = ?      /* Tar alle ikke oppdaterte batcher.      */
        wStart   = FALSE  /* Flagger at oppstartskjoring er utfort. */
        .
      
      IF VALID-HANDLE(wParentHandle) THEN
        RUN Melding1 IN wParentHandle ("Oppdaterer alle klargjorte batcher (Nye og gamle)...").

      RUN x-oppdlagertrans.w (?,wBatchNr).

      IF cStatTilHK = "1" THEN DO:
          IF VALID-HANDLE(wParentHandle) THEN
            RUN Melding1 IN wParentHandle ("Starter eksport til butikk...").

          RUN eksportButikk.p.

          STATLOOPEN2:
          REPEAT:
            IF CAN-FIND(FIRST ELogg WHERE
            ELogg.TabellNAvn = 'StLinje' AND
            ELogg.EksterntSystem = 'HK') 
              THEN 
              RUN eksportbutikk.p.
            ELSE 
              LEAVE STATLOOPEN2.
          END. /* STATLOOPEN */

          IF VALID-HANDLE(wParentHandle) THEN
            RUN Melding1 IN wParentHandle ("Ferdig med eksport til butikk...").
      END.
    END.

  /* Blanker meldingsraden */
  IF VALID-HANDLE(wParentHandle) THEN
    RUN Melding1 IN wParentHandle ("Bygger liste over nye batcher... ").

  /* Bygger en liste over alle nye batcher */
  /* EVIG LOOP                             */
  BATCHLOOP:
  DO WHILE TRUE:
      ASSIGN 
          wBatchListe = ""
          piLoop      = 0
          .
      LOOPEN:
      FOR EACH BatchLogg NO-LOCK WHERE
        BatchLogg.OppdStatus = 2:
        /* Sjekker at batchen 100% sikker ikke er låst. */
        DO FOR bufBatchLogg TRANSACTION:
          FIND bufBatchLogg EXCLUSIVE-LOCK WHERE
            bufBatchLogg.BatchNr = BatchLogg.BatchNr NO-WAIT NO-ERROR.

          IF LOCKED(bufBatchLogg) THEN
            DO:
              .
            END.
          ELSE ASSIGN
            wBatchListe = wBatchListe +
                          (IF wBatchListe = ""
                             THEN ""
                             ELSE ",") +
                          STRING(BatchLogg.BatchNr).
        END. /* TRANSACTION */
        /* Maks 50 batcher ad gangen */
        IF NUM-ENTRIES(wBatchListe) > 49 THEN
            LEAVE LOOPEN.
      END. /* LOOPEN */
      IF wBatchListe = "" THEN
          LEAVE BATCHLOOP.

      /* Dagens batcher som er delhvis oppdatert skal leses en gang til. */
      LOOPEN_2:
      FOR EACH BatchLogg NO-LOCK WHERE
        BatchLogg.OppdStatus = 3 AND 
        BatchLogg.RegistrertDato = TODAY:
        /* Sjekker at batchen 100% sikker ikke er låst. */
        DO FOR bufBatchLogg TRANSACTION:
          FIND bufBatchLogg EXCLUSIVE-LOCK WHERE
            bufBatchLogg.BatchNr = BatchLogg.BatchNr NO-WAIT NO-ERROR.

          IF LOCKED(bufBatchLogg) THEN
            DO:
              .
            END.
          ELSE ASSIGN
            wBatchListe = wBatchListe +
                          (IF wBatchListe = ""
                             THEN ""
                             ELSE ",") +
                          STRING(BatchLogg.BatchNr).
        END. /* TRANSACTION */
        /* Maks 50 batcher ad gangen */
        IF NUM-ENTRIES(wBatchListe) > 49 THEN
            LEAVE LOOPEN_2.
      END. /* LOOPEN_2 */
      /* Slutt på dagens batcher */

      /* Behandler nye batcher. */
      IF wBatchListe <> "" THEN
        LOOP_LISTE:
        DO wLoop = 1 TO NUM-ENTRIES(wBatchListe):
          ASSIGN
            wBatchNr = INT(ENTRY(wLoop,wBatchListe))
            .

          IF VALID-HANDLE(wParentHandle) THEN
            RUN Melding1 IN wParentHandle ("Oppdaterer batch " + STRING(wBatchNr) + "...").
          RUN x-oppdlagertrans.w (?,wBatchNr).

        END. /* LOOP_LISTE */
  END. /* BATCHLOOP */

  /* Gir beskjed om at vi lever */
  IF VALID-HANDLE(wParentHandle) THEN
    RUN Melding1 IN wParentHandle ("Venter på nye batcher (Pause " + STRING(wPause) + ") " + STRING(TODAY) + " " + STRING(TIME,"HH:MM:SS")).

  IF VALID-HANDLE(wParentHandle) THEN
    RUN VisIcon IN wParentHandle (1) NO-ERROR.

  {syspara.i 200 1 2 wPause int}
  IF TERMINAL <> "" THEN
    WAIT-FOR "close":U OF THIS-PROCEDURE PAUSE wPause.
  ELSE
    PAUSE wPause.

  /* Skriver ut plukkliste til kvittoskriver. */
  IF VALID-HANDLE(wParentHandle) THEN
    RUN Melding1 IN wParentHandle ("Behandler plukkliste...").
  RUN master-tick-row.p.


  {syspara.i 200 1 1 wStopp}
  IF CAN-DO("Stopp,Stop,Avbryt,Quit,End",wStopp) OR wMsgStopp = TRUE THEN
    LEAVE LOOPEN.
END. /* LOOPEN */

/* Gir beskjed om at vi stopper */
IF VALID-HANDLE(wParentHandle) THEN
  RUN Melding1 IN wParentHandle ("Batch kjøring avsluttet " + STRING(TODAY) + " " + STRING(TIME,"HH:MM:SS")).

/* Avslutter hvis vi ikke har noen over oss. */
IF NOT VALID-HANDLE(wParentHandle) THEN
  QUIT.

END PROCEDURE.

PROCEDURE StoppBatch:

  /* Gir beskjed om at vi stopper */
  IF VALID-HANDLE(wParentHandle) THEN
    RUN Melding1 IN wParentHandle ("Stoppmelding mottatt..." + STRING(TODAY) + " " + STRING(TIME,"HH:MM:SS")).
  ASSIGN
    wMsgStopp = TRUE.
END.
