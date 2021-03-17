&IF NOT DEFINED (BrowseFellesTing) <> 0 &THEN
   DEF VAR wBrowseSortH AS HANDLE NO-UNDO.
   PUBLISH "GetBrsortHandle" (OUTPUT wBrowseSortH).
   IF VALID-HANDLE(wBrowseSortH) THEN
        RUN Cleanup IN wBrowseSortH.
   ELSE RUN brsort.p PERSISTENT SET wBrowseSortH.

   PROCEDURE ByttSort:
      RUN VALUE("ByttSort" + SELF:NAME) IN THIS-PROCEDURE ("",0,"").
   END PROCEDURE.

   PROCEDURE CursorSort:
      RUN VALUE("CursorSort" + SELF:NAME) IN THIS-PROCEDURE.
   END PROCEDURE.

   PROCEDURE ViewSearch:
      DEF INPUT PARAMETER wiBrowseName AS CHAR NO-UNDO.
      RUN ViewSearch IN wBrowseSortH (wiBrowseName).
   END.

&ENDIF

&GLOB BrowseFellesTing DEFINERT

DEF VAR w{&Browse}Intd AS LOGI NO-UNDO.

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN
   RUN SettCurrSort{&Browse} IN THIS-PROCEDURE.
&ELSE
   IF NOT CAN-DO(THIS-PROCEDURE:INTERNAL-ENTRIES,"SaveBrowseSettings") THEN
      RUN SettCurrSort{&Browse} IN THIS-PROCEDURE.
&ENDIF

&IF "{&SokA1}{&SokA2}{&SokA3}{&SokA4}{&SokB1}{&SokB2}{&SokB3}{&SokB4}{&SokC1}{&SokC2}{&SokC3}{&SokC4}{&SokD1}{&SokD2}{&SokD3}{&SokD4}{&SokE1}{&SokE2}{&SokE3}{&SokE4}{&SokF1}{&SokF2}{&SokF3}{&SokF4}" <> "" &THEN
    RUN MakeSearch IN wBrowseSortH (BROWSE {&Browse}:HANDLE, THIS-PROCEDURE, "{&SokA1}" <> "").
&ENDIF

&IF "{&Mb1}{&Mb2}{&Mb3}{&Mb4}{&Mb5}{&Mb6}{&Mb7}{&Mb8}{&Mb9}{&Mb10}" <> "" &THEN
    RUN MakeMbMenu IN wBrowseSortH (BROWSE {&Browse}:HANDLE,&IF "{&Mb1}"  <> "" &THEN   STRING({&Mb1}:HANDLE)  + "," &ENDIF
                                                            &IF "{&Mb2}"  <> "" &THEN + STRING({&Mb2}:HANDLE)  + "," &ENDIF
                                                            &IF "{&Mb3}"  <> "" &THEN + STRING({&Mb3}:HANDLE)  + "," &ENDIF
                                                            &IF "{&Mb4}"  <> "" &THEN + STRING({&Mb4}:HANDLE)  + "," &ENDIF
                                                            &IF "{&Mb5}"  <> "" &THEN + STRING({&Mb5}:HANDLE)  + "," &ENDIF
                                                            &IF "{&Mb6}"  <> "" &THEN + STRING({&Mb6}:HANDLE)  + "," &ENDIF
                                                            &IF "{&Mb7}"  <> "" &THEN + STRING({&Mb7}:HANDLE)  + "," &ENDIF
                                                            &IF "{&Mb8}"  <> "" &THEN + STRING({&Mb8}:HANDLE)  + "," &ENDIF
                                                            &IF "{&Mb9}"  <> "" &THEN + STRING({&Mb9}:HANDLE)  + "," &ENDIF
                                                            &IF "{&Mb10}" <> "" &THEN + STRING({&Mb10}:HANDLE) + "," &ENDIF).
&ENDIF

&IF DEFINED(Meny) <> 0 &THEN
   RUN MakeMenu IN wBrowseSortH (BROWSE {&Browse}:HANDLE,"{&Meny}",THIS-PROCEDURE).
&ENDIF

&IF NOT DEFINED(NoReadOnly) <> 0 &THEN
   RUN SettBrowseReadOnly{&Browse}.
&ENDIF

&IF NOT DEFINED(NoTriggers) <> 0 &THEN
  ON START-SEARCH OF BROWSE {&Browse} DO:
     RUN ByttSort.
  END.
  ON CURSOR-RIGHT, CURSOR-LEFT OF BROWSE {&Browse} DO:
     IF "{&KolonneA}{&KolonneB}{&KolonneC}{&KolonneD}{&KolonneE}{&KolonneF}" <> "{&KolonneA}" THEN
        RUN CursorSort.
     RETURN NO-APPLY.
  END.
  &IF NOT DEFINED(NoSearch) <> 0 &THEN
     ON ANY-PRINTABLE OF BROWSE {&Browse} DO:
        RUN InitSearch IN wBrowseSortH (BROWSE {&Browse}:HANDLE).
     END.
  &ENDIF
&ENDIF

PROCEDURE ByttSort{&Browse}:
   DEF INPUT PARAMETER wiColName   AS CHAR NO-UNDO.
   DEF INPUT PARAMETER wiSeq#      AS INTE NO-UNDO.
   DEF INPUT PARAMETER wiQueryName AS CHAR NO-UNDO.
   IF NOT w{&Browse}Intd THEN  RUN InitBrowse{&Browse}.
   RUN ByttSort IN wBrowseSortH(BROWSE {&Browse}:HANDLE,ROWID({&Tabell}),wiColName,wiSeq#,wiQueryName,THIS-PROCEDURE).
END.

PROCEDURE CursorSort{&Browse}:
   IF NOT w{&Browse}Intd THEN  RUN InitBrowse{&Browse}.
   RUN CursorSort IN wBrowseSortH (THIS-PROCEDURE,"{&KolonneA}","{&KolonneB}","{&KolonneC}","{&KolonneD}","{&KolonneE}","{&KolonneF}").
END.

&IF DEFINED (QueryA1) <> 0 &THEN PROCEDURE OpenQuery{&Browse}{&KolonneA}{&QueryA1}: {&OPEN-QUERY-{&QueryA1}} END. &ENDIF
&IF DEFINED (QueryA2) <> 0 &THEN PROCEDURE OpenQuery{&Browse}{&KolonneA}{&QueryA2}: {&OPEN-QUERY-{&QueryA2}} END. &ENDIF
&IF DEFINED (QueryA3) <> 0 &THEN PROCEDURE OpenQuery{&Browse}{&KolonneA}{&QueryA3}: {&OPEN-QUERY-{&QueryA3}} END. &ENDIF
&IF DEFINED (QueryA4) <> 0 &THEN PROCEDURE OpenQuery{&Browse}{&KolonneA}{&QueryA4}: {&OPEN-QUERY-{&QueryA4}} END. &ENDIF
&IF DEFINED (QueryB1) <> 0 &THEN PROCEDURE OpenQuery{&Browse}{&KolonneB}{&QueryB1}: {&OPEN-QUERY-{&QueryB1}} END. &ENDIF
&IF DEFINED (QueryB2) <> 0 &THEN PROCEDURE OpenQuery{&Browse}{&KolonneB}{&QueryB2}: {&OPEN-QUERY-{&QueryB2}} END. &ENDIF
&IF DEFINED (QueryB3) <> 0 &THEN PROCEDURE OpenQuery{&Browse}{&KolonneB}{&QueryB3}: {&OPEN-QUERY-{&QueryB3}} END. &ENDIF
&IF DEFINED (QueryB4) <> 0 &THEN PROCEDURE OpenQuery{&Browse}{&KolonneB}{&QueryB4}: {&OPEN-QUERY-{&QueryB4}} END. &ENDIF
&IF DEFINED (QueryC1) <> 0 &THEN PROCEDURE OpenQuery{&Browse}{&KolonneC}{&QueryC1}: {&OPEN-QUERY-{&QueryC1}} END. &ENDIF
&IF DEFINED (QueryC2) <> 0 &THEN PROCEDURE OpenQuery{&Browse}{&KolonneC}{&QueryC2}: {&OPEN-QUERY-{&QueryC2}} END. &ENDIF
&IF DEFINED (QueryC3) <> 0 &THEN PROCEDURE OpenQuery{&Browse}{&KolonneC}{&QueryC3}: {&OPEN-QUERY-{&QueryC3}} END. &ENDIF
&IF DEFINED (QueryC4) <> 0 &THEN PROCEDURE OpenQuery{&Browse}{&KolonneC}{&QueryC4}: {&OPEN-QUERY-{&QueryC4}} END. &ENDIF
&IF DEFINED (QueryD1) <> 0 &THEN PROCEDURE OpenQuery{&Browse}{&KolonneD}{&QueryD1}: {&OPEN-QUERY-{&QueryD1}} END. &ENDIF
&IF DEFINED (QueryD2) <> 0 &THEN PROCEDURE OpenQuery{&Browse}{&KolonneD}{&QueryD2}: {&OPEN-QUERY-{&QueryD2}} END. &ENDIF
&IF DEFINED (QueryD3) <> 0 &THEN PROCEDURE OpenQuery{&Browse}{&KolonneD}{&QueryD3}: {&OPEN-QUERY-{&QueryD3}} END. &ENDIF
&IF DEFINED (QueryD4) <> 0 &THEN PROCEDURE OpenQuery{&Browse}{&KolonneD}{&QueryD4}: {&OPEN-QUERY-{&QueryD4}} END. &ENDIF
&IF DEFINED (QueryE1) <> 0 &THEN PROCEDURE OpenQuery{&Browse}{&KolonneE}{&QueryE1}: {&OPEN-QUERY-{&QueryE1}} END. &ENDIF
&IF DEFINED (QueryE2) <> 0 &THEN PROCEDURE OpenQuery{&Browse}{&KolonneE}{&QueryE2}: {&OPEN-QUERY-{&QueryE2}} END. &ENDIF
&IF DEFINED (QueryE3) <> 0 &THEN PROCEDURE OpenQuery{&Browse}{&KolonneE}{&QueryE3}: {&OPEN-QUERY-{&QueryE3}} END. &ENDIF
&IF DEFINED (QueryE4) <> 0 &THEN PROCEDURE OpenQuery{&Browse}{&KolonneE}{&QueryE4}: {&OPEN-QUERY-{&QueryE4}} END. &ENDIF
&IF DEFINED (QueryF1) <> 0 &THEN PROCEDURE OpenQuery{&Browse}{&KolonneF}{&QueryF1}: {&OPEN-QUERY-{&QueryF1}} END. &ENDIF
&IF DEFINED (QueryF2) <> 0 &THEN PROCEDURE OpenQuery{&Browse}{&KolonneF}{&QueryF2}: {&OPEN-QUERY-{&QueryF2}} END. &ENDIF
&IF DEFINED (QueryF3) <> 0 &THEN PROCEDURE OpenQuery{&Browse}{&KolonneF}{&QueryF3}: {&OPEN-QUERY-{&QueryF3}} END. &ENDIF
&IF DEFINED (QueryF4) <> 0 &THEN PROCEDURE OpenQuery{&Browse}{&KolonneF}{&QueryF4}: {&OPEN-QUERY-{&QueryF4}} END. &ENDIF

PROCEDURE InitBrowse{&Browse}:
   IF w{&Browse}Intd THEN RETURN NO-APPLY.
   &IF DEFINED (QueryA1) <> 0 &THEN RUN Initier IN wBrowseSortH (BROWSE {&Browse}:HANDLE,"{&KolonneA}",QUERY {&QueryA1}:HANDLE,"{&SokA1}"<>""). &ENDIF
   &IF DEFINED (QueryA2) <> 0 &THEN RUN Initier IN wBrowseSortH (BROWSE {&Browse}:HANDLE,"{&KolonneA}",QUERY {&QueryA2}:HANDLE,"{&SokA2}"<>""). &ENDIF
   &IF DEFINED (QueryA3) <> 0 &THEN RUN Initier IN wBrowseSortH (BROWSE {&Browse}:HANDLE,"{&KolonneA}",QUERY {&QueryA3}:HANDLE,"{&SokA3}"<>""). &ENDIF
   &IF DEFINED (QueryA4) <> 0 &THEN RUN Initier IN wBrowseSortH (BROWSE {&Browse}:HANDLE,"{&KolonneA}",QUERY {&QueryA4}:HANDLE,"{&SokA4}"<>""). &ENDIF
   &IF DEFINED (QueryB1) <> 0 &THEN RUN Initier IN wBrowseSortH (BROWSE {&Browse}:HANDLE,"{&KolonneB}",QUERY {&QueryB1}:HANDLE,"{&SokB1}"<>""). &ENDIF
   &IF DEFINED (QueryB2) <> 0 &THEN RUN Initier IN wBrowseSortH (BROWSE {&Browse}:HANDLE,"{&KolonneB}",QUERY {&QueryB2}:HANDLE,"{&SokB2}"<>""). &ENDIF
   &IF DEFINED (QueryB3) <> 0 &THEN RUN Initier IN wBrowseSortH (BROWSE {&Browse}:HANDLE,"{&KolonneB}",QUERY {&QueryB3}:HANDLE,"{&SokB3}"<>""). &ENDIF
   &IF DEFINED (QueryB4) <> 0 &THEN RUN Initier IN wBrowseSortH (BROWSE {&Browse}:HANDLE,"{&KolonneB}",QUERY {&QueryB4}:HANDLE,"{&SokB4}"<>""). &ENDIF
   &IF DEFINED (QueryC1) <> 0 &THEN RUN Initier IN wBrowseSortH (BROWSE {&Browse}:HANDLE,"{&KolonneC}",QUERY {&QueryC1}:HANDLE,"{&SokC1}"<>""). &ENDIF
   &IF DEFINED (QueryC2) <> 0 &THEN RUN Initier IN wBrowseSortH (BROWSE {&Browse}:HANDLE,"{&KolonneC}",QUERY {&QueryC2}:HANDLE,"{&SokC2}"<>""). &ENDIF
   &IF DEFINED (QueryC3) <> 0 &THEN RUN Initier IN wBrowseSortH (BROWSE {&Browse}:HANDLE,"{&KolonneC}",QUERY {&QueryC3}:HANDLE,"{&SokC3}"<>""). &ENDIF
   &IF DEFINED (QueryC4) <> 0 &THEN RUN Initier IN wBrowseSortH (BROWSE {&Browse}:HANDLE,"{&KolonneC}",QUERY {&QueryC4}:HANDLE,"{&SokC4}"<>""). &ENDIF
   &IF DEFINED (QueryD1) <> 0 &THEN RUN Initier IN wBrowseSortH (BROWSE {&Browse}:HANDLE,"{&KolonneD}",QUERY {&QueryD1}:HANDLE,"{&SokD1}"<>""). &ENDIF
   &IF DEFINED (QueryD2) <> 0 &THEN RUN Initier IN wBrowseSortH (BROWSE {&Browse}:HANDLE,"{&KolonneD}",QUERY {&QueryD2}:HANDLE,"{&SokD2}"<>""). &ENDIF
   &IF DEFINED (QueryD3) <> 0 &THEN RUN Initier IN wBrowseSortH (BROWSE {&Browse}:HANDLE,"{&KolonneD}",QUERY {&QueryD3}:HANDLE,"{&SokD3}"<>""). &ENDIF
   &IF DEFINED (QueryD4) <> 0 &THEN RUN Initier IN wBrowseSortH (BROWSE {&Browse}:HANDLE,"{&KolonneD}",QUERY {&QueryD4}:HANDLE,"{&SokD4}"<>""). &ENDIF
   &IF DEFINED (QueryE1) <> 0 &THEN RUN Initier IN wBrowseSortH (BROWSE {&Browse}:HANDLE,"{&KolonneE}",QUERY {&QueryE1}:HANDLE,"{&SokE1}"<>""). &ENDIF
   &IF DEFINED (QueryE2) <> 0 &THEN RUN Initier IN wBrowseSortH (BROWSE {&Browse}:HANDLE,"{&KolonneE}",QUERY {&QueryE2}:HANDLE,"{&SokE2}"<>""). &ENDIF
   &IF DEFINED (QueryE3) <> 0 &THEN RUN Initier IN wBrowseSortH (BROWSE {&Browse}:HANDLE,"{&KolonneE}",QUERY {&QueryE3}:HANDLE,"{&SokE3}"<>""). &ENDIF
   &IF DEFINED (QueryE4) <> 0 &THEN RUN Initier IN wBrowseSortH (BROWSE {&Browse}:HANDLE,"{&KolonneE}",QUERY {&QueryE4}:HANDLE,"{&SokE4}"<>""). &ENDIF
   &IF DEFINED (QueryF1) <> 0 &THEN RUN Initier IN wBrowseSortH (BROWSE {&Browse}:HANDLE,"{&KolonneF}",QUERY {&QueryF1}:HANDLE,"{&SokF1}"<>""). &ENDIF
   &IF DEFINED (QueryF2) <> 0 &THEN RUN Initier IN wBrowseSortH (BROWSE {&Browse}:HANDLE,"{&KolonneF}",QUERY {&QueryF2}:HANDLE,"{&SokF2}"<>""). &ENDIF
   &IF DEFINED (QueryF3) <> 0 &THEN RUN Initier IN wBrowseSortH (BROWSE {&Browse}:HANDLE,"{&KolonneF}",QUERY {&QueryF3}:HANDLE,"{&SokF3}"<>""). &ENDIF
   &IF DEFINED (QueryF4) <> 0 &THEN RUN Initier IN wBrowseSortH (BROWSE {&Browse}:HANDLE,"{&KolonneF}",QUERY {&QueryF4}:HANDLE,"{&SokF4}"<>""). &ENDIF
   ASSIGN w{&Browse}Intd = YES.
END PROCEDURE.

PROCEDURE SettBrowseReadOnly{&Browse}:
   DEF VAR wh AS HANDLE NO-UNDO.
   ASSIGN wh = BROWSE {&Browse}:FIRST-COLUMN.
   DO WHILE VALID-HANDLE(wh):
      ASSIGN wh:READ-ONLY = YES NO-ERROR.
      ASSIGN wh = wh:NEXT-COLUMN.
   END.
END PROCEDURE.


PROCEDURE SettCurrSort{&Browse}:
   IF NOT w{&Browse}Intd THEN  RUN InitBrowse{&Browse}.
   RUN SettCurrSort IN wBrowseSortH (BROWSE {&Browse}:HANDLE,BROWSE {&Browse}:QUERY,"{&KolonneA}").
END.

PROCEDURE SettNyCurrSort{&Browse}:
   DEF INPUT PARAMETER wiBrowseH   AS HANDLE NO-UNDO.
   DEF INPUT PARAMETER wiQueryName AS CHAR   NO-UNDO.
   DEF INPUT PARAMETER wiColName   AS CHAR   NO-UNDO.
   IF NOT w{&Browse}Intd THEN  RUN InitBrowse{&Browse}.
   RUN SettNyCurrSort IN wBrowseSortH (wiBrowseH,wiQueryName,wiColName).
   RETURN RETURN-VALUE.
END.

PROCEDURE FindSearch{&Browse}:
   DEF INPUT  PARAMETER wiSearchString AS CHAR  NO-UNDO.
   DEF INPUT  PARAMETER wiColName      AS CHAR  NO-UNDO.
   DEF INPUT  PARAMETER wiSeq#         AS INTE  NO-UNDO.
   DEF OUTPUT PARAMETER woRowid        AS ROWID NO-UNDO.

   DEF VAR wSok AS CHAR NO-UNDO.

   DEF BUFFER Buff{&Tabell} FOR {&Tabell}.

   ASSIGN wSok = wiSearchString.

   CASE wiColNAME:
      &IF DEFINED(KolonneA) <> 0 &THEN
      WHEN "{&KolonneA}" THEN DO:
         CASE wiSeq#:
            &IF DEFINED(SokA1) <> 0 &THEN WHEN 1 THEN FOR EACH Buff{&Tabell} FIELDS() {&SokA1} &IF NOT "{&SokA1}" MATCHES "* BY *" &THEN NO-LOCK NO-PREFETCH &ENDIF: LEAVE. END. &ENDIF
            &IF DEFINED(SokA2) <> 0 &THEN WHEN 2 THEN FOR EACH Buff{&Tabell} FIELDS() {&SokA2} &IF NOT "{&SokA2}" MATCHES "* BY *" &THEN NO-LOCK NO-PREFETCH &ENDIF: LEAVE. END. &ENDIF
            &IF DEFINED(SokA3) <> 0 &THEN WHEN 3 THEN FOR EACH Buff{&Tabell} FIELDS() {&SokA3} &IF NOT "{&SokA3}" MATCHES "* BY *" &THEN NO-LOCK NO-PREFETCH &ENDIF: LEAVE. END. &ENDIF
            &IF DEFINED(SokA4) <> 0 &THEN WHEN 4 THEN FOR EACH Buff{&Tabell} FIELDS() {&SokA4} &IF NOT "{&SokA4}" MATCHES "* BY *" &THEN NO-LOCK NO-PREFETCH &ENDIF: LEAVE. END. &ENDIF
         END CASE.
      END.
      &ENDIF
      &IF DEFINED(KolonneB) <> 0 &THEN
      WHEN "{&KolonneB}" THEN DO:
         CASE wiSeq#:
            &IF DEFINED(SokB1) <> 0 &THEN WHEN 1 THEN FOR EACH Buff{&Tabell} FIELDS() {&SokB1} &IF NOT "{&SokB1}" MATCHES "* BY *" &THEN NO-LOCK NO-PREFETCH &ENDIF: LEAVE. END. &ENDIF
            &IF DEFINED(SokB2) <> 0 &THEN WHEN 2 THEN FOR EACH Buff{&Tabell} FIELDS() {&SokB2} &IF NOT "{&SokB2}" MATCHES "* BY *" &THEN NO-LOCK NO-PREFETCH &ENDIF: LEAVE. END. &ENDIF
            &IF DEFINED(SokB3) <> 0 &THEN WHEN 3 THEN FOR EACH Buff{&Tabell} FIELDS() {&SokB3} &IF NOT "{&SokB3}" MATCHES "* BY *" &THEN NO-LOCK NO-PREFETCH &ENDIF: LEAVE. END. &ENDIF
            &IF DEFINED(SokB4) <> 0 &THEN WHEN 4 THEN FOR EACH Buff{&Tabell} FIELDS() {&SokB4} &IF NOT "{&SokB4}" MATCHES "* BY *" &THEN NO-LOCK NO-PREFETCH &ENDIF: LEAVE. END. &ENDIF
         END CASE.
      END.
      &ENDIF
      &IF DEFINED(KolonneC) <> 0 &THEN
      WHEN "{&KolonneC}" THEN DO:
         CASE wiSeq#:
            &IF DEFINED(SokC1) <> 0 &THEN WHEN 1 THEN FOR EACH Buff{&Tabell} FIELDS() {&SokC1} &IF NOT "{&SokC1}" MATCHES "* BY *" &THEN NO-LOCK NO-PREFETCH &ENDIF: LEAVE. END. &ENDIF
            &IF DEFINED(SokC2) <> 0 &THEN WHEN 2 THEN FOR EACH Buff{&Tabell} FIELDS() {&SokC2} &IF NOT "{&SokC2}" MATCHES "* BY *" &THEN NO-LOCK NO-PREFETCH &ENDIF: LEAVE. END. &ENDIF
            &IF DEFINED(SokC3) <> 0 &THEN WHEN 3 THEN FOR EACH Buff{&Tabell} FIELDS() {&SokC3} &IF NOT "{&SokC3}" MATCHES "* BY *" &THEN NO-LOCK NO-PREFETCH &ENDIF: LEAVE. END. &ENDIF
            &IF DEFINED(SokC4) <> 0 &THEN WHEN 4 THEN FOR EACH Buff{&Tabell} FIELDS() {&SokC4} &IF NOT "{&SokC4}" MATCHES "* BY *" &THEN NO-LOCK NO-PREFETCH &ENDIF: LEAVE. END. &ENDIF
         END CASE.
      END.
      &ENDIF
      &IF DEFINED(KolonneD) <> 0 &THEN
      WHEN "{&KolonneD}" THEN DO:
         CASE wiSeq#:
            &IF DEFINED(SokD1) <> 0 &THEN WHEN 1 THEN FOR EACH Buff{&Tabell} FIELDS() {&SokD1} &IF NOT "{&SokD1}" MATCHES "* BY *" &THEN NO-LOCK NO-PREFETCH &ENDIF: LEAVE. END. &ENDIF
            &IF DEFINED(SokD2) <> 0 &THEN WHEN 2 THEN FOR EACH Buff{&Tabell} FIELDS() {&SokD2} &IF NOT "{&SokD2}" MATCHES "* BY *" &THEN NO-LOCK NO-PREFETCH &ENDIF: LEAVE. END. &ENDIF
            &IF DEFINED(SokD3) <> 0 &THEN WHEN 3 THEN FOR EACH Buff{&Tabell} FIELDS() {&SokD3} &IF NOT "{&SokD3}" MATCHES "* BY *" &THEN NO-LOCK NO-PREFETCH &ENDIF: LEAVE. END. &ENDIF
            &IF DEFINED(SokD4) <> 0 &THEN WHEN 4 THEN FOR EACH Buff{&Tabell} FIELDS() {&SokD4} &IF NOT "{&SokD4}" MATCHES "* BY *" &THEN NO-LOCK NO-PREFETCH &ENDIF: LEAVE. END. &ENDIF
         END CASE.
      END.
      &ENDIF
      &IF DEFINED(KolonneE) <> 0 &THEN
      WHEN "{&KolonneE}" THEN DO:
         CASE wiSeq#:
            &IF DEFINED(SokE1) <> 0 &THEN WHEN 1 THEN FOR EACH Buff{&Tabell} FIELDS() {&SokE1} &IF NOT "{&SokE1}" MATCHES "* BY *" &THEN NO-LOCK NO-PREFETCH &ENDIF: LEAVE. END. &ENDIF
            &IF DEFINED(SokE2) <> 0 &THEN WHEN 2 THEN FOR EACH Buff{&Tabell} FIELDS() {&SokE2} &IF NOT "{&SokE2}" MATCHES "* BY *" &THEN NO-LOCK NO-PREFETCH &ENDIF: LEAVE. END. &ENDIF
            &IF DEFINED(SokE3) <> 0 &THEN WHEN 3 THEN FOR EACH Buff{&Tabell} FIELDS() {&SokE3} &IF NOT "{&SokE3}" MATCHES "* BY *" &THEN NO-LOCK NO-PREFETCH &ENDIF: LEAVE. END. &ENDIF
            &IF DEFINED(SokE4) <> 0 &THEN WHEN 4 THEN FOR EACH Buff{&Tabell} FIELDS() {&SokE4} &IF NOT "{&SokE4}" MATCHES "* BY *" &THEN NO-LOCK NO-PREFETCH &ENDIF: LEAVE. END. &ENDIF
         END CASE.
      END.
      &ENDIF
      &IF DEFINED(KolonneF) <> 0 &THEN
      WHEN "{&KolonneF}" THEN DO:
         CASE wiSeq#:
            &IF DEFINED(SokF1) <> 0 &THEN WHEN 1 THEN FOR EACH Buff{&Tabell} FIELDS() {&SokF1} &IF NOT "{&SokF1}" MATCHES "* BY *" &THEN NO-LOCK NO-PREFETCH &ENDIF: LEAVE. END. &ENDIF
            &IF DEFINED(SokF2) <> 0 &THEN WHEN 2 THEN FOR EACH Buff{&Tabell} FIELDS() {&SokF2} &IF NOT "{&SokF2}" MATCHES "* BY *" &THEN NO-LOCK NO-PREFETCH &ENDIF: LEAVE. END. &ENDIF
            &IF DEFINED(SokF3) <> 0 &THEN WHEN 3 THEN FOR EACH Buff{&Tabell} FIELDS() {&SokF3} &IF NOT "{&SokF3}" MATCHES "* BY *" &THEN NO-LOCK NO-PREFETCH &ENDIF: LEAVE. END. &ENDIF
            &IF DEFINED(SokF4) <> 0 &THEN WHEN 4 THEN FOR EACH Buff{&Tabell} FIELDS() {&SokF4} &IF NOT "{&SokF4}" MATCHES "* BY *" &THEN NO-LOCK NO-PREFETCH &ENDIF: LEAVE. END. &ENDIF
         END CASE.
      END.
      &ENDIF
   END CASE.
   IF AVAIL Buff{&Tabell} THEN DO:
      ASSIGN woRowid = ROWID(Buff{&Tabell}).
      RELEASE Buff{&Tabell}.
   END.
END.

