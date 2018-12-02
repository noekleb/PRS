/* Tildeling av fakturanummer 
   Parametere:  <"idlist"/"query">: Angir om en liste med fakturanumre skal prosesseres eller det kommer en where-betingelse
                Det kan også sendes en temp-tabell med fakturaer.
   Opprettet: 01.05.05 av BHa                  
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR hQuery      AS HANDLE NO-UNDO.
DEF VAR ix          AS INT    NO-UNDO.
DEF VAR cIdList     AS CHAR   NO-UNDO.
DEF VAR hBuffer     AS HANDLE NO-UNDO.
DEF VAR iLayout     AS INT    NO-UNDO.


IF NUM-ENTRIES(icParam,"|") <> 3 THEN
DO:
    ASSIGN
        ocReturn = "** Feil antall parmaterte til faktura_fakturaskriver.p"
        obOk     = FALSE
        .
    RETURN.

END.
FIND butiker NO-LOCK WHERE
    Butiker.Butik = INT(ENTRY(1,icParam,"|")) NO-ERROR.
FIND Kasse NO-LOCK WHERE
    Kasse.ButikkNr = INT(ENTRY(1,icParam,"|")) AND
    Kasse.GruppeNr = INT(ENTRY(2,icParam,"|")) AND
    Kasse.KasseNr  = INT(ENTRY(3,icParam,"|")) NO-ERROR.

IF AVAILABLE Kasse AND Kasse.Fakturaskriver <> "" THEN 
    ocReturn = "TRUE|" + STRING(Kasse.Fakturaskriver) + "|" + string(Kasse.FakturaKopi) + "||" + (IF Kasse.FakturaLayout > 0 THEN string(Kasse.FakturaLayout) ELSE "1").
ELSE IF AVAILABLE Butiker AND Butiker.Fakturaskriver <> "" THEN
    ocReturn = "TRUE|" + STRING(Butiker.Fakturaskriver) + "|" + string(Butiker.FakturaKopi) + "||" + (IF Butiker.FakturaLayout > 0 THEN string(Butiker.FakturaLayout) ELSE "1").
ELSE
    ocReturn = "TRUE||3||1".
obOk = TRUE.


/*         DEF BUFFER bFakturaHode FOR FakturaHode. */
/* IF ENTRY(1,icParam,"|") = "idlist" THEN DO:                                                    */
/*   cIdList = ENTRY(2,icParam,"|").                                                              */
/*   DO ix = 1 TO NUM-ENTRIES(cIdList):                                                           */
/*     FIND FIRST bFakturaHode                                                                    */
/*          WHERE bFakturaHode.Faktura_id = DEC(ENTRY(ix,cIdList))                                */
/*            AND bFakturaHode.FakturaNr  = ?                                                     */
/*          EXCLUSIVE-LOCK NO-ERROR.                                                              */
/*     IF AVAIL bFakturaHode THEN DO:                                                             */
/*       RUN ProduserFaktura (OUTPUT obOK).                                                       */
/*       IF NOT obOk THEN LEAVE.                                                                  */
/*     END.                                                                                       */
/*   END.                                                                                         */
/* END.                                                                                           */
/* /* WHERE betingelse: */                                                                        */
/* ELSE IF ihBuffer = ? THEN DO:                                                                  */
/*   hBuffer = BUFFER FakturaHode:HANDLE.                                                         */
/*   CREATE QUERY hQuery.                                                                         */
/*   hQuery:SET-BUFFERS(hBuffer).                                                                 */
/*   hQuery:QUERY-PREPARE("FOR EACH FakturaHode NO-LOCK " + ENTRY(2,icParam)).                    */
/*   hQuery:QUERY-OPEN().                                                                         */
/*                                                                                                */
/*   hQuery:GET-FIRST().                                                                          */
/*   REPEAT WHILE NOT hQuery:QUERY-OFF-END:                                                       */
/*     FIND FIRST bFakturaHode                                                                    */
/*          WHERE bFakturaHode.Faktura_id = DEC(hBuffer:BUFFER-FIELD("Faktura_id"):BUFFER-VALUE)  */
/*            AND bFakturaHode.FakturaNr  = ?                                                     */
/*          EXCLUSIVE-LOCK NO-ERROR.                                                              */
/*     IF AVAIL bFakturaHode THEN DO:                                                             */
/*       RUN ProduserFaktura (OUTPUT obOK).                                                       */
/*       IF NOT obOk THEN LEAVE.                                                                  */
/*     END.                                                                                       */
/*                                                                                                */
/*     hQuery:GET-NEXT().                                                                         */
/*   END.                                                                                         */
/*   DELETE OBJECT hQuery.                                                                        */
/* END.                                                                                           */
/* /* Temp-tabell: */                                                                             */
/* ELSE DO:                                                                                       */
/*   CREATE QUERY hQuery.                                                                         */
/*   hQuery:SET-BUFFERS(ihBuffer).                                                                */
/*   hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " BY Dato").                              */
/*   hQuery:QUERY-OPEN().                                                                         */
/*                                                                                                */
/*   hQuery:GET-FIRST().                                                                          */
/*   REPEAT WHILE NOT hQuery:QUERY-OFF-END:                                                       */
/*     FIND FIRST bFakturaHode                                                                    */
/*          WHERE bFakturaHode.Faktura_id = DEC(ihBuffer:BUFFER-FIELD("Faktura_id"):BUFFER-VALUE) */
/*            AND bFakturaHode.FakturaNr  = ?                                                     */
/*          EXCLUSIVE-LOCK NO-ERROR.                                                              */
/*     IF AVAIL bFakturaHode THEN DO:                                                             */
/*       RUN ProduserFaktura (OUTPUT obOK).                                                       */
/*       IF NOT obOk THEN LEAVE.                                                                  */
/*     END.                                                                                       */
/*                                                                                                */
/*     hQuery:GET-NEXT().                                                                         */
/*   END.                                                                                         */
/*   DELETE OBJECT hQuery.                                                                        */
/* END.                                                                                           */
/*                                                                                                */
/* IF ocReturn = "" THEN obOk = TRUE.                                                             */
/*                                                                                                */
/* PROCEDURE ProduserFaktura:                                                                     */
/*   DEF OUTPUT PARAM obOK AS LOG NO-UNDO INIT TRUE.                                              */
/*                                                                                                */
/*   DEF VAR fFakturaNr  AS DEC    NO-UNDO.                                                       */
/*                                                                                                */
/*   RUN getfakturanr.p (OUTPUT fFakturaNr).                                                      */
/*                                                                                                */
/*   FIND FIRST BetalingsBetingelser                                                              */
/*        OF bFakturaHode NO-LOCK NO-ERROR.                                                       */
/*   IF NOT AVAIL Betalingsbetingelser THEN DO:                                                   */
/*     ASSIGN ocReturn = "Faktura " + STRING(Faktura_id) + " mangler betalingsbetingelse".        */
/*            obOk     = FALSE.                                                                   */
/*     RETURN.                                                                                    */
/*   END.                                                                                         */
/*                                                                                                */
/*   ASSIGN bFakturaHode.FakturaNr       = fFakturaNr                                             */
/*          bFakturaHode.FakturertDato   = TODAY                                                  */
/*          bFakturaHode.ProduksjonsDato = TODAY                                                  */
/*          bFakturaHode.ForfallsDato    = TODAY + Betalingsbetingelser.AntKredittDager           */
/*          .                                                                                     */
/*                                                                                                */
/*   CREATE Kundereskontr.                                                                        */
/*   BUFFER-COPY bFakturaHode TO Kundereskontr.                                                   */
/*   ASSIGN Kundereskontr.Belop      = bFakturahode.Totalt                                        */
/*          Kundereskontr.Saldo      = bFakturahode.Totalt                                        */
/*          Kundereskontr.BArtNr     = 1                                                          */
/*          Kundereskontr.BilagsType = 1                                                          */
/*          .                                                                                     */
/* END PROCEDURE.                                                                                 */
