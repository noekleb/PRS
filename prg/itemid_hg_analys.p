DEFINE VAR dFraDato       AS DATE      NO-UNDO.
DEFINE VAR dTilDato       AS DATE      NO-UNDO.
DEFINE VARIABLE cFilename AS CHARACTER NO-UNDO.
DEFINE VARIABLE dDato     AS DATE      NO-UNDO.
DEFINE VARIABLE ii        AS INTEGER   NO-UNDO.
DEFINE VARIABLE cMailhub  AS CHARACTER NO-UNDO.
DEFINE VARIABLE lMailOK   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMailTo   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMailFrom AS CHARACTER INIT "prs@preem.se" NO-UNDO.
DEFINE VARIABLE cItemId   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lOK       AS LOGICAL   NO-UNDO.

DEFINE TEMP-TABLE tt_OK NO-UNDO
    FIELD Itemid AS INTE
    FIELD hg     AS INTE
    INDEX Itemid IS PRIMARY UNIQUE Itemid hg.

DEFINE TEMP-TABLE tt_itemid NO-UNDO
     FIELD butikknr AS INTE
     FIELD Itemid AS INTE
     FIELD hg     AS INTE
    INDEX butikk IS PRIMARY UNIQUE butikknr itemid hg.

/* DO ii = 1 TO 7:                                                  */
/*     CREATE tt_ok.                                                */
/*     ASSIGN tt_ok.ItemId = INT(ENTRY(ii,"1,2,3,4,4,4,9"))         */
/*            tt_ok.hg     = INT(ENTRY(ii,"70,71,72,73,74,79,78")). */
/* END.                                                             */
DO ii = 1 TO 24:
    CREATE tt_ok.
    ASSIGN tt_ok.ItemId = INT(ENTRY(ii," 1, 1, 2, 3, 4, 4, 4, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20"))
           tt_ok.hg     = INT(ENTRY(ii,"70,77,71,72,73,74,79,80,86,82,85,83,78,84,87,88,81,75,76,77,79,80,73,89")).
END.
cFilename = SESSION:TEMP-DIR + "itemid_hg_analys.txt".
OS-DELETE value(cFilename).
ASSIGN dTilDato = TODAY - 1
       dFraDato = dTilDato - 6.

FOR EACH butiker NO-LOCK:
    FOR EACH kasse WHERE kasse.butik = butiker.butik NO-LOCK:
        DO dDato = dFraDato TO dTilDato:
            B_LINJE:
            FOR EACH bonglinje WHERE bonglinje.butikknr = butiker.butik AND
                                     bonglinje.gruppenr = 1 AND
                                     bonglinje.kassenr  = kasse.kassenr AND
                                     bonglinje.dato = ddato NO-LOCK:
                lOK = FALSE.
                IF bonglinje.originaldata BEGINS "ITEMID" AND bonglinje.makulert = FALSE THEN DO:
                    cItemId = TRIM(ENTRY(2,originaldat,"=")).
                    RELEASE tt_OK.
                    lOK = FALSE.
/*                     IF CAN-DO("1,2,3,4,9",cItemId) THEN DO: */
                    IF CAN-DO("1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20",cItemId) THEN DO:
                        FIND tt_OK WHERE tt_OK.itemid = INT(cItemId) AND
                                         tt_OK.hg     = bonglinje.hovedgr NO-ERROR.
                        IF AVAIL tt_OK THEN
                            lOK = TRUE.
                    END.
/*                     ELSE IF CAN-DO("5,6,7,8,10",cItemId) THEN DO:                         */
/*                         lOK = TRUE.                                                       */
/*                         FIND FIRST tt_OK WHERE tt_OK.hg     = bonglinje.hovedgr NO-ERROR. */
/*                         IF AVAIL tt_OK THEN                                               */
/*                             lOK = FALSE.                                                  */
/*                     END.                                                                  */
/*                     IF NOT AVAIL tt_OK THEN DO: */
/*                     IF lOK = FALSE AND CAN-DO("70,71,72,73,74,78,79",STRING(bonglinje.hovedgr)) THEN DO: */
                    IF lOK = FALSE AND CAN-DO("70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89",STRING(bonglinje.hovedgr)) THEN DO:
                        FIND tt_itemid WHERE tt_itemid.butikknr = butiker.butik AND
                                         tt_itemid.itemid = INT(cItemId) AND
                                         tt_itemid.hg = bonglinje.hovedgr NO-ERROR.
                        IF NOT AVAIL tt_itemid THEN DO:
                            CREATE tt_itemId.
                            ASSIGN tt_itemid.butikknr = butiker.butik
                                   tt_itemid.itemid = INT(cItemId).
                                   tt_itemid.hg = bonglinje.hovedgr.
                        END.
                    END.
                END.
            END.
        END.
    END.
END.
IF CAN-FIND(FIRST tt_itemid) THEN DO:
    OUTPUT TO value(cFilename).
    PUT UNFORMATTED "ItemId Hganalys" CHR(9) STRING(dFraDato) " - " dTilDato SKIP
                    "Sation" CHR(9) "ItemId" CHR(9) "Hg" SKIP.
    FOR EACH tt_itemid:
        PUT UNFORMATTED tt_itemid.butikknr CHR(9) tt_itemid.itemid CHR(9) tt_itemid.hg SKIP.
    END.
    OUTPUT CLOSE.
END.

IF SEARCH(cFilename) = ? THEN
    cFilename = "".
FIND Syspara WHERE SysPara.SysHId         = 50 AND
                           SysPara.SysGr  = 50 AND
                           SysPara.ParaNr = 1  NO-LOCK NO-ERROR.
IF AVAIL SysPara THEN
    ASSIGN cMailhub    = Syspara.parameter1.

FIND Syspara WHERE SysPara.SysHId         = 50 AND
                           SysPara.SysGr  = 50 AND
                           SysPara.ParaNr = 40 NO-LOCK NO-ERROR.
IF AVAIL syspara THEN
    cMailFrom = SysPara.parameter1.
cMailTo = "per-ake.stalberg@preem.se;g.jansbo@polygon.se;ken1@polygonsoftware.no;bssupport@preem.se".
/* cMailTo = "ken1@polygonsoftware.no". */

    RUN prssmtpmailv5_7a.p (
/*mailhub    */   cMailhub,
/*EmailTo    */   cMailTo,
/*EmailFrom  */   cMailFrom,
/*EmailCC    */   "",
/*Attachments*/   IF cFilename = "" THEN "" ELSE ENTRY(NUM-ENTRIES(cFileName,"\"),cFileName,"\"),
/*LocalFiles */   IF cFilename = "" THEN "" ELSE cFileName,
/*Subject    */   "Item - Hg analys " + STRING(TODAY) + (IF cFilename = "" THEN "- Alla OK" ELSE ""),
/*Body       */   "",
/*MIMEHeader */   "",
/*BodyType   */   "",
/*Importance */   0,
/*L_DoAUTH   */   FALSE,
/*C_AuthType */   "",
/*C_User     */   "",
/*C_Password */   "",
/*oSuccessful*/  OUTPUT lMailOK,
/*vMessage   */  OUTPUT cMessage) NO-ERROR.
IF cFileName <> "" THEN
    OS-DELETE VALUE(cFileName).
QUIT.
