DEFINE INPUT  PARAMETER iplPkSdlId    AS DEC        NO-UNDO.
DEFINE INPUT  PARAMETER ipcFilkatalog AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER ipcFilnavn    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cOutputFil            AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cOutputFiltmp         AS CHARACTER  NO-UNDO.
DEFINE VARIABLE eloggordrenr          AS INTEGER    NO-UNDO.
DEFINE VARIABLE ii                    AS INTEGER    NO-UNDO.
DEFINE VARIABLE cButikkListe          AS CHARACTER  NO-UNDO.

DEFINE TEMP-TABLE TT_LevBas       NO-UNDO LIKE LevBas.
DEFINE TEMP-TABLE TT_Farg         NO-UNDO LIKE Farg.
DEFINE TEMP-TABLE TT_Sasong       NO-UNDO LIKE Sasong.
DEFINE TEMP-TABLE TT_StrType      NO-UNDO LIKE StrType.
DEFINE TEMP-TABLE TT_StrKonv      NO-UNDO LIKE StrKonv.
DEFINE TEMP-TABLE TT_Produsent    NO-UNDO LIKE Produsent.
DEFINE TEMP-TABLE TT_Varemerke    NO-UNDO LIKE Varemerke.
DEFINE TEMP-TABLE TT_ArtBas       NO-UNDO LIKE ArtBas.

DEFINE TEMP-TABLE TT_Bilderegister NO-UNDO LIKE Bilderegister.
DEFINE TEMP-TABLE TT_BildeData     NO-UNDO LIKE Bildedata.

/* Pakkseddel. Mottaksdel er uaktuelt å sende over */
DEFINE TEMP-TABLE TT_PkSdlHode    NO-UNDO LIKE PkSdlHode.
DEFINE TEMP-TABLE TT_PkSdlLinje   NO-UNDO LIKE PkSdlLinje.
DEFINE TEMP-TABLE TT_PkSdlPris    NO-UNDO LIKE PkSdlPris.

DEFINE VARIABLE cTMPStrKonv  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cAltLevNr    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cLevSAnt     AS CHARACTER  NO-UNDO.

DEFINE VARIABLE iAntall AS INTEGER NO-UNDO.

/* MESSAGE "Fra HK_PkSdl_to_Butikk.p" SKIP       */
/*         "iplPkSdlId    "  iplPkSdlId   SKIP   */
/*         "ipcFilkatalog "  ipcFilkatalog  SKIP */
/*         "ipcFilnavn    "  ipcFilnavn     SKIP */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK.        */

/* PAKKSEDDEL: */
FIND PkSdlHode WHERE PkSdlHode.PkSdlId = iplPkSdlId EXCLUSIVE NO-WAIT NO-ERROR.

IF NOT AVAIL PkSdlHode OR PkSdlHode.PkSdlStatus <> 6 THEN
    RETURN IF NOT AVAIL PkSdlHode THEN "AVBRYT" ELSE "OK".

FOR EACH butiker WHERE butiker.clButikkNr = PkSdlHode.CL NO-LOCK:
    ASSIGN cButikkListe = cButikkListe + (IF cButikkListe = "" THEN "" ELSE ",") + STRING(butiker.butik).
END.
IF cButikkListe = "" THEN
    ASSIGN cButikkListe = STRING(PkSdlHode.CL).
ASSIGN ipcFilKatalog = RIGHT-TRIM(ipcFilKatalog,"\")
       ipcFilKataLog = RIGHT-TRIM(ipcFilKatalog,"\") + "\"
       cOutputFil    = ipcFilKatalog + ipcFilnavn + "." + STRING(PkSdlHode.CL).

IF SEARCH(cOutputFil) <> ? THEN
    OS-DELETE VALUE(cOutputFil).
ASSIGN cOutputFilTMP = ipcFilKatalog + "TMP" + ipcFilnavn.

OUTPUT TO VALUE(cOutputFilTMP).

/* Legger ut pakkseddel */
CREATE TT_PkSdlHode.
BUFFER-COPY PkSdlHode TO TT_PkSdlHode.
ASSIGN 
    TT_PkSdlHode.MeldingFraLev = REPLACE(REPLACE(TT_PkSdlHode.MeldingFraLev,CHR(10),CHR(1)),CHR(13),CHR(2))
    TT_PkSdlHode.Merknad       = REPLACE(REPLACE(TT_PkSdlHode.Merknad,CHR(10),CHR(1)),CHR(13),CHR(2))
    .
FIND CURRENT TT_PkSdlHode EXCLUSIVE-LOCK.
EXPORT "H" "PkSdlHode" 1 "1.0" 1.
EXPORT TT_PkSdlHode.

/* gör klart för registerdump */

FOR EACH PkSdlLinje OF PkSdlHode NO-LOCK 
    BREAK BY PkSdlLinje.ArtikkelNr:
    /* Vi lägger ut artikeln före utlägg av pakkseddel */
    IF NOT CAN-FIND(TT_ArtBas OF PkSdlLinje) THEN DO:
        FIND ArtBas OF PkSdlLinje NO-LOCK NO-ERROR.
        IF AVAIL ArtBas THEN DO:
            IF NOT CAN-FIND(TT_LevBas OF ArtBas) THEN DO:
                FIND LevBas OF ArtBas NO-LOCK NO-ERROR.
                IF AVAIL LevBas THEN DO:
                    CREATE TT_LevBas.
                    BUFFER-COPY LevBas TO TT_LevBas.
                    ASSIGN TT_LevBas.Notat = REPLACE(REPLACE(TT_LevBas.Notat,CHR(10),CHR(1)),CHR(13),CHR(2)).
                    FIND CURRENT TT_LevBas.
                    EXPORT "H" "LevBas" 1 "1.0" 1.
                    EXPORT TT_LevBas.
                    IF CAN-FIND(FIRST LevSort OF LevBas) THEN DO:
                        EXPORT "H" "LevSort" 1 "1.0" 1.
                        FOR EACH LevSort OF LevBas NO-LOCK:
                            EXPORT LevSort.
                            IF CAN-FIND(FIRST LevSAnt OF LevSort) THEN DO:
                                ASSIGN cTMPStrKonv = "".
                                EXPORT "H" "LevSAnt" 1 "1.0" 1.
                                FOR EACH LevSAnt OF LevSort NO-LOCK.
                                    EXPORT LevSAnt.
                                    IF NOT CAN-FIND(TT_StrKonv WHERE TT_StrKonv.storl = TRIM(LevSant.sostorl)) THEN DO:
                                        ASSIGN cTMPStrKonv = cTMPStrKonv + (IF cTMPStrKonv <> "" THEN "," ELSE "") + TRIM(LevSant.sostorl).
                                    END.
                                END.
                                IF cTMPStrKonv <> "" THEN DO:
                                    EXPORT "H" "StrKonv" 1 "1.0" 1.
                                    DO ii = 1 TO NUM-ENTRIES(cTMPStrKonv):
                                        FIND StrKonv WHERE TRIM(StrKonv.Storl) = ENTRY(ii,cTMPStrKonv) NO-LOCK NO-ERROR.
                                        IF AVAIL StrKonv THEN
                                        DO:
                                            EXPORT StrKonv.
                                            IF NOT CAN-FIND(TT_StrKonv WHERE TT_StrKonv.storl = ENTRY(ii,cTMPStrKonv)) THEN
                                            DO:
                                                CREATE TT_StrKonv.
                                                ASSIGN 
                                                    TT_StrKonv.Storl   = ENTRY(ii,cTMPStrKonv)
                                                    TT_StrKonv.StrKode = StrKonv.StrKode NO-ERROR.
                                            END.
                                            IF AVAILABLE TT_StrKonv THEN RELEASE TT_StrKonv.
                                        END.
                                    END.
                                END.
                            END.
                        END.
                    END.
                END.
            END.
            IF ArtBas.Farg > 0 AND NOT CAN-FIND(TT_Farg OF ArtBas) THEN DO:
                FIND Farg OF ArtBas NO-LOCK NO-ERROR.
                IF AVAIL Farg THEN DO:
                    EXPORT "H" "Farg" 1 "1.0" 1.
                    EXPORT Farg.
                END.
                CREATE TT_Farg.
                ASSIGN TT_Farg.Farg = ArtBas.Farg.
                RELEASE TT_Farg.
            END.
            IF NOT CAN-FIND(TT_Sasong OF ArtBas) THEN DO:
                FIND Sasong OF ArtBas NO-LOCK NO-ERROR.
                IF AVAIL Sasong THEN DO:
                    EXPORT "H" "Sasong" 1 "1.0" 1.
                    EXPORT Sasong.
                END.
                CREATE TT_Sasong.
                ASSIGN TT_Sasong.Sasong = ArtBas.Sasong.
                RELEASE TT_Sasong.
            END.
            IF ArtBas.StrTypeID > 2 AND NOT CAN-FIND(TT_StrType OF ArtBas) THEN DO:
                FIND StrType OF ArtBas NO-LOCK NO-ERROR.
                IF AVAIL StrType THEN DO:
                    EXPORT "H" "StrType" 1 "1.0" 1.
                    EXPORT StrType.
                    /* TN 2/6-08 Lagt inn FIRST for at StrTStr skal bli med ut. */
                    IF CAN-FIND(FIRST StrTstr OF StrType) THEN DO:
                        EXPORT "H" "StrTstr" 1 "1.0" 1.
                        
                        ASSIGN cTMPStrKonv = ""
                               iAntall     = 0.
                        STRTSTR:
                        FOR EACH StrTstr OF StrType NO-LOCK:
                            EXPORT StrTStr.
                            IF NOT CAN-FIND(TT_StrKonv WHERE TT_StrKonv.storl = TRIM(StrTStr.sostorl)) THEN DO:
                                ASSIGN cTMPStrKonv = cTMPStrKonv + (IF cTMPStrKonv <> "" THEN "," ELSE "") + TRIM(StrTStr.sostorl).
                            END.
                            iantall = iantall + 1.
                            IF iAntall >= 48 THEN LEAVE STRTSTR.
                        END. /* STRTSTR */
                        /*
                        ASSIGN cTMPStrKonv = "".
                        FOR EACH StrTstr OF StrType NO-LOCK:
                            EXPORT StrTStr.
                            IF NOT CAN-FIND(TT_StrKonv WHERE TT_StrKonv.storl = TRIM(StrTStr.sostorl)) THEN DO:
                                ASSIGN cTMPStrKonv = cTMPStrKonv + (IF cTMPStrKonv <> "" THEN "," ELSE "") + TRIM(StrTStr.sostorl).
                            END.
                        END.
                        */
                        IF cTMPStrKonv <> "" THEN DO:
                            EXPORT "H" "StrKonv" 1 "1.0" 1.
                            DO ii = 1 TO NUM-ENTRIES(cTMPStrKonv):
                                FIND StrKonv WHERE TRIM(StrKonv.Storl) = ENTRY(ii,cTMPStrKonv) NO-LOCK NO-ERROR.
                                IF AVAIL StrKonv THEN
                                DO:
                                    EXPORT StrKonv.
                                    IF NOT CAN-FIND(TT_StrKonv WHERE TT_StrKonv.storl = ENTRY(ii,cTMPStrKonv)) THEN
                                    DO:
                                        CREATE TT_StrKonv.
                                        ASSIGN 
                                            TT_StrKonv.Storl   = ENTRY(ii,cTMPStrKonv)
                                            TT_StrKonv.StrKode = StrKonv.StrKode NO-ERROR.
                                    END.
                                    IF AVAILABLE TT_StrKonv THEN RELEASE TT_StrKonv.
                                END.
                            END.
                        END.
                    END.
                END.
                CREATE TT_StrType.
                ASSIGN TT_StrType.StrTypeID = ArtBas.StrTypeID.
            END.
            IF ArtBas.ProdNr > 0 AND NOT CAN-FIND(TT_Produsent OF ArtBas) THEN DO:
                FIND Produsent OF ArtBas NO-LOCK NO-ERROR.
                IF AVAIL Produsent THEN DO:
                    CREATE TT_Produsent.
                    BUFFER-COPY Produsent TO TT_Produsent.
                    ASSIGN TT_Produsent.Notat     = REPLACE(REPLACE(TT_Produsent.Notat,CHR(10),CHR(1)),CHR(13),CHR(2)).
                    EXPORT "H" "Produsent" 1 "1.0" 1.
                    EXPORT TT_Produsent.
                    RELEASE TT_Produsent.
                END.
            END.
            IF ArtBas.VMId > 0 AND NOT CAN-FIND(TT_Varemerke WHERE TT_Varemerke.VMId = ArtBas.VMId) THEN DO:
                FIND Varemerke WHERE Varemerke.VMId = ArtBas.VMId NO-LOCK NO-ERROR.
                IF AVAIL Varemerke THEN DO:
                    CREATE TT_Varemerke.
                    BUFFER-COPY Varemerke TO TT_Varemerke.
                    FIND CURRENT TT_Varemerke.
                    EXPORT "H" "Varemerke" 1 "1.0" 1.
                    EXPORT TT_Varemerke.
                END.
            END.
            IF CAN-FIND(FIRST AltLevBas WHERE AltLevBas.ArtikkelNr = ArtBas.ArtikkelNr AND AltLevBas.LevNr > 0) THEN DO:
                ASSIGN cAltLevNr = "".
                EXPORT "H" "AltLevBas" 1 "1.0" 1.
                FOR EACH AltLevBas WHERE AltLevBas.Artikkelnr = ArtBas.ArtikkelNr AND 
                                         AltLevBas.LevNr > 0 AND
                                         CAN-FIND(LevBas OF AltLevBas) NO-LOCK.
                    IF NOT CAN-FIND(TT_LevBas OF AltLevBas) THEN
                        ASSIGN cAltLevNr = cAltLevNr + (IF cAltLevNr <> "" THEN "," ELSE "") + STRING(AltLevBas.LevNr).
                    EXPORT AltLevBas.
                END.
                IF cAltLevNr <> "" THEN DO:
                    EXPORT "H" "LevBas" 1 "1.0" 1.
                    DO ii = 1 TO NUM-ENTRIES(cAltLevNr):
                        FIND LevBas WHERE LevBas.LevNr = INT(ENTRY(ii,cAltLevNr)) NO-LOCK NO-ERROR.
                        IF AVAIL LevBas THEN DO:
                            CREATE TT_LevBas.
                            BUFFER-COPY LevBas TO TT_LevBas.
                            ASSIGN TT_LevBas.Notat = REPLACE(REPLACE(TT_LevBas.Notat,CHR(10),CHR(1)),CHR(13),CHR(2)).
                            FIND CURRENT TT_LevBas.
                            EXPORT TT_LevBas.
                        END.
                    END.
                END.
            END.
            IF ArtBas.BildNr > 0 AND NOT CAN-FIND(TT_Bilderegister WHERE TT_BildeRegister.BildNr = ArtBas.BildNr) THEN DO:
                FIND Bilderegister WHERE Bilderegister.BildNr = ArtBas.BildNr NO-LOCK NO-ERROR.
                IF AVAIL Bilderegister THEN DO:
                    CREATE TT_Bilderegister.
                    BUFFER-COPY Bilderegister TO TT_Bilderegister.
                    FIND CURRENT TT_Bilderegister.
                    EXPORT "H" "Bilderegister" 1 "1.0" 1.
                    EXPORT TT_Bilderegister.
                    IF CAN-FIND(FIRST BildeData OF Bilderegister) THEN
                    DO:
                        EXPORT "H" "BildeData" 1 "1.0" 1.
                        FOR EACH BildeData OF BildeRegister NO-LOCK:
                            EXPORT BildeData.
                        END.
                    END.
                END.
            END.
            CREATE TT_ArtBas.
            BUFFER-COPY ArtBas TO TT_ArtBas.
            ASSIGN TT_ArtBas.Notat     = REPLACE(REPLACE(TT_ArtBas.Notat,CHR(10),CHR(1)),CHR(13),CHR(2))
                   TT_ArtBas.Varefakta = REPLACE(REPLACE(TT_ArtBas.Varefakta,CHR(10),CHR(1)),CHR(13),CHR(2))
                   TT_ArtBas.KjedeInnkPris = 0 
                   TT_ArtBas.KjedeRab% = 0.
            FIND CURRENT TT_ArtBas.
            EXPORT "H" "ArtBas" 1 "1.0" 1.
            EXPORT TT_ArtBas.
            IF CAN-FIND(FIRST ArtPris OF ArtBas) THEN DO:
                EXPORT "H" "ArtPris" 1 "1.0" 1.
                FOR EACH ArtPris OF ArtBas NO-LOCK.
                    EXPORT ArtPris.
                END.
            END.
        END.
        IF CAN-FIND(FIRST StrekKode OF ArtBas WHERE NOT StrekKode.Kode BEGINS "02") THEN DO:
            EXPORT "H" "StrekKode" 1 "1.0" 1.
            FOR EACH StrekKode OF ArtBas WHERE NOT StrekKode.Kode BEGINS "02" NO-LOCK:
                EXPORT StrekKode.
            END.
        END.

        /* Pakkseddelpris */
        IF CAN-FIND(FIRST PkSdlPris WHERE 
                    PkSdlPris.PkSdlId    = PkSdlLinje.PkSdlId AND 
                    PkSdlPris.ArtikkelNr = PkSdlLinje.ArtikkelNr) THEN DO:

            EXPORT "H" "PkSdlPris" 1 "1.0" 1.
            FOR EACH PkSdlPris WHERE 
                    PkSdlPris.PkSdlId    = PkSdlLinje.PkSdlId AND 
                    PkSdlPris.ArtikkelNr = PkSdlLinje.ArtikkelNr NO-LOCK:
                EXPORT PkSdlPris.
            END.
        END.
    END.

    /* Pakkseddellinje */
    CREATE TT_PkSdlLinje.
    BUFFER-COPY PkSdlLinje TO TT_PkSdlLinje.
    FIND CURRENT TT_PkSdlLinje.
    IF FIRST-OF(PkSdlLinje.ArtikkelNr) THEN
        EXPORT "H" "PkSdlLinje" 1 "1.0" 1.
    EXPORT TT_PkSdlLinje.
END.
IF CAN-FIND(FIRST TT_PkSdlLinje OF TT_PkSdlHode) THEN DO:
    ASSIGN PkSdlHode.PkSdlStatus = 90.
END.

PUT UNFORMATTED "." SKIP. /* Vi avslutar med '.' för lättare inläsning */
OUTPUT CLOSE.
OS-RENAME VALUE(cOutputFilTMP) VALUE(cOutputFil). 
RETURN IF CAN-FIND(FIRST TT_PkSdlLinje OF TT_PkSdlHode) THEN "OK" ELSE "AVBRYT".
