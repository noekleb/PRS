DEFINE INPUT  PARAMETER ipiOrdreNr    AS INTEGER    NO-UNDO.
DEFINE INPUT  PARAMETER ipcFilkatalog AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER ipcFilnavn    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cOutputFil            AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cOutputFiltmp         AS CHARACTER  NO-UNDO.
DEFINE VARIABLE eloggordrenr          AS INTEGER    NO-UNDO.
DEFINE VARIABLE ii                    AS INTEGER    NO-UNDO.
DEFINE VARIABLE cButikkListe          AS CHARACTER  NO-UNDO.
DEFINE TEMP-TABLE TT_Ordre        NO-UNDO LIKE Ordre.
DEFINE TEMP-TABLE TT_ArtBas       NO-UNDO LIKE ArtBas.
DEFINE TEMP-TABLE TT_BestHode     NO-UNDO LIKE BestHode.
DEFINE TEMP-TABLE TT_LevBas       NO-UNDO LIKE LevBas.
DEFINE TEMP-TABLE TT_Farg         NO-UNDO LIKE Farg.
DEFINE TEMP-TABLE TT_Sasong       NO-UNDO LIKE Sasong.
DEFINE TEMP-TABLE TT_StrType      NO-UNDO LIKE StrType.
DEFINE TEMP-TABLE TT_StrKonv      NO-UNDO LIKE StrKonv.
DEFINE TEMP-TABLE TT_Produsent    NO-UNDO LIKE Produsent.
DEFINE TEMP-TABLE TT_Varemerke    NO-UNDO LIKE Varemerke.
DEFINE TEMP-TABLE TT_VareBehHode  NO-UNDO LIKE VareBehHode.
DEFINE TEMP-TABLE TT_VareBehLinje NO-UNDO LIKE VareBehLinje.
DEFINE TEMP-TABLE TT_PakkeLinje   NO-UNDO LIKE PakkeLinje.

DEFINE VARIABLE cTMPStrKonv  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cAltLevNr    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cLevSAnt     AS CHARACTER  NO-UNDO.
DEFINE TEMP-TABLE TT_Bilderegister NO-UNDO LIKE Bilderegister.
DEFINE TEMP-TABLE TT_BildeData     NO-UNDO LIKE Bildedata.

DEF VAR piLoop AS INT NO-UNDO.
DEFINE VARIABLE iAntall AS INTEGER NO-UNDO.

DEFINE BUFFER bBestHode FOR BestHode.
/* MESSAGE PROGRAM-NAME(1) SKIP                                                       */
/*   "Om vi kan undvika att lägga ut hur många poster som kommer vore det fint," SKIP */
/*   "alltså i headerrecorden" SKIP                                                   */
/*     "MArkere Order och Beställningar som exporterade"                              */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK.                                             */
RELEASE VareBehHode.
/* ORDRE: */
FIND ordre WHERE ordre.ordrenr = ipiOrdreNr EXCLUSIVE NO-WAIT NO-ERROR.

IF NOT AVAIL Ordre OR ordre.ordrestatus < 3 OR ordre.ordrestatus > 4 THEN
    RETURN IF NOT AVAIL Ordre THEN "AVBRYT" ELSE "OK".

FOR EACH butiker WHERE butiker.clButikkNr = Ordre.CL NO-LOCK:
    ASSIGN cButikkListe = cButikkListe + (IF cButikkListe = "" THEN "" ELSE ",") + STRING(butiker.butik).
END.
IF cButikkListe = "" THEN
    ASSIGN cButikkListe = STRING(Ordre.CL).
ASSIGN ipcFilKatalog = RIGHT-TRIM(ipcFilKatalog,"\")
       ipcFilKataLog = RIGHT-TRIM(ipcFilKatalog,"\") + "\"
       cOutputFil    = ipcFilKatalog + ipcFilnavn + "." + STRING(Ordre.CL).

IF SEARCH(cOutputFil) <> ? THEN
    OS-DELETE VALUE(cOutputFil).
ASSIGN cOutputFilTMP = ipcFilKatalog + "TMP" + ipcFilnavn.

OUTPUT TO VALUE(cOutputFilTMP).
/* Test om vi tillhör en varebok skall göras när vi vet vilka besthode ordern tillhör */
IF Ordre.VareBehNr <> 0 THEN
    FIND VareBehHode OF Ordre.
IF AVAIL VareBehHode THEN DO:
    CREATE TT_VareBehHode.
    BUFFER-COPY VareBehHode TO TT_VareBehHode.
    ASSIGN TT_VareBehHode.VareBehNotat = REPLACE(REPLACE(TT_VareBehHode.VareBehNotat,CHR(10),CHR(1)),CHR(13),CHR(2))
           TT_VareBehHode.butikkliste  = cbutikkliste.
    FIND CURRENT TT_VareBehHode.
    EXPORT "H" "VareBehHode" 1 "1.0" 1.
    EXPORT TT_VareBehHode.
END.

/* Lägg ut ordre */
CREATE TT_Ordre.
BUFFER-COPY Ordre TO TT_Ordre.
ASSIGN TT_Ordre.Notat = REPLACE(REPLACE(TT_Ordre.Notat,CHR(10),CHR(1)),CHR(13),CHR(2)).
FIND CURRENT TT_Ordre.
EXPORT "H" "Ordre" 1 "1.0" 1.
EXPORT TT_Ordre.

/* gör klart för registerdump */

FOR EACH BestHode WHERE BestHode.Ordrenr = Ordre.OrdreNr NO-LOCK BREAK BY BestHode.BestNr:
    /* Vi lägger ut artikeln före utlägg av besthode och varebeh */
    IF NOT CAN-FIND(TT_ArtBas OF BestHode) THEN DO:
        FIND ArtBas OF BestHode NO-LOCK NO-ERROR.
        IF AVAIL ArtBas THEN DO:
            IF AVAIL VareBehHode AND NOT CAN-FIND(TT_VareBehLinje OF VareBehHode WHERE TT_VareBehLinje.ArtikkelNr = ArtBas.ArtikkelNr) THEN DO:
                FIND VareBehLinje OF VareBehHode WHERE VareBehLinje.ArtikkelNr = ArtBas.ArtikkelNr NO-LOCK NO-ERROR.
                IF AVAIL VareBehLinje THEN DO:
                    CREATE TT_VareBehLinje.
                    BUFFER-COPY VareBehLinje TO TT_VareBehLinje NO-ERROR.
                    IF ERROR-STATUS:ERROR THEN
                        DELETE TT_VareBehLinje.
                    ELSE DO:
                        ASSIGN TT_VareBehLinje.LinjeMerknad = REPLACE(REPLACE(TT_VareBehLinje.LinjeMerknad,CHR(10),CHR(1)),CHR(13),CHR(2))
                               TT_VareBehLinje.KjedeInnkPris = 0
                               TT_VareBehLinje.KjedeRab% = 0.
                        EXPORT "H" "VareBehLinje" 1 "1.0" 1.
                        EXPORT TT_VareBehLinje.
                    END.
                END.
            END.
            IF ArtBas.Pakke THEN DO:
                FOR EACH PakkeLinje OF ArtBas NO-LOCK:
                    piLoop = piLoop + 1.
                    CREATE tt_PakkeLinje.
                    BUFFER-COPY PakkeLinje TO TT_PakkeLinje.
                END.
                EXPORT "H" "PakkeLinje" 1 "1.0" 1.
                FOR EACH TT_PakkeLinje:
                    EXPORT TT_Pakkelinje.
                END.
            END.
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
    END.
    IF AVAIL VareBehHode THEN DO:
        FIND VareBehBestHode WHERE VareBehBestHode.VareBehNr = VareBehHode.VareBehNr AND 
                                   VareBehBestHode.BestNr    = BestHode.BestNr NO-LOCK NO-ERROR.
        IF AVAIL VareBehBestHode THEN DO:
            EXPORT "H" "VareBehBestHode" 1 "1.0" 1.
            EXPORT VareBehBestHode.
            IF CAN-FIND(FIRST VareBehBestLinje OF VareBehBestHode) THEN DO:
                EXPORT "H" "VareBehBestLinje" 1 "1.0" 1.
                FOR EACH VareBehBestLinje OF VareBehBestHode NO-LOCK.
                    EXPORT VareBehBestLinje.
                END.
            END.
        END.
    END.
    CREATE TT_BestHode.
    BUFFER-COPY BestHode TO TT_BestHode.
    ASSIGN TT_BestHode.Merknad = REPLACE(REPLACE(TT_BestHode.Merknad,CHR(10),CHR(1)),CHR(13),CHR(2)).
    FIND CURRENT TT_BestHode.
    EXPORT "H" "BestHode" 1 "1.0" 1.
    EXPORT TT_BestHode.
    IF CAN-FIND(FIRST BestLinje OF BestHode) THEN DO:
        EXPORT "H" "BestLinje" 1 "1.0" 1.
        FOR EACH BestLinje OF BestHode NO-LOCK:
            EXPORT BestLinje.
        END.
    END.
    IF CAN-FIND(FIRST BestPris OF BestHode) THEN DO:
        EXPORT "H" "BestPris" 1 "1.0" 1.
        FOR EACH Bestpris OF BestHode NO-LOCK:
            EXPORT BestPris.
        END.
    END.
    IF CAN-FIND(FIRST BestSort OF BestHode) THEN DO:
        EXPORT "H" "BestSort" 1 "1.0" 1.
        FOR EACH BestSort OF BestHode NO-LOCK:
            EXPORT BestSort.
        END.
    END.
    IF CAN-FIND(FIRST FriButik OF BestHode) THEN DO:
        EXPORT "H" "FriButik" 1 "1.0" 1.
        FOR EACH FriButik OF BestHode NO-LOCK:
            EXPORT FriButik.
        END.
    END.
    IF CAN-FIND(FIRST BestKasse OF BestHode) THEN DO:
        EXPORT "H" "BestKasse" 1 "1.0" 1.
        FOR EACH BestKasse OF BestHode NO-LOCK:
            EXPORT BestKasse.
        END.
    END.
    IF CAN-FIND(FIRST BestStr OF BestHode) THEN DO:
        EXPORT "H" "BestStr" 1 "1.0" 1.
        FOR EACH BestStr OF BestHode NO-LOCK:
            EXPORT BestStr.
        END.
    END.
    FIND bBestHode WHERE ROWID(bBestHode) = ROWID(BestHode) EXCLUSIVE NO-WAIT NO-ERROR.
    IF AVAIL bBestHode THEN DO:
        ASSIGN bBestHode.sendtButikkFlagg = TRUE
               bBestHode.sendtButikkDato  = TODAY
               bBestHode.sendtButikkTid   = TIME.
    END.
END.
IF CAN-FIND(FIRST TT_BestHode OF TT_Ordre) THEN DO:
    ASSIGN Ordre.sendtButikkFlagg = TRUE
           Ordre.sendtButikkDato  = TODAY
           Ordre.sendtButikkTid   = TIME.
END.
PUT UNFORMATTED "." SKIP. /* Vi avslutar med '.' för lättare inläsning */
OUTPUT CLOSE.
OS-RENAME VALUE(cOutputFilTMP) VALUE(cOutputFil). 
RETURN IF CAN-FIND(FIRST TT_BestHode OF TT_Ordre) THEN "OK" ELSE "AVBRYT".
