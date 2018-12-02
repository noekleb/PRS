CURRENT-WINDOW:WIDTH = 300.

DEF VAR cFilNavn   AS CHAR NO-UNDO.
DEF VAR lVareBehNr AS DEC NO-UNDO.
DEF VAR cSep       AS CHAR NO-UNDO.
DEF VAR cLinje     AS CHAR NO-UNDO.
DEF VAR cMedlemmer AS CHAR NO-UNDO.
DEF VAR cMedlLst   AS CHAR NO-UNDO.
DEF VAR cTekst     AS CHAR NO-UNDO.
DEF VAR cTekst2    AS CHAR FORMAT "x(15)" NO-UNDO.
DEF VAR iEntry     AS INT  NO-UNDO.
DEF VAR lVareHNr   AS DEC  NO-UNDO.
DEF VAR iButikkNr  AS INT  NO-UNDO.
DEF VAR cEAN       AS CHAR FORMAT "x(15)" NO-UNDO.
DEF VAR cLevKod    AS CHAR FORMAT "x(25)"  NO-UNDO.
DEF VAR lAntall    AS DEC  NO-UNDO.
DEF VAR dDato      AS DATE FORMAT "99/99/9999" NO-UNDO.
DEF VAR iKontrollSum AS INT NO-UNDO.
DEF VAR iKontrDump   AS INT NO-UNDO.

DEF VAR oiWeek1    AS INT  FORMAT ">>>>>9"  NO-UNDO.
DEF VAR dLevDato1  AS DATE   NO-UNDO.

DEF STREAM Inn.
DEF STREAM Ut.

DEF BUFFER bufArtBas FOR ArtBas.

ASSIGN
    lVareHNr   = 9000010
    cSep       = ';' /*CHR(9)*/
    cFilNavn   = "Q:\Appdir\tn\Sport1ForhåndFra010109.csv"
    cMedlemmer = "Odda Sport og Fritid AS," +
                 "Bjørn Myhre Sport AS," +
                 "Sporten Voss AS," +
                 "Sport 1 Jessheim AS," +
                 "Sport 1 Superstore Sentrum," +
                 "Christiania Sport AS," +
                 "Sport 1 Kongsberg AS," +
                 "Falksenteret AS," +
                 "Axel Bruun Sport AS," +
                 "Persbu AS," +
                 "Sport 1 Åsane AS," +
                 "Christiania Sport Paleèt," +
                 "Mo Sport AS," +
                 "Sporten Brattvåg AS," +
                 "Sport 1 Lillesand AS," +
                 "Sportshuset Mo I Rana AS," +
                 "Standard Sport AS," +
                 "Sport 1 Sortland," +
                 "Sport 1 - Varebørsen Elverum AS," +
                 "Erling A Fardal," +
                 "Gutta på Sporten AS," +
                 "M. Holum Sport AS," +
                 "Sport 1 Seljord," +
                 "Sporten Stoa AS," +
                 "Sport 1 Rygge Storsenter," +
                 "Sport 1 Notodden  avd. Tuven," +
                 "Xsport Bodø AS," +
                 "Dybvik Sport AS," +
                 "Parabolflua Sport AS," +
                 "Christiania Sport Paleèt AS," +
                 "Sport 1 Superstore Sørlandsparken," +
                 "Sportsdepotet AS," + 
                 "L. Barth Sport AS," + 
                 "Flisa Sport n AS," + 
                 "Sport 1 Kirkenes AS," + 
                 "Sport 1 Hvaltorvet AS," + 
                 "Sarpsborg Sport AS," +
                 "Chatlet Sandane AS," + 
                 "Outdoor Bergen AS," +
                 "Sport 1 Sjøvegan A/S," +
                 "JK Sport AS," +
                 "Focus Sport A/S," +
                 "Sport 1 Porsgrunn AS," +
                 "Sport 1 Melhus," +
                 "Sport 1 Varebørsen Elverum AS," +
                 "Eiker Sport AS," +
                 "Valdres Sport A/S," +
                 "Albert Bøe AS," +
                 "Sportshuset AS," +
                 "Sport 1 Notodden  avd. Sentrum," +
                 "Amundsen Sport AS," +
                 "Husnes Sport AS," + 
                 "Sanderød Sport og Fritid AS," +
                 "Arkaden Sport og Fritid AS," +
                 "Sandberg Sport Hønefoss," + 
                 "Sport 1 Superstore Hamar AS," +
                 "Bremseth Sport AS," + 
                 "Sport 1 Sola," +
                 "Nygård Sport AS," + 
                 "Coop Orkla BA Sport 1," +
                 "Sport 1 Lillehammer"
    cMedlLst   = "167," +
                 "051," +
                 "256," +
                 "136," +
                 "551," +
                 "232," +
                 "019," +
                 "032," +
                 "243," +
                 "023," +
                 "272," +
                 "349," +
                 "209," +
                 "246," +
                 "277," +
                 "176," +
                 "187," +
                 "270," +
                 "312," +
                 "075," +
                 "211," +
                 "081," +
                 "326," +
                 "193," +
                 "325," +
                 "281," +
                 "311," +
                 "192," +
               "160," +
               "349," +
               "552," +
               "044," +
               "169," + 
               "292," + 
               "276," +
               "350," +
               "068," +
               "093," +
               "329," +
               "289," + 
               "011," +
               "184," +
               "300," +
               "340," +
               "312," +
               "178," + 
               "310," +
               "026," +
               "090," + 
               "282," +
               "001," +
               "225," +
               "067," +
               "199," +
               "336," +
               "314," +
               "083," +
               "337," +
               "213," +
               "338," + 
                 "003"
    .

INPUT STREAM Inn FROM VALUE(cFilNavn) NO-ECHO.
OUTPUT STREAM Ut TO VALUE("Bergans_error.txt").
LINJE:
REPEAT:
    IMPORT STREAM Inn UNFORMATTED
        cLinje.

    ASSIGN
        cTekst = trim(replace(ENTRY(1,cLinje,cSep),',',' '))               /* Renser , i liste */
        .
    /* Sjekker at butikken ligger i listen. */
    IF LOOKUP(cTekst,cMedlemmer) = 0 THEN
    DO:
        MESSAGE cLinje SKIP(1)
                cMedlemmer SKIP(1)
                LOOKUP(cTekst,cMedlemmer) cTekst
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        NEXT.
    END.

    ASSIGN
        cTekst2   = entry(1,trim(replace(ENTRY(7,cLinje,cSep),',',' ')),' ')  /* Dato */
        iEntry    = LOOKUP(cTekst,cMedlemmer)
        iButikkNr = int(entry(iEntry,cMedlLst))
        cLevKod   = TRIM(ENTRY(2,cLinje,cSep))
        cEAN      = TRIM(ENTRY(3,cLinje,cSep))
        lAntall   = 0
        lAntall   = dec(TRIM(ENTRY(6,cLinje,cSep))) / 100
        lAntall   = IF lAntall = ? THEN 0 ELSE lAntall
        dDato     = ?
        .
    ASSIGN
        dDato     = DATE(int(ENTRY(2,cTekst2,'.')),
                         int(ENTRY(1,cTekst2,'.')),
                         int(ENTRY(3,cTekst2,'.')))
        NO-ERROR.

    IF iButikkNr = 81 THEN
    ASSIGN
        iKontrollSum = iKontrollSum + lAntall.

    IF dDato <> ? THEN 
        RUN weeknum(dDato, OUTPUT oiWeek1).
    

    FIND Butiker NO-LOCK WHERE
        Butiker.Butik = iButikkNr NO-ERROR.

    /* Er det satt inn strekkode bruker vi denne */
    IF cEAN <> "" then DO:
        FIND Strekkode NO-LOCK WHERE
          Strekkode.Kode = cEAN NO-ERROR.
    END.
    /* Strekkode manger, slår opp på bestillingsnr. */
    ELSE DO:
        FIND FIRST Strekkode NO-LOCK WHERE
          Strekkode.BestillingsNummer = cLevKod AND 
            can-find(ArtBas OF Strekkode WHERE ArtBas.LevNr = 10) NO-ERROR.
    END.

    IF AVAILABLE Strekkode THEN
    DO:
        IF Strekkode.StrKode = 264 THEN
        DO TRANSACTION:
            FIND CURRENT Strekkode EXCLUSIVE-LOCK.
            ASSIGN
                Strekkode.StrKode = 1.
            FIND CURRENT Strekkode NO-LOCK.
        END. /* TRANSACTION */

        FIND StrKonv NO-LOCK WHERE
            StrKonv.StrKode = Strekkode.StrKode NO-ERROR.
    END.

    /* Sjekker at den finnes i vareboken */
    IF AVAILABLE Strekkode THEN
        FIND VareBehLinje NO-LOCK WHERE
          VareBehLinje.VareBehNr = lVareHNr AND
          VareBehLinje.ArtikkelNr = Strekkode.ArtikkelNr NO-ERROR.

    /* Varer som ikke ligger i varebok skal ikke med. */
    IF NOT AVAILABLE VareBehLinje THEN DO:
        NEXT.
        /*
        DISPLAY
            iButikkNr
            cTekst
            cEAN
            lAntall
            dDato
            NUM-ENTRIES(cTekst2,'.')
            cTekst2
            WITH WIDTH 300.
        */
        /*
        OUTPUT TO VALUE('Bergans_err.csv') APPEND.
        PUT UNFORMATTED cLinje SKIP.
        OUTPUT CLOSE.
        */
    END.

    /* Sjekker at artikkelen finnes */
    IF AVAILABLE Strekkode THEN
        FIND ArtBas OF Strekkode NO-LOCK NO-ERROR.

    /* Linjer med ugyldige strekkoder. */
    IF dDato = ? OR NOT AVAILABLE Strekkode OR NOT AVAILABLE VareBehLinje OR NOT AVAILABLE ArtBas THEN
    DO:
        IF ibutikkNr = 81 THEN
            iKontrDump = iKontrDump + lAntall.
        PUT STREAM Ut UNFORMATTED
            Butiker.Butik ";" 
            Butiker.ButNamn ";" 
            cLevKod ";"
            cEAN ";"
            TRIM(ENTRY(4,cLinje,cSep)) ";"
            lAntall
            SKIP.
        NEXT LINJE.
    END.

    /*
    IF AVAILABLE Strekkode AND Strekkode.StrKode = 264 THEN
    DISPLAY
        Butiker.ButNamn
        cLevKod
        cEAN 
        ENTRY(4,cLinje,cSep) FORMAT "x(40)"
        Strekkode.StrKode
        StrKonv.Storl WHEN AVAILABLE StrKonv
        ENTRY(5,cLinje,cSep) FORMAT "x(30)"
        lAntall
        dDato
        oiWeek1
        WITH width 300.
    */

    /* ------------------------  */
    /* Legger opp hovedrecord for butikken */
    IF NOT CAN-FIND(VareBehLinjeTHode WHERE
                    VareBehLinjeTHode.VareBehNr = lVareHNr AND
                    VareBehLinjeTHode.ButikkNr  = Butiker.Butik) THEN
    DO TRANSACTION:
        CREATE VareBehLinjeTHode.
        ASSIGN
            VareBehLinjeTHode.VareBehNr = lVareHNr 
            VareBehLinjeTHode.ButikkNr  = Butiker.Butik
            VareBehLinjeTHode.Godkjent  = TRUE
            .
        RELEASE VareBehLinjeTHode.
    END.

    /* legger opp variantlinjen. */
    DO TRANSACTION:
        FIND VareBehLinjeTrans EXCLUSIVE-LOCK WHERE
            VareBehLinjeTrans.VareBehNr  = lVareHNr AND
            VareBehLinjeTrans.ButikkNr   = Butiker.Butik AND
            VareBehLinjeTrans.Kode       = string(Strekkode.StrKode) AND
            VareBehLinjeTrans.ArtikkelNr = Strekkode.ArtikkelNr
            NO-ERROR.
        IF NOT AVAILABLE VareBehLinjeTrans THEN
        DO:
            CREATE VareBehLinjeTrans.
            ASSIGN
                VareBehLinjeTrans.VareBehNr  = lVareHNr 
                VareBehLinjeTrans.ButikkNr   = Butiker.Butik 
                VareBehLinjeTrans.Kode       = string(VareBehLinjeTrans.ArtikkelNr) + string(Strekkode.StrKode,"999") 
                VareBehLinjeTrans.StrKode    = Strekkode.StrKode 
                VareBehLinjeTrans.ArtikkelNr = Strekkode.ArtikkelNr
                VareBehLinjeTrans.LevDato1   = dDato
                VareBehLinjeTrans.LevDato2   = dDato
                VareBehLinjeTrans.LevDato3   = dDato
                VareBehLinjeTrans.LevDato4   = dDato
                VareBehLinjeTrans.LevUke1    = oiWeek1
                VareBehLinjeTrans.LevUke2    = oiWeek1
                VareBehLinjeTrans.LevUke3    = oiWeek1
                VareBehLinjeTrans.LevUke4    = oiWeek1
                VareBehLinjeTrans.GodkjentBestilling   = TRUE
                VareBehLinjeTrans.RegistrertBestilling = TRUE
                VareBehLinjeTrans.SeqNr      = 0 /* Det var ikke lagt opp pakker på noen varer på Bergans. */
                .
        END.
        ASSIGN
            VareBehLinjeTrans.Bestilt1 = VareBehLinjeTrans.Bestilt1 + lAntall
            .

    END. /* TRANSACTION */
    /* -------------------------- */
END.

OUTPUT STREAM ut CLOSE.
INPUT STREAM Inn CLOSE.

MESSAGE "iKontrollSum" iKontrollSum SKIP
        "iKontrDump" iKontrDump
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
