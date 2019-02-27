/* Rutine for oppdatering av fraktfrie leveringsdager. */

DEF VAR cButNrListe AS CHAR EXTENT 5 NO-UNDO.
DEF VAR cButMangler AS CHAR          NO-UNDO.
DEF VAR cBLst       AS CHAR          NO-UNDO.
DEF VAR cLevNrListe AS CHAR          NO-UNDO.
DEF VAR cLevMangler AS CHAR          NO-UNDO.
DEF VAR piLoop1     AS INT           NO-UNDO.
DEF VAR piLoop2     AS INT           NO-UNDO.
DEF VAR piLoop3     AS INT           NO-UNDO.
DEF VAR piButikkNr  AS INT           NO-UNDO.
DEF VAR piLevNr     AS INT           NO-UNDO.
DEF VAR piUkedag    AS INT           NO-UNDO.
DEF VAR piTid       AS INT           NO-UNDO.

ASSIGN
    cLevNrListe = "001,006,010,011,013,018,019,031,037,038,039,042,044,045,048,049,052,054,056,057,059,066,"
                  + "068,069,070,072,076,108,112,157,158,159,163,169,173,189,191,197,209,212,213,215,223,300,"
                  + "413,421,427,432,441,442,443,447,451,463,465,468,475,480,483,487,488,490,491,492,493,494,"
                  + "495,496,497,499,500,513,558,633,908,914,919,924,928,961,036,064,074,208,915,921"
    piTid       = 50400 /* 14:00 */
    /* Man */ cButNrListe[1] = "001,003,006,007,011,016,020,023,044,053,055,056,081,101,116,126,130,133,136,151,167,183,193,197,199,209,213,248,254,258,270,277,280,281,282,284,286,294,314,327"
    /* Tir */ cButNrListe[2] = "008,012,015,018,030,034,036,052,068,069,085,100,122,131,159,178,187,196,204,205,225,232,235,243,256,257,261,265,292,310,322"
    /* Ons */ cButNrListe[3] = "021,028,032,040,051,059,063,067,079,127,154,160,163,211,212,222,233,247,253,271,272,276,289,300,304,305,312"
    /* Tor */ cButNrListe[4] = "019,048,076,090,164,169,170,176,180,192,229,263,268,285,295,296,297,299,301,303,311,316,317,319,321,323,324,325,326"
    /* Fre */ cButNrListe[5] = "014,024,029,031,041,042,075,083,093,094,105,109,119,155,184,191,195,208,226,246,264,273,287,288,298,307,315"
    .

/* Rydder opp rasket */
FOR EACH DefaultLevDato:
    DELETE DefaultLevDato.
END.

/* Legger opp leverandørliste hvis denne er blank */
IF cLevNrListe = "" THEN
DO:
    FOR EACH LevBas NO-LOCK WHERE
        LevBas.LevNr > 0 AND
        LevBas.LevNr <= 999:
        IF NOT CAN-DO(cLevNrListe,STRING(LevBAs.LevNr,"999")) THEN
            cLevNrListe = cLevNrListe + 
                          (IF cLevNrListe = "" THEN "" ELSE ",") +
                          STRING(LevBAs.LevNr,"999").
    END.
END.

/* Løper igjennom ukedagene. */
UKEDAGLOOP:
DO piLoop1 = 1 TO 5:
    ASSIGN
        cBLst    = cButNrListe[piLoop1]
        piUkedag = piLoop1 + 1
        .
    BUTIKKLOOP:
    DO piLoop2 = 1 TO NUM-ENTRIES(cBLst):
        ASSIGN
            piButikkNr = int(ENTRY(piLoop2,cBLst))
            .
        /* Logger butikker som ikke finnes i databasen */
        IF NOT CAN-FIND(Butiker WHERE
                        Butiker.Butik = piButikkNr) THEN
        DO:
            cButMangler = cButMangler + 
                          (IF cButMangler = "" THEN "" ELSE ",") +
                          trim(STRING(piButikkNr,"zzz999")).
            NEXT BUTIKKLOOP.
        END.

        LEVLOOP:
        DO piLoop3 = 1 TO NUM-ENTRIES(cLevNrListe):
            ASSIGN
                piLevNr = int(ENTRY(piLoop3,cLevNrListe))
                .
            /* Logger leverandører som ikke finnes i databasen */
            IF NOT CAN-FIND(LevBas WHERE
                            LevBas.LevNr = piLevNr) THEN
            DO:
                cLevMangler = cLevMangler + 
                              (IF cLevMangler = "" THEN "" ELSE ",") +
                              STRING(piLevNr,"999").
                NEXT LEVLOOP.
            END.


/*             MESSAGE                                */
/*                 piButikkNr                         */
/*                 piLevNr                            */
/*                 piUkedag                           */
/*                 piTid                              */
/*                 VIEW-AS ALERT-BOX INFO BUTTONS OK. */

            IF NOT CAN-FIND(DefaultLevDato WHERE
                            DefaultLevDato.ButikkNr = piButikkNr AND
                            DefaultLevDato.LevNr    = piLevNr) THEN
            DO TRANSACTION:
                CREATE DefaultLevDato.
                ASSIGN
                  DefaultLevDato.ButikkNr = piButikkNr
                  DefaultLevDato.LevNr    = piLevNr
                  DefaultLevDato.Ukedag   = piUkedag  /* Amerikansk ukedag begynner på søndag */
                  DefaultLevDato.Tid      = piTid
                  .
            END. /* TRANSACTION */

        END. /* LEVLOOP */
    END. /* BUTIKKLOOP */
END. /* UKEDAG */

IF cLevMangler <> "" THEN
    MESSAGE "Leverandører som ikke finnes i databasen: (Antall = " + string(num-entries(cLevMangler)) + "):" SKIP
            cLevMangler
            VIEW-AS ALERT-BOX.

IF cButMangler <> "" THEN
    MESSAGE "Butikker som ikke finnes i databasen: (Antall = " + string(num-entries(cButMangler)) + "):" SKIP
            cButMangler
            VIEW-AS ALERT-BOX.




