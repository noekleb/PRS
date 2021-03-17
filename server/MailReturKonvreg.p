DEFINE INPUT  PARAMETER cMailRowids AS CHARACTER   NO-UNDO.

DEFINE VARIABLE cMailTo AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cTekst  AS CHARACTER   NO-UNDO.

DEFINE VARIABLE ii    AS INTEGER     NO-UNDO.
DEFINE VARIABLE rID   AS ROWID       NO-UNDO.
DEFINE VARIABLE cBody AS CHARACTER   NO-UNDO.
{syspara.i 2 3 10 cTekst}

IF cTekst <> "1" THEN
    RETURN. /* egentligen onödogt. Vi kommer aldrig hit om <> "1" */
{syspar2.i 2 3 10 cMailTo}
IF cMailTo = "" THEN
    RETURN.
DO ii = 1 TO NUM-ENTRIES(cMailRowids,CHR(1)):
    rID = TO-ROWID(ENTRY(ii,cMailRowids,CHR(1))) NO-ERROR.
    FIND bonglinje WHERE ROWID(bonglinje) = rID NO-LOCK NO-ERROR.
    IF AVAIL bonglinje THEN DO:
        FIND butiker WHERE butiker.butik = bonglinje.butikknr NO-LOCK NO-ERROR.
         
        cBody = "Returnerad datum" + CHR(9)  + ": " + STRING(Bonglinje.dato) /* + "-" + STRING(bonglinje.transtid,"HH:MM:SS") */ + " \n" + 
                "Returkvittonr" + CHR(9) + CHR(9) + ": " + STRING(Bonglinje.bongnr)   + " \n" + 
                 Bonglinje.DivInfo   + " \n" + 
                "Återlämnad i butik/kassa" + CHR(9) + ": " + STRING(Bonglinje.Butikknr) + " \n" + 
                "NYTT Vg/Löpnr/Stlk" + CHR(9) + ": " + STRING(bonglinje.varegr) + "/" + STRING(bonglinje.lopenr) + "/" + bonglinje.storrelse.

        RUN sendmail_tsl.p ("KONVREGRETUR","Returnerad vara som behöver märkas om","",cBody ,Butiker.ePostAdresse,"") NO-ERROR.
    END.
END.

