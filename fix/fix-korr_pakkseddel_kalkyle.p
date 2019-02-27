DEF VAR iAntMRab  AS INT NO-UNDO.
DEF VAR iAntPksdl AS INT NO-UNDO.
DEF VAR iAntPrisMRab  AS INT NO-UNDO.
DEF VAR iAntPrisPksdl AS INT NO-UNDO.
DEF VAR iAntNye AS INT NO-UNDO.
DEF VAR iAntOverstyrt AS INT NO-UNDO.
DEF VAR cArtListe AS CHAR NO-UNDO.
DEF VAR iAntMottak AS INT NO-UNDO.
DEF VAR cTekst AS CHAR NO-UNDO.
DEF VAR iBatchNr AS INT NO-UNDO.
DEF VAR iTransNr AS INT NO-UNDO.
DEF VAR bKorrSalg AS LOG NO-UNDO.
DEF VAR piSeqNr AS INT NO-UNDO.
DEF VAR cRecIdLst AS CHAR NO-UNDO.
DEF VAR piLoop AS INT NO-UNDO.
DEF VAR cFilNavn AS CHAR NO-UNDO.

DEF VAR iCL AS INT NO-UNDO.
DEF VAR piTid AS INT NO-UNDO.

DEF BUFFER bufTransLogg FOR TransLogg.
DEF BUFFER buf2TransLogg FOR TransLogg.
DEF BUFFER clButiker FOR Butiker.
{syspara.i 5 1 1 iCL INT}
FIND clButiker NO-LOCK WHERE
    clButiker.Butik = iCL NO-ERROR.

ASSIGN
    piTid = TIME
    cFilNavn = "PksdlKorr" + 
               replace(STRING(TODAY),"/","-") + "_" +  
               replace(STRING(TIME,"HH:MM:SS"),":","-") + ".csv".

DEF STREAM Ut.

CURRENT-WINDOW:WIDTH = 350.

/* Batch for TransLogg */
run batchlogg.p (program-name(1),
                 "Pakkseddel KORR " +
                 string(today) +
                 " " +
                 string(time,"HH:MM") +
                 " " +
                 userid("dictdb"),
                 output iBatchNr).

OUTPUT STREAM Ut TO VALUE(cFilNavn).

PUT STREAM ut UNFORMATTED
    "Dato/Tid;"
    "PkSdlLinje.PkSdlId;"
    "PkSdlHode.EkstId;" 
    "PkSdlHode.SendtDato;"
    "PkSdlMottak.MottattDato;"
    "PkSdlLinje.ButikkNr;"
    "PkSdlLinje.ArtikkelNr;"
    "PkSdlLinje.LevKod;"    
    "PkSdlLinje.Beskr;"     
    "PkSdlLinje.LevFargKod;"
    "StrKonv.Storl;"
    "PkSdlLinje.AntLevert;"
    "PkSdlPris.Rab1%;"
    "ArtPris.Rab1%[1];"
    "ArtPris.Rab1Kr[1];"
    "BRabKr;"
    "ArtPris.VareKost[1];"
    "Feil rabkr;"
    "MvaKr;" 
    "TransLogg.Antall;" 
    "TransLogg.Pris;" 
    "KKOST;"
    SKIP.


FOR EACH PkSdlPris NO-LOCK WHERE
    PkSdlPris.Rab1% > 0 AND
    PkSdlPris.OverstyrPris = FALSE,
    EACH PkSdlLinje OF PkSdlPris NO-LOCK /*WHERE
         PkSdlLinje.ArtikkelNr = 9909448*/ ,
    EACH PkSdlHode OF PkSdlLinje NO-LOCK WHERE
        /*PkSdlHode.EkstId = "311298" AND
        PkSdlHode.PkSdlNr = "8836" */,
    EACH PkSdlMottak OF PkSdlHode NO-LOCK,
    FIRST StrKonv  OF PkSdlLinje,
    FIRST TransLogg NO-LOCK WHERE
         TransLogg.ArtikkelNr = PkSdlLinje.ArtikkelNr AND
         Translogg.Dato       = PkSdlMottak.MottattDato AND
         /*TransLogg.Tid       >= PkSdlMottak.ETid AND*/
         TransLogg.Butik      = PkSdlLinje.butikkNr AND
         TransLogg.TTId       = 5 AND
         TransLogg.Storl      = StrKonv.Storl
    BREAK BY PkSdlLinje.ArtikkelNr 
          BY PkSdlLinje.ButikkNr
          BY PksdlLinje.StrKode
          BY PkSdlLinje.PkSdlId:

       
    FIND ArtBas OF PkSdlLinje NO-LOCK.
    FIND ArtPris OF ArtBas EXCLUSIVE-LOCK WHERE
        ArtPris.ProfilNr = clButiker.ProfilNr NO-ERROR.

    /* Nullstiller pr. artikkel */
    IF FIRST-OF(PkSdlLinje.StrKode
                ) THEN
        ASSIGN
        bKorrSalg = FALSE
        .

    /* Forutsetter at kalkyle på pakkseddel er korrekt.                            */
    /* Artikler som ligger med feil i kalkyle i artikkelregisteret skal korrigeres.*/
    /* 1 - Rabatten sjekkes mot pakkseddelrabatten                                 */
    /* 2 - Den beregnede rab1kr sjekkes i forhold til rabatt fra pakkseddel.       */
    /* 3 - Det sjekkes om mvakr er beregnet feil.                                  */
    IF  PkSdlPris.NyRab1%        <> ArtPris.Rab1%[1] OR
        PkSdlPris.NyInnkjopsPris <> ArtPris.InnkjopsPris[1] THEN
        ASSIGN
            ArtPris.InnkjopsPris[1] = PkSdlPris.NyInnkjopsPris
            ArtPris.Rab1%[1]        = PkSdlPris.NyRab1%.

    /* Korrigerere feil utregnet rabatt 1 */
    IF (ArtPris.Rab1Kr[1] <> ROUND(((ArtPris.Innkjopspris[1] * ArtPris.Rab1%[1]) / 100),2)) THEN
        ASSIGN
            ArtPris.Rab1Kr[1]       =  ArtPris.InnkjopsPris[1] * ArtPris.Rab1%[1] / 100
            ArtPris.VareKost[1]     =  ArtPris.InnkjopsPris[1] - ArtPris.Rab1Kr[1].

    /* Rekalkulerer */
    REKALKULER:
    DO:
      ASSIGN
        /* Nullstilles */
        ArtPris.Rab2%[1]        = 0
        ArtPris.Rab2Kr[1]       = 0
        ArtPris.Rab3%[1]        = 0
        ArtPris.Rab3Kr[1]       = 0
        ArtPris.Frakt[1]        = 0
        ArtPris.Frakt%[1]       = 0
        ArtPris.DivKostKr[1]    = 0
        ArtPris.DivKost%[1]     = 0
        
        /* Beregnes */
        ArtPris.MvaKr[1]        =  ArtPris.Pris[1] - ArtPris.Pris[1] / (1 + ArtPris.Mva%[1] / 100)
        ArtPris.Rab1Kr[1]       =  ArtPris.InnkjopsPris[1] * ArtPris.Rab1%[1] / 100
        ArtPris.VareKost[1]     =  ArtPris.InnkjopsPris[1] - ArtPris.Rab1Kr[1]
        ArtPris.DbKr[1]         =  ArtPris.Pris[1] - ArtPris.MvaKr[1] - ArtPris.VareKost[1]
        ArtPris.Db%[1]          =  round(ArtPris.DbKr[1] * 100 / (ArtPris.Pris[1] - ArtPris.MvaKr[1]),2)
        .
    END. /* REKALKULER */

    PUT STREAM ut UNFORMATTED
        STRING(TODAY) + " " + STRING(TIME,"HH:MM:SS") + ";"
        PkSdlLinje.PkSdlId ";"
        PkSdlHode.EkstId  ";" 
        PkSdlHode.SendtDato   ";"
        PkSdlMottak.MottattDato  ";"
        PkSdlLinje.ButikkNr  ";"
        PkSdlLinje.ArtikkelNr  ";"
        PkSdlLinje.LevKod  ";"    
        PkSdlLinje.Beskr  ";"     
        PkSdlLinje.LevFargKod  ";"
        StrKonv.Storl  ";"
        PkSdlLinje.AntLevert  ";"
        PkSdlPris.Rab1%  ";"
        ArtPris.Rab1%[1]  ";"
        ArtPris.Rab1Kr[1]  ";"
        ROUND(((ArtPris.Innkjopspris[1] * ArtPris.Rab1%[1]) / 100),2)  ";"
        ArtPris.VareKost[1]  ";"
        (IF ArtPris.Rab1Kr[1] <> 
         ROUND(((ArtPris.Innkjopspris[1] * ArtPris.Rab1%[1]) / 100),2)
         THEN "*" ELSE ""
         )  ";"
        (IF 
         ArtPris.MvaKr[1] <> round(ArtPris.Pris[1] * ( ArtPris.Mva%[1] / (100 + ArtPris.Mva%[1])),2)  
            THEN "*" ELSE "")  ";" 
        TransLogg.Antall  ";" 
        TransLogg.Pris  ";" 
        (IF (TransLogg.Pris <> PkSdlPris.NyVarekost) THEN "KKOST" ELSE "")  ";"
        SKIP.

    /*
    DISPLAY
        PkSdlLinje.PkSdlId
        /*
        PkSdlHode.PkSdlNr FORMAT "x(5)" COLUMN-LABEL "Pksdlnr"
         */
        PkSdlHode.EkstId  FORMAT "x(6)" COLUMN-LABEL "Ekstid"
       
        PkSdlHode.SendtDato
        PkSdlMottak.MottattDato
        /*PkSdlHode.PkSdlId*/
        PkSdlLinje.ButikkNr FORMAT ">>9" COLUMN-LABEL "But"
        PkSdlLinje.ArtikkelNr FORMAT ">>>>>>9" COLUMN-LABEL "Art.nr"
        PkSdlLinje.LevKod     FORMAT "x(15)"
        PkSdlLinje.Beskr      FORMAT "x(20)"
        PkSdlLinje.LevFargKod FORMAT "x(15)"
        StrKonv.Storl FORMAT "x(4)" COLUMN-LABEL "Str"
        PkSdlLinje.AntLevert FORMAT ">>9" COLUMN-LABEL "Levert"
        /*ArtPris.Innkjopspris[1] */
        PkSdlPris.Rab1%
        ArtPris.Rab1%[1]
        ArtPris.Rab1Kr[1] COLUMN-LABEL "Rab1Kr"
        ROUND(((ArtPris.Innkjopspris[1] * ArtPris.Rab1%[1]) / 100),2) COLUMN-LABEL "BRab1Kr"
        ArtPris.VareKost[1]
        (IF ArtPris.Rab1Kr[1] <> 
         ROUND(((ArtPris.Innkjopspris[1] * ArtPris.Rab1%[1]) / 100),2)
         THEN "*" ELSE ""
         ) COLUMN-LABEL "Feil rabkr"
        (IF 
         ArtPris.MvaKr[1] <> round(ArtPris.Pris[1] * ( ArtPris.Mva%[1] / (100 + ArtPris.Mva%[1])),2)  
            THEN "*" ELSE "") FORMAT "x(3)" COLUMN-LABEL "MVA"
        /*PkSdlMottak.BrukerId FORMAT "x(6)" COLUMN-LABEL "Bruker"*/
        TransLogg.Antall FORMAT "->>9" COLUMN-LABEL "Ant"
        TransLogg.Pris FORMAT ">>>9.99" COLUMN-LABEL "Pris"
        "KKOST" WHEN (TransLogg.Pris <> PkSdlPris.NyVarekost)
        WITH WIDTH 350.
    */

    /* Korreksjon av translogg varekjøp.             */
    /* Moposter varekjøp og poster med ny vvarekost. */
    IF Translogg.Pris <> PkSdlPris.NyVarekost AND
        TransLogg.Butik = PkSdlLinje.ButikkNr THEN
    KORR_VAREMOTTAK:
    DO:
        /* Setter transaksjonsnummer  */
        if iTransNr = 0 then
          DO:
            find last buf2TransLogg where
              buf2TransLogg.Butik = TransLogg.Butik
              USE-INDEX TransLogg no-error.
            if available buf2TransLogg then
              iTransNr = buf2TransLogg.TransNr + 1.
            else
              iTransNr = 1.
          END.
        else
          iTransNr = iTransNr + 1.
        
        /* Motposteringen */
        CREATE bufTransLogg.
        RUN getSeqNr(OUTPUT piSeqNr).
        BUFFER-COPY TransLogg TO bufTransLogg
            ASSIGN
            bufTransLogg.BatchNr     = iBatchNr
            bufTransLogg.TransNr     = iTransNr
            bufTransLogg.SeqNr       = piSeqNr
            bufTransLogg.Antall      = TransLogg.Antall * -1
            bufTransLogg.Postert     = FALSE
            bufTransLogg.PostertDato = ?
            bufTransLogg.PostertTid  = 0
            .
        RELEASE bufTransLogg.        
        
        /* Ny postering med riktig varekost. */
        CREATE bufTransLogg.
        RUN getSeqNr(OUTPUT piSeqNr).
        BUFFER-COPY TransLogg TO bufTransLogg
            ASSIGN
            bufTransLogg.BatchNr     = iBatchNr
            bufTransLogg.TransNr     = iTransNr
            bufTransLogg.SeqNr       = piSeqNr
            bufTransLogg.Pris        = PkSdlPris.NyVarekost
            bufTransLogg.VVarekost   = PkSdlPris.NyVarekost
            bufTransLogg.Postert     = FALSE
            bufTransLogg.PostertDato = ?
            bufTransLogg.PostertTid  = 0
            .
        RELEASE bufTransLogg.

        /* Setter vvlager for artikkel og butikk */
        FIND Lager EXCLUSIVE-LOCK WHERE
            Lager.ArtikkelNr = PkSdlPris.ArtikkelNr AND
            Lager.butik      = PkSdlLinje.butik NO-ERROR.
        IF AVAILABLE Lager THEN
            ASSIGN
            Lager.VVarekost = PkSdlPris.NyVarekost.


        /* Korreksjon av salgstransaksjoner */
        IF bKorrSalg = FALSE THEN
        KORR_SALGSTRANSER:
        DO:

            /* ---------------------------- SALGSTRANSAKSJONER -------------------------- */
            cRecIdLst = "".
            /* Salgstransaksjoner samme dag. */
            SAMME_DAG:
            FOR EACH BufTransLogg NO-LOCK WHERE
                bufTransLogg.ArtikkelNr  = PkSdlLinje.ArtikkelNr AND
                bufTransLogg.Dato        = TransLogg.Dato AND
                bufTransLogg.Tid        >= TransLogg.Tid AND
                bufTransLogg.Butik       = TransLogg.Butik AND
                bufTransLogg.TTId        = 1 AND /* Varesalg */
                bufTransLogg.Storl       = TransLogg.Storl 
                USE-INDEX OppslagDatoTid:

                IF NOT CAN-DO(cRecIdLst,STRING(RECID(bufTransLogg))) THEN
                    ASSIGN
                    cRecidLst = cRecidLst 
                                + (IF cRecidLst = "" THEN "" ELSE ",")
                                + STRING(RECID(bufTransLogg)).
            END. /* SAMME_DAG */

            IF cRecIdLst <> "" THEN
            SAMME_DAG_OPPRETT:
            DO piLoop = 1 TO NUM-ENTRIES(cRecIdLst):
                FIND bufTransLogg NO-LOCK WHERE
                    RECID(bufTransLogg) = int(ENTRY(piLoop,cRecIdLst)).

                /* Motposteringen */
                CREATE buf2TransLogg.
                RUN getSeqNr(OUTPUT piSeqNr).
                BUFFER-COPY bufTransLogg TO buf2TransLogg
                    ASSIGN
                    buf2TransLogg.BatchNr     = iBatchNr
                    buf2TransLogg.TransNr     = iTransNr
                    buf2TransLogg.SeqNr       = piSeqNr
                    buf2TransLogg.Postert     = FALSE
                    buf2TransLogg.PostertDato = ?
                    buf2TransLogg.PostertTid  = 0
                    .
                ASSIGN
                    buf2TransLogg.Antall = buf2TransLogg.Antall * -1.

                RELEASE buf2TransLogg.        
                
                /* Ny postering med riktig varekost. */
                CREATE buf2TransLogg.
                RUN getSeqNr(OUTPUT piSeqNr).
                BUFFER-COPY bufTransLogg TO buf2TransLogg
                    ASSIGN
                    buf2TransLogg.BatchNr     = iBatchNr
                    buf2TransLogg.TransNr     = iTransNr
                    buf2TransLogg.SeqNr       = piSeqNr
                    buf2TransLogg.VVarekost   = PkSdlPris.NyVarekost
                    buf2TransLogg.Postert     = FALSE
                    buf2TransLogg.PostertDato = ?
                    buf2TransLogg.PostertTid  = 0
                    .
                RELEASE buf2TransLogg.
            END. /* SAMME_DAG_OPPRETT */

            cRecidLSt = "".

            /* Salgstransaksjoner senere dager. */
            SENERE_DAGER:
            FOR EACH BufTransLogg NO-LOCK WHERE
                bufTransLogg.ArtikkelNr  = PkSdlLinje.ArtikkelNr AND
                bufTransLogg.Dato        > TransLogg.Dato AND
                bufTransLogg.Tid         > 0 AND
                bufTransLogg.Butik       = TransLogg.Butik AND
                bufTransLogg.TTId        = 1 AND /* Varesalg */
                bufTransLogg.Storl       = TransLogg.Storl 
                USE-INDEX OppslagDatoTid:
                IF NOT CAN-DO(cRecIdLst,STRING(RECID(bufTransLogg))) THEN
                    ASSIGN
                    cRecidLst = cRecidLst 
                                + (IF cRecidLst = "" THEN "" ELSE ",")
                                + STRING(RECID(bufTransLogg)).
            END. /* SENERE_DAGER */

            IF cRecIdLst <> "" THEN
            SENERE_DAGER_OPPRETT:
            DO piLoop = 1 TO NUM-ENTRIES(cRecIdLst):
                FIND bufTransLogg NO-LOCK WHERE
                    RECID(bufTransLogg) = int(ENTRY(piLoop,cRecIdLst)).

                /* Motposteringen */
                CREATE buf2TransLogg.
                RUN getSeqNr(OUTPUT piSeqNr).
                BUFFER-COPY bufTransLogg TO buf2TransLogg
                    ASSIGN
                    buf2TransLogg.BatchNr     = iBatchNr
                    buf2TransLogg.TransNr     = iTransNr
                    buf2TransLogg.SeqNr       = piSeqNr
                    buf2TransLogg.Postert     = FALSE
                    buf2TransLogg.PostertDato = ?
                    buf2TransLogg.PostertTid  = 0
                    .
                ASSIGN
                    buf2TransLogg.Antall = buf2TransLogg.Antall * -1.

                RELEASE buf2TransLogg.        
                
                /* Ny postering med riktig varekost. */
                CREATE buf2TransLogg.
                RUN getSeqNr(OUTPUT piSeqNr).
                BUFFER-COPY bufTransLogg TO buf2TransLogg
                    ASSIGN
                    buf2TransLogg.BatchNr     = iBatchNr
                    buf2TransLogg.TransNr     = iTransNr
                    buf2TransLogg.SeqNr       = piSeqNr
                    buf2TransLogg.VVarekost   = PkSdlPris.NyVarekost
                    buf2TransLogg.Postert     = FALSE
                    buf2TransLogg.PostertDato = ?
                    buf2TransLogg.PostertTid  = 0
                    .
                RELEASE buf2TransLogg.
            END. /* SENERE_DAGER_OPPRETT */


            /* ---------------------------- RETURTRANSAKSJONER -------------------------- */
            cRecIdLst = "".
            /* Salgstransaksjoner samme dag. */
            SAMME_DAG:
            FOR EACH BufTransLogg NO-LOCK WHERE
                bufTransLogg.ArtikkelNr  = PkSdlLinje.ArtikkelNr AND
                bufTransLogg.Dato        = TransLogg.Dato AND
                bufTransLogg.Tid        >= TransLogg.Tid AND
                bufTransLogg.Butik       = TransLogg.Butik AND
                bufTransLogg.TTId        = 10 AND /* Varesalg */
                bufTransLogg.Storl       = TransLogg.Storl 
                USE-INDEX OppslagDatoTid:

                IF NOT CAN-DO(cRecIdLst,STRING(RECID(bufTransLogg))) THEN
                    ASSIGN
                    cRecidLst = cRecidLst 
                                + (IF cRecidLst = "" THEN "" ELSE ",")
                                + STRING(RECID(bufTransLogg)).
            END. /* SAMME_DAG */

            IF cRecIdLst <> "" THEN
            SAMME_DAG_OPPRETT:
            DO piLoop = 1 TO NUM-ENTRIES(cRecIdLst):
                FIND bufTransLogg NO-LOCK WHERE
                    RECID(bufTransLogg) = int(ENTRY(piLoop,cRecIdLst)).

                /* Motposteringen */
                CREATE buf2TransLogg.
                RUN getSeqNr(OUTPUT piSeqNr).
                BUFFER-COPY bufTransLogg TO buf2TransLogg
                    ASSIGN
                    buf2TransLogg.BatchNr     = iBatchNr
                    buf2TransLogg.TransNr     = iTransNr
                    buf2TransLogg.SeqNr       = piSeqNr
                    buf2TransLogg.Postert     = FALSE
                    buf2TransLogg.PostertDato = ?
                    buf2TransLogg.PostertTid  = 0
                    .
                ASSIGN
                    buf2TransLogg.Antall = buf2TransLogg.Antall * -1.

                RELEASE buf2TransLogg.        
                
                /* Ny postering med riktig varekost. */
                CREATE buf2TransLogg.
                RUN getSeqNr(OUTPUT piSeqNr).
                BUFFER-COPY bufTransLogg TO buf2TransLogg
                    ASSIGN
                    buf2TransLogg.BatchNr     = iBatchNr
                    buf2TransLogg.TransNr     = iTransNr
                    buf2TransLogg.SeqNr       = piSeqNr
                    buf2TransLogg.VVarekost   = PkSdlPris.NyVarekost
                    buf2TransLogg.Postert     = FALSE
                    buf2TransLogg.PostertDato = ?
                    buf2TransLogg.PostertTid  = 0
                    .
                RELEASE buf2TransLogg.
            END. /* SAMME_DAG_OPPRETT */

            cRecidLSt = "".

            /* Salgstransaksjoner senere dager. */
            SENERE_DAGER:
            FOR EACH BufTransLogg NO-LOCK WHERE
                bufTransLogg.ArtikkelNr  = PkSdlLinje.ArtikkelNr AND
                bufTransLogg.Dato        > TransLogg.Dato AND
                bufTransLogg.Tid         > 0 AND
                bufTransLogg.Butik       = TransLogg.Butik AND
                bufTransLogg.TTId        = 10 AND /* Varesalg */
                bufTransLogg.Storl       = TransLogg.Storl 
                USE-INDEX OppslagDatoTid:
                IF NOT CAN-DO(cRecIdLst,STRING(RECID(bufTransLogg))) THEN
                    ASSIGN
                    cRecidLst = cRecidLst 
                                + (IF cRecidLst = "" THEN "" ELSE ",")
                                + STRING(RECID(bufTransLogg)).
            END. /* SENERE_DAGER */

            IF cRecIdLst <> "" THEN
            SENERE_DAGER_OPPRETT:
            DO piLoop = 1 TO NUM-ENTRIES(cRecIdLst):
                FIND bufTransLogg NO-LOCK WHERE
                    RECID(bufTransLogg) = int(ENTRY(piLoop,cRecIdLst)).

                /* Motposteringen */
                CREATE buf2TransLogg.
                RUN getSeqNr(OUTPUT piSeqNr).
                BUFFER-COPY bufTransLogg TO buf2TransLogg
                    ASSIGN
                    buf2TransLogg.BatchNr     = iBatchNr
                    buf2TransLogg.TransNr     = iTransNr
                    buf2TransLogg.SeqNr       = piSeqNr
                    buf2TransLogg.Postert     = FALSE
                    buf2TransLogg.PostertDato = ?
                    buf2TransLogg.PostertTid  = 0
                    .
                ASSIGN
                    buf2TransLogg.Antall = buf2TransLogg.Antall * -1.

                RELEASE buf2TransLogg.        
                
                /* Ny postering med riktig varekost. */
                CREATE buf2TransLogg.
                RUN getSeqNr(OUTPUT piSeqNr).
                BUFFER-COPY bufTransLogg TO buf2TransLogg
                    ASSIGN
                    buf2TransLogg.BatchNr     = iBatchNr
                    buf2TransLogg.TransNr     = iTransNr
                    buf2TransLogg.SeqNr       = piSeqNr
                    buf2TransLogg.VVarekost   = PkSdlPris.NyVarekost
                    buf2TransLogg.Postert     = FALSE
                    buf2TransLogg.PostertDato = ?
                    buf2TransLogg.PostertTid  = 0
                    .
                RELEASE buf2TransLogg.
            END. /* SENERE_DAGER_OPPRETT */

            /* Korr av salgstranser skal bare kjøres en gang pr. artikkel og da for alle salgstransene. */
            ASSIGN
                bKorrSalg = TRUE.
        END.
    END. /* KORR_VAREMOTTAK */

END.

PUT STREAM Ut UNFORMATTED 
    "*** Ferdig..." SKIP.

OUTPUT STREAM Ut CLOSE.

/* Flagger batchen klar for oppdatering. */
run batchstatus.p (iBatchNr, 2).
QUIT.

PROCEDURE getSeqNr:
    DEF OUTPUT PARAMETER iSeqNr AS INT NO-UNDO.
    DEF BUFFER qTransLogg FOR translogg.

    FIND LAST qTransLogg NO-LOCK WHERE
        qTransLogg.Butik = PkSdlLinje.ButikkNr AND
        qTransLogg.TransNr = iTransNr 
        USE-INDEX TransLogg NO-ERROR.
    IF AVAILABLE qTransLogg THEN
        iSeqNr = qTransLogg.SeqNr + 1.
    ELSE
        iSeqNr + 1.

END PROCEDURE.


