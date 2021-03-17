DEF VAR piBokforingsNr AS INT NO-UNDO.

DEF BUFFER bBokforingsbilag FOR Bokforingsbilag.

FOR EACH Dags_Rap NO-LOCK WHERE
    Dags_Rap.Butik > 0
  BREAK BY Dags_Rap.butik
        BY Dags_Rap.Dato:

    IF NOT CAN-FIND(FIRST Bokforingsbilag WHERE
                    Bokforingsbilag.ButikkNr = Dags_Rap.butik AND
                    Bokforingsbilag.OmsetningsDato = Dags_Rap.Dato) THEN
    DO:
        FIND LAST bBokforingsbilag NO-LOCK WHERE
            bBokforingsbilag.ButikkNr = Dags_Rap.butik AND
            bBokforingsbilag.Aar      = YEAR(Dags_Rap.Dato) NO-ERROR.
        IF AVAILABLE bBokforingsbilag THEN
            piBokforingsNr = bBokforingsbilag.BokforingsNr + 1.
        ELSE
            piBokforingsNr = 1.

        CREATE Bokforingsbilag.
        ASSIGN
            Bokforingsbilag.ButikkNr           = Dags_Rap.Butik
            Bokforingsbilag.Aar                = YEAR(Dags_Rap.Dato)
            Bokforingsbilag.OmsetningsDato     = Dags_Rap.Dato
            BokforingsBilag.BokforingsNr       = piBokforingsNr
            Bokforingsbilag.GodkjentFlagg      = TRUE
            Bokforingsbilag.GodkjentDato       = TODAY
            Bokforingsbilag.GodkjentAv         = USERID("SkoTex")
            Bokforingsbilag.GodkjentTid        = TIME 
            .
    END.

    DISPLAY
        Dags_Rap.butik
        Dags_Rap.Dato
        .

END.
