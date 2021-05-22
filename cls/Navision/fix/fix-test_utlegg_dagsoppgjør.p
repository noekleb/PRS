DEF VAR cTekst AS CHAR NO-UNDO.

FOR EACH Bokforingsbilag EXCLUSIVE-LOCK WHERE 
    Bokforingsbilag.OmsetningsDato = 05/21/2021:
    ASSIGN 
        BokforingsBilag.SendtDato     = ?
        BokforingsBilag.SendtTid      = 0 
        BokforingsBilag.SendAv        = ''
        BokforingsBilag.SendtRegnskap = FALSE
        .
END.

/*RUN cls\navision\run_asEksportNavision.p.*/
RUN cls\navision\runEksportNavision.p.
