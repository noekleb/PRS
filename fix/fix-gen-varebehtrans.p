DEF VAR oiWeek1                AS INT    NO-UNDO.
DEF VAR oiWeek2                AS INT    NO-UNDO.
DEF VAR oiWeek3                AS INT    NO-UNDO.
DEF VAR oiWeek4                AS INT    NO-UNDO.

FOR EACH VareBEhLinje WHERE
    VareBehLinje.VAreBehNr = ???? AND
    VareBehLinje.ArtikkelNR = ????:

    FIND FIRST ArtBas 
         WHERE ArtBas.ArtikkelNr = DEC(hBuffVarebehLinje:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE)
         NO-LOCK NO-ERROR.

    FOR EACH Butiker NO-LOCK:
        IF CAN-FIND(FIRST VareBEhLinjeTrans OF VareBehLinje WHERE
                    VAreBEhLinjeTrans.ButikkNr = Butiker.Butik) THEN
        DO:
            IF artbas.levdato1 NE ? THEN RUN weeknum.p (artbas.levdato1,OUTPUT oiWeek1).
            ELSE oiWeek1 = 0.
            IF artbas.levdato2 NE ? THEN RUN weeknum.p (artbas.levdato2,OUTPUT oiWeek2).
            ELSE oiWeek2 = 0.
            IF artbas.levdato3 NE ? THEN RUN weeknum.p (artbas.levdato3,OUTPUT oiWeek3).
            ELSE oiWeek3 = 0.
            IF artbas.levdato4 NE ? THEN RUN weeknum.p (artbas.levdato4,OUTPUT oiWeek4).
            ELSE oiWeek4 = 0.

            CREATE VaerBehLinjeTrans.
            ASSIGN
                ASSIGN hBuffVarebehLinjeTrans:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE = DEC(ENTRY(1,icParam))
                       hBuffVarebehLinjeTrans:BUFFER-FIELD("ButikkNr"):BUFFER-VALUE = DEC(ENTRY(2,icParam))
                       hBuffVarebehLinjeTrans:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE = hBuffVarebehLinje:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE
                       hBuffVarebehLinjeTrans:BUFFER-FIELD("Kode"):BUFFER-VALUE     = ArtSort.SortId
                       hBuffVarebehLinjeTrans:BUFFER-FIELD("SeqNr"):BUFFER-VALUE    = 0
                       hBuffVarebehLinjeTrans:BUFFER-FIELD("LevDato1"):BUFFER-VALUE = ArtBas.LevDato1
                       hBuffVarebehLinjeTrans:BUFFER-FIELD("LevDato2"):BUFFER-VALUE = ArtBas.LevDato2
                       hBuffVarebehLinjeTrans:BUFFER-FIELD("LevDato3"):BUFFER-VALUE = ArtBas.LevDato3
                       hBuffVarebehLinjeTrans:BUFFER-FIELD("LevDato4"):BUFFER-VALUE = ArtBas.LevDato4
                       hBuffVarebehLinjeTrans:BUFFER-FIELD("Levuke1"):BUFFER-VALUE  = oiWeek1
                       hBuffVarebehLinjeTrans:BUFFER-FIELD("Levuke2"):BUFFER-VALUE  = oiWeek2
                       hBuffVarebehLinjeTrans:BUFFER-FIELD("Levuke3"):BUFFER-VALUE  = oiWeek3
                       hBuffVarebehLinjeTrans:BUFFER-FIELD("Levuke4"):BUFFER-VALUE  = oiWeek4
                .

        END.
    END.
END.
