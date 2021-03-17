FOR EACH Messe:
    DELETE Messe.
END.
FOR EACH MesseForButikk:
    DELETE MesseForButikk.
END.

FOR EACH VareBokHode:
    FOR EACH VareBokLinje OF VareBokHode:
        DELETE VareBokLinje.
    END.
    DELETE VareBokHode.
END.

FOR EACH VareBehHode:
    DELETE VareBehHode.
END.

FOR EACH VareBehLinje:
    DELETE VarebehLinje.
END.

FOR EACH VareBehLinjeTHode:
    DELETE VareBehLinjeTHode.
END.

FOR EACH VareBehLinjeTrans:
    DELETE VareBehLinjeTrans.
END.

FOR EACH VareBehBestHode:
    DELETE VareBehBestHode.
END.

FOR EACH VareBehBestLinje:
    DELETE VareBehBestLinje.
END.

FOR EACH VAreBehPris:
    DELETE VareBehPris.
END.
