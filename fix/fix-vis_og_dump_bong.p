DEF VAR bDump AS LOG NO-UNDO.
DEF VAR bVis AS LOG NO-UNDO.
DEF VAR cFilNavn AS CHAR NO-UNDO.

DEF STREAM Ut.

ASSIGN 
    bDump = FALSE
    bVis = TRUE
    cFilNavn = 'konv\InnUtLogging' + REPLACE(STRING(TODAY),'/','') + '.csv'
    .

CURRENT-WINDOW:WIDTH = 350.

OUTPUT STREAM Ut TO VALUE(cFilNAvn).

PUT STREAM Ut UNFORMATTED
    'Butik;'
    'KasseNr;'
    'Dato;'
    'Kl.;'
    'BongNr;'
    'LinjeNr;'
    'SelgerNr;'
    'Navn;'
    'TTId;'
    'TbId;'
    'BongTekst'
    SKIP.

FOR EACH BongHode EXCLUSIVE-LOCK WHERE 
    BongHode.Butik = 16 AND
    BongHode.GruppeNr = 1 AND
    BongHode.KasseNr >= 0 AND 
    BongHode.Dato >= 04/09/2019 AND 
    BongHode.bongNr >= 0 AND 
    BongHode.SelgeRNr = 100322 USE-INDEX Bong,
    EACH BongLinje EXCLUSIVE-LOCK WHERE 
        BongLinje.B_Id = BongHode.B_Id AND 
        BongLinje.TTId >= 96 AND 
        BongLinje.TTId <= 97:
    FIND Selger No-lock WHERE 
        Selger.SelgeRNr = BongHode.SelgerNr NO-ERROR.

    IF bVis THEN 
    DISPLAY
        BongHode.Butik
        BongHode.KasseNr
        BongHode.Dato
        STRING(BongHode.Tid,"HH:MM:SS") COLUMN-LABEL 'Kl.'
        STRING(BongLinje.TransTid,"HH:MM:SS") COLUMN-LABEL 'Kl.'
        BongHode.BongNr
        BongLinje.LinjeNr
        '|'
        BongHode.SelgeRNr
        BongHode.KassererNavn
        Selger.Navn WHEN AVAILABLE Selger
        '|'
        BongLinje.TTId
        BongLinje.TbId COLUMN-LABEL 'TBId'
        BongLinje.BongTekst
    WITH WIDTH 350.

    PUT STREAM Ut UNFORMATTED
        BongHode.Butik ';'
        BongHode.KasseNr ';'
        BongHode.Dato ';'
        STRING(BongHode.Tid,"HH:MM:SS") ';'
        BongHode.BongNr ';'
        BongLinje.LinjeNr ';'
        BongHode.SelgeRNr ';'
        BongHode.KassererNavn ';'
        (IF AVAILABLE Selger THEN Selger.Navn ELSE '') ';'
        BongLinje.TTId ';'
        BongLinje.TbId ';'
        BongLinje.BongTekst
        SKIP.
END.

OUTPUT STREAM Ut CLOSE.
