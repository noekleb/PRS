DEF VAR iOrdretype AS INT NO-UNDO.
DEF VAR iAnt AS INT NO-UNDO.
DEF VAR iantstk AS INT NO-UNDO.
DEF VAR cFil AS CHAR NO-UNDO.
DEFINE VARIABLE iMButikkNr AS INTEGER NO-UNDO.
DEF VAR plMinusButikk   AS LOG NO-UNDO.
DEFINE VARIABLE lKalkvarekost AS DECIMAL NO-UNDO.
DEFINE VARIABLE bInnkjopsPris AS LOG NO-UNDO.
DEFINE VARIABLE ctekst AS CHARACTER NO-UNDO.
DEFINE VARIABLE lPkSdlId LIKE PkSdlHode.PkSdlId NO-UNDO.
DEF VAR hBuffer AS HANDLE NO-UNDO.
DEF VAR cReturn AS CHAR NO-UNDO.
DEF VAR bOk AS LOG NO-UNDO.

CURRENT-WINDOW:WIDTH = 350.

DEFINE BUFFER bufPkSdlLinje FOR PkSdlLinje.
DEFINE BUFFER bufBongLinje FOR BongLinje.
DEFINE BUFFER bufFAkturaLinje FOR FakturaLinje.
DEFINE BUFFER ovButiker FOR Butiker.
DEFINE BUFFER bufPksdlHode FOR PkSdlHode.

DEFINE TEMP-TABLE ttpkSdlLinje LIKE PkSdlLinje.
DEFINE TEMP-TABLE tt2pkSdlLinje LIKE PkSdlLinje.

DEF STREAM Ut.

/* Temp-Table and Buffer definitions                                    */
DEFINE NEW SHARED TEMP-TABLE TT_OvBuffer NO-UNDO LIKE OvBuffer.

{syspara.i 19 100 3 cTekst}
IF CAN-DO("Ja,yes,true,1",cTekst) THEN
    bInnkjopspris = TRUE. 
ELSE
    bInnkjopspris = FALSE. 

ASSIGN
    cFil = 'konv\pksdl_ikke_innlevert' + REPLACE(STRING(TODAY),'/','') + '.csv' 
    .

OUTPUT STREAM Ut TO VALUE(cFil) APPEND.
PUT STREAM Ut UNFORMATTED
    'SendtDato;'
    'PkSdlNr;'
    'PkSdlStatus;'
    'PkSdlOpphav;'
    'FakturaNr;'
    'Bilagstype;'
    'ArtikkelNr;'
    'Varetekst;'
    'BongNr;'
    'ButikkNr;'
    'MButikkNr;'
    'TTId;'
    'TBId;'
    'antall'
    SKIP.

OUTPUT STREAM Ut CLOSE.


LESALLE:
FOR EACH PkSdlhode EXCLUSIVE-LOCK WHERE 
    PkSdlHode.PkSdlStatus = 10 AND 
    PkSdlHode.SendtDato   >= 01/01/2017 AND
    CAN-FIND(FIRST PkSdlLinje OF PkSdlHode WHERE 
             PkSdlLinje.butikkNr = 849) /*,
    FIRST FakturaHode NO-LOCK WHERE
        FakturaHode.Bilagstype = 1 AND
        FakturaHode.FakturaNr = DEC(PkSdlHode.PkSdlNr)*/ :

    DO:
        iAnt = iant + 1.

        DISPLAY
            PkSdlHode.SendtDato
            PkSdlHode.PkSdlNr
            PkSdlHode.PkSdlStatus
            PkSdlHode.PkSdlOpphav
            /*
            (IF AVAILABLE FakturaHode THEN STRING(FakturaHode.FakturaNr) ELSE '')
            (IF AVAILABLE FakturaHode THEN STRING(FakturaHode.Bilagstype) ELSE '')
            bufFakturaLinje.ArtikkelNr
            bufFakturaLinje.Varetekst
            bufBongLinje.BongNr
            bufBongLinje.ButikkNr
            bufBongLinje.MButikkNr
            bufBongLinje.TTId
            bufBongLinje.TBId COLUMN-LABEL 'TBId'
            bufBongLinje.antall
            */
        WITH WIDTH 350.

        OUTPUT STREAM Ut TO VALUE(cFil) APPEND.
        PUT STREAM Ut UNFORMATTED 
            PkSdlHode.SendtDato ';'
            PkSdlHode.PkSdlNr ';'
            PkSdlHode.PkSdlStatus ';'
            PkSdlHode.PkSdlOpphav ';'
            /*
            (IF AVAILABLE FakturaHode THEN STRING(FakturaHode.FakturaNr) ELSE '') ';'
            (IF AVAILABLE FakturaHode THEN STRING(FakturaHode.Bilagstype) ELSE '') ';'
            bufFakturaLinje.ArtikkelNr ';'
            bufFakturaLinje.Varetekst ';'
            bufBongLinje.BongNr ';'
            bufBongLinje.ButikkNr ';'
            bufBongLinje.MButikkNr ';'
            bufBongLinje.TTId ';'
            bufBongLinje.TBId ';'
            bufBongLinje.antall
            */
            SKIP.
        OUTPUT STREAM Ut CLOSE.

        INNLEVER:
        DO:
            EMPTY TEMP-TABLE tt2PkSdlLinje.
            FOR EACH pkSdlLinje NO-LOCK WHERE
                PkSdlLinje.PkSdlId = PkSdlHode.PkSdlId:
                CREATE tt2PkSdlLinje.
                  BUFFER-COPY PkSdlLinje TO tt2PkSdlLinje.
            END.
            hBuffer = TEMP-TABLE tt2PkSdlLinje:DEFAULT-BUFFER-HANDLE.
            RUN pksdl_innlever.p ('', hBuffer, '', OUTPUT cReturn, OUTPUT bOk).                      
            EMPTY TEMP-TABLE tt2PkSdlLinje.   
        END. /* INNLEVER */

    END.
END. /* LESALLE */
MESSAGE 'Gurre er ferdig'
    VIEW-AS ALERT-BOX INFO BUTTONS OK.



/* **********************  Internal Procedures  *********************** */



