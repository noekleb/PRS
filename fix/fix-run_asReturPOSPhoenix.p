/* fix-run_asReturPOSPhoenix.p */
                  
                   
/* Input parametre */                     
DEFINE VAR iButikkNr  AS INTEGER NO-UNDO.
DEFINE VAR iSelgernr  AS INTEGER NO-UNDO.
DEFINE VAR cTyp       AS CHAR    NO-UNDO.
DEFINE VAR cKOrdre_Id AS CHAR    NO-UNDO.
/* Input-Output */
DEFINE VAR lcTT       AS LONGCHAR  NO-UNDO.
/* Output parametre */
DEFINE VAR lcReturKoder     AS LONGCHAR  NO-UNDO.
DEFINE VAR bOk              AS LOG       NO-UNDO.
DEFINE VAR cKvittotext      AS CHARACTER NO-UNDO.
DEFINE VAR cEksterntOrdrenr AS CHARACTER NO-UNDO.
DEFINE VAR dReturKOrdre_Id  AS DECIMAL   NO-UNDO.
DEFINE VAR cReturn          AS CHARACTER NO-UNDO.
                   
{tt_kolinjer.i}

ASSIGN
    iButikkNr  = 15
    iSelgerNr  = 99
    cTyp       = "RETURNER"
    cKordre_Id = "1190000001"
    .

FIND KOrdreHode NO-LOCK WHERE 
    KOrdrEHode.KOrdre_Id = DEC(cKordre_Id) NO-ERROR.
FOR EACH KOrdreLinje OF KOrdrEHode NO-LOCK:
    IF KOrdreLinje.VareNr = 'BETALT' THEN
        NEXT.
    ELSE DO:
        CREATE tt_linjer.
        ASSIGN 
            tt_Linjer.artikkelnr = KORdreLinje.VareNr            
            tt_Linjer.linjenr = KOrdreLinje.KOrdreLinjeNr
            tt_Linjer.ean = KOrdreLinje.Kode
            tt_Linjer.varetekst = KORdreLinje.Varetekst
            tt_Linjer.antall = KOrdreLinje.Antall
            tt_Linjer.levfargkod = KORdreLinje.LevFargKod
            tt_Linjer.storl = KORdreLinje.Storl
            tt_Linjer.kundpris = KORdreLinje.Pris
            tt_Linjer.feilkode = 10
            tt_Linjer.used = false
            
            .
    END.
END.

TEMP-TABLE tt_Linjer:WRITE-JSON('file','kom\in\kolinjer.json').

COPY-LOB FROM FILE "kom\in\kolinjer.json" TO lcTT.

/*
MESSAGE 'Melding:' SKIP
    'lcTT:' STRING(lcTT)
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
*/

RUN asReturPOSPhoenix.p(INPUT iButikkNr,
                        INPUT iSelgerNr,
                        INPUT cTyp,
                        INPUT cKOrdre_Id,
                        INPUT-OUTPUT lcTT,
                        OUTPUT lcReturkoder,
                        OUTPUT bOk,
                        OUTPUT cKvittoText,
                        OUTPUT cEksterntOrdreNr,
                        OUTPUT dReturKOrdre_Id,
                        OUTPUT cReturn
                       ).

MESSAGE 'Svar' SKIP
    'lcReturKoder:' STRING(lcReturKoder) SKIP
    'bok:' bok SKIP
    'cKvittotext:' cKvittotext SKIP
    'cEksterntOrdreNr:' cEksterntOrdreNr SKIP
    'dReturKOrdre_Id:' dReturKOrdre_Id SKIP
    'cReturn:' cReturn SKIP(1)
    'lcTT' STRING(lcTT)

    VIEW-AS ALERT-BOX INFO BUTTONS OK.

                                   
                                   

    
