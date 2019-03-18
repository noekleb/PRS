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
                   
ASSIGN
    iButikkNr  = 15
    iSelgerNr  = 99
    cTyp       = "RETURNER"
    cKordre_Id = "1190000001"
    .

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

                                   
                                   

    
