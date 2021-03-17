DEFINE VAR iButNr    AS INTEGER NO-UNDO.
DEFINE VAR dDato     AS DATE    NO-UNDO.
DEFINE VAR iRappType AS INTEGER NO-UNDO.
DEFINE VAR lOK       AS LOGICAL NO-UNDO.
DEFINE VAR cMelding  AS CHAR    NO-UNDO.

ASSIGN 
    iButNr    = 40
    dDato     = 09/25/2020
    iRappType = 99
    .

FIND FIRST Bokforingsbilag EXCLUSIVE-LOCK WHERE 
    BokforingsBilag.OmsetningsDato = dDato AND 
    BokforingsBilag.ButikkNr = iButNr NO-ERROR.
ASSIGN 
    BokforingsBilag.GodkjentFlagg = FALSE
    . 

RUN asSkrivRapport(iButNr,
                   dDato,
                   iRapptype,
                   OUTPUT lOk,
                   OUTPUT cMelding).

MESSAGE lOk cMelding
    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
