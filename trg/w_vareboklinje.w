TRIGGER PROCEDURE FOR WRITE OF VareBokLinje.

DEFINE BUFFER trgVareBokHode  FOR VareBokHode.
DEFINE BUFFER trgVarebokLinje FOR VareBokLinje.
DEFINE BUFFER trgVareBehHode  FOR VareBehHode.
DEFINE BUFFER trgVareBehLinje FOR VareBehLinje.
/* Disse er definert i include fil
DEFINE BUFFER trgVarGr        FOR VarGr.
DEFINE BUFFER trgHuvGr        FOR HuvGr.
DEFINE BUFFER trgAvdeling     FOR Avdeling.
*/
DEF VAR lMesseNr LIKE Messe.MesseNr NO-UNDO.

/* Henter messenr. */
FIND trgVarebokHode OF VareBokLinje NO-LOCK NO-ERROR.
IF AVAILABLE trgVareBokHode THEN
    lMesseNr = trgVareBokHode.MesseNr.

{trg\c_w_trg.i &Fil=SkoTex.VareBokLinje &TYPE=W}

{w_vareboklinje.i}

IF VareBokLinje.AvdelingNr = 0 AND VareBokLinje.Vg > 0 THEN 
DO:
    FIND trgVarGr NO-LOCK WHERE
        trgVarGr.Vg = VareBoklinje.Vg NO-ERROR.
    IF AVAILABLE trgVarGr THEN 
      FIND trgHuvGr OF trgVarGr NO-LOCK.
    IF AVAILABLE trgHuvGr THEN 
      FIND trgAvdeling OF trgHuvGr NO-LOCK.
    IF AVAILABLE trgHuvGr THEN
      ASSIGN
        VareBokLinje.Hg            = trgHuvGr.HG
        VareBokLinje.AvdelingNr    = trgHuvGr.AvdelingNr
        VareBokLinje.HgBeskr       = trgHuvGr.HgBeskr
        VareBokLinje.AvdelingNavn  = IF AVAILABLE trgAvdeling THEN trgAvdeling.AvdelingNavn ELSE VareBokLinje.AvdelingNavn 
        . 
END.

/* Alle varebøker på samme messe, skal oppdateres. og  */
/* underliggende vareh.bøker.                          */
IF lMesseNr > 0 THEN
HODE:
FOR EACH trgVareBokHode NO-LOCK WHERE
    trgVareBokHode.MesseNr = lMesseNr AND
    trgVareBokHode.VareBokNr <> VareBokLinje.VareBokNr:
    /* Skipper behandlet varebok */
    IF trgVareBokHode.VareBokNr = VareBokLinje.VareBokNr THEN
        NEXT HODE.
    /* Oppdterer vareboklinjene */
    /* Koblet ut !!!! gir rekursiv kjøring av triggerprocedure.
    VAREBOKLINJE:
    /* Leser linjer med samme artikkel */
    FOR EACH trgVareBokLinje OF trgVareBokHode EXCLUSIVE-LOCK WHERE
        trgVareBokLinje.ArtikkelNr = VareBokLinje.ArtikkelNr:
        ASSIGN
            trgVareBokLinje.UtvidetSok = VareBokLinje.UtvidetSok
            .
    END. /* VAREBOKLINJE */
    */
    /* Oppdterer messeordren for vareboken. */
    FOR EACH trgVareBehHode NO-LOCK WHERE
        trgVareBehHode.Kilde = trgVarebokHode.VareBokNr:
        /* Leser linjer med samme artikkel */
        MESSEORDRELINJE:
        FOR EACH trgVareBehLinje OF trgVareBehHode EXCLUSIVE-LOCK WHERE
            trgVareBehLinje.ArtikkelNr = VareBokLinje.ArtikkelNr:
            ASSIGN
                trgVareBehLinje.UtvidetSok = VareBokLinje.UtvidetSok
                .
        END. 
    END. /* MESSEORDRELINJE */

END. /* HODE */




