CURRENT-WINDOW:WIDTH = 300.

DEF VAR h_Prisko AS HANDLE NO-UNDO.
DEF VAR iAnt     AS INT    NO-UNDO.
DEF VAR dTotal   AS DEC    NO-UNDO.
DEF VAR d2Total   AS DEC    NO-UNDO.
DEF VAR dGTotal   AS DEC    NO-UNDO.
DEF VAR d2GTotal   AS DEC    NO-UNDO.

DEF STREAM Ut.

IF NOT VALID-HANDLE(h_PrisKo) THEN
    RUN PrisKo.p PERSISTENT SET h_PrisKo.

FOR EACH MedKjop:
    DELETE MedKjop.
END.

BLOKKEN:
FOR EACH BongHode WHERE 
    BongHode.Dato >= 03/17/2011 AND 
    BongHode.MedlemsNr > 0 AND
    BongHode.Makulert < 2:
    iAnt = iant + 1.
    /*IF iant > 100 THEN LEAVE BLOKKEN.*/

    /* posterer kjøpet */
    RUN posterMedlemsKjop (BongHode.B_Id, h_Prisko).

END. /* BLOKKEN */

IF VALID-HANDLE(h_PrisKo) THEN
    DELETE PROCEDURE h_PrisKo.

OUTPUT STREAM Ut TO VALUE('test.txt').
FOR EACH MedKjop NO-LOCK
    BREAK BY MedKjop.ButikkNr
          BY MedKjop.KjopsDato
          BY MedKjop.KjopsTid:

    dTotal  = dTotal  + MedKjop.Saldo.
    d2Total = d2Total + MedKjop.Saldo.
    dGTotal  = dGTotal  + MedKjop.KjopsGrunnlag.
    d2GTotal = d2GTotal + MedKjop.KjopsGrunnlag.
    IF LAST-OF(MedKjop.KjopsDato) THEN
    DO:
        MESSAGE dTotal dgTotal MedKjop.KjopsDato
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        dTotal = 0.
        dGTotal = 0.
    END.

    PUT STREAM Ut UNFORMATTED 
        Medkjop.KjopsDato ';'
        MedKjop.KjopsBelop ';'
        MedKjop.KjopsGrunnlag SKIP.

    /*
    DISPLAY 
        MedKjop
        WITH WIDTH 300.
    */
END.
OUTPUT STREAM Ut CLOSE.

MESSAGE "Totalt:" d2Total d2GTotal
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

