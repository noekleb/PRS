/* Genererer rabattsjekker på bakgrunn av medlemssalg.

  RUN genererrabsjekker.p (DATE(FraDato:SCREEN-VALUE),
                           DATE(TilDato:SCREEN-VALUE),
                           DEC(FraMedlemNr:SCREEN-VALUE),
                           DEC(TilMedlemNr:SCREEN-VALUE),
                           INPUT-OUTPUT iAntLest,
                           INPUT-OUTPUT iAntPostert,
                           OUTPUT cMsgs).
*/

DEF INPUT PARAMETER dFraDato AS DATE NO-UNDO.
DEF INPUT PARAMETER dFraMedlemsNr AS DEC NO-UNDO.
DEF INPUT PARAMETER dTilMedlemsNr AS DEC NO-UNDO.
DEF INPUT-OUTPUT PARAMETER iAntLest     AS INT NO-UNDO.
DEF INPUT-OUTPUT PARAMETER iAntGenerert AS INT NO-UNDO.
DEF OUTPUT PARAMETER cMsgs AS CHAR NO-UNDO.

DEF VAR dSum     AS DEC NO-UNDO.
DEF VAR dTerskel AS DEC NO-UNDO.
DEF VAR dBelop   AS DEC NO-UNDO.
DEF VAR iAntallDagerGyldig AS INT NO-UNDO.
DEF VAR dRabSjekkSerieNr   AS DEC NO-UNDO.
DEF VAR iCL                AS INT NO-UNDO.

DEF BUFFER bMedKjop FOR MedKjop.

{syspara.i  5 1 1 iCL INT}

/* Henter terkselverdi og rabattsjekkbeløp */
FIND RabSjekkType NO-LOCK WHERE
    RabSjekkType.RabSjekkTypeNr = 2 NO-ERROR.
IF AVAILABLE RabSjekkType THEN 
    ASSIGN
    dTerskel = RabSjekkType.TerskelTildeling
    dBelop   = RabSjekkType.VerdiPaSjekk
    /*iAntallDagerGyldig = RabSjekkType.AntallDagerGyldig */
    .
/* Default verdier. */
IF dTerskel = 0 THEN dTerskel = 0.
IF dBelop   = 0 THEN dBelop = 200.
IF iAntallDagerGyldig = 0 THEN iAntallDagerGyldig = 360.

MEDLEMSLOOP:
FOR EACH Medlem NO-LOCK WHERE
    Medlem.MedlemsNr >= dFraMedlemsNr AND
    Medlem.MedlemsNr <= dTilMedlemsNr AND
    CAN-FIND(MedKjop OF Medlem WHERE 
             MedKjop.KjopsDato >= dFraDato):

    RUN opprettRabSjekk.
END. /* MEDLEMSLOOP */

PROCEDURE opprettRabSjekk:
  DO TRANSACTION:
    RUN getRabSjekkSerieNr.p (iCL,OUTPUT dRabSjekkSerieNr).

    CREATE MedRabSjekk.
    ASSIGN
        /* Id tildeles i trigger */
        iAntGenerert                = iAntGenerert + 1
        MedRabSjekk.ButikkNr        = iCL
        MedRabSjekk.RabSjekkTypeNr  = RabSjekkType.RabSjekkTypeNr /* Medlems rabattsjekk */
        MedRabSjekk.MedlemsNr       = Medlem.MedlemsNr
        MedRabSjekk.Belop           = dBelop
        MedRabSjekk.DatoUtstedt     = TODAY
        MedRabSjekk.TidUtstedt      = TIME
        MedRabSjekk.DatoGyldig      = TODAY + iAntallDagerGyldig
        MedRabSjekk.RabSjekkSerieNr = dRabSjekkSerieNr
        .
  END. /* TRANSACTION */
END PROCEDURE.
