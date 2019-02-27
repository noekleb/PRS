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
DEF INPUT PARAMETER dTilDato AS DATE NO-UNDO.
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
    RabSjekkType.RabSjekkTypeNr = 1 NO-ERROR.
IF AVAILABLE RabSjekkType THEN 
    ASSIGN
    dTerskel = RabSjekkType.TerskelTildeling
    dBelop   = RabSjekkType.VerdiPaSjekk
    /*iAntallDagerGyldig = RabSjekkType.AntallDagerGyldig */
    .
/* Default verdier. */
IF dTerskel = 0 THEN dTerskel = 2000.
IF dBelop   = 0 THEN dBelop = 100.
IF iAntallDagerGyldig = 0 THEN iAntallDagerGyldig = 360.

MEDLEMSLOOP:
FOR EACH Medlem NO-LOCK WHERE
    Medlem.MedlemsNr >= dFraMedlemsNr AND
    Medlem.MedlemsNr <= dTilMedlemsNr AND
    Medlem.Bonus_Berettiget = TRUE:
  ASSIGN
        dSum = 0.
  BUTIKKBLOKK:
  FOR EACH Butiker NO-LOCK WHERE
      CAN-FIND(FIRST bMedKjop WHERE
               bMedKjop.MedlemsNr = Medlem.MedlemsNr AND
               bMedKjop.ButikkNr  = Butiker.Butik):
    GENERER:
    DO WHILE TRUE TRANSACTION:
        KJOP_BLOKK:
        FOR EACH MedKjop OF Medlem EXCLUSIVE-LOCK WHERE
            MedKjop.ButikkNr   = Butiker.Butik AND
            MedKjop.Saldo      > 0 AND 
            MedKjop.KjopsDato >= dFraDato AND
            MedKjop.KjopsDato <= dTilDato:

            ASSIGN
              dSum                 = dSum + MedKjop.Saldo
              iAntLest             = iAntLest + 1
              Medkjop.TildeltBelop = Medkjop.TildeltBelop + MedKjop.Saldo.
              
            IF dSum >= dTerskel THEN 
            DO:            
                IF (dSum - dTerskel) > 0 THEN 
                  Medkjop.TildeltBelop = Medkjop.TildeltBelop - (dSum - dTerskel).
                RUN opprettRabSjekk.
                dSum = 0.
                NEXT GENERER. /* Commit av transaksjonsblokk */ 
            END.                 
        END. /* KJOP_BLOKK */
        
        /* Poster hvor saldo er endret men terskel ikke nådd, skal tilbakestilles. */
        IF dSum > 0 THEN 
          UNDO GENERER, LEAVE GENERER.
        ELSE 
          LEAVE GENERER.
    END. /* EVIG LOOP - GENERER */
  END. /* BUTIKKBLOKK */
END. /* MEDLEMSLOOP */

PROCEDURE opprettRabSjekk:
  DO TRANSACTION:
    FIND LAST MedRabSjekk EXCLUSIVE-LOCK WHERE
        MedRabsjekk.ButikkNr    = iCL AND
        MedRabSjekk.MedlemsNr   = Medlem.MedlemsNr AND
        MedRabSjekk.RabSjekkId >= 0 AND
        MedRabSjekk.Brukt       = FALSE AND
        MedRabSjekk.RabSjekkType = 1 NO-ERROR.
    IF AVAILABLE MedRabSjekk THEN
    DO:
        ASSIGN
            /* Id tildeles i trigger */
            iAntGenerert                = iAntGenerert + 1
            MedRabSjekk.Belop           = MedRabSjekk.Belop + dBelop
            MedRabSjekk.DatoUtstedt     = TODAY
            MedRabSjekk.TidUtstedt      = TIME
            MedRabSjekk.DatoGyldig      = TODAY + iAntallDagerGyldig
            .
    END.
    ELSE DO:
        RUN getRabSjekkSerieNr.p (iCL,OUTPUT dRabSjekkSerieNr).
        CREATE MedRabSjekk.
        ASSIGN
            /* Id tildeles i trigger */
            iAntGenerert                = iAntGenerert + 1
            MedRabSjekk.ButikkNr        = iCL
            MedRabSjekk.RabSjekkTypeNr  = 1 /* Rabattsjekk */
            MedRabSjekk.MedlemsNr       = Medlem.MedlemsNr
            MedRabSjekk.Belop           = dBelop
            MedRabSjekk.DatoUtstedt     = TODAY
            MedRabSjekk.TidUtstedt      = TIME
            MedRabSjekk.DatoGyldig      = TODAY + iAntallDagerGyldig
            MedRabSjekk.RabSjekkSerieNr = dRabSjekkSerieNr
            .
    END.
  END. /* TRANSACTION */
END PROCEDURE.
