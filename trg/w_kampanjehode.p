TRIGGER PROCEDURE FOR WRITE OF KampanjeHode.

{trg\c_w_trg.i &Fil=SkoTex.KampanjeHode &Type=W}

/* Sikrer AT MixMatch er opprettet. */
IF KampanjeHode.KampId = 0 THEN
MIXMATCH: 
DO:
  FIND FIRST KampanjeEier NO-LOCK NO-ERROR.
  IF NOT AVAILABLE KampanjeEier THEN LEAVE MIXMATCH.
  CREATE KampanjeMixMatch.
  ASSIGN
    KampanjeMixMatch.KampEierId    = KampanjeEier.KampEierId
    KampanjeMixMatch.KampKlar      = TRUE
    KampanjeMixMatch.KampNavn      = 'Extrapris: (' + STRING(KampanjeHode.KampanjeId) + ') ' + KampanjeHode.Beskrivelse
    KampanjeMixMatch.KampStartDato = KampanjeHode.StartDato 
    KampanjeMixMatch.KampSLuttDato = KampanjeHode.SluttDato
    KampanjeMixMatch.KampSluttTid  = 86000
    KampanjeMixMatch.KampSendtDato = KampanjeMixMatch.KampStartDato 
    KampanjeHode.KampId            = KampanjeMixMatch.KampId
    .
  CREATE KampanjeTilbud.
  ASSIGN
    KampanjeTilbud.KampId         = KampanjeMixMatch.KampId
    KampanjeTilbud.KampTilbTypeId = 9
    KampanjeTilbud.KampTilbNavn   = KampanjeMixMatch.KampNavn 
    .
  IF AVAILABLE KampanjeTilbud THEN RELEASE KampanjeTilbud.
  IF AVAILABLE KampanjeMixMatch THEN RELEASE KampanjeMixMatch.
END. /* MIXMATCH */

