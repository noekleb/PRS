DEFINE INPUT  PARAMETER ihBuffer    AS HANDLE NO-UNDO.
DEFINE INPUT  PARAMETER icAction    AS CHARACTER   NO-UNDO.  /* Create or Update */
DEFINE INPUT  PARAMETER icSessionId AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER ocReturn    AS CHARACTER NO-UNDO.

DEFINE VARIABLE cFields      AS CHARACTER NO-UNDO.  /* Last modified field */
DEFINE VARIABLE cFieldValues AS CHARACTER NO-UNDO.   
DEFINE VARIABLE iCL          AS INTEGER   NO-UNDO.
DEFINE VARIABLE bTest AS LOG NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEFINE VARIABLE iTTId AS INTEGER NO-UNDO.
DEFINE VARIABLE iTBId AS INTEGER NO-UNDO.
DEFINE VARIABLE cKode AS CHARACTER NO-UNDO.

DEFINE BUFFER bufTranslogg FOR Translogg.
DEFINE BUFFER clbutiker    FOR Butiker.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.

{syspara.i 5 1 1 iCl INT}

ASSIGN 
  bTest      = TRUE
  cLogg      = 'translogg_post_update' + REPLACE(STRING(TODAY),'/','') 
  NO-ERROR.

rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.
IF bTest THEN 
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Start' 
    ).    

cFields      = DYNAMIC-FUNCTION("getCurrentValueFields" IN SOURCE-PROCEDURE) NO-ERROR.
cFieldValues = DYNAMIC-FUNCTION("getCurrentValues" IN SOURCE-PROCEDURE) NO-ERROR.

IF bTest THEN 
DO:
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '  Parametre: ' 
    ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    cFields: ' + (IF cFields <> ? THEN cFields ELSE '?') 
    ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    cFieldValues: ' + (IF cFieldValues <> ? THEN cFieldValues ELSE '?') 
    ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    iCl: ' + STRING(iCL) 
    ).    
END.

FIND Translogg WHERE Translogg.Butik = ihBuffer:BUFFER-FIELD("Butik"):BUFFER-VALUE AND 
  Translogg.TransNr = ihBuffer:BUFFER-FIELD("TransNr"):BUFFER-VALUE AND 
  Translogg.SeqNr = ihBuffer:BUFFER-FIELD("SeqNr"):BUFFER-VALUE
  EXCLUSIVE-LOCK NO-ERROR.
IF AVAIL Translogg AND TransLogg.Postert = TRUE THEN
DO:
  IF bTest THEN 
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    Posten er allerede oppdatert. Avbryter.' 
    ).    
  RETURN. 
END.
FIND Butiker NO-LOCK WHERE 
  Butiker.butik = TransLogg.butik NO-ERROR.
FIND clButiker NO-LOCK WHERE 
  clButiker.Butik = iCL NO-ERROR.
FIND LAST Kasse WHERE 
  Kasse.ButikkNr = Butiker.Butik NO-ERROR.
IF bTest THEN 
DO:
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    Butiker avail: ' + STRING(AVAILABLE Butiker) 
    ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    clButiker avail: ' + STRING(AVAILABLE clButiker) 
    ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    BongTekst: ' + TransLogg.Bongtekst 
    ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    ArtikkelNr: ' + STRING(TransLogg.ArtikkelNr) 
    ).    
END.
  
IF TransLogg.BongTekst = '' AND CAN-FIND(ArtBas WHERE ArtBas.ArtikkelNr = TransLogg.ArtikkelNr) THEN 
BERIK_RECORD:
DO:
  IF bTest THEN 
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    BERIK_RECORD' 
    ).    
 
  FIND ArtBas NO-LOCK WHERE 
    ArtBas.ArtikkelNr = TransLogg.ArtikkelNr NO-ERROR.
  IF NOT AVAILABLE ArtBas THEN 
  DO:
  IF bTest THEN 
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '    Ukjent artikkel: ' + STRING(TransLogg.ArtikkelNr) + '. Avbryter.' 
      ).    
    LEAVE BERIK_RECORD.
  END.
  
  IF translogg.Kode = '' THEN 
    DO:
      FIND StrKonv NO-LOCK WHERE 
        StrKonv.Storl = TransLogg.Storl NO-ERROR.
      IF NOT AVAILABLE StrKonv THEN 
        DO:
          IF bTest THEN 
            rStandardFunksjoner:SkrivTilLogg(cLogg,
              '    Ukjent strkode for størrelse: ' + TransLogg.Storl + '. Avbryter.' 
              ).    
            LEAVE BERIK_RECORD.
        END.
      FIND LAST Strekkode NO-LOCK WHERE 
        Strekkode.ArtikkelNr = TransLogg.ArtikkelNr AND 
        StrekKode.StrKode = StrKonv.StrKode NO-ERROR.   
    END.
  FIND ArtPris NO-LOCK WHERE 
    ArtPris.ArtikkelNr = TransLogg.ArtikkelNr AND 
    ArtPris.ProfilNr = Butiker.ProfilNr NO-ERROR.
  IF NOT AVAILABLE ArtPris THEN 
    FIND ArtPris NO-LOCK WHERE 
      ArtPris.ArtikkelNr = TransLogg.ArtikkelNr AND 
      ArtPris.ProfilNr = clButiker.ProfilNr NO-ERROR.
  
  IF bTest THEN 
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '    Avail pris: ' + STRING(AVAILABLE ArtBas) + '. Avbryter.' 
      ).    
    
  ASSIGN
    TransLogg.Kode          = IF TransLogg.Kode <> '' THEN TransLogg.Kode ELSE (IF AVAILABLE Strekkode THEN Strekkode.Kode ELSE '')
    TransLogg.Vg            = ArtBas.Vg
    TransLogg.LopNr         = ArtBas.LopNr
    TransLogg.Pris          = IF AVAILABLE ArtPris
                                          THEN ArtPris.Pris[IF ArtPris.Tilbud THEN 2 ELSE 1]
                                          ELSE Translogg.Pris
    TransLogg.RabKr         = 0
    TransLogg.KundNr        = 0

    TransLogg.LevNr         = IF AVAILABLE ArtBas
                                          THEN ArtBas.LevNr
                                          ELSE 0
    TransLogg.BongId        = 0
    TransLogg.BongLinjeNr   = 0
    TransLogg.KassaNr       = IF AVAILABLE Kasse THEN Kasse.KasseNr ELSE TransLogg.KassaNr
    TransLogg.ForsNr        = 0
    TransLogg.Plukket       = TRUE 
    TransLogg.Dato          = TODAY
    TransLogg.Tid           = TIME
    TransLogg.SelgerNr      = 0
    TransLogg.BestNr        = 0
    TransLogg.Postert       = FALSE
    TransLogg.KortNr        = ''
    TransLogg.RefNr         = TransLogg.RefNr
    TransLogg.RefTekst      = TransLogg.RefTekst
    Translogg.BongTekst     = ArtBas.Beskr
    TransLogg.VVareKost     = IF AVAILABLE ArtPris
                                          THEN ArtPris.Varekost[IF ArtPris.Tilbud THEN 2 ELSE 1]
                                          ELSE TransLogg.Varekost
    TransLogg.VareKost      = TransLogg.VVarekost
    TransLogg.SattVVarekost = TRUE 
    TransLogg.KalkylePris   = IF AVAILABLE ArtPris
                                          THEN ArtPris.Pris[IF ArtPris.Tilbud THEN 2 ELSE 1]
                                          ELSE Translogg.KalkylePris
    TransLogg.Varekost      = IF AVAILABLE ArtPris
                                          THEN ArtPris.Varekost[IF ArtPris.Tilbud THEN 2 ELSE 1]
                                          ELSE TransLogg.Varekost
    TransLogg.Pris          = TransLogg.VVarekost 
    TransLogg.Mva           = 0
    TransLogg.Mva%          = IF AVAILABLE ArtPris
                                          THEN ArtPris.Mva%[IF ArtPris.Tilbud THEN 2 ELSE 1]
                                          ELSE Translogg.Mva%
    TransLogg.Pris          = IF TransLogg.Pris = ?
                                          THEN 0
                                          ELSE TransLogg.Pris
    TransLogg.VareKost      = IF TransLogg.VareKost = ?
                                          THEN 0
                                          ELSE TransLogg.VareKost
    TransLogg.Mva%          = IF TransLogg.Mva% = ?
                                          THEN 0
                                          ELSE TransLogg.Mva%
    TransLogg.Mva           = IF TransLogg.Mva = ?
                                          THEN 0
                                          ELSE TransLogg.Mva
    .

  IF bTest THEN 
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  Oppdatering klar.' 
      ).    
  
END. /* BERIK_RECORD */

ASSIGN 
  ocReturn = ''
  .

IF bTest THEN 
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Slutt' 
    ).    
