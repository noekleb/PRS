DEFINE INPUT PARAMETER rRowId AS ROWID NO-UNDO.
DEFINE INPUT PARAMETER iKampanjeId AS INTEGER NO-UNDO.

DEFINE VARIABLE h_PrisKo AS HANDLE NO-UNDO.
DEFINE VARIABLE bTest               AS LOG                            NO-UNDO.
DEFINE VARIABLE cLogg               AS CHARACTER                      NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.

ASSIGN 
  bTest = IF SEARCH('tnc.txt') <> ? THEN TRUE ELSE FALSE 
  cLogg = 'Klargjor_kampanje_prisko' + REPLACE(STRING(TODAY),'/','') 
  NO-ERROR.

rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.
IF bTest THEN 
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Start ' + STRING(TODAY) + ' ' + STRING(TIME,"HH:MM:SS") 
    ).    


IF NOT VALID-HANDLE(h_PrisKo) THEN
  RUN prisko.p PERSISTENT SET h_PrisKo.

FIND KampanjeHode NO-LOCK WHERE 
  KampanjeHode.KampanjeId = iKampanjeId NO-ERROR.

IF bTest THEN
DO: 
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '  Kampanje: ' + STRING(iKampanjeId)
    ).    
END.
  
IF NOT AVAILABLE KampanjeHode THEN
DO: 
  IF bTest THEN
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  Ukjent kampanje - avbryter: ' + STRING(iKampanjeId)
      ).    
  RETURN.
END.

FIND Butiker NO-LOCK WHERE
    Butiker.Butik = 16 NO-ERROR.
FIND ArtBas WHERE ROWID(ArtBas) = rRowId NO-ERROR.
IF NOT AVAILABLE ArtBas THEN
DO: 
  IF bTest THEN
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  Ukjent artikkel med rowid - avbryter: ' + STRING(rRowId)
      ).    
  RETURN.
END.  
FIND ArtPris NO-LOCK WHERE
    ArtPris.ArtikkelNr = ArtBas.ArtikkelNR AND
    ArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.
IF NOT AVAILABLE ArtPris THEN
DO: 
  IF bTest THEN
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  Ukjent pris på artikkel: ' + STRING(ArtBas.ArtikkelNr) + ' ProfilNr ' + STRING(Butiker.ProfilNr)
      ).    
  RETURN.
END.  
/* TN 10/9-19 Denne funksjonen er koblet ut. En pågående kampanje avsluttes nå automatisk ved aktivering av ny kampanje som da overtar for den gamle. */
/*/* Sletter ikke startede kampanjer på artikkelen. */                                                                           */
/*FOR EACH PrisKo EXCLUSIVE-LOCK WHERE                                                                                           */
/*    Prisko.ArtikkelNr = ArtBAs.ArtikkelNr AND                                                                                  */
/*    PrisKo.ProfilNr   = ArtPris.ProfilNr AND                                                                                   */
/*    PrisKo.TYPE       = 2: /* P åkampanje */                                                                                   */
/*                                                                                                                               */
/*    IF bTest THEN                                                                                                              */
/*      rStandardFunksjoner:SkrivTilLogg(cLogg,                                                                                  */
/*        '  Sletter priskø (PÅ Tilbud - 2) for artikkel: ' + STRING(ArtBas.ArtikkelNr) + ' ProfilNr ' + STRING(ArtPris.ProfilNr)*/
/*        ).                                                                                                                     */
/*                                                                                                                               */
/*    DELETE Prisko.                                                                                                             */
/*END.                                                                                                                           */
/*                                                                                                                               */
/*/* Slår av pågående kampanjer på artikkelen. */                                                                    */
/*DO:                                                                                                                */
/*    FOR EACH PrisKo EXCLUSIVE-LOCK WHERE                                                                           */
/*        Prisko.ArtikkelNr = ArtBas.ArtikkelNr AND                                                                  */
/*        PrisKo.ProfilNr   = ArtPris.ProfilNr AND                                                                   */
/*        PrisKo.TYPE       = 3: /* Av kampanje */                                                                   */
/*                                                                                                                   */
/*        IF bTest THEN                                                                                              */
/*          rStandardFunksjoner:SkrivTilLogg(cLogg,                                                                  */
/*            '  Endrer (AV Tilbud - 3) for artikkel: ' + STRING(ArtBas.ArtikkelNr) +                                */
/*            ' ProfilNr ' + STRING(ArtPris.ProfilNr) +                                                              */
/*            ' Aktiveres dato ' + STRING(Prisko.AktiveresDato) +                                                    */
/*            ' Aktiveres tid ' + STRING(Prisko.AktiveresTid) +                                                      */
/*            ' Dato sammenligning ' + STRING(Prisko.AktiveresDato = KampanjeHode.StartDato) +                       */
/*            ' Tid sammenligning ' + STRING(KampanjeHode.AktiveresTid <= 60)                                        */
/*            ).                                                                                                     */
/*                                                                                                                   */
/*        IF Prisko.AktiveresDato = KampanjeHode.StartDato AND KampanjeHode.AktiveresTid <= 60 THEN                  */
/*          ASSIGN                                                                                                   */
/*              Prisko.AktiveresDato = KampanjeHode.StartDato - 1                                                    */
/*              Prisko.AktiveresTid  = (24 * 60 * 60) - 60                                                           */
/*              .                                                                                                    */
/*        ELSE                                                                                                       */
/*          ASSIGN                                                                                                   */
/*              Prisko.AktiveresDato = KampanjeHode.StartDato                                                        */
/*              Prisko.AktiveresTid  = KampanjeHode.AktiveresTid - (IF KampanjeHode.AktiveresTid > 10 THEN 10 ELSE 0)*/
/*              .                                                                                                    */
/*                                                                                                                   */
/*        IF bTest THEN                                                                                              */
/*          rStandardFunksjoner:SkrivTilLogg(cLogg,                                                                  */
/*            '  Etter endringl: ' + STRING(ArtBas.ArtikkelNr) +                                                     */
/*            ' ProfilNr ' + STRING(ArtPris.ProfilNr) +                                                              */
/*            ' Aktiveres dato ' + STRING(Prisko.AktiveresDato) +                                                    */
/*            ' Aktiveres tid ' + STRING(Prisko.AktiveresTid) +                                                      */
/*            ' Pris ' + STRING(Prisko.Pris)                                                                         */
/*            ).                                                                                                     */
/*    END.                                                                                                           */
/*/*    RUN KlargjorPriskoEn IN h_PrisKo (ROWID(ArtBas)).*/                                                          */
/*END.                                                                                                               */

IF VALID-HANDLE(h_PrisKo) THEN
  DELETE PROCEDURE h_PrisKo.

IF bTest THEN 
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Slutt' 
    ).    
