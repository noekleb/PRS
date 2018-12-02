DEF INPUT  PARAM icParam     AS CHAR  NO-UNDO. 
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR  NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR  NO-UNDO.
DEF OUTPUT PARAM obOk        AS LOG NO-UNDO. 

DEF VAR qH       AS HANDLE  NO-UNDO.
DEF VAR bhRLogg  AS HANDLE  NO-UNDO.
DEF VAR bhRLinje AS HANDLE  NO-UNDO.

DEF VAR ii AS INTEGER NO-UNDO.
DEF VAR iCL AS INTEGER NO-UNDO.
DEF BUFFER clButiker FOR Butiker.
DEF VAR dReklamasjonsNr LIKE Reklamasjonslogg.ReklamasjonsNr NO-UNDO.

{syspara.i 5 1 1 iCL INT}.

FIND clbutiker NO-LOCK WHERE
  clButiker.Butik = iCL NO-ERROR.
  
CREATE QUERY qH.
qH:ADD-BUFFER(ihBuffer). 
qH:QUERY-PREPARE('for each ' + ihBuffer:NAME).
qH:QUERY-OPEN().
qH:GET-FIRST().

/* RUN show (ihbuffer). */

DO WHILE ihBuffer:AVAILABLE:
  FIND Reklamasjonslogg EXCLUSIVE-LOCK WHERE
      Reklamasjonslogg.ReklamasjonsNr = dec(ihBuffer:BUFFER-FIELD('ReklamasjonsNr'):BUFFER-VALUE) NO-ERROR.
  IF NOT AVAILABLE Reklamasjonslogg THEN
      CREATE Reklamasjonslogg.
  ASSIGN bhRLogg = BUFFER ReklamasjonsLogg:HANDLE.

  ASSIGN
      dReklamasjonsNr = Reklamasjonslogg.ReklamasjonsNr.
  IF Reklamasjonslogg.ReklamStatus < 2 THEN Reklamasjonslogg.ReklamStatus = 2. /* Ikke behandlet */

  /*Alle navnlike felt blir tatt med, de som ikke har navnliket MÅ legges inn manuelt*/ 
  bhRLogg:BUFFER-COPY(ihBuffer).
  
  /* Henter Artikkel */
  FIND ArtBas NO-LOCK WHERE
      ArtBas.ArtikkelNr = dec(ihBuffer:BUFFER-FIELD('ArtikkelNr'):BUFFER-VALUE) NO-ERROR.
  FIND Butiker NO-LOCK WHERE
    Butiker.Butik = int(ihBuffer:BUFFER-FIELD('Butik'):BUFFER-VALUE) NO-ERROR.
  IF AVAILABLE Butiker THEN
    FIND ArtPris OF ArtBas NO-LOCK WHERE
      ArtPris.ProfilNr = Butiker.ProfilNr NO-ERROR.
  IF NOT AVAILABLE ArtPris THEN
    FIND ArtPris OF ArtBas NO-LOCK WHERE
      ArtPris.ProfilNr = clButiker.ProfilNr NO-ERROR.

  FIND FIRST Reklamasjonslinje EXCLUSIVE-LOCK WHERE
      Reklamasjonslinje.Reklamasjonsnr = Reklamasjonslogg.Reklamasjonsnr  AND
      Reklamasjonslinje.ArtikkelNr = dec(ihBuffer:BUFFER-FIELD('ArtikkelNr'):BUFFER-VALUE) AND
      Reklamasjonslinje.Storl = string(ihBuffer:BUFFER-FIELD('Storl'):BUFFER-VALUE) NO-ERROR.
  IF NOT AVAILABLE Reklamasjonslinje THEN
      CREATE ReklamasjonsLinje.
  bhRLinje = BUFFER ReklamasjonsLinje:HANDLE.

  /* Alle navnlike felt blir tatt med, de som ikke har navnliket MÅ legges inn manuelt*/ 
  bhRLinje:BUFFER-COPY(ihBuffer).

  ASSIGN
      bhRLinje:BUFFER-FIELD('LinjeNr'):BUFFER-VALUE        = 1
      bhRLinje:BUFFER-FIELD('reklamasjonsnr'):BUFFER-VALUE = bhRLogg:BUFFER-FIELD('ReklamasjonsNr'):BUFFER-VALUE
      bhRLinje:BUFFER-FIELD('ArtikkelNr'):BUFFER-VALUE     = ihBuffer:BUFFER-FIELD('ArtikkelNr'):BUFFER-VALUE
      bhRLinje:BUFFER-FIELD('Beskr'):BUFFER-VALUE          = ihBuffer:BUFFER-FIELD('Beskr'):BUFFER-VALUE
      bhRLinje:BUFFER-FIELD('Varetekst'):BUFFER-VALUE      = ihBuffer:BUFFER-FIELD('Beskr'):BUFFER-VALUE
      bhRLinje:BUFFER-FIELD('LevKod'):BUFFER-VALUE         = ihBuffer:BUFFER-FIELD('LevKod'):BUFFER-VALUE
      bhRLinje:BUFFER-FIELD('Vg'):BUFFER-VALUE             = ihBuffer:BUFFER-FIELD('Vg'):BUFFER-VALUE            
      bhRLinje:BUFFER-FIELD('LopNr'):BUFFER-VALUE          = ihBuffer:BUFFER-FIELD('Lopnr'):BUFFER-VALUE         
      bhRLinje:BUFFER-FIELD('FeilNotat'):BUFFER-VALUE      = ihBuffer:BUFFER-FIELD('FeilNotat'):BUFFER-VALUE
      bhRLinje:BUFFER-FIELD('LevFargKod'):BUFFER-VALUE     = ihBuffer:BUFFER-FIELD('LevFargKod'):BUFFER-VALUE
      bhRLinje:BUFFER-FIELD('Butik'):BUFFER-VALUE          = ihBuffer:BUFFER-FIELD('Butik'):BUFFER-VALUE
      bhRLinje:BUFFER-FIELD('Storl'):BUFFER-VALUE          = ihBuffer:BUFFER-FIELD('Storl'):BUFFER-VALUE
      bhRLinje:BUFFER-FIELD('Antall'):BUFFER-VALUE         = ihBuffer:BUFFER-FIELD('Antall'):BUFFER-VALUE
      bhRLinje:BUFFER-FIELD('Pris'):BUFFER-VALUE           = ihBuffer:BUFFER-FIELD('Pris'):BUFFER-VALUE
      bhRLinje:BUFFER-FIELD('RabKr'):BUFFER-VALUE          = ihBuffer:BUFFER-FIELD('RabKr'):BUFFER-VALUE
      bhRLinje:BUFFER-FIELD('VVarekost'):BUFFER-VALUE      = ihBuffer:BUFFER-FIELD('VVarekost'):BUFFER-VALUE
      bhRLinje:BUFFER-FIELD('ReklamUtgifter'):BUFFER-VALUE = ihBuffer:BUFFER-FIELD('ReklamUtgifter'):BUFFER-VALUE
      bhRLinje:BUFFER-FIELD('ForsNr'):BUFFER-VALUE         = ihBuffer:BUFFER-FIELD('ForsNr'):BUFFER-VALUE
      bhRLinje:BUFFER-FIELD('FeilKode'):BUFFER-VALUE       = ihBuffer:BUFFER-FIELD('FeilKode'):BUFFER-VALUE
      bhRLinje:BUFFER-FIELD('TTId'):BUFFER-VALUE           = ihBuffer:BUFFER-FIELD('TTId'):BUFFER-VALUE
      bhRLinje:BUFFER-FIELD('Dato'):BUFFER-VALUE           = TODAY      
      bhRLinje:BUFFER-FIELD('ReklamVerdi'):BUFFER-VALUE    = IF dec(ihBuffer:BUFFER-FIELD('ReklamUtgifter'):BUFFER-VALUE) = 0 THEN (
                                                              (dec(ihBuffer:BUFFER-FIELD('Pris'):BUFFER-VALUE) - dec(ihBuffer:BUFFER-FIELD('RabKr'):BUFFER-VALUE)) * DEC(ihBuffer:BUFFER-FIELD('Antall'):BUFFER-VALUE)
                                                              ) ELSE
                                                               dec(ihBuffer:BUFFER-FIELD('ReklamUtgifter'):BUFFER-VALUE) 

      bhRLinje:BUFFER-FIELD('ReklamTotal'):BUFFER-VALUE    = IF dec(ihBuffer:BUFFER-FIELD('ReklamUtgifter'):BUFFER-VALUE) = 0 THEN(dec(ihBuffer:BUFFER-FIELD('VVarekost'):BUFFER-VALUE)  * DEC(ihBuffer:BUFFER-FIELD('Antall'):BUFFER-VALUE))
                                                              ELSE
                                                              dec(ihBuffer:BUFFER-FIELD('ReklamUtgifter'):BUFFER-VALUE)
/*       bhRLinje:BUFFER-FIELD('ReklamVerdi'):BUFFER-VALUE    = (                                                                                                                                                         */
/*                                                               (dec(ihBuffer:BUFFER-FIELD('Pris'):BUFFER-VALUE) - dec(ihBuffer:BUFFER-FIELD('RabKr'):BUFFER-VALUE)) * DEC(ihBuffer:BUFFER-FIELD('Antall'):BUFFER-VALUE) */
/*                                                               ) +                                                                                                                                                      */
/*                                                                dec(ihBuffer:BUFFER-FIELD('ReklamUtgifter'):BUFFER-VALUE)                                                                                               */
/*                                                                                                                                                                                                                        */
/*       bhRLinje:BUFFER-FIELD('ReklamTotal'):BUFFER-VALUE    = (dec(ihBuffer:BUFFER-FIELD('VVarekost'):BUFFER-VALUE)  * DEC(ihBuffer:BUFFER-FIELD('Antall'):BUFFER-VALUE)) +                                             */
/*                                                               dec(ihBuffer:BUFFER-FIELD('ReklamUtgifter'):BUFFER-VALUE)                                                                                                */
       .

  /* Oppdateringsfelt for hodet (les totalpris etc.)*/
  ASSIGN 
    bhRLogg:BUFFER-FIELD('LevNr'):BUFFER-VALUE = ArtBas.LevNr.
  qH:GET-NEXT().
END.
/* RUN show (bhRLogg). */

/* Oppdaterer totaler i reklamasjonshode. */
RUN reklamasjonslogg_recalc.p (STRING(dReklamasjonsnr),ihBuffer,'',OUTPUT ocReturn,OUTPUT obOk).

ASSIGN 
  ocReturn = STRING(bhRLogg:ROWID) + '|' + STRING(bhRLinje:ROWID) + '|' + STRING(bhRLogg:BUFFER-FIELD('reklamasjonsnr'):BUFFER-VALUE)
  obOk     = TRUE
.
IF VALID-HANDLE(qH) THEN DELETE OBJECT qH.

PROCEDURE show:  /*for test*/
DEFINE INPUT PARAMETER iph AS HANDLE NO-UNDO.
DEF VAR i AS INT NO-UNDO.

DO i = 1 TO iph:NUM-FIELDS:
  MESSAGE PROGRAM-NAME(1) 
    iph:BUFFER-FIELD(i):NAME ' ' iph:BUFFER-FIELD(i):BUFFER-VALUE
    VIEW-AS ALERT-BOX TITLE iph:NAME.
END.

END PROCEDURE.
