/* Bokforingsbilag_getBlob.p
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.


DEFINE VARIABLE iKampanjeId AS INTEGER NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEFINE VARIABLE cBrukerId AS CHARACTER NO-UNDO.
DEFINE VARIABLE lKamp% AS DECIMAL NO-UNDO.
DEFINE VARIABLE iAddKampanjeId AS INTEGER NO-UNDO.
DEFINE VARIABLE iLinjeNr AS INTEGER NO-UNDO.

DEFINE BUFFER bufKampanjeHode FOR KampanjeHode.
DEFINE BUFFER bufKampanjeLinje FOR KampanjeLinje.
DEFINE BUFFER addKampanjeHode FOR KampanjeHode.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.

ASSIGN 
  iKampanjeId   = INT(ENTRY(1,icParam,'|'))
  cBrukerId     = ENTRY(2,icParam,'|')
  lKamp%        = DEC(ENTRY(3,icParam,'|'))
  ocReturn      = 'Feil ved kopiering av kampanje.'
  cLogg         = 'Kampanjehode_kopier' + REPLACE(STRING(TODAY),'/','')
  .
/* Det skal kopieres til denne kamanjen, ikke opprettes ny. */
IF NUM-ENTRIES(icParam,'|')>= 4 THEN 
  iAddKampanjeId = INTEGER(ENTRY(4,icParam,'|')).  
  
rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner() NO-ERROR.

rStandardFunksjoner:SkrivTilLogg(cLogg,
            '  Start.' 
            ).   
rStandardFunksjoner:SkrivTilLogg(cLogg,
            '  Parametre:' 
            ).   
rStandardFunksjoner:SkrivTilLogg(cLogg,
            '    cBrukerId: ' + cBrukerId 
            ).   
rStandardFunksjoner:SkrivTilLogg(cLogg,
            '    iKampanjeId: ' + STRING(iKampanjeId) 
            ).   
rStandardFunksjoner:SkrivTilLogg(cLogg,
            '    iAddKampanjeId: ' + STRING(iAddKampanjeId) 
            ).   
rStandardFunksjoner:SkrivTilLogg(cLogg,
            '    lKamp%: ' + STRING(lKamp%) 
            ).   
            
IF iKampanjeId > 0 THEN 
DO:
  FIND bufKampanjeHode NO-LOCK WHERE 
    bufKampanjeHode.KampanjeId = iKampanjeId NO-ERROR.
        
  /* Kopierer til ny kampanje. */
  IF AVAILABLE bufKampanjeHode AND iAddKampanjeId = 0 THEN 
  KOPIER:
  DO TRANSACTION:
    CREATE KampanjeHode.
    BUFFER-COPY bufKampanjeHode 
      EXCEPT KampanjeId StartDato SluttDato Beskrivelse Notat Aktivert RegistrertDato RegistrertTid EDato ETid
      TO KampanjeHode 
      ASSIGN 
        KampanjEHode.Beskrivelse    = 'Ny kampanje ' + STRING(TODAY,"99/99/9999") + ' ' + STRING(TIME,"HH:MM:SS")
        KampanjeHode.Notat          = 'Kopiert fra kampanje ' + STRING(bufKampanjeHode.KampanjeId) + ' av ' + cBrukerId + '.'
        KampanjeHode.Aktivert       = FALSE
        KampanjeHode.RegistrertDato = TODAY
        KampanjeHode.RegistrertTid  = TIME 
        KampanjeHode.EDato          = TODAY
        KampanjeHode.ETid           = TIME
      .
    FOR EACH bufKampanjeLinje NO-LOCK WHERE 
      bufKampanjeLinje.KampanjeId = bufKampanjeHode.KampanjeId:
      CREATE KampanjeLinje.
      BUFFER-COPY bufKampanjeLinje
        EXCEPT KampanjeId RegistrertDato RegistrertTid BrukerId Behandlet
        TO KampanjeLinje
        ASSIGN 
          KampanjeLinje.KampanjeId     = KampanjeHode.KampanjeId
          KampanjeLinje.RegistrertDato = TODAY
          KampanjeLinje.RegistrertTid  = TIME 
          KampanjeLinje.BrukerId       = cBrukerId
          KampanjeLinje.Behandlet      = FALSE 
          .
    END.  
    IF AVAILABLE KampanjeHode THEN RELEASE KampanjeHode.
    IF AVAILABLE KampanjeLinje THEN RELEASE KampanjeLinje.
    ASSIGN 
      ocReturn = ''.
  END. /* TRANSACTION KOPIER */
  
  /* Adderer linjene fra kampanjen inn på en eksisterende kampanje. */
  ELSE IF (AVAILABLE bufKampanjeHode AND iAddKampanjeId > 0) THEN 
  ADDER:
  DO TRANSACTION:
    rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  ADDER:' 
                ).   
    FIND KampanjeHode EXCLUSIVE-LOCK WHERE 
      ROWID(KampanjeHode) = ROWID(bufKampanjeHode) NO-ERROR.
    rStandardFunksjoner:SkrivTilLogg(cLogg,
                '    AVAILABLE KampanjeHode: ' + STRING(AVAILABLE KampanjeHode) 
                ).   
    IF NOT AVAILABLE Kampanjehode THEN 
    DO:
      rStandardFunksjoner:SkrivTilLogg(cLogg,
                  '    Kampanjehode ikke funnet. Avslutter.' 
                  ).   
      LEAVE ADDER.
    END.
    ASSIGN 
      KampanjeHode.Notat          = 'Addert inn linjer fra kampanje ' + STRING(iAddKampanjeId) + ' av ' + cBrukerId + '.' + 
                                    (IF KampanjeHode.Notat <> '' THEN CHR(10) ELSE '') + KampanjeHode.Notat
      KampanjeHode.RegistrertDato = TODAY
      KampanjeHode.RegistrertTid  = TIME 
      .
    rStandardFunksjoner:SkrivTilLogg(cLogg,
                '    Feil ved oppdatering av Kampanjehode: ' + STRING(ERROR-STATUS:ERROR)  
                ).   
      
    /* Henter siste linjenr. */
    FIND LAST KampanjeLinje NO-LOCK WHERE 
      KampanjeLinje.KampanjeId = KampanjeHode.KampanjeId USE-INDEX idxKampanjeLinje NO-ERROR.
    IF AVAILABLE KampanjeLinje THEN 
      iLinjeNr = KampanjeLinje.KampanjeLinje + 1.
    ELSE 
      iLinjeNr = 1.

    rStandardFunksjoner:SkrivTilLogg(cLogg,
                '    Nytt linjenr: ' + STRING(iLinjeNr)  
                ).   
      
    FOR EACH bufKampanjeLinje NO-LOCK WHERE 
      bufKampanjeLinje.KampanjeId = iAddKampanjeId:

      rStandardFunksjoner:SkrivTilLogg(cLogg,
                  '    Kopierer fra linje: ' + STRING(bufKampanjeLinje.KampanjeId) + '/' + STRING(bufKampanjeLinje.KampanjeLinje)  
                  ).   
        
      IF NOT CAN-FIND(FIRST KampanjeLinje WHERE 
                      KampanjeLinje.KampanjeId = KampanjeHode.KampanjeId AND 
                      KampanjeLinje.Vg         = bufKampanjeLinje.Vg AND 
                      KampanjeLinje.LopNr      = bufKampanjeLinje.LopNr) THEN 
      DO:
        rStandardFunksjoner:SkrivTilLogg(cLogg,
                    '    Adderer linje: ' + STRING(iLinjeNr)  
                    ).   
        CREATE KampanjeLinje.
        BUFFER-COPY bufKampanjeLinje
          EXCEPT KampanjeId RegistrertDato RegistrertTid BrukerId Behandlet KampanjeLinje
          TO KampanjeLinje
          ASSIGN 
            KampanjeLinje.KampanjeId     = KampanjeHode.KampanjeId
            KampanjeLinje.KampanjeLinje  = iLinjeNr
            KampanjeLinje.RegistrertDato = TODAY
            KampanjeLinje.RegistrertTid  = TIME 
            KampanjeLinje.BrukerId       = cBrukerId
            KampanjeLinje.Behandlet      = FALSE 
            iLinjeNr = iLinjeNr + 1
            .
      END.
      rStandardFunksjoner:SkrivTilLogg(cLogg,
                  '    Kopiert ' + STRING(iLinjeNr) + ' varelinjer på kampanjen.'  
                  ).   
      ASSIGN 
        ocReturn = ''.
    END.  
    IF AVAILABLE KampanjeHode THEN RELEASE KampanjeHode.
    IF AVAILABLE KampanjeLinje THEN RELEASE KampanjeLinje.
  END. /* TRANSACTION ADDER */
END.

obOk = ocReturn = "".

 rStandardFunksjoner:SkrivTilLogg(cLogg,
            '   Resultat: ' + STRING(obOk) + ' ' + ocReturn  
            ).   
 rStandardFunksjoner:SkrivTilLogg(cLogg,
            '  Slutt.' 
            ).   


/* **********************  Internal Procedures  *********************** */

