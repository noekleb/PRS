DEFINE VARIABLE cButLst    AS CHARACTER NO-UNDO.
DEFINE VARIABLE iButNr     AS INTEGER  NO-UNDO.
DEFINE VARIABLE cFil       AS CHARACTER NO-UNDO.
DEF VAR iant AS INT NO-UNDO.

DEFINE VARIABLE dDato      AS DATE NO-UNDO.
DEFINE VARIABLE dStartDato AS DATE NO-UNDO.
DEFINE VARIABLE iLoop      AS INTEGER  NO-UNDO.

DEFINE TEMP-TABLE ttVaretrans NO-UNDO
  FIELD Aar       AS INTEGER   FORMAT ">>>9"
  FIELD ButNr     AS INTEGER   FORMAT ">>>>>9"
  FIELD ButNamn   AS CHAR      FORMAT "x(40)"
  FIELD AvdNr     AS INTEGER   FORMAT ">>>9"
  FIELD AvdTekst  AS CHARACTER FORMAT "x(40)"
  FIELD HG        AS INTEGER   FORMAT ">>>9"
  FIELD HgTekst   AS CHARACTER FORMAT "x(40)"
  FIELD VmId      AS INTEGER   FORMAT ">>>>>>9"
  FIELD VmTekst   AS CHARACTER FORMAT "x(40)"
  FIELD Omsetning AS DECIMAL   FORMAT "->>,>>>,>>>,>>9.99"
  INDEX idxPM AS UNIQUE Aar ButNr AvdNr Hg VmId
  .

CURRENT-WINDOW:WIDTH = 350.
CURRENT-WINDOW:HEIGHT = 40.

DEFINE STREAM Ut.

ASSIGN 
  cbutLst    = '1'
  dStartDato = 01/01/2008
  cFil       = 'Konv\TransEksport\TransEksport_' + REPLACE(STRING(TODAY,'99/99/9999'),'/','-') + '.csv'
  .

IF cButLst = '' THEN 
  FOR EACH Butiker NO-LOCK WHERE 
    CAN-FIND(FIRST Lager OF butiker):
    ASSIGN  
      cbutLst = cbutLst + 
            (IF cbutLst <> '' THEN ',' ELSE '') + 
            STRING(Butiker.butik).
  END.

RUN Akkumuler.
RUN Eksporter.

RETURN.

/* **********************  Internal Procedures  *********************** */

PROCEDURE Akkumuler:
  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  BUTIKKBLOKK:
  DO iLoop = 1 TO NUM-ENTRIES(cButLst):
    ibutNr = INT(ENTRY(iLoop,cButLst)).  

    FIND Butiker NO-LOCK WHERE 
        Butiker.butik = iButNr NO-ERROR.

    TRANSLOGGBLOKK:
    FOR EACH TransLogg NO-LOCK WHERE 
      TransLogg.butik = iButNr AND 
      TransLogg.TransNr >= 0 AND 
      TransLogg.SeqNr   >= 0:

      IF CAN-DO('1,3,10',STRING(TransLogg.TTId)) AND 
        TransLogg.Dato >= dStartDato    
        THEN 
      BLOKKEN:
      DO:
        RELEASE LevBas NO-ERROR.
        RELEASE Varemerke NO-ERROR.
        RELEASE HuvGr NO-ERROR.
        RELEASE VarGr NO-ERROR.

        FIND Transtype NO-LOCK WHERE 
          TransType.TTId = TransLogg.TTId NO-ERROR.
            
        FIND ArtBas NO-LOCK WHERE
          ArtBas.ArtikkelNr = TransLogg.ArtikkelNr NO-ERROR.
        IF AVAILABLE ArtBas THEN 
        DO:
          FIND LevBas OF ArtBas NO-LOCK NO-ERROR.
          FIND Varemerke OF ArtBas NO-LOCK NO-ERROR.
          FIND HuvGr OF ArtBas NO-LOCK NO-ERROR.
          FIND Avdeling OF HuvGr NO-LOCK NO-ERROR.
          FIND VarGr OF ArtBas NO-LOCK NO-ERROR.
        END.

        FIND ttVaretrans WHERE
          ttVareTrans.AAr   = YEAR(TransLogg.Dato) AND  
          ttVareTrans.ButNr = iButNr AND 
          ttVaretrans.AvdNr = (IF AVAILABLE HuvGr THEN HuvGr.AvdelingNr ELSE 0) AND 
          ttVaretrans.Hg    = (IF AVAILABLE HuvGr THEN HuvGr.Hg ELSE 0) AND 
          ttVaretrans.VmId  = (IF AVAILABLE Varemerke THEN Varemerke.VmId ELSE 0) NO-ERROR.
        IF NOT AVAILABLE ttVaretrans THEN 
        DO:
          CREATE ttVaretrans.
          ASSIGN 
            ttVareTrans.Aar      = YEAR(TransLogg.Dato)
            ttVareTrans.ButNr    = ibutNr
            ttVareTrans.ButNamn  = (IF AVAILABLE Butiker THEN Butiker.butNamn ELSE 'Ukjent')
            ttVaretrans.AvdNr    = (IF AVAILABLE HuvGr THEN HuvGr.AvdelingNr ELSE 0)  
            ttVaretrans.Hg       = (IF AVAILABLE HuvGr THEN HuvGr.Hg ELSE 0)   
            ttVaretrans.VmId     = (IF AVAILABLE VareMerke THEN Varemerke.VmId ELSE 0)
            ttVaretrans.AvdTekst = (IF AVAILABLE Avdeling THEN Avdeling.AvdelingNavn ELSE 'Ukjent avd.')
            ttVaretrans.HgTekst  = (IF AVAILABLE HuvGr THEN HuvGr.HgBeskr ELSE 'Ukjent hg')
            ttVaretrans.VmTekst  = (IF AVAILABLE Varemerke THEN Varemerke.Beskrivelse ELSE 'Ukjent VM')
            .
        END.
        ASSIGN 
          iAnt = iAnt + 1
          ttVaretrans.Omsetning = ttVaretrans.Omsetning + (TransLogg.Pris * TransLogg.Antall)
          .

        /* TEST */
        /*
        DISPLAY
            iAnt
            ttVareTrans.ButNr + 11000
            ttVareTrans.ButNamn
            ttVaretrans.AvdNr   
            ttVaretrans.AvdTekst
            ttVaretrans.Hg      
            ttVaretrans.HgTekst 
            ttVaretrans.VmId    
            ttVaretrans.VmTekst 
            ttVareTrans.Omsetning
        WITH WIDTH 350.
        IF iAnt > 100 THEN
            LEAVE TRANSLOGGBLOKK.
        */
        /* TEST */
      END. /* BLOKKEN */
    END. /* TRANSLOGGBLOKK */
  END. /* BUTIKKBLOKK */

END PROCEDURE.

PROCEDURE Eksporter:
  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  OUTPUT STREAM Ut TO VALUE(cFil).
  PUT STREAM Ut UNFORMATTED
    'År;'
    'ButikkNr;'
    'Navn;'
    'AvdNr;'
    'Beskrivelse;'
    'HovedGr;'
    'Beskrivelse;'
    'Varemerke;'
    'Beskrivelse;'
    'Omsetning'
    SKIP.

  FOR EACH ttVareTrans:

    PUT STREAM Ut UNFORMATTED
      ttVareTrans.Aar ';'
      11000 + ttVaretrans.ButNr ';'
      ttVareTrans.ButNamn ';'
      ttVaretrans.AvdNr ';'
      ttVaretrans.AvdTekst ';'
      ttVaretrans.Hg ';'
      ttVaretrans.HgTekst ';'
      ttVaretrans.VmId ';'
      ttVaretrans.VmTekst ';'
      ttVaretrans.Omsetning 
      SKIP.
    
  END.
  OUTPUT STREAM Ut CLOSE.
  
END PROCEDURE.


