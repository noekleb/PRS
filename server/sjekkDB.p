
/*------------------------------------------------------------------------
    File        : sjekkDB.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : tomn
    Created     : Thu apr 14 
    Notes       : Sjekker db ekstenter for å se om de har overskredet maks verdi.
                  Har de det, sendes eMail varsel om dette. 
                   
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSendLst            AS CHARACTER                      NO-UNDO.
DEFINE VARIABLE cLogg               AS CHARACTER                      NO-UNDO.
DEFINE VARIABLE iX                  AS INTEGER                        NO-UNDO.
DEFINE VARIABLE iLoop               AS INTEGER                        NO-UNDO.
DEFINE VARIABLE ceMailLst           AS CHARACTER                      NO-UNDO.
DEFINE VARIABLE bTest               AS LOG                            NO-UNDO.
DEFINE VARIABLE obOk AS LOG NO-UNDO.


DEFINE VARIABLE lprcnt_full         AS DECIMAL                        FORMAT ">>9.99" LABEL "% Full" NO-UNDO.
DEFINE VARIABLE lempty_blocks       AS DECIMAL                        FORMAT ">>,>>>,>>9" LABEL "Empty" NO-UNDO.
DEFINE VARIABLE lhiwater            AS DECIMAL                        FORMAT ">>,>>>,>>9" LABEL "Hiwater" NO-UNDO.
DEFINE VARIABLE lmb_used            AS DECIMAL                        FORMAT ">>>,>>9.99" LABEL "MB Used" NO-UNDO.
DEFINE VARIABLE lmb_avail           AS DECIMAL                        FORMAT ">>>,>>9.99" LABEL "MB Avail" NO-UNDO.
DEFINE VARIABLE lmb_tused           AS DECIMAL                        FORMAT ">>>,>>9.99" LABEL "Total MB used" INITIAL 0.0 NO-UNDO.
DEFINE VARIABLE lmb_tavail          AS DECIMAL                        FORMAT ">>>,>>9.99" LABEL "Total MB avail" INITIAL 0.0 NO-UNDO.
DEFINE VARIABLE iLedigeEkstenter    AS INTEGER                        FORMAT ">>9" NO-UNDO.
DEFINE VARIABLE bFixed              AS LOG                            NO-UNDO.
DEFINE VARIABLE lFillimit           AS DECIMAL                        FORMAT ">>>,>>>,>>>,>>9" NO-UNDO.
DEFINE VARIABLE lMax                AS DECIMAL                        FORMAT ">>>,>>>,>>>,>>9" NO-UNDO.
DEFINE VARIABLE cFilNavn            AS CHARACTER                      NO-UNDO.
DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.
DEFINE VARIABLE rSendEMail          AS cls.SendEMail.SendEMail        NO-UNDO.
DEFINE VARIABLE cError              AS CHARACTER                      NO-UNDO.
DEFINE STREAM Ut.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.
rSendEMail  = NEW cls.SendEMail.SendEMail( ) NO-ERROR.

/*{syspara.i 50 50 53 ceMailLst}*/

ASSIGN
  bTest    = TRUE 
  cLogg    = 'sjekkDB' + REPLACE(STRING(TODAY),'/','')
  cFilNavn = REPLACE(rStandardFunksjoner:getTempFileName(),'.tmp','.txt')
  ceMailLst = 'tomn@nsoft.no'
  .

rStandardFunksjoner:SkrivTilLogg(cLogg,
  'Start.' 
  ).

SJEKKBLOKK:
DO:
  ASSIGN 
    lFilLimit = 1600000000
    lMax      = 2000000000
    .
    
  IF bTest THEN 
  DO:
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  DictDB  : ' + DBNAME
      ).
  END.
  
  RUN SjekkOmOverskredet (OUTPUT obOk).
  
  IF obOk THEN
  DO: 
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  Varsel om overskredet størrelse på dbekstent er sendt.' 
      ).
    RUN sendVarselSjekkDB.
  END.
  ELSE     
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  DB sjekk av extent størrelser OK.' 
      ).

  LEAVE SJEKKBLOKK.
  
END. /* SJEKKBLOKK */  

rStandardFunksjoner:SkrivTilLogg(cLogg,
  'Slutt.' 
  ).

QUIT.

/* **********************  Internal Procedures  *********************** */

PROCEDURE sendVarselSjekkDB:
  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/
  IF ceMailLst = '' THEN 
  DO:
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  Det er ikke satt opp mottager for eMail varsel av db sjekk (Syspara 50 50 53).' 
      ).
    RETURN.
  END.
    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '  Mailvarsel sendt: ' + STRING(NOW,"99/99/9999 HH:MM:SS") 
    ).

  rSendEMail:parToADDRESS       = ceMailLst.
  rSendEMail:parMailType        = 'PAKKSEDDEL'.
  rSendEMail:parSUBJECT         = "Sjekk av DB extent DB " + CAPS(DBNAME) +  
    ' Dato/Tid: ' + STRING(NOW,"99/99/99 hh:mm:ss") + ".".    
  rSendEMail:parMessage-Charset = ''. /* Blank eller 'UTF-8' når det går fra fil. */
  rSendEMail:parFILE            = cFilNavn.  
  rSendEMail:parMESSAGE = "DBSjekk av " + CAPS(DBNAME) + " databasen: En eller flere DB ekstenter er over 80%. ".

  rSendEMail:send( ).

END PROCEDURE.

PROCEDURE SjekkOmOverskredet:
  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER obOk AS LOG NO-UNDO.

  DEFINE VARIABLE pbOk AS LOG NO-UNDO.
  
  OUTPUT STREAM Ut TO value(cFilNavn).

  PUT STREAM Ut UNFORMATTED 
    'AreNavn                  '
    'Area#       '
    'DBBlockSize '
    '#Extenter   ' 
    '#LedigeEkstenter  '
    'HWM extent               ' 
    '%Full ' 
    'Fil størrelse disk'
    SKIP.

  FOR EACH _Area NO-LOCK:
    IF _Area-name = 'control area' THEN
      NEXT.
    cTekst = ''.
    FIND _Areastatus WHERE 
      _Areastatus-Areanum = _Area._Area-number NO-LOCK.
    FIND _AreaThreshold WHERE 
      _AreaThreshold._AreaThresholdArea = _AreaStatus-AreaNum NO-ERROR.

    lhiwater = _AreaStatus-Hiwater.
    IF lhiwater = ? THEN lhiwater = 0.0.

    lempty_blocks = _AreaStatus-Totblocks - lhiwater - _AreaStatus-Extents.

    lprcnt_full = (1.0 - (lempty_blocks / _AreaStatus-Totblocks)) * 100.0.
    lprcnt_full = ROUND(lprcnt_full,2).
 
    lmb_avail = lempty_blocks / 1048576 * _Area-BlockSize.
    lmb_tavail = lmb_tavail + lmb_avail.
     
    lmb_used = lhiwater / 1048576 * _Area-BlockSize.
    lmb_tused = lmb_tused + lmb_used.

    iLedigeekstenter = 0.
    ASSIGN 
      iLedigeEkstenter = _AreaStatus-Extents - INT(REPLACE(REPLACE(ENTRY(2,_AreaStatus-Lastextent,'.'),'d',''),'b',''))
     NO-ERROR.
    IF ERROR-STATUS:ERROR THEN 
      iLedigeEkstenter =  ?.

    FILE-INFO:FILE-NAME = _AreaStatus-Lastextent.

    FIND LAST _FileList NO-LOCK WHERE 
      _fileList._FileList-Name = _AreaStatus-Lastextent NO-ERROR.
 
     rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  Area: ' + _Area-name
      ).
     rStandardFunksjoner:SkrivTilLogg(cLogg,
      '    FILE-NAME: ' + FILE-INFO:FILE-NAME
      ).
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '    lprcnt_full: ' + STRING(lprcnt_full)
      ).
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '    iLedigeEkstenter: ' + STRING(iLedigeEkstenter)
      ).
 
    /*
       - Er ledige ekstenter > 0 skal det ikke varsles.
       - Er ledige ekstenter = 0
           - Er %Full >= 98, skal filstørrelse på disk sjekkes.
               - Er denne større enn 1,6GB skal >80% varsel sendes.
           - Er %full >= 80% skal det varsles.
    */
    /* Fixed størrelse på ekstent og det er ledige ekstenter. */
    IF iLedigeekstenter > 0 THEN
    DO: 
      cTekst = _AreaStatus-Lastextent + ' ' + 'FIXED  og ledige ekstenter(' + STRING(iLedigeekstenter) + ').'. 
      rStandardFunksjoner:SkrivTilLogg(cLogg,
        '    ' + cTekst
        ).
      obOk = FALSE.
    END.
    ELSE DO:
      /* Variabel størrelse på ekstent. */
      IF lprcnt_full >= 98 THEN 
      DO:
        /* Fyllningsgrad i henhold til filstørrelse */
/*        lprcnt_full = ROUND((FILE-INFO:FILE-SIZE / lMax) * 100,2).*/

        cTekst = _AreaStatus-Lastextent + ' ' + 'VARIABEL  lprcnt_full: ' + 
            STRING(lprcnt_full) + 
            ' FileSiez: ' + STRING(FILE-INFO:FILE-SIZE) + 
            ' Max: ' + STRING(lMax) + 
            ' Limit: ' + STRING(lFilLimit) +
            ' Sjekk: ' + STRING(FILE-INFO:FILE-SIZE > lFilLimit). 
        rStandardFunksjoner:SkrivTilLogg(cLogg,
          '    ' + cTekst
          ).

        IF FILE-INFO:FILE-SIZE > lFilLimit THEN 
          obOk = TRUE.
        ELSE 
          obOk = FALSE.
      END.
      /* Fixed ekstent, og fyllningsgrad er over 80% */
      ELSE DO:
        IF lprcnt_full >= 80 THEN
        DO: 
          cTekst = _AreaStatus-Lastextent + ' ' + 'FIXED  og over eller lik 80%: ' + STRING(lprcnt_full).  
          rStandardFunksjoner:SkrivTilLogg(cLogg,
            '    ' + cTekst
            ).
          obOk = TRUE.
        END.
        ELSE DO: 
          cTekst = _AreaStatus-Lastextent + ' ' + 'FIXED  og under 80%: ' + STRING(lprcnt_full).  
          rStandardFunksjoner:SkrivTilLogg(cLogg,
            '    ' + cTekst
            ).
          obOk = FALSE.
        END.
      END.
    END.
    
    PUT STREAM Ut  
      _Area-name FORMAT "x(25)" 
      STRING(_AreaStatus-AreaNum) FORMAT "x(12)" 
      STRING(_Area-blocksize) FORMAT "x(12)" 
      STRING(_AreaStatus-Extents) FORMAT "x(12)" 
      STRING(iLedigeEkstenter) FORMAT "x(17)"
      _AreaStatus-Lastextent FORMAT "x(25)" 
      STRING(lprcnt_full,"->>,>>9.9") FORMAT "x(12)"  
      STRING(FILE-INFO:FILE-SIZE) FORMAT "x(12)"
      SKIP.
    IF obOk AND cTekst <> '' THEN 
      PUT STREAM Ut UNFORMATTED 
        '    ' + cTekst 
        SKIP.
     ELSE 
      cTekst = ''.
            
     /* Tar vare på resultatet av sjekken av dette Area. */ 
     IF pbOk = FALSE THEN 
     ASSIGN 
      pbOk   = obOk
      .  
  END.

  OUTPUT STREAM Ut CLOSE.
  
  /* Setter inn resultatet.                              */
  /* Hvis en av Area'ene er over, skal det sendes eMail. */
  obOk = pbOk.
  
END PROCEDURE.

