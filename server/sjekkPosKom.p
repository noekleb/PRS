
/*------------------------------------------------------------------------
    File        : sjekkPosKom.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : tomn
    Created     : Thu mai 5 
    Notes       : Sjekker at kassene kommuniserer med BO.
                   
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
DEFINE VARIABLE bSettEOD            AS LOG NO-UNDO.

DEFINE VARIABLE cFilNavn            AS CHARACTER                      NO-UNDO.
DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.
DEFINE VARIABLE rSendEMail          AS cls.SendEMail.SendEMail        NO-UNDO.
DEFINE VARIABLE cError              AS CHARACTER                      NO-UNDO.
DEFINE VARIABLE iantDager           AS INTEGER                        NO-UNDO.

DEFINE STREAM Ut.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.
rSendEMail  = NEW cls.SendEMail.SendEMail( ) NO-ERROR.

{syspara.i 50 50 56 ceMailLst}

ASSIGN
  bTest     = TRUE 
  cLogg     = 'sjekkPosKom' + REPLACE(STRING(TODAY),'/','')
  cFilNavn  = REPLACE(rStandardFunksjoner:getTempFileName(),'.tmp','.txt')
  .

rStandardFunksjoner:SkrivTilLogg(cLogg,
  'Start.' 
  ).

SJEKKBLOKK:
DO:
  ASSIGN 
    .
    
  RUN sjekkPosKommunikasjon (OUTPUT obOk).
  
  IF obOk THEN
  DO: 
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  Varsel om manglende kommunikasjon med kasser er sendt.' 
      ).
    RUN sendVarselPosKom.
  END.
  ELSE     
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  Kommunikasjon med kassene er OK.' 
      ).

  LEAVE SJEKKBLOKK.
  
END. /* SJEKKBLOKK */  

rStandardFunksjoner:SkrivTilLogg(cLogg,
  'Slutt.' 
  ).

QUIT.

/* **********************  Internal Procedures  *********************** */

PROCEDURE sendVarselPosKom:
  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/
  IF ceMailLst = '' THEN 
  DO:
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  Det er satt opp mottagere av sjekk av kassekommunikasjon via eMail (Syspara 50 50 56).' 
      ).
    RETURN.
  END.
    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '  Mailvarsel sendt: ' + STRING(NOW,"99/99/9999 HH:MM:SS") 
    ).

  rSendEMail:parToADDRESS       = ceMailLst.
  rSendEMail:parMailType        = 'PAKKSEDDEL'.
  rSendEMail:parSUBJECT         = "Sjekk av kassekommunikasjon fra butikkene" +  
    ' Dato/Tid: ' + STRING(NOW,"99/99/99 hh:mm:ss") + ".".    
  rSendEMail:parMessage-Charset = ''. /* Blank eller 'UTF-8' når det går fra fil. */
  rSendEMail:parFILE            = cFilNavn.  
  rSendEMail:parMESSAGE = "Sjekk av at kassekommunikasjonen er ok i butikkene. ".

  rSendEMail:send( ).

END PROCEDURE.

PROCEDURE sjekkPosKommunikasjon:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER pbSettPosKom AS LOG NO-UNDO.
  
  DEFINE VARIABLE piLoop AS INTEGER NO-UNDO.
  DEFINE VARIABLE cTid AS CHARACTER NO-UNDO.
  DEFINE VARIABLE piDiff AS INTEGER NO-UNDO.
  DEFINE VARIABLE cDato AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iMakstid AS INTEGER NO-UNDO.
  DEFINE VARIABLE cDmy AS CHARACTER NO-UNDO.

  ASSIGN 
    /* Makstid = 15 min. */
    iMaksTid = 32 * 60
    cDmy = SESSION:DATE-FORMAT 
    SESSION:DATE-FORMAT = 'dmy'
    .

  OUTPUT STREAM Ut TO value(cFilNavn).

  PUT STREAM Ut UNFORMATTED   
    'Butikk' FORMAT "x(6)" ' ' 
    'Butikkens navn' FORMAT "x(50)" ' '
    'Kasse' FORMAT "x(8)" ' '
    'Dato/tid sist sendt' FORMAT "x(20)" ' '
    'Stillstand '
    'Makstid    '
    'Dato/tid sjekket' FORMAT "x(20)" ' '
    SKIP.
  PUT STREAM Ut  
    '------' ' '
    '--------------------------------------------------' ' '
    '--------' ' '
    '--------------------' ' '
    '----------' ' '
    '----------' ' '
    '--------------------' 
    SKIP.
    
  pbSettPosKom = FALSE.
  BUTIKKLOOP:
  FOR EACH Butiker NO-LOCK WHERE 
    Butiker.ApningsDato <> ? AND 
    Butiker.harButikksystem = TRUE AND 
    (Butiker.NedlagtDato = ? OR 
     Butiker.NedlagtDato <= TODAY): 
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  Butikk: ' + STRING(Butiker.Butik) + ' ' + Butiker.ButNamn 
      ).

    KASSELOOP:
    FOR EACH Kasse NO-LOCK WHERE 
      Kasse.ButikkNr = Butiker.butik AND
      Kasse.Aktiv    = TRUE AND 
      Kasse.KasseNr  < 90:

      /* Henter kassens systemparameter. */
      ASSIGN 
        cTid   = ''
        cDato  = ''
        piDiff = 0
        .
      {syspara.i 201 Kasse.ButikkNr Kasse.KasseNr cTid}
      IF cTid <> '' AND NUM-ENTRIES(cTid,' ') = 2 THEN
      DO: 
        ASSIGN 
          cDato = ENTRY(1,cTid,' ')
          cTid  = ENTRY(2,cTid,' ')
          .
        /* Innenfor samme døgn. */
        IF cDato = STRING(TODAY,"99/99/9999") THEN   
          piDiff  = TIME - 
                   (
                     (
                      INT(ENTRY(1,cTid,':')) * 60 * 60
                      ) +
                     (
                      INT(ENTRY(2,cTid,':')) * 60
                      ) +
                     INT(ENTRY(3,cTid,':'))
                   )
          .
        /* Døgnskifte */
        ELSE 
          piDiff  = /* Tid siden midnatt. */
                  TIME + 
                  /* gjenstående til til midnatt forrige dag. 86400 er ant. sec i et døgn. */
                  (86400 -
                    (
                     (
                      INT(ENTRY(1,cTid,':')) * 60 * 60
                      ) +
                     (
                      INT(ENTRY(2,cTid,':')) * 60
                      ) +
                     INT(ENTRY(3,cTid,':'))
                    )
                  )
          .
      END.
      ELSE 
        NEXT.
 
      /* Innenfor tillatt tidsavvik */        
      IF piDiff <= iMakstid OR piDiff = 0 THEN 
        NEXT.
        
      /* Ligger det ikke systemparameter på kassen, går vi bare videre. */
      /* Hentes via include syspara.i.                                  */
      IF NOT AVAILABLE SysPara THEN 
        NEXT.

      /* Logg kasser hvor makstid er overskredet. */
      IF piDiff > 0 THEN 
      DO:   
        PUT STREAM Ut UNFORMATTED
          STRING(Butiker.Butik) FORMAT "x(6)" ' ' 
          (IF AVAILABLE Butiker THEN Butiker.ButNamn ELSE '*Ukjent butikk') FORMAT "x(50)" ' '
          STRING(Kasse.KasseNr) FORMAT "x(8)" ' '
          SysPara.Parameter1 FORMAT "x(20)" ' '
          STRING(piDiff,"HH:MM:SS") '   '
          STRING(iMakstid,"HH:MM:SS") '   '
          STRING(NOW,"99/99/9999 HH:MM:SS")  
          SKIP.
    
        pbSettPosKom = TRUE.
      END.
        
    END. /* KASSELOOP */ 
  END. /* BUTIKKLOOP*/
  OUTPUT STREAM Ut CLOSE.

  ASSIGN 
    SESSION:DATE-FORMAT = cDmy
    .
    
END PROCEDURE.

