
/*------------------------------------------------------------------------
    File        : sjekkDagsoppgjor.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : tomn
    Created     : Thu apr 23 
    Notes       : Sjekker at dagsoppgjør er kommet inn.
                   
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
DEFINE VARIABLE iantDager           AS INTEGER NO-UNDO.

DEFINE STREAM Ut.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.
rSendEMail  = NEW cls.SendEMail.SendEMail( ) NO-ERROR.

{syspara.i 50 50 54 ceMailLst}

ASSIGN
  bTest     = TRUE 
  cLogg     = 'sjekkDagsoppgjor' + REPLACE(STRING(TODAY),'/','')
  cFilNavn  = REPLACE(rStandardFunksjoner:getTempFileName(),'.tmp','.txt')
  iAntDager = 11
  ceMailLst = 'tomn@nsoft.no' /* TEST TEST */
  .

rStandardFunksjoner:SkrivTilLogg(cLogg,
  'Start.' 
  ).

SJEKKBLOKK:
DO:
  ASSIGN 
    .
    
  RUN sjekkDagsoppgjor (OUTPUT obOk).
  
  IF obOk THEN
  DO: 
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  Varsel om manglende dagsoppgjor er sendt.' 
      ).
    RUN sendVarselDagsoppgjor.
  END.
  ELSE     
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  Dagsoppgjør sjekk OK.' 
      ).

  LEAVE SJEKKBLOKK.
  
END. /* SJEKKBLOKK */  

rStandardFunksjoner:SkrivTilLogg(cLogg,
  'Slutt.' 
  ).

QUIT.

/* **********************  Internal Procedures  *********************** */

PROCEDURE sendVarselDagsoppgjor:
  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/
  IF ceMailLst = '' THEN 
  DO:
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  Det er satt opp mottagere av sjekk av ikke utført dagsoppgjør via eMail (Syspara 50 50 55).' 
      ).
    RETURN.
  END.
    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '  Mailvarsel sendt: ' + STRING(NOW,"99/99/9999 HH:MM:SS") 
    ).

  rSendEMail:parToADDRESS       = ceMailLst.
  rSendEMail:parMailType        = 'PAKKSEDDEL'.
  rSendEMail:parSUBJECT         = "Sjekk av dagsoppgjør fra butikkene" +  
    ' Dato/Tid: ' + STRING(NOW,"99/99/99 hh:mm:ss") + ".".    
  rSendEMail:parMessage-Charset = ''. /* Blank eller 'UTF-8' når det går fra fil. */
  rSendEMail:parFILE            = cFilNavn.  
  rSendEMail:parMESSAGE = "Sjekk av at dagsoppgjør er utført i butikkene. ".

  rSendEMail:send( ).

END PROCEDURE.

PROCEDURE sjekkDagsoppgjor:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER pbSettDagsoppgjor AS LOG NO-UNDO.
  
  DEFINE VARIABLE piLoop AS INTEGER NO-UNDO.
  DEFINE VARIABLE piAnt  AS INTEGER NO-UNDO.

  OUTPUT STREAM Ut TO value(cFilNavn).

  PUT STREAM Ut UNFORMATTED   
    'Butikk' FORMAT "x(6)" ' ' 
    'Butikkens navn' FORMAT "x(50)" ' '
    'Dato' FORMAT "x(10)" ' '
    SKIP.
  PUT STREAM Ut  
    '------' ' '
    '--------------------------------------------------' ' '
    '----------' ' '
    SKIP.
    
  pbSettDagsoppgjor = FALSE.
  FOR EACH Butiker NO-LOCK WHERE 
    Butiker.ApningsDato <> ? AND 
    Butiker.harButikksystem = TRUE AND 
    (Butiker.NedlagtDato = ? OR 
     Butiker.NedlagtDato <= TODAY): 
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  Butikk: ' + STRING(Butiker.Butik) + ' ' + Butiker.ButNamn 
      ).

    DO piLoop = 2 TO iAntDager:
      FOR EACH Bokforingsbilag NO-LOCK WHERE
        BokforingsBilag.ButikkNr       = Butiker.butik AND
        BokforingsBilag.OmsetningsDato = TODAY - piLoop AND 
        BokforingsBilag.EODMottatt     = TRUE AND 
        BokforingsBilag.GodkjentFlagg  = FALSE:  
          
        piant = 0.
        FOR EACH BongHode NO-LOCK WHERE 
          BongHode.butikkNr = BokforingsBilag.ButikkNr AND 
          BongHode.Dato = BokforingsBilag.OmsetningsDato:
          IF NOT CAN-FIND(FIRST BongLinje WHERE 
                      BongLinje.B_Id = BongHode.B_Id AND 
                      BongLinje.TTId = 92) THEN
              piAnt = piAnt + 1.
        END.
        /* Flagger bare oppgjør hvor det har kommet inn andre bonger en EOD bonger. */
        IF piant > 0 THEN 
        DO:   
          PUT STREAM Ut UNFORMATTED
            STRING(Bokforingsbilag.butikkNr) FORMAT "x(6)" ' ' 
            (IF AVAILABLE Butiker THEN Butiker.ButNamn ELSE '*Ukjent butikk') FORMAT "x(50)" ' '
            STRING(TODAY - piLoop) FORMAT "x(10)" ' '
            SKIP.
      
          pbSettDagsoppgjor = TRUE.
        END.
      END.
    END.
  END.
  OUTPUT STREAM Ut CLOSE.

END PROCEDURE.

