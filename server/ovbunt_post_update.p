/* Kjøres etter oppdatering. Se prosedyre MySaveBrowseFillIn i kallende rutine  
   
-----------------------------------------------------------------------------------------*/   
DEFINE INPUT  PARAMETER ihBuffer       AS HANDLE NO-UNDO.  /* Handle to current buffer. Her: Ovbunt */
DEFINE INPUT  PARAMETER icAction       AS CHARACTER   NO-UNDO.  /* New, Delete, Create or Update */
DEFINE INPUT  PARAMETER icSessionId    AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER ocValue        AS CHARACTER   NO-UNDO.  /* Error message. If <> blank the transaction is backed out */

DEFINE VARIABLE cFelt       AS CHARACTER  NO-UNDO.  /* Last modified field */
DEFINE VARIABLE cVerdi AS CHARACTER NO-UNDO.
DEFINE VARIABLE cfields AS CHARACTER NO-UNDO.
DEFINE VARIABLE cInputParam AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldValues AS CHARACTER NO-UNDO. /* Verdiene i endrede felt. */
DEFINE VARIABLE iLoop AS INTEGER NO-UNDO.
DEFINE VARIABLE iBuntNr AS INTEGER NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEFINE VARIABLE hdOvbunt AS HANDLE NO-UNDO.
DEFINE VARIABLE cparToADDRESS AS CHARACTER NO-UNDO.
DEFINE VARIABLE bOk AS LOG NO-UNDO.
DEFINE VARIABLE iX AS INTEGER NO-UNDO.
DEFINE VARIABLE iPreFix AS INTEGER NO-UNDO.
DEFINE VARIABLE cBrukerId AS CHARACTER NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.
rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( ) NO-ERROR.
DEFINE VARIABLE rSendEMail AS cls.SendEMail.SendEMail NO-UNDO.
rSendEMail  = NEW cls.SendEMail.SendEMail( ) NO-ERROR.

DEFINE STREAM Ut.

/* Overstyrer for å kunne sette riktig brukerid når rutinen kjøres via AppServer kall. */
ON CREATE OF OvBunt OVERRIDE DO: END.
ON WRITE OF OvBunt OVERRIDE DO: END.

cFields      = DYNAMIC-FUNCTION("getCurrentValueFields" IN SOURCE-PROCEDURE).
cFieldValues = REPLACE(DYNAMIC-FUNCTION("getCurrentValues" IN SOURCE-PROCEDURE),'¤','|').
cInputParam  = DYNAMIC-FUNCTION("getInputParam" IN SOURCE-PROCEDURE). /* i kallende rutine: JBoxServerAPI:Instance:serverTransInputParam = "RESERVASJON " */
 
ASSIGN 
  cLogg = 'ovbunt_post_update' + REPLACE(STRING(TODAY),'/','')
  .  

rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Start.' 
    ).

rStandardFunksjoner:SkrivTilLogg(cLogg,
    ' Parametre' 
    ).
rStandardFunksjoner:SkrivTilLogg(cLogg,
    '   ihBuffer avail: ' + STRING(ihBuffer:AVAILABLE) + ' ' + ihBuffer:NAME
    ).
rStandardFunksjoner:SkrivTilLogg(cLogg,
    '   icAction: ' + icAction 
    ).
rStandardFunksjoner:SkrivTilLogg(cLogg,
    '   cFields: ' + cFields 
    ).
rStandardFunksjoner:SkrivTilLogg(cLogg,
    '   cFieldValues: ' + cFieldValues 
    ).
rStandardFunksjoner:SkrivTilLogg(cLogg,
    '   cInputParam: ' + cInputParam 
    ).

DO:
  ASSIGN 
    cBrukerId = ENTRY(1,cFieldValues,'|')
    .
  FIND Bruker NO-LOCK WHERE 
    Bruker.BrukerID = cBrukerId NO-ERROR.
  IF NOT AVAILABLE Bruker THEN 
  DO:
    ocValue = 'Bruker ' + cBrukerId + ' er ikke gitt tilgang til reservasjonsordre. Kontakt systemansvarlig.'.
    RETURN.
  END.

  CASE icAction:
    WHEN 'create' THEN 
      DO:
                  
        IF AVAILABLE Butiker THEN RELEASE Butiker.
        FIND Butiker NO-LOCK WHERE
          Butiker.Butik = INT(ENTRY(2,cFieldValues,'|')) NO-ERROR.
        ASSIGN 
          iPreFix   = INT(ENTRY(2,cFieldValues,'|'))
          iPrefix   = IF iPrefix < 100 THEN iPrefix + 100 ELSE iPreFix
          .
        RUN getBuntNr (INPUT iPreFix, OUTPUT iBuntNr).
        ASSIGN 
          ihBuffer:BUFFER-FIELD("BuntNr"):BUFFER-VALUE         = iBuntNr
          ihBuffer:BUFFER-FIELD("RegistrertAv"):BUFFER-VALUE   = cBrukerId
          ihBuffer:BUFFER-FIELD("RegistrertDato"):BUFFER-VALUE = TODAY 
          ihBuffer:BUFFER-FIELD("RegistrertTid"):BUFFER-VALUE  = TIME
          ihBuffer:BUFFER-FIELD("TilButNr"):BUFFER-VALUE = INT(ENTRY(2,cFieldValues,'|'))
          ihBuffer:BUFFER-FIELD("FraButNr"):BUFFER-VALUE = INT(ENTRY(3,cFieldValues,'|'))

          ihBuffer:BUFFER-FIELD("Merknad"):BUFFER-VALUE = ENTRY(4,cFieldValues,'|') + ' fra ' + 
                  cBrukerId + ' i butikk ' + (IF AVAILABLE Butiker THEN Butiker.ButNamn ELSE '**Ukjent') + 
                  ' ' + STRING(NOW,"99/99/9999 HH:MM:SS").

          ihBuffer:BUFFER-FIELD("BuntStatus"):BUFFER-VALUE = INT(ENTRY(5,cFieldValues,'|')).
          ihBuffer:BUFFER-FIELD("Opphav"):BUFFER-VALUE = INT(ENTRY(6,cFieldValues,'|')).
          .

        RUN AddLogg (iBuntNr, 'Reservasjon opprettet.', cBrukerId).         
        rStandardFunksjoner:SkrivTilLogg(cLogg,
          ' Ny OvBunt: ' + STRING(iBuntNr) + ' ' + ihBuffer:BUFFER-FIELD("Merknad"):BUFFER-VALUE 
          ).
      END.
    WHEN 'update' THEN 
      DO:
        ASSIGN 
          cBrukerId = ENTRY(1,cFieldValues,'|')
          .
        ASSIGN 
          ihBuffer:BUFFER-FIELD("Brukerid"):BUFFER-VALUE = cBrukerId
          ihBuffer:BUFFER-FIELD("EDato"):BUFFER-VALUE    = TODAY 
          ihBuffer:BUFFER-FIELD("ETid"):BUFFER-VALUE     = TIME
          .
        RUN AddLogg (ihBuffer:BUFFER-FIELD("BuntNr"):BUFFER-VALUE, 'Endret: ' + cFields + ' ' + cFieldValues, ihBuffer:BUFFER-FIELD("Brukerid"):BUFFER-VALUE).                 
      END.
    WHEN 'Delete' THEN 
      DO:
        FOR EACH OvNotat EXCLUSIVE-LOCK WHERE 
          OvNotat.BuntNr = ihBuffer:BUFFER-FIELD("BuntNr"):BUFFER-VALUE:
          DELETE OvNotat.  
        END.
      END.
  END CASE.

  rStandardFunksjoner:SkrivTilLogg(cLogg,
      ' Feltoppdateringer' 
      ).
  
  IF cFields <> '' THEN 
  LOOPEN:
  DO iLoop = 1 TO NUM-ENTRIES(cFields):
    ASSIGN 
      cFelt  = ENTRY(iLoop,cFields)
      cVerdi = ENTRY(iLoop,cFieldValues,'|')
      .
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '   cFelt/Verdi: ' + cFelt + '/' + cVerdi 
        ).

    CASE cFelt:
/*      WHEN 'Merknad'  THEN                                                                                                                  */
/*      DO:                                                                                                                                   */
/*        ASSIGN                                                                                                                              */
/*          ihBuffer:BUFFER-FIELD("Merknad"):BUFFER-VALUE = (IF ihBuffer:BUFFER-FIELD("Merknad"):BUFFER-VALUE <> '' THEN ' ' ELSE '') + cVerdi*/
/*          .                                                                                                                                 */
/*        rStandardFunksjoner:SkrivTilLogg(cLogg,                                                                                             */
/*            '    Merknad: ' + ihBuffer:BUFFER-FIELD("Merknad"):BUFFER-VALUE                                                                 */
/*            ).                                                                                                                              */
/*      END.                                                                                                                                  */
      WHEN 'BuntStatus' THEN 
      DO:        
        IF INT(cVerdi) = 20 THEN 
          NYORDREGODKJENT:
          DO:
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                ' Ordre SENDT - Status: ' + cVerdi + '.' 
                ).
/*            ihBuffer:BUFFER-FIELD("BuntStatus"):BUFFER-VALUE = INT(cVerdi).*/
            RUN sendEMail(1,ihBuffer:BUFFER-FIELD("BuntNr"):BUFFER-VALUE).
          END. /* NYORDREGODKJENT */
        ELSE IF INT(cVerdi) = 30 THEN
        OPPDATERBUNT: 
        DO:
          IF SEARCH("dOvBunt.w") NE ? OR SEARCH("dOvBunt.r") NE ? THEN
          DO:
            RUN dOvBunt.w PERSIST SET hdOvBunt.
            IF VALID-HANDLE(hdOvBunt) THEN 
            DO:
              RUN OppdaterTransLogg IN hdOvBunt (ihBuffer:BUFFER-FIELD("BuntNr"):BUFFER-VALUE).
              DELETE PROCEDURE hdOvBunt.
/*              ihBuffer:BUFFER-FIELD("BuntStatus"):BUFFER-VALUE = INT(cVerdi).*/
              rStandardFunksjoner:SkrivTilLogg(cLogg,
                  ' Ordre GODKJENT - Status: ' + cVerdi + '.' 
                  ).
              RUN sendEMail(2,ihBuffer:BUFFER-FIELD("BuntNr"):BUFFER-VALUE).
            END.
          END.        
        END. /* OPPDATERBUNT */
        ELSE IF INT(cVerdi) = 40 THEN 
          ORDREAVVIST:
          DO:
/*            ihBuffer:BUFFER-FIELD("BuntStatus"):BUFFER-VALUE = INT(cVerdi).*/
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                ' Ordre AVVIST - Status: ' + cVerdi + '.' 
                ).
            RUN sendEMail(3,ihBuffer:BUFFER-FIELD("BuntNr"):BUFFER-VALUE).
          END. /* ORDREAVVIST */
        ELSE 
          ihBuffer:BUFFER-FIELD("BuntStatus"):BUFFER-VALUE = INT(cVerdi).
      END. /* BuntStatus */
    END CASE.
  END. /* LOOPEN */
END.

rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Slutt.' 
    ).

/* **********************  Internal Procedures  *********************** */

PROCEDURE getBuntNr:
/*------------------------------------------------------------------------------
 Purpose: Fra Create trigger. Setting av neste buntnr.
 Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER piPrefix AS INTEGER NO-UNDO.
  DEFINE OUTPUT PARAMETER piBuntNr AS INTEGER NO-UNDO.
  
  DEFINE VARIABLE trgBuntNr AS INTEGER NO-UNDO.
  DEFINE BUFFER trgOvBunt FOR OvBunt.

  FIND LAST trgOvBunt NO-LOCK WHERE 
    trgOvBunt.BuntNr >= INT(STRING(piPrefix,"999") + "0000000") AND  
    trgOvBunt.BuntNr <= INT(STRING(piPrefix,"999") + "9999999")   
    USE-INDEX BuntNr NO-ERROR.
  IF AVAILABLE trgOvbunt THEN 
    trgBuntNr = trgOvbunt.BuntNr + 1.
  ELSE 
    trgBuntNr = INT(STRING(piPrefix,"999") + "0000001").
  ASSIGN 
    piBuntNr = trgBuntNr.

END PROCEDURE.

PROCEDURE AddLogg:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER piBuntNr AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pcTekst AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pcBrukerId AS CHARACTER NO-UNDO.
  
  DEFINE VARIABLE piLinjeNr AS INTEGER NO-UNDO.
  
  DEFINE BUFFER bufOvNotat FOR OvNotat.
  
  DO FOR bufOvNotat TRANSACTION:
    FIND FIRST bufOvNotat NO-LOCK WHERE 
      bufOvNotat.BuntNr = piBuntNr USE-INDEX idxOvNotat NO-ERROR.
    IF AVAILABLE bufOvNotat THEN 
      piLinjeNr = bufOvNotat.LinjeNr + 1.
    ELSE 
      piLinjeNr = 1.
      
    CREATE bufOvNotat.
    ASSIGN 
      bufOvNotat.BuntNr   = piBuntNr
      bufOvNotat.Notat    = pcTekst
      bufOvNotat.LinjeNr  = piLinjeNr
      bufOvNotat.BrukerId = IF pcBrukerId = '' THEN 'batch' ELSE pcBrukerId
      bufOvNotat.DatoTid  = NOW
      .
    RELEASE bufOvNotat. 
  END.

END PROCEDURE.

PROCEDURE sendEMail:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER piModus AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER piBuntNr AS INTEGER NO-UNDO.

DEFINE VARIABLE cFil AS CHARACTER NO-UNDO.
DEFINE VARIABLE pcMerknad AS CHARACTER NO-UNDO.

DEFINE BUFFER bufFraButiker FOR Butiker.
DEFINE BUFFER bufTilButiker FOR Butiker.
DEFINE VARIABLE cTest AS CHARACTER NO-UNDO.

FIND Ovbunt NO-LOCK WHERE 
  OvBunt.BuntNr = piBuntNr NO-ERROR.
IF AVAILABLE OvBunt THEN 
DO:
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '   Sender eMail.' 
      ).
  FIND bufFraButiker NO-LOCK WHERE 
    BufFraButiker.butik = OvBunt.FraButNr NO-ERROR.
  FIND bufTilButiker NO-LOCK WHERE 
    BufTilButiker.butik = OvBunt.TilButNr NO-ERROR.
  cFil = 'konv\ReservasjonsMail' + REPLACE(STRING(TODAY),'/','') + '_' + REPLACE(STRING(TIME,"HH:MM:SS"),':','') + '.txt'.
  OUTPUT STREAM Ut TO VALUE(cFil).
  PUT STREAM Ut 
    'Lev.art.nr          ' /* 20 */
    'Lev.fargekode       ' /* 20 */
    'Varetekst                     ' /* 30 */
    'Størrelse     ' /* 14*/
    'Antall   ' /* 9 */
    'Pris      ' /* 10 */
    'Merknad             ' /* 20 */
    SKIP.
    
  /* Klargjør og sender eMail */
  FOR EACH OvBuffer OF OvBunt NO-LOCK: 

    FIND ArtBas NO-LOCK WHERE 
      ArtBas.ArtikkelNr = OvBuffer.ArtikkelNr NO-ERROR.
    IF AVAILABLE bufFraButiker THEN 
      FIND ArtPris NO-LOCK WHERE 
        ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND 
        ArtPris.ProfilNr = bufFraButiker.ProfilNr NO-ERROR.
    IF NOT AVAILABLE ArtPris THEN 
      FIND FIRST ArtPris OF ArtBas NO-LOCK NO-ERROR.
    /*Merknad */
    IF OvBuffer.Antall = 0 THEN 
      pcMerknad = 'AVVIST'.
    ELSE IF OvBuffer.Antall > 0 AND NUM-ENTRIES(Ovbuffer.Merknad,'=') >= 2 THEN 
      pcMerknad = 'ANTALL ENDRET'.
    ELSE 
      pcMerknad = ''.  
      
    PUT STREAM Ut 
      STRING(ArtBas.LevKod) + fill(' ',20 - LENGTH(STRING(ArtBas.LevKod))) FORMAT "x(20)"
      STRING(ArtBas.LevFargKod) + fill(' ',20 - LENGTH(STRING(ArtBas.LevFargKod))) FORMAT "x(20)"
      STRING(ArtBas.Beskr) + fill(' ',30 - LENGTH(STRING(ArtBas.Beskr))) FORMAT "x(30)"
      STRING(OvBuffer.Storl) + fill(' ',14 - LENGTH(STRING(OvBuffer.Storl))) FORMAT "x(14)"
      STRING(OvBuffer.Antall) + fill(' ',9 - LENGTH(STRING(OvBuffer.Antall))) FORMAT "x(9)"
      STRING(ArtPris.Pris[1]) + fill(' ',10 - LENGTH(STRING(ArtPris.Pris[1]))) FORMAT "x(10)"
      STRING(pcMerknad) + fill(' ',20 - LENGTH(STRING(pcMerknad))) FORMAT "x(20)"
      SKIP.
  END.
  OUTPUT STREAM Ut CLOSE.
END.

FILE-INFO:FILE-NAME = cFil.

rSendEMail:parMailType = 'PRISMODELL'.
IF piModus = 1 THEN 
  DO:
    {syspara.i 50 50 61 cparToADDRESS}   
    rSendEMail:parSUBJECT  = cTest + OvBunt.Merknad.    
    rSendEMail:parMESSAGE  = "Reservasjonsordre: " + STRING(OvBunt.BuntNr) + '.' + CHR(10) + 
                             "Anmoding om reservering av varer fra butikk " + STRING(bufTilButiker.Butik) + ' ' + bufTilButiker.ButNamn.
  END.
ELSE IF piModus = 2 THEN 
  DO:
    cparToADDRESS = bufTilButiker.ePostAdresse.
    rSendEMail:parSUBJECT  = cTest + 'GODKJENT eCom: ' + OvBunt.Merknad.    
    rSendEMail:parMESSAGE  = "Reservasjonsordre: " + STRING(OvBunt.BuntNr) + '.' + CHR(10) + 
                             "Reserversjonsanmoding GODKJENT fra " + STRING(bufFraButiker.Butik) + ' ' + bufFraButiker.ButNamn.
  END.
ELSE IF piModus = 3 THEN 
  DO:
    cparToADDRESS = bufTilButiker.ePostAdresse.
    rSendEMail:parSUBJECT  = cTest + 'AVVIST eCom: ' + OvBunt.Merknad.    
    rSendEMail:parMESSAGE  = "Reservasjonsordre: " + STRING(OvBunt.BuntNr) + '.' + CHR(10) + 
                             "Reserversjonsanmoding AVVIST fra " + STRING(bufFraButiker.Butik) + ' ' + bufFraButiker.ButNamn.
  END.
/* Legger på ekstra adresser for oppfølging. */
cparToADDRESS = cparToADDRESS + ';are@gant.no;tomn@nsoft.no'.
rSendEMail:parToADDRESS = cparToADDRESS.

/* TEST TEST TEST TEST */
IF SEARCH('tnc.txt') <> ? THEN
DO:
/*  rStandardFunksjoner:SkrivTilLogg (cLogg,'    TEST eMail. mottagerliste før overstyring: ' + cparToADDRESS + '.').*/
/*  cparToADDRESS = 'tomn@nsoft.no'. /* TEST */                                                                      */
/*  cTest = 'TEST '. /* TEST */                                                                                      */
/*  rSendEMail:parSUBJECT  = cTest + rSendEMail:parSUBJECT.                                                          */
END.
/* TEST TEST TEST TEST */


IF cparToADDRESS = '' THEN 
DO:
  rStandardFunksjoner:SkrivTilLogg (cLogg,'      Adresseliste for mottager av prissjekk Syspara: 50 50 36, er ikke satt opp.').
  RETURN.
END.

rSendEMail:parFILE     = FILE-INFO:FULL-PATHNAME.  

rStandardFunksjoner:SkrivTilLogg (cLogg,'    eMail info:').
rStandardFunksjoner:SkrivTilLogg (cLogg,'        ' + rSendEMail:parMailType).
rStandardFunksjoner:SkrivTilLogg (cLogg,'        ' + rSendEMail:parSUBJECT).
rStandardFunksjoner:SkrivTilLogg (cLogg,'        ' + STRING(rSendEMail:parMESSAGE)).
rStandardFunksjoner:SkrivTilLogg (cLogg,'        ' + rSendEMail:parFILE).

bOk = rSendEMail:send( ).

rStandardFunksjoner:SkrivTilLogg (cLogg,'    eMail sende resultat: ' + STRING(bOk)).
                    
IF ERROR-STATUS:ERROR THEN 
    DO:
        rStandardFunksjoner:SkrivTilLogg (cLogg,'    **FEIL. eMail ikke sendt. Vedlegg ' + FILE-INFO:FULL-PATHNAME + '.').
        DO ix = 1 TO ERROR-STATUS:NUM-MESSAGES:
            rStandardFunksjoner:SkrivTilLogg (cLogg, '          ' 
                + STRING(ERROR-STATUS:GET-NUMBER(ix)) + ' ' + ERROR-STATUS:GET-MESSAGE(ix)    
                ).
        END.            
    END.

END PROCEDURE.
