DEFINE VARIABLE dDato AS DATE NO-UNDO.
DEFINE VARIABLE cLogg               AS CHARACTER                      NO-UNDO.
DEFINE VARIABLE bOk                 AS LOG                            NO-UNDO.
DEFINE VARIABLE cReturn             AS CHARACTER                      NO-UNDO.
DEFINE VARIABLE bTest               AS LOG                            NO-UNDO.
DEFINE VARIABLE iX                  AS INTEGER                        NO-UNDO.
DEFINE VARIABLE iTime               AS INTEGER                        NO-UNDO.
DEFINE VARIABLE cStoppFil           AS CHARACTER                      NO-UNDO.
DEFINE VARIABLE cLogKat             AS CHARACTER                      NO-UNDO.
DEFINE VARIABLE iAntfiler           AS INTEGER                        NO-UNDO.
DEFINE VARIABLE cMailLogg           AS CHARACTER NO-UNDO. 

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.
DEFINE VARIABLE rpksdlEDIWeb        AS cls.Pakkseddel.pksdlEDIWeb     NO-UNDO.
DEFINE VARIABLE rSendEMail          AS cls.SendEMail.SendEMail        NO-UNDO.

DEFINE STREAM Ut.

{ cls\StdFunk\filliste.i }

HOVEDBLOKK:
DO  ON ERROR  UNDO, LEAVE
  ON ENDKEY UNDO, LEAVE
  ON STOP   UNDO, LEAVE
  ON QUIT   UNDO, LEAVE:

  ASSIGN 
    cLogKat = 'log\' 
    dDato   = TODAY 
    iTime   = TIME
    cLogg   = 'pksdlEDIWeb' + REPLACE(STRING(TODAY),'/','')
    bTest   = TRUE 
    .
  rStandardFunksjoner = NEW cls.Stdfunk.StandardFunksjoner( INPUT cLogg ).
  rSendeMail          = NEW cls.SendeMail.SendeMail( ) NO-ERROR.
      
  /* Starter med tom linje i loggen. */
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '' 
    ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Start.' 
    ).    

  /* Starter klassene. */
  rpksdlEDIWeb  = NEW cls.Pakkseddel.pksdlEDIWeb( INPUT cLogg ).

  BLOKKEN:
  DO:
    /* Henter liste med filer fra POS. */
    rpksdlEDIWeb:byggFilListe( INPUT-OUTPUT TABLE tmpfiler ).

    IF CAN-FIND(FIRST tmpFiler) THEN
    DO: 
      /* Teller og loggger antall filer som skal leses inn. */
      RUN tellAntallFiler(OUTPUT iAntfiler).

      /* Leser inn filene i listen. */
      IMPORTBLOKK:
      FOR EACH tmpFiler:
        bOk = rpksdlEDIWeb:importerFil( tmpFiler.Full-Path-Name, OUTPUT cMailLogg ).
        IF bOk AND cMailLogg <> '' THEN
        MAILBLOKK: 
        DO:
          FILE-INFO:FILE-NAME = SEARCH(cMailLogg).
          RUN sendMail ( FILE-INFO:FULL-PATHNAME ).
          rpksdlEDIWeb:backupAvFil( tmpFiler.Full-Path-Name ).
        END. /* MAILBLOKK */
      END. /* IMPORTBLOKK */
    END.
        
  END. /* BLOKKEN */

  CATCH e1 AS Progress.Lang.AppError: 
    DO ix = 1 TO e1:NumMessages:
      rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  AppError: ' + e1:GetMessage(ix) 
        ).    
    END.
    
    IF e1:ReturnValue > "" THEN
      rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  Returverdi: ' + e1:ReturnValue 
        ).    
  END CATCH.
  CATCH e2 AS Progress.Lang.Error:
    DO ix = 1 TO e2:NumMessages:
      rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  Lang.Error: ' + e2:GetMessage(ix) 
        ).    
    END.
  END CATCH.
END. /* HOVEDBLOKK */

FINALLY.
  /*    IF VALID-OBJECT(rTemp) THEN DELETE OBJECT rTemp NO-ERROR.*/
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '  Tidsbruk ' + STRING(TIME - iTime,'HH:MM:SS') 
    ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Ferdig.' 
    ).    
  RETURN cReturn.
END FINALLY.

/* **********************  Internal Procedures  *********************** */

PROCEDURE tellAntallFiler:
  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER piAntFiler AS INTEGER NO-UNDO.
  
  FOR EACH tmpfiler:
    piAntFiler = piantfiler + 1.
  END.
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '  Antall filer ' + STRING(piantFiler) 
    ).    

END PROCEDURE.

PROCEDURE sendMail:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER pcFile AS CHARACTER NO-UNDO.
  
  DEFINE VARIABLE ctmpFil AS CHARACTER NO-UNDO.
  
  {syspara.i 50 50 52 rSendEMail:parToADDRESS}
  
  ctmpFil = rStandardFunksjoner:getTempFileName(  ).
  
  OUTPUT STREAM Ut TO value(ctmpFil).
  PUT STREAM Ut UNFORMATTED
    'Import av webedi fil fra Gøteborg lager.'  
    'Oppmerking av pakkseddler som er sendt butikk.' 
    SKIP.
  OUTPUT STREAM Ut CLOSE.
  
  rSendEMail:parToADDRESS       = IF rSendEMail:parToADDRESS = '' THEN 'tomn@nsoft.no' ELSE rSendEMail:parToADDRESS.
  rSendEMail:parMailType        = 'PAKKSEDDEL'.
  rSendEMail:parSUBJECT         = 'EDIWeb Merking av sendte pakkseddler'.
  rSendEMail:parMessage-Charset = 'iso-8859-1'. /* iso-8859-1 Blank eller 'UTF-8' når det går fra fil. */
  /*rSendEMail:parMessage-File    =  'c:\tmp\mailtxt.htm'.*/
  rSendEMail:parMessage-File    = ctmpFil.
  rSendEMail:parFILE            = pcFile.
  rSendEMail:parLOGFILE         = ''.  
  rSendEMail:send( ).

END PROCEDURE.

