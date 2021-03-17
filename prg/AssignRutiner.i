/* Standard rutiner FOR ASSIGN av felt */

PROCEDURE AssignDate:
  /*------------------------------------------------------------------------------
      Purpose:                                      
      Notes:                                      
  ------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER iInt AS INTEGER NO-UNDO.
  DEFINE OUTPUT PARAMETER dOut AS DATE NO-UNDO.
  DEFINE OUTPUT PARAMETER obOk AS LOG INITIAL TRUE NO-UNDO.
  
    ASSIGN 
        dOut = IF ENTRY(iInt,pcLinje,';') <> ''
                 THEN DATE(REPLACE(REPLACE(ENTRY(iInt,pcLinje,';'),'.','/'),'-','/'))  /* Forventet format DD/MM/ÅÅÅÅ */
                 ELSE ?
        NO-ERROR.
    
    IF ERROR-STATUS:ERROR THEN 
    DO:
      ASSIGN
        piAntFeil = piAntFeil + 1
        .
      CREATE ttError.
      ASSIGN
        obOk            = FALSE
        ttError.LinjeNr = piAntFeil
        ttError.Tekst   = '** Feil ved ASSIGN på dato kolonne (Linje ikke innlest) ' + STRING(iInt) +  ' LinjeNr: ' + STRING(iAntLinjer) + ' Data: ' + ENTRY(iInt,pcLinje,';')
        .
    END.
END PROCEDURE.

PROCEDURE AssignDec:
  /*------------------------------------------------------------------------------
      Purpose:                                      
      Notes:                                      
  ------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER iInt AS INTEGER NO-UNDO.
  DEFINE OUTPUT PARAMETER lOut AS DECIMAL NO-UNDO.
  DEFINE OUTPUT PARAMETER obOk AS LOG INITIAL TRUE NO-UNDO.
  
    ASSIGN lOut = DECIMAL(TRIM(REPLACE(ENTRY(iInt,pcLinje,';'),'%',''))) NO-ERROR.
    
    IF ERROR-STATUS:ERROR THEN 
    DO:
      ASSIGN
        piAntFeil = piAntFeil + 1
        .
      CREATE ttError.
      ASSIGN
        obOk            = FALSE
        ttError.LinjeNr = piAntFeil
        ttError.Tekst   = '** Feil ved ASSIGN på decimal kolonne (Linje ikke innlest) ' + STRING(iInt) +  ' LinjeNr: ' + STRING(iAntLinjer) + ' Data: ' + ENTRY(iInt,pcLinje,';')
        .
    END.
 


END PROCEDURE.
  
PROCEDURE AssignInt:
  /*------------------------------------------------------------------------------
      Purpose:                                      
      Notes:                                      
  ------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER iInt AS INTEGER NO-UNDO.
  DEFINE OUTPUT PARAMETER iOut AS INTEGER NO-UNDO.
  DEFINE OUTPUT PARAMETER obOk AS LOG INITIAL TRUE NO-UNDO.
  
    ASSIGN iOut = INT(ENTRY(iInt,pcLinje,';')) NO-ERROR.
    
    IF ERROR-STATUS:ERROR THEN 
    DO:
      ASSIGN
        piAntFeil = piAntFeil + 1
        .
      CREATE ttError.
      ASSIGN
        obOk            = FALSE
        ttError.LinjeNr = piAntFeil
        ttError.Tekst   = '** Feil ved ASSIGN på integer kolonne (Linje ikke innlest) ' + STRING(iInt) +  ' LinjeNr: ' + STRING(iAntLinjer) + ' Data: ' + ENTRY(iInt,pcLinje,';')
        .
    END. 
END PROCEDURE.

PROCEDURE ErrorLogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE pcTekst AS CHARACTER NO-UNDO.

IF NOT CAN-FIND(FIRST ttError)
  THEN RETURN.
  
IF AVAILABLE VPIFilHode 
  THEN pcTekst = "Feil i fil: " + VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn.
  ELSE pcTekst = "Ukjent/slettet VPI filhode (xPRSButInnles.p).".

  OUTPUT TO VALUE("Error.Txt").
    PUT UNFORMATTED
      "Innlesning " + STRING(TODAY) + "  " + STRING(TIME,"HH:MM:SS") + "." SKIP
      pcTekst SKIP
      .
    FOR EACH ttError:
      PUT UNFORMATTED ttError.Tekst SKIP.
    END.
  OUTPUT CLOSE.
  IF SEARCH("Error.Txt") <> ? THEN
  DO:
    DEF VAR hInstance AS INT.

    RUN ShellExecute{&A} IN hpApi(0,
                                  "open",
                                  "notepad.exe",
                                  SEARCH("Error.Txt"),
                                  "",
                                  1,
                                  OUTPUT hInstance).

  END.

END PROCEDURE.

PROCEDURE TellOppLinjer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN
      iTotAntLinjer = 0
      .
  INPUT STREAM InnFil FROM VALUE(VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn) NO-ECHO.
  REPEAT:
    IMPORT STREAM InnFil UNFORMATTED cLinje.
    ASSIGN
        iTotAntLinjer = iTotAntLinjer + 1
        .
  END.
  INPUT STREAM InnFil CLOSE.

END PROCEDURE.


