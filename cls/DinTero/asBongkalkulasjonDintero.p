
/*------------------------------------------------------------------------
    File        : bongkalkulasjonDintero.p
    Purpose     : App server rutine som lar kassen kalle opp Dintero og 
                  få påført rabatter på bongen.

    Syntax      :

    Description : 

    Author(s)   : Tom Nøkleby
    Created     : Thu Nov 15 10:54:47 CET 2020
    
    Notes       : NB: --- VIKTIG ---
                  Kallende rutine må benytte BY-REFERENCE. Gjøres ikke 
                  dette, vil denne rutinen arbeide med en kopi av 
                  datasettet, og ikke det datasettet som skal endres.
                  
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
  {cls\dintero\ttPOSBong.i}
  {cls\dintero\dsPOSBong.i}

  DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsPOSBong.
  DEFINE OUTPUT PARAMETER bReturn AS LOG.
  DEFINE OUTPUT PARAMETER cReturn AS CHARACTER NO-UNDO.
  
  DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lB_Id AS DECIMAL NO-UNDO.
  DEFINE VARIABLE bTest AS LOG NO-UNDO.  

  DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.
  DEFINE VARIABLE rCustomerDintero    AS cls.Dintero.CustomerDintero NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
rStandardFunksjoner = NEW cls.StdFunk.StandardFunksjoner( ) NO-ERROR.
rCustomerDintero = NEW cls.Dintero.CustomerDintero( ) NO-ERROR.

/*SESSION:ERROR-STACK-TRACE = TRUE.                                                                             */
/*SESSION:DEBUG-ALERT       = TRUE.                                                                             */
/*LOG-MANAGER:LOGFILE-NAME  = 'konv\bongkalkulasjonDinteroERROR-STACK' + REPLACE(STRING(TODAY),'/','') + '.log'.*/
/*LOG-MANAGER:LOGGING-LEVEL = 4.                                                                                */
/*LOG-MANAGER:CLEAR-LOG().                                                                                      */

ASSIGN 
  bTest = TRUE
  cLogg = 'bongkalkulasjonDintero' + REPLACE(STRING(TODAY,'99/99/9999'),'/','')
  .

RUN behandleBONG.

RETURN.

/* **********************  Internal Procedures  *********************** */

PROCEDURE behandleBong:
/*------------------------------------------------------------------------------
 Purpose:
 Notes: 
------------------------------------------------------------------------------*/
  /* Sjekker at det finnes data å behnadle. */
  FIND FIRST ttPOSBongHode NO-ERROR.
  IF NOT AVAILABLE ttPOSBongHode THEN 
    DO:
      cReturn = 'Mangler datasett for kvittering.'.
      bReturn = FALSE.
      RETURN.
    END. 
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Bongkalkulasjon kvittering:' + STRING(ttPOSBongHode.B_Id) + 
    ' ButNr: ' + STRING(ttPOSBongHode.ButikkNr) + 
    ' KasseNr: ' + STRING(ttPOSBongHode.KasseNr) + 
    ' Dato: ' + STRING(ttPOSBongHode.Dato) + 
    ' BongNr: ' + STRING(ttPOSBongHode.BongNr) + 
    ' MedlemsNr: ' + STRING(ttPOSBongHode.MedlemsNr) + '.'    
    ).    

  IF bTest THEN 
  DO:
    /* NB: Denne gjør ttPOSBongHode ikke tilgjengelig !!! */
    DATASET dsPOSBong:WRITE-JSON('file', 'konv\dsPOSBong' + STRING(ttPOSBongHode.B_Id) + '_' + STRING(TIME) + '_.json', TRUE).
    FIND FIRST ttPOSBongHode NO-ERROR.
  END.
  IF NOT rCustomerDintero:PostReceiptForDiscount( INPUT-OUTPUT DATASET dsPOSBong  BY-REFERENCE, OUTPUT cReturn ) THEN
    DO:
      FIND FIRST ttPOSBongHode NO-ERROR.
      rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  Feil kalkulering av bong ' + STRING(ttPOSBongHode.B_Id) + '. Årsak: ' + cReturn
        ).
      cReturn = '  Feil kalkulering av bong ' + STRING(ttPOSBongHode.B_Id) + '. Årsak: ' + cReturn.
    END.
  ELSE DO:
    FIND FIRST ttPOSBongHode NO-ERROR.
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  Opprettet Bong. '  + STRING(ttPOSBongHode.B_Id) + '.'
      ).
    cReturn = '  OK kalkulering av bong ' + STRING(ttPOSBongHode.B_Id) + '.'.
    bReturn = TRUE.

    IF bTest THEN 
    DO:
      DATASET dsPOSBong:WRITE-JSON('file', 'konv\dsPOSBongKalkulert' + STRING(ttPOSBongHode.B_Id) + '.json', TRUE).
    END.
  END.

END PROCEDURE.


