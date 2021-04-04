
/*------------------------------------------------------------------------
    File        : receiptsToDintero.p
    Purpose     : Batch rutine som kontinuerlig sender nye bonger til Dintero.

    Syntax      :

    Description : Oppdaterer bonghode logget i ELogg til Dintero.

    Author(s)   : Tom Nøkleby
    Created     : Thu Nov 15 10:54:47 CET 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
  {cls\dintero\ttBong.i}
  {cls\dintero\dsBong.i}
  {server\ttELogg.i} 
  
  DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lB_Id AS DECIMAL NO-UNDO.
  DEFINE VARIABLE bTest AS LOG NO-UNDO.  
  DEFINE VARIABLE iMaksAnt AS INTEGER NO-UNDO.

  DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.
  DEFINE VARIABLE rCustomerDintero    AS cls.Dintero.CustomerDintero NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
rStandardFunksjoner = NEW cls.StdFunk.StandardFunksjoner( ) NO-ERROR.
rCustomerDintero = NEW cls.Dintero.CustomerDintero( ) NO-ERROR.

/*SESSION:ERROR-STACK-TRACE = TRUE.                                                                */
/*SESSION:DEBUG-ALERT       = TRUE.                                                                */
/*LOG-MANAGER:LOGFILE-NAME  = 'konv\REQreceiptsToDintero' + REPLACE(STRING(TODAY),'/','') + '.log'.*/
/*LOG-MANAGER:LOGGING-LEVEL = 6.                                                                   */
/*LOG-MANAGER:CLEAR-LOG().                                                                         */

ASSIGN 
  bTest = FALSE
  cLogg = 'receiptsToDintero' + REPLACE(STRING(TODAY,'99/99/9999'),'/','')
  iMaksant = 1000 /* Settes til 1000 i produksjon. */
  .

RUN lesELogg.  
IF NOT CAN-FIND(FIRST ttELogg) THEN 
DO:
  RETURN.  
END.

RUN behandleELogg.

RETURN.

/* **********************  Internal Procedures  *********************** */

PROCEDURE lesELogg:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE VARIABLE piAnt AS INTEGER INITIAL 1 NO-UNDO.
  
  EMPTY TEMP-TABLE ttELogg.
  
  LOOPEN:
  FOR EACH ELogg NO-LOCK WHERE 
    ELogg.TabellNavn     = 'BongHode' AND
    ELogg.EksterntSystem = "WEBINIT":
      
    IF iMaksant > 0 AND piAnt > iMaksant THEN 
      LEAVE LOOPEN.  
      
    BUFFER-COPY ELogg TO ttELogg NO-ERROR.
    IF AVAILABLE ttElogg THEN
        RELEASE ttELogg.
    
    piAnt = piAnt + 1.
  END. /* LOOPEN */

END PROCEDURE.

PROCEDURE behandleELogg:
/*------------------------------------------------------------------------------
 Purpose:
 Notes: 
------------------------------------------------------------------------------*/

  LESELOGG:
  FOR EACH ttELogg:
    ASSIGN 
      lB_Id = DEC(ttELogg.Verdier) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN 
      NEXT.
    
    /* Sjekker om medlemmet finnes. */
    FIND BongHode NO-LOCK WHERE 
      BongHode.B_Id = lB_Id NO-ERROR.
    /* Finnes medlemmet, sjekkes det om medlemmet finnes i DinTero. */
    IF AVAILABLE BongHode THEN
    OPPRETTBONG: 
    DO:
      /* Bonger som er sendt tildligere. */
      IF CAN-FIND(BongCRMLogg WHERE 
                  BongCRMLogg.B_Id = BongHode.B_Id) THEN 
        DO:
          RUN SlettELogg.
          LEAVE OPPRETTBONG.
        END.
      
        /* Bare salg, retur og reklamasjon som skal sendes over. */
        FIND FIRST BongLinje NO-LOCK WHERE
          BongLinje.B_Id = BongHode.B_Id AND  
          CAN-DO('1,3,10',STRING(BongLinje.TTId)) NO-ERROR.
        IF NOT AVAILABLE BongLinje THEN
          DO:
            RUN SlettELogg.
            LEAVE OPPRETTBONG.
          END.
                      
      rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  Ny bong opprettes i Dintero: B_Id:' + STRING(BongHode.B_Id) + 
        ' ButNr: ' + STRING(BongHode.ButikkNr) + 
        ' KasseNr: ' + STRING(BongHode.KasseNr) + 
        ' Dato: ' + STRING(BongHode.Dato) + 
        ' BongNr: ' + STRING(BongHode.BongNr) + '.'    
        ).    
      
      /* Fyll opp datasett og sender medlemmet til Dintero.                            */
      /* Ligger det medlemmer der fra før, tømmes datasettet før medlemmet legges inn. */
      DATASET dsBong:EMPTY-DATASET ().
      RUN cls\dintero\fyllDatasettBong.p (BongHode.B_Id, FALSE, INPUT-OUTPUT DATASET dsBong BY-REFERENCE).
      IF bTest THEN 
        DATASET dsBong:WRITE-JSON('file', 'konv\dsBong' + STRING(BongHode.B_Id) + '.json', TRUE).
        
      IF NOT rCustomerDintero:CreateNewReceipts( INPUT DATASET dsBong, OUTPUT cReturn ) THEN
        DO:
          rStandardFunksjoner:SkrivTilLogg(cLogg,
            '  Feil ved opprettelse av bong. ' + STRING(BongHode.B_Id) + ' Årsak: ' + cReturn
            ).
        END.
      ELSE
        rStandardFunksjoner:SkrivTilLogg(cLogg,
          '    Opprettet Bong. '  + STRING(BongHode.B_Id) + '.'
          ).

      /* Sletter loggpostene ettersom bongene behandles. */
      RUN SlettELogg.    
    END. /* OPPRETTBONG */
  END. /* LESELOGG */ 


END PROCEDURE.

PROCEDURE SlettELogg:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE BUFFER bufELogg FOR ELogg. 

  DO FOR bufELogg TRANSACTION:
    FIND bufELogg EXCLUSIVE-LOCK WHERE 
      bufELogg.TabellNavn = ttELogg.TabellNavn AND 
      bufELogg.EksterntSystem = ttELogg.EksterntSystem AND 
      bufELogg.Verdier = ttELogg.Verdier NO-WAIT NO-ERROR.
    IF AVAILABLE bufELogg THEN 
      DELETE bufELogg.
  END.


END PROCEDURE.

