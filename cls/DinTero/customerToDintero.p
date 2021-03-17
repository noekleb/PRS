
/*------------------------------------------------------------------------
    File        : customerToDintero.p
    Purpose     : Batch rutine som kontinuerlig sender endrede medlemmer til Dintero.

    Syntax      :

    Description : Oppdaterer medlemmer logget i ELogg til Dintero.

    Author(s)   : Tom Nøkleby
    Created     : Thu Nov 05 10:54:47 CET 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
  {cls\dintero\ttMedlem.i}
  {cls\dintero\dsMedlem.i}
  {server\ttELogg.i}
  
  DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lMedlemsNr AS DECIMAL NO-UNDO.
  DEFINE VARIABLE bTest AS LOG NO-UNDO.  

  DEFINE BUFFER bELogg FOR ELogg.
  
  DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.
  DEFINE VARIABLE rCustomerDintero    AS cls.Dintero.CustomerDintero NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
rStandardFunksjoner = NEW cls.StdFunk.StandardFunksjoner( ) NO-ERROR.
rCustomerDintero = NEW cls.Dintero.CustomerDintero( ) NO-ERROR.

ASSIGN 
  bTest = TRUE
  cLogg = 'customerToDintero' + REPLACE(STRING(TODAY,'99/99/9999'),'/','')
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
  EMPTY TEMP-TABLE ttELogg.
  
  LOOPEN:
  FOR EACH ELogg NO-LOCK WHERE 
    ELogg.TabellNavn     = 'Medlem' AND
    ELogg.EksterntSystem = "WEBINIT":
      
    FIND bElogg EXCLUSIVE WHERE 
      ROWID(bElogg) = ROWID(Elogg) NO-WAIT NO-ERROR.
    IF NOT AVAIL bElogg OR LOCKED bELogg THEN
        NEXT.
    BUFFER-COPY ELogg TO ttELogg NO-ERROR.
    DELETE bELogg.
    IF AVAILABLE ttElogg THEN
        RELEASE ttELogg.
  END. /* LOOPEN */

END PROCEDURE.

PROCEDURE behandleELogg:
/*------------------------------------------------------------------------------
 Purpose:
 Notes: Medlem.EksterntMedlemsNr inneholder customer_Id fra Dintero hvis
        medlemmet tidligere er opprettet hos Dintero.
------------------------------------------------------------------------------*/
  DEFINE VARIABLE piType AS INTEGER NO-UNDO.
  DEFINE VARIABLE piNy AS INTEGER NO-UNDO. /* 0-Ukjent, 1-Finnes */
  
  LESELOGG:
  FOR EACH ttELogg:
    ASSIGN 
      lMedlemsNr = DEC(ttELogg.Verdier) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN 
      NEXT.
    
    /* Sjekker om medlemmet finnes. */
    FIND Medlem NO-LOCK WHERE 
      Medlem.MedlemsNr = lMedlemsNr NO-ERROR.
    /* Finnes medlemmet, sjekkes det om medlemmet finnes i DinTero. */
    IF AVAILABLE Medlem THEN 
    DO:
      piNy = 0.
      /* Bestemmer om det er nytt medlem, eller oppdatering av eksisterende medlem. */
      piType = 1. /* Sjekker mot eMail. */    
      IF rCustomerDintero:UserExist(piType, Medlem.ePostAdresse, cReturn) THEN
        DO:
          piNy = 1.
          rStandardFunksjoner:SkrivTilLogg(cLogg,
            '  Medlem funnet via epostadresse. ' + STRING(Medlem.MedlemsNr) + ' ' + Medlem.fornavn + ' ' + Medlem.etternavn + '.'    
            ).    
        END.
      piType = 2. /* Sjekker mot mobilNr med landkode. */    
      IF piNy = 0 THEN 
        DO:
          IF rCustomerDintero:UserExist(piType, Medlem.ePostAdresse, cReturn) THEN
            DO:
              piNy = 1.
              rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  Medlem funnet via mobilnr. med landkode ' + STRING(Medlem.MedlemsNr) + ' ' + Medlem.fornavn + ' ' + Medlem.etternavn + '.'    
                ).    
            END.
        END.
      piType = 3. /* Sjekker mot mobilNr uten landkode. */    
      IF piNy = 0 THEN 
        DO:
          IF rCustomerDintero:UserExist(piType, Medlem.ePostAdresse, cReturn) THEN
            DO:
              piNy = 1.
              rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  Medlem funnet via mobilnr. uten landkode ' + STRING(Medlem.MedlemsNr) + ' ' + Medlem.fornavn + ' ' + Medlem.etternavn + '.'    
                ).    
            END.
        END.
      
      /* Oppretter ny kunde i dintero. */
      IF piNy = 0 THEN 
        OPPRETTKUNDE:
        DO:
          rStandardFunksjoner:SkrivTilLogg(cLogg,
            '  Nytt medlem. Opprettes i Dintero: ' + STRING(Medlem.MedlemsNr) + ' ' + Medlem.fornavn + ' ' + Medlem.etternavn + '.'    
            ).    
          
          /* Fyll opp datasett og sender medlemmet til Dintero.                            */
          /* Ligger det medlemmer der fra før, tømmes datasettet før medlemmet legges inn. */
          RUN cls\dintero\fyllDatasettMedlem.p (Medlem.MedlemsNr, FALSE, INPUT-OUTPUT DATASET dsMedlem BY-REFERENCE).
          IF bTest THEN 
            DATASET dsMedlem:WRITE-JSON('file', 'konv\dsMedlem' + STRING(ETIME) + '.json', TRUE).
          
          IF NOT rCustomerDintero:CreateNewCustomer( INPUT DATASET dsMedlem, OUTPUT cReturn ) THEN 
            DO:
              rStandardFunksjoner:SkrivTilLogg(cLogg,
                'Feil ved opprettelse av medlemsnr. ' + STRING(lMedlemsNr) + ' Årsak: ' + cReturn  
                ).    
            END.
          ELSE   
            rStandardFunksjoner:SkrivTilLogg(cLogg,
              'Opprettet medlemsnr. '  + STRING(Medlem.MedlemsNr) + ' ' + Medlem.fornavn + ' ' + Medlem.etternavn + '.'  
              ).    
        END. /* OPPRETTKUNDE */
      ELSE 
      OPPDATERKUNDE:
      DO:
        IF LENGTH(Medlem.EksterntMedlemsnr) <= 15 THEN 
          DO:
            rStandardFunksjoner:SkrivTilLogg(cLogg,
              '  Medlemmet finnes i Dintero, men lokalt er ikkcustomer_id satt. ' + STRING(Medlem.MedlemsNr)  
              ).    
            LEAVE OPPDATERKUNDE.
          END.        
        rStandardFunksjoner:SkrivTilLogg(cLogg,
          '  Eksisterende medlem oppdateres. ' + STRING(lMedlemsNr) + ' Melding: ' + cReturn  
          ).    
        /* Fyll opp datasett og sender medlemmet til Dintero.                            */
        /* Ligger det medlemmer der fra før, tømmes datasettet før medlemmet legges inn. */
        RUN cls\dintero\fyllDatasettMedlem.p (Medlem.MedlemsNr, FALSE, INPUT-OUTPUT DATASET dsMedlem BY-REFERENCE).
        IF bTest THEN
          DATASET dsMedlem:WRITE-JSON('file', 'konv\dsMedlem' + STRING(ETIME) + '.json', TRUE).
        
        IF NOT rCustomerDintero:UpdateCustomer( INPUT DATASET dsMedlem, OUTPUT cReturn ) THEN 
          DO:
            rStandardFunksjoner:SkrivTilLogg(cLogg,
              'Feil ved oppdatering av medlem. ' + STRING(lMedlemsNr) + ' Årsak: ' + cReturn  
              ).    
          END.
        ELSE   
          rStandardFunksjoner:SkrivTilLogg(cLogg,
            'Oppdatert medlem: '  + STRING(Medlem.MedlemsNr) + ' ' + Medlem.fornavn + ' ' + Medlem.etternavn + '.'  
            ).    
      END. /* OPPDATERKUNDE */
    END.
  
END. /* LESELOGG */ 


END PROCEDURE.

