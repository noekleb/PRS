/* Registrer Eksporter tgexport record
   Parameter:  
   Opprettet: 5.4.2011             
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEFINE VARIABLE cFilNavn     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cKatalog     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cExtent      AS CHARACTER NO-UNDO.
DEFINE VARIABLE bTgMed       AS LOG NO-UNDO.
DEFINE VARIABLE bTgSales     AS LOG NO-UNDO.
DEFINE VARIABLE bTgSales_Ext AS LOG NO-UNDO.
DEFINE VARIABLE bTgTimeStamp AS LOG NO-UNDO.
DEFINE VARIABLE cTekst       AS CHARACTER NO-UNDO.
DEFINE VARIABLE bGant AS LOG NO-UNDO.

DEFINE TEMP-TABLE bTGTimeStamp LIKE TGTimeStamp.

DEFINE STREAM Ut.

DEF VAR hQuery          AS HANDLE NO-UNDO.

/* Timegrip eksportkatalog. */
{syspara.i 50 21 3 cKatalog}
IF cKatalog <> '' THEN
  cKatalog = RIGHT-TRIM(cKatalog,'\').
ELSE DO:
  {syspara.i 1 1 51 cKatalog}
  IF cKatalog = '' THEN
    cKatalog = 'c:\home\lindbak\sendes'.
  ELSE
    cKatalog = RIGHT-TRIM(cKatalog,'\').
END.
IF cKatalog = '' THEN 
  RETURN.

/* Timegrip eksportparametre. */
{syspara.i 50 21 4 cTekst}
IF CAN-DO('1,J,Ja,True,Yes',cTekst) THEN bTgMed = TRUE.
{syspara.i 50 21 5 cTekst}
IF CAN-DO('1,J,Ja,True,Yes',cTekst) THEN bTgSales = TRUE.
{syspara.i 50 21 6 cTekst}
IF CAN-DO('1,J,Ja,True,Yes',cTekst) THEN bTgSales_Ext = TRUE.
{syspara.i 50 21 7 cTekst}
IF CAN-DO('1,J,Ja,True,Yes',cTekst) THEN bTgTimeStamp = TRUE.
{syspar2.i 50 21 7 cTekst}
IF cTekst = 'GANT' THEN 
    bGant = TRUE.
ELSE 
    bGant = FALSE.
  
CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " NO-LOCK").
hQuery:QUERY-OPEN().

hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  ASSIGN 
    ocReturn = ''
    obOk     = TRUE
  .

  DO TRANSACTION:
    FIND FIRST TGExport WHERE TGExport.TGExportId = DEC(ihBuffer:BUFFER-FIELD('TGExportId'):BUFFER-VALUE)
                         EXCLUSIVE-LOCK NO-ERROR.
    
    IF AVAIL TGExport THEN
    DO:
      
      IF bTgMed       THEN RUN eksportTGMed.       /* Utlegg av ansattinformasjon. Eksporteres IKKE for Gant. */
      IF bTgSales     THEN RUN eksportTGSales.     /* Utlegg av salgsdata pr. time    */
      IF bTgSales_Ext THEN RUN eksportTGSales_Ext. /* Utlegg av omseting pr. datatype */
      IF bTgTimeStamp THEN RUN eksportTGTimeStamp. /* Utlegg av selger transaksjoner. */
    
      ASSIGN
        TGExport.TGExportDate = TODAY 
        TGExport.TGExportTime = TIME 
        TGExport.TGNote       = TGExport.TGNote 
                                + (IF TGExport.TGNote <> '' THEN CHR(10) ELSE '')
                                + 'Eksportert ' + STRING(TODAY) + ' ' + STRING(TIME,"HH:MM:SS") + ' ' + USERID('SkoTex')
        .
      /*  
      obOk = NOT ERROR-STATUS:ERROR.
      IF NOT obOk THEN
      DO:
        ocReturn = ERROR-STATUS:GET-MESSAGE(1).
        LEAVE.
      END.
      */
    END.
  END.
  IF AVAIL TGExport THEN RELEASE TGExport.
  hQuery:GET-NEXT().
END.



/* **********************  Internal Procedures  *********************** */

PROCEDURE eksportTGMed:
/*------------------------------------------------------------------------------
		Purpose:  																	  
		Notes:  																	  
------------------------------------------------------------------------------*/
DEFINE VARIABLE cCl AS CHARACTER NO-UNDO.

ASSIGN
  cExtent  = STRING(TGExport.TGStore_Id)
  cFilNavn = '_POSTGEmp' + STRING(YEAR(TGExport.TGSalesDate),'9999') 
                         + STRING(MONTH(TGExport.TGSalesDate),'99')  
                         + STRING(DAY(TGExport.TGSalesDate),'99')
                         + '_' 
                         + REPLACE(STRING(TGExport.RegistrertTid,'HH:MM:SS'),':','') 
                         + '.' + cExtent.
IF CAN-FIND(FIRST TGEmp OF TGExport) THEN
DO:
    OUTPUT STREAM Ut TO VALUE(cKatalog + '\' + cFilNavn) NO-ECHO.
    FOR EACH TGEmp OF TGExport NO-LOCK:

      FIND Butiker NO-LOCK WHERE
        Butiker.Butik = INT(TGEmp.TGStore_Id) NO-ERROR.
      IF AVAILABLE Butiker THEN 
        cCl = STRING(Butiker.Cl).
      ELSE
        cCl = ''.

      PUT STREAM Ut UNFORMATTED
        TGEmp.TGEmployeeId ';'
        TGEmp.TGFirstName ';'
        TGEmp.TGSureName ';'
        TGEmp.TGSalaryProfile ';'
        TGEmp.TGWorkPercentage ';'
        TGEmp.TGHourlyRate ';'
        TGEmp.TGFixedSalary ';'
        TGEmp.TGStore_Id ';'
        TGEmp.TGStoreName ';'
        TGEmp.TGAdress1 ';'
        TGEmp.TGAdress2 ';'
        TGEmp.TGPostalCode ';'
        TGEmp.TGPostalArea ';'
        (  STRING(DAY(TGEmp.TGBirthDate),'99')
         + '.' + STRING(MONTH(TGEmp.TGBirthDate),'99')  
         + '.' + STRING(YEAR(TGEmp.TGBirthDate),'9999')) ';' 
        (  STRING(DAY(TGEmp.TGBeginDate),'99')
         + '.' + STRING(MONTH(TGEmp.TGBeginDate),'99')  
         + '.' + STRING(YEAR(TGEmp.TGBeginDate),'9999')) ';' 
        (  STRING(DAY(TGEmp.TGLeaveDate),'99')
         + '.' + STRING(MONTH(TGEmp.TGLeaveDate),'99')  
         + '.' + STRING(YEAR(TGEmp.TGLeaveDate),'9999')) ';' 
        TGEmp.TGEmployeeTitle ';'
        cCl   
        SKIP. 
    END.
    OUTPUT STREAM Ut CLOSE.  
    
    /* Håndterer tmpfil. */
    RUN flyttTmpFil.
END.

END PROCEDURE.

PROCEDURE eksportTGSales:
/*------------------------------------------------------------------------------
		Purpose:  																	  
		Notes:  																	  
------------------------------------------------------------------------------*/

DEFINE BUFFER bufTGExport FOR TGExport.

ASSIGN
  cExtent  = STRING(TGExport.TGStore_Id)
  cFilNavn = '_POSTGSales' + STRING(YEAR(TGExport.TGSalesDate),'9999') 
                         + STRING(MONTH(TGExport.TGSalesDate),'99')  
                         + STRING(DAY(TGExport.TGSalesDate),'99')
                         + '_' 
                         + REPLACE(STRING(TGExport.RegistrertTid,'HH:MM:SS'),':','') 
                         + '.' + cExtent.

IF CAN-FIND(FIRST TGSales OF TGExport) THEN
DO:
    OUTPUT STREAM Ut TO VALUE(cKatalog + '\' + cFilNavn) NO-ECHO.

    SISTE_5_SALGSDAGER:
    FOR EACH bufTGExport NO-LOCK WHERE
        bufTGExport.TGStore_Id    = TGExport.TGStore_Id AND
        bufTGExport.TGSalesDate  >= TGExport.TGSalesDate - 4  AND
        bufTGExport.TGSalesDate  <= TGExport.TGSalesDate 
        USE-INDEX TGSalesDate
        BREAK BY TGExport.TGStore_Id 
              BY TGExport.TGSalesDate DESCENDING:

        FOR EACH TGSales OF bufTGExport NO-LOCK:
            PUT STREAM Ut UNFORMATTED
                TGSales.TGDate ';'
                TGSales.TGStore_Id ';'     
                TGSales.TGOperator_Id ';'  
                TGSales.TGTimePeriod ';'  
                TGSales.TGTurnOver ';'    
                TGSales.TGVAT ';'         
                TGSales.TGNoCustomers ';'  
                TGSales.TGNoItems ';'       
                TGSales.TGGrossProfit ';'  
                TGSales.TGVendorId    
            SKIP. 
        END.
              
    END. /* SISTE_5_SALGSDAGER */
    OUTPUT STREAM Ut CLOSE.  

    /* Håndterer tmpfil. */
    RUN flyttTmpFil.
END.

END PROCEDURE.

PROCEDURE eksportTGSales_Ext:
/*------------------------------------------------------------------------------
		Purpose:  																	  
		Notes:  																	  
------------------------------------------------------------------------------*/
/* TN 15/6-11 Ref. Bredo. Skal ikke legges ut. 
ASSIGN
  cExtent  = STRING(TGExport.TGStore_Id)
  cFilNavn = '_POSTGSales_Ext' + STRING(YEAR(TGExport.TGSalesDate),'9999') 
                         + STRING(MONTH(TGExport.TGSalesDate),'99')  
                         + STRING(DAY(TGExport.TGSalesDate),'99')
                         + '_' 
                         + REPLACE(STRING(TGExport.RegistrertTid,'HH:MM:SS'),':','') 
                         + '.' + cExtent.

IF CAN-FIND(FIRST TGSales_ext OF TGExport) THEN
DO:
    OUTPUT STREAM Ut TO VALUE(cKatalog + '\' + cFilNavn) NO-ECHO.
    FOR EACH TGSales_Ext OF TGExport NO-LOCK:
      PUT STREAM Ut UNFORMATTED
        TGSales_Ext.TGDate ';'
        TGSales_Ext.TGStore_Id ';'     
        TGSales_Ext.TGTimePeriod ';'  
        TGSales_Ext.TGDataType ';'    
        TGSales_Ext.TGValue    
        SKIP. 
    END.
    OUTPUT STREAM Ut CLOSE.  

    /* Håndterer tmpfil. */
    RUN flyttTmpFil.
END.
*/

END PROCEDURE.

PROCEDURE eksportTGTimeStamp:
/*------------------------------------------------------------------------------
		Purpose:  																	  
		Notes:  																	  
------------------------------------------------------------------------------*/

ASSIGN
  cExtent  = STRING(TGExport.TGStore_Id)
  cFilNavn = '_POSTGTimeStamp' + STRING(YEAR(TGExport.TGSalesDate),'9999') 
                         + STRING(MONTH(TGExport.TGSalesDate),'99')  
                         + STRING(DAY(TGExport.TGSalesDate),'99')
                         + '_' 
                         + REPLACE(STRING(TGExport.RegistrertTid,'HH:MM:SS'),':','') 
                         + '.' + cExtent.

IF CAN-FIND(FIRST TGTimeStamp  OF TGExport) THEN
DO:
    /* For sikkerhets skyld :) */
    FOR EACH bTGTimeStamp:
      DELETE bTGTimeStamp.
    END.

    /* Bruker temp-tabell for å kunne sortere uhemmet. */
    FOR EACH TGTimeStamp OF TGExport NO-LOCK:
      CREATE bTGTimeStamp.
      BUFFER-COPY TGTimeStamp TO bTGTimeStamp.
    END.

    /* Legger ut data fra temp-file. */
    OUTPUT STREAM Ut TO VALUE(cKatalog + '\' + cFilNavn) NO-ECHO.
    FOR EACH bTGTimeStamp NO-LOCK
      BREAK BY bTGTimeStamp.TGStore_Id
            BY bTGTimeStamp.TGDate
            BY bTGTimeStamp.TGTime:
      /* TN 26/9-18 Spesiell formattering for Gant på butikk 16. */          
      PUT STREAM Ut UNFORMATTED
        bTGTimeStamp.TGDate ';'
/*        (IF (bTGTimeStamp.TGStore_Id = 16 AND bGant) THEN 10010 ELSE bTGTimeStamp.TGStore_Id) ';'*/
        bTGTimeStamp.TGStore_Id ';'     
        bTGTimeStamp.TGTime ';'  
        bTGTimeStamp.TGOperator_Id ';'    
        bTGTimeStamp.TGOperator_Name ';'    
        bTGTimeStamp.TGOperator_Id2 ';'    
        bTGTimeStamp.TGAction ';' 
        bTGTimeStamp.TGComment   
        SKIP. 
    END.
    OUTPUT STREAM Ut CLOSE.  

    /* Håndterer tmpfil. */
    RUN flyttTmpFil.
END.

END PROCEDURE.

PROCEDURE flyttTmpFil:
/*------------------------------------------------------------------------------
		Purpose:  																	  
		Notes:  																	  
------------------------------------------------------------------------------*/

/* Gir filen dens riktige navn og tar bort den temporære filen. */
IF SEARCH(cKatalog + '\' + cFilNavn) <> ? THEN 
DO:
  OS-COPY VALUE(cKatalog + '\' + cFilNavn) VALUE(cKatalog + '\' + LEFT-TRIM(cFilNavn,'_')).
  IF SEARCH(cKatalog + '\' + LEFT-TRIM(cFilNavn,'_')) <> ? THEN
      OS-DELETE VALUE(cKatalog + '\' + cFilNavn).
END.

END PROCEDURE.
