
/*------------------------------------------------------------------------
    File        : SPAR_Personsokninfraga.p
    Purpose     : 
    Created     : Fri Dec 06 13:18:31 CET 2013
    Notes       :
  ----------------------------------------------------------------------*/


FUNCTION getDataSet RETURNS HANDLE 
	( INPUT xsdFile AS CHAR ) FORWARD.

FUNCTION getFullPathname RETURNS CHARACTER 
	( INPUT cFilename AS CHAR ) FORWARD.

FUNCTION getTempFileName RETURNS CHARACTER 
	(  ) FORWARD.

FUNCTION getTempFileNameResult RETURNS CHARACTER 
	(  ) FORWARD.


/* ************** MAIN BLOCK ************************ */
    DEFINE INPUT  PARAMETER Personid AS CHAR NO-UNDO. 
    DEFINE OUTPUT PARAMETER ocPersondetaljer_Fornamn AS CHAR NO-UNDO. 
    DEFINE OUTPUT PARAMETER ocPersondetaljer_Efternamn AS CHAR NO-UNDO. 
    DEFINE OUTPUT PARAMETER ocPersondetaljer_Fodelsetid AS CHAR NO-UNDO. 
    DEFINE OUTPUT PARAMETER ocPersondetaljer_Kon AS CHAR NO-UNDO. 
    DEFINE OUTPUT PARAMETER ocFolkbokforingsadress_Utdelningsadress2 AS CHAR NO-UNDO. 
    DEFINE OUTPUT PARAMETER ocFolkbokforingsadress_PostNr AS CHAR NO-UNDO. 
    DEFINE OUTPUT PARAMETER ocFolkbokforingsadress_Postort AS CHAR NO-UNDO. 
    DEFINE OUTPUT PARAMETER ocFolkbokforingsadress_FolkbokfordLanKod AS CHAR NO-UNDO. 
    DEFINE OUTPUT PARAMETER ocFolkbokforingsadress_FolkbokfordKommunKod AS CHAR NO-UNDO. 
    DEFINE OUTPUT PARAMETER ocFolkbokforingsadress_FolkbokfordForsamlingKod AS CHAR NO-UNDO. 
    DEFINE OUTPUT PARAMETER ocErrorMessage AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER ocPersonFunnet AS LOG   NO-UNDO.
    DEFINE OUTPUT PARAMETER olSuccess AS LOGICAL INIT FALSE NO-UNDO. 

    DEFINE VARIABLE cXsdfil                     AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cEndPoint                   AS CHAR NO-UNDO.
    DEFINE VARIABLE hIdentifieringsInformation  AS HANDLE NO-UNDO. 
    DEFINE VARIABLE hPersonid                   AS HANDLE NO-UNDO. 
    DEFINE VARIABLE hDataSet                    AS HANDLE NO-UNDO. 
    DEFINE VARIABLE lok                         AS LOGICAL NO-UNDO. 
    DEFINE VARIABLE cTempFileName               AS CHAR NO-UNDO. 
    DEFINE VARIABLE cTempFileResult             AS CHAR NO-UNDO. 
    DEFINE VARIABLE cCmd                        AS CHAR NO-UNDO. 
    DEFINE VARIABLE cert                        AS CHAR NO-UNDO.
    DEFINE VARIABLE curl                        AS CHAR NO-UNDO. 
    DEFINE VARIABLE hHandler                    AS HANDLE NO-UNDO. 
    DEFINE VARIABLE hParser                     AS HANDLE NO-UNDO. 

    {syspara.i 14 1 8 cXsdfil}
    IF cXsdfil = '' THEN 
        cXsdfil = "c:\appdir\se\SPAR\PS_SPARpersonsokningfraga.xsd".

    {syspar2.i 14 1 8 cEndPoint}
    IF cEndPoint = '' THEN 
        cEndPoint = "https://ext-ws.statenspersonadressregister.se/spar-webservice/SPARPersonsokningService/".

    curl      = "curl.exe".
    cert      = "cert\SPAR.pem".
    
    hDataSet = getDataSet(cXsdFil).
    
    cTempFileName   = getTempFileName(). 
    cTempFileResult = getTempFileNameResult(). 
    
    hIdentifieringsInformation = hDataSet:GET-BUFFER-HANDLE('IdentifieringsInformation').     
    hPersonid                  = hDataSet:GET-BUFFER-HANDLE('Personid').
    hPersonid::fysiskpersonid = Personid. 

    lok =  hDataSet:WRITE-XML("FILE",cTempFileName,TRUE,?,?,FALSE,FALSE).
    FILE-INFO:FILE-NAME = cTempFileName.
    cTempFileName = FILE-INFO:FULL-PATHNAME.
    DELETE OBJECT hDataSet. 
        
    ASSIGN 
        cert          = getFullPathName(cert) 
        curl          = getFullPathName(curl)
        cTempFileName = getFullPathname(cTempFileName).
/*     OUTPUT TO "CLIPBOARD".            */
/*     PUT UNFORMATTED cert         skip */
/*                     curl         skip */
/*                     cTempFileName.    */
/*     OUTPUT CLOSE.                     */
    
/*     cCmd = curl + ' -k --cert ' + cert + ' -d @' + cTempFileName + ' --header "content-type: application/soap+xml" ' + cEndpoint + '> ' + cTempFileResult. */
    cCmd = curl + ' -k --cert ' + cert + ' -d @' + cTempFileName + ' --header "content-type: text/xml" ' + cEndpoint + '> ' + cTempFileResult.
    OS-COMMAND SILENT  VALUE(cCmd) NO-ERROR. 

    CREATE SAX-READER hParser.
    
    RUN "SPAR_PersonsokninfragaParser.p" PERSISTENT SET hHandler.
    hParser:HANDLER = hHandler.
    
    hParser:SET-INPUT-SOURCE("FILE",cTempFileResult) NO-ERROR.
    hParser:SAX-PARSE( ) NO-ERROR.
    
    IF ERROR-STATUS:ERROR THEN 
    DO:
        IF ERROR-STATUS:NUM-MESSAGES > 0 THEN
            ocErrorMessage = ERROR-STATUS:GET-MESSAGE(1).
        ELSE
            ocErrorMessage = RETURN-VALUE.
        olSuccess = FALSE. 
    END.
    ELSE
    DO:
        RUN getData IN hHandler (
            OUTPUT ocPersondetaljer_Fornamn,
            OUTPUT ocPersondetaljer_Efternamn,
            OUTPUT ocPersondetaljer_Fodelsetid,
            OUTPUT ocPersondetaljer_Kon,
            OUTPUT ocFolkbokforingsadress_Utdelningsadress2,
            OUTPUT ocFolkbokforingsadress_PostNr,
            OUTPUT ocFolkbokforingsadress_Postort,
            OUTPUT ocFolkbokforingsadress_FolkbokfordLanKod,
            OUTPUT ocFolkbokforingsadress_FolkbokfordKommunKod,
            OUTPUT ocFolkbokforingsadress_FolkbokfordForsamlingKod,
            OUTPUT ocErrorMessage,
            OUTPUT ocPersonFunnet,
            OUTPUT olSuccess ).                    
    END. 

    DELETE OBJECT hParser NO-ERROR.
    DELETE PROCEDURE hHandler NO-ERROR.
    
    
    IF SEARCH(cTempFileName) <> ? THEN OS-DELETE VALUE (cTempFileName)   NO-ERROR.
    IF SEARCH(cTempFileResult) <> ? THEN OS-DELETE VALUE (cTempFileResult) NO-ERROR.
    /*
    MESSAGE 
    'cTempFileName' cTempFileName SKIP 
    'cTempFileResult' cTempFileResult
    VIEW-AS ALERT-BOX.
    */
/* ************************  Function Implementations ***************** */
FUNCTION getFullPathname RETURNS CHARACTER 
        (  ):
/*------------------------------------------------------------------------------
        Purpose:                                                                      
        Notes:                                                                        
------------------------------------------------------------------------------*/    
    FILE-INFO:FILE-NAME = SEARCH("SPAR\bin\" + cFileName).
    IF FILE-INFO:FULL-PATHNAME = ?  THEN
    FILE-INFO:FILE-NAME = SEARCH("bin\" + cFileName).
    IF FILE-INFO:FULL-PATHNAME = ?  THEN
    FILE-INFO:FILE-NAME = SEARCH(cFileName).
    
    RETURN FILE-INFO:FULL-PATHNAME.
END FUNCTION.

FUNCTION getTempFileNameResult RETURNS CHARACTER 
	    (  ):
/*------------------------------------------------------------------------------
		Purpose:  																	  
		Notes:  																	  
------------------------------------------------------------------------------*/	

     RETURN "c:\tmp\SPAR_Restult_" + STRING(YEAR(TODAY),"9999") +  STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") + STRING(TIME) + ".xml".
		
END FUNCTION.

FUNCTION getTempFileName RETURNS CHARACTER 
        (  ):
/*------------------------------------------------------------------------------
        Purpose:                                                                      
        Notes:                                                                        
------------------------------------------------------------------------------*/    

     RETURN "c:\tmp\SPAR_Request_" + STRING(YEAR(TODAY),"9999") +  STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") + STRING(TIME) + ".xml".
        
END FUNCTION.

FUNCTION getDataSet RETURNS HANDLE 
        ( INPUT xsdFile AS CHAR ):
/*------------------------------------------------------------------------------
        Purpose:                                                                      
        Notes:                                                                        
------------------------------------------------------------------------------*/    
    DEFINE VARIABLE iCnt                        AS INT NO-UNDO. 
    DEFINE VARIABLE hIdentifieringsInformation  AS HANDLE NO-UNDO. 
    DEFINE VARIABLE lok                         AS LOGICAL NO-UNDO. 
    DEFINE VARIABLE hPersonid                   AS HANDLE NO-UNDO. 
    DEFINE VARIABLE URI_Komponent               AS CHAR INIT "http://skatteverket.se/spar/komponent/1.0" NO-UNDO. 
    DEFINE VARIABLE URI_Instans                 AS CHAR INIT "http://skatteverket.se/spar/instans/1.0"   NO-UNDO. 
    DEFINE VARIABLE hDataSet                    AS HANDLE NO-UNDO. 

    CREATE DATASET hDataSet. 
    lok =  hDataSet:READ-XMLSCHEMA ("file",xsdFile,FALSE,?,"strict").
    
    hDataSet:GET-BUFFER-HANDLE('SPARPersonsokningFraga'):NAMESPACE-URI = URI_Instans. 
    hDataSet:GET-BUFFER-HANDLE('SPARPersonsokningFraga'):NAMESPACE-PREFIX = "ns2".
    hDataSet:GET-BUFFER-HANDLE('IdentifieringsInformation'):NAMESPACE-URI = URI_Komponent. 
    hDataSet:GET-BUFFER-HANDLE('IdentifieringsInformation'):NAMESPACE-PREFIX = "ns2".
    hDataSet:GET-BUFFER-HANDLE('PersonId'):NAMESPACE-URI = URI_Komponent. 
    hDataSet:GET-BUFFER-HANDLE('PersonId'):NAMESPACE-PREFIX = "ns2".
    hDataSet:GET-BUFFER-HANDLE('PersonsokningFraga'):NAMESPACE-URI = URI_Komponent. 
    hDataSet:GET-BUFFER-HANDLE('PersonsokningFraga'):NAMESPACE-PREFIX = "ns2".

    DO icnt = 1 TO hDataSet:NUM-BUFFERS: 
        hDataSet:GET-BUFFER-HANDLE(iCnt):BUFFER-FIELD('hidden'):XML-NODE-TYPE = "hidden". 
        hDataSet:GET-BUFFER-HANDLE(iCnt):BUFFER-CREATE().
    END. 
    
    RETURN hDataSet. 
        
END FUNCTION.

