 

DEFINE VARIABLE gcCurrentCharacters AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcCurrentElementName AS CHARACTER NO-UNDO.
DEFINE VARIABLE ReturnLevel AS LOGICAL INIT TRUE NO-UNDO.  
DEFINE VARIABLE iLevel AS INT INIT 1 NO-UNDO. 
DEFINE VARIABLE LevelElement AS CHAR EXTENT 10 NO-UNDO. 

DEFINE VARIABLE Persondetaljer_Fornamn AS CHAR NO-UNDO. 
DEFINE VARIABLE Persondetaljer_Efternamn AS CHAR NO-UNDO. 
DEFINE VARIABLE Persondetaljer_Fodelsetid AS CHAR NO-UNDO. 
DEFINE VARIABLE Persondetaljer_Kon AS CHAR NO-UNDO. 
DEFINE VARIABLE Folkbokforingsadress_Utdelningsadress2 AS CHAR NO-UNDO. 
DEFINE VARIABLE Folkbokforingsadress_PostNr AS CHAR NO-UNDO. 
DEFINE VARIABLE Folkbokforingsadress_Postort AS CHAR NO-UNDO. 
DEFINE VARIABLE Folkbokforingsadress_FolkbokfordLanKod AS CHAR NO-UNDO. 
DEFINE VARIABLE Folkbokforingsadress_FolkbokfordKommunKod AS CHAR NO-UNDO. 
DEFINE VARIABLE Folkbokforingsadress_FolkbokfordForsamlingKod AS CHAR NO-UNDO. 
DEFINE VARIABLE sparfel_FelBeskrivning AS CHAR NO-UNDO. 
DEFINE VARIABLE lSuccess AS LOGICAL INIT TRUE NO-UNDO. 
DEFINE VARIABLE faultCode AS CHAR NO-UNDO. 
DEFINE VARIABLE lPersonFunnet AS LOGICAL     NO-UNDO.
DEFINE VARIABLE faultString AS CHAR NO-UNDO. 


PROCEDURE GetData : 
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
    DEFINE OUTPUT PARAMETER olPersonFunnet AS LOG   NO-UNDO.
    DEFINE OUTPUT PARAMETER olSuccess AS LOGICAL NO-UNDO. 


    ASSIGN 
        ocPersondetaljer_Fornamn = Persondetaljer_Fornamn 
        ocPersondetaljer_Efternamn  = Persondetaljer_Efternamn 
        ocPersondetaljer_Fodelsetid =  Persondetaljer_Fodelsetid
        ocPersondetaljer_Kon = Persondetaljer_Kon 
        ocFolkbokforingsadress_Utdelningsadress2 = Folkbokforingsadress_Utdelningsadress2 
        ocFolkbokforingsadress_PostNr = Folkbokforingsadress_PostNr  
        ocFolkbokforingsadress_Postort = Folkbokforingsadress_Postort
        ocFolkbokforingsadress_FolkbokfordLanKod = Folkbokforingsadress_FolkbokfordLanKod   
        ocFolkbokforingsadress_FolkbokfordKommunKod = Folkbokforingsadress_FolkbokfordKommunKod  
        ocFolkbokforingsadress_FolkbokfordForsamlingKod = Folkbokforingsadress_FolkbokfordForsamlingKod
        olPersonFunnet = lPersonFunnet.  
/*     OUTPUT TO c:\temp\spar.txt.                                                                         */
/*     PUT UNFORMATTED                                                                                     */
/*                                                                                                         */
/*     "Persondetaljer_Fornamn                       " Persondetaljer_Fornamn                        skip  */
/*     "Persondetaljer_Efternamn                     " Persondetaljer_Efternamn                      skip  */
/*     "Persondetaljer_Fodelsetid                    " Persondetaljer_Fodelsetid                     skip  */
/*     "Persondetaljer_Kon                           " Persondetaljer_Kon                            skip  */
/*     "Folkbokforingsadress_Utdelningsadress2       " Folkbokforingsadress_Utdelningsadress2        skip  */
/*     "Folkbokforingsadress_PostNr                  " Folkbokforingsadress_PostNr                   skip  */
/*     "Folkbokforingsadress_Postort                 " Folkbokforingsadress_Postort                  skip  */
/*     "Folkbokforingsadress_FolkbokfordLanKod       " Folkbokforingsadress_FolkbokfordLanKod        skip  */
/*     "Folkbokforingsadress_FolkbokfordKommunKod    " Folkbokforingsadress_FolkbokfordKommunKod     skip  */
/*     "Folkbokforingsadress_FolkbokfordForsamlingKod" Folkbokforingsadress_FolkbokfordForsamlingKod SKIP. */
/*                                                                                                         */
/*     OUTPUT CLOSE.                                                                                       */
/*                                                                                                         */
/*                                                                                                         */
/*                                                                                                         */
    IF lSuccess = FALSE THEN
       ocErrorMessage = "FaultCode:" + faultcode + " FaultString:" + faultString + 
                        "sparfel:" + sparfel_FelBeskrivning. 
    olSuccess = lSuccess. 

END. 


PROCEDURE StartElement:
    DEFINE INPUT PARAMETER pcNamespaceURI AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER pcLocalName AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER pcQName AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER phAttributes AS HANDLE NO-UNDO.
    
    iLevel = iLevel + 1. 
    levelElement[iLevel] = pcQName. 

    ASSIGN gcCurrentCharacters = ""
           gcCurrentElementName = pcQName.
                
/*     OUTPUT TO c:\temp\spar.txt APPEND.                                 */
/*     PUT UNFORMATTED string(iLevel,">9") " " levelElement[iLevel] SKIP. */
/*     OUTPUT CLOSE.                                                      */
END PROCEDURE /* StartElement */.

PROCEDURE Characters:
    DEFINE INPUT PARAMETER pmCharData AS MEMPTR NO-UNDO.
    DEFINE INPUT PARAMETER piNumChars AS INTEGER NO-UNDO.
    
    ASSIGN gcCurrentCharacters = gcCurrentCharacters + GET-STRING(pmCharData, 1, GET-SIZE(pmCharData)).
END PROCEDURE /* Characters */.

PROCEDURE EndElement:
    DEFINE INPUT PARAMETER pcNameSpaceURI AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER pcLocalName AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER pcQName AS CHARACTER no-undo.
    
    levelElement[iLevel] = "". 
    iLevel = iLevel - 1. 
/*     OUTPUT TO c:\temp\spar.txt APPEND.                                 */
/*     PUT UNFORMATTED string(iLevel,">9") " " levelElement[iLevel] SKIP. */
/*     OUTPUT CLOSE.                                                      */

    IF levelElement[iLevel] = "spako:Persondetaljer" THEN DO:
        lPersonFunnet = TRUE.
        CASE gcCurrentElementName :
            WHEN "spako:Fornamn"     THEN ASSIGN Persondetaljer_Fornamn = gcCurrentCharacters.
            WHEN "spako:Efternamn"  THEN ASSIGN Persondetaljer_Efternamn = gcCurrentCharacters.
            WHEN "spako:Fodelsetid"  THEN ASSIGN Persondetaljer_Fodelsetid  = gcCurrentCharacters.
            WHEN "spako:Kon"         THEN ASSIGN Persondetaljer_Kon = gcCurrentCharacters.
        END CASE.        
    END.
    
    IF levelElement[iLevel] = "spako:Folkbokforingsadress" THEN
    CASE gcCurrentElementName :
        WHEN "spako:Utdelningsadress2"        THEN ASSIGN Folkbokforingsadress_Utdelningsadress2 = gcCurrentCharacters.
        WHEN "spako:PostNr"                   THEN ASSIGN Folkbokforingsadress_PostNr = gcCurrentCharacters.
        WHEN "spako:Postort"                  THEN ASSIGN Folkbokforingsadress_Postort = gcCurrentCharacters.
        WHEN "spako:FolkbokfordLanKod"        THEN ASSIGN Folkbokforingsadress_FolkbokfordLanKod = gcCurrentCharacters.
        WHEN "spako:FolkbokfordKommunKod"     THEN ASSIGN Folkbokforingsadress_FolkbokfordKommunKod = gcCurrentCharacters.
        WHEN "spako:FolkbokfordForsamlingKod" THEN ASSIGN Folkbokforingsadress_FolkbokfordForsamlingKod = gcCurrentCharacters.
    END CASE.        
    
/*         OUTPUT TO c:\temp\spar.txt APPEND.                                */
/*         PUT UNFORMATTED string(iLevel,">9") " " gcCurrentCharacters SKIP. */
/*         OUTPUT CLOSE.                                                     */
    IF levelElement[iLevel] = "env:Fault" THEN
    DO:
        lSuccess = FALSE. 

        CASE gcCurrentElementName :
            WHEN "faultcode"        THEN ASSIGN faultCode = gcCurrentCharacters.
            WHEN "faultstring"      THEN ASSIGN faultString = gcCurrentCharacters.
        END CASE.        
    END.

    IF lSuccess = FALSE AND 
        levelElement[iLevel] = "detail" AND 
        gcCurrentElementName = "sparfel:FelBeskrivning" THEN 
        sparfel_FelBeskrivning = gcCurrentCharacters.

    ASSIGN gcCurrentElementName = "".
    
END PROCEDURE /* EndElement */.
    
