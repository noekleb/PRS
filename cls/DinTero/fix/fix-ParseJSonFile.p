USING Progress.Json.ObjectModel.ObjectModelParser.
USING progress.Json.*.
USING progress.Json.ObjectModel.*.

DEFINE VARIABLE oParser      AS ObjectModelParser NO-UNDO.
DEFINE VARIABLE oJsonArray   AS JsonArray         NO-UNDO.
DEFINE VARIABLE oJsonObject  AS JsonObject        NO-UNDO.
DEFINE VARIABLE oJsonObject2 AS JsonObject        NO-UNDO.

DEFINE VARIABLE cText        AS CHARACTER         NO-UNDO.
DEFINE VARIABLE myArray      AS CHARACTER         EXTENT NO-UNDO.
DEFINE VARIABLE i1           AS INTEGER           NO-UNDO.
DEFINE VARIABLE cParent      AS CHARACTER         NO-UNDO.
DEFINE VARIABLE cJSonLChar   AS LONGCHAR          NO-UNDO. 
DEFINE VARIABLE cTabell      AS CHARACTER NO-UNDO.   
 
FIX-CODEPAGE(cJSonLChar) = 'UTF-8'.
/* Parse the JSON file into a JSON object */
oParser = NEW ObjectModelParser().
/*oJsonObject = CAST(oParser:ParseFile("cls\dintero\getGetSessionDetailsResponse5427499.json"), JsonObject).*/

COPY-LOB FROM FILE "cls\dintero\getGetSessionDetailsResponse5427499.json" TO cJSonLChar.
cJSonLChar = '~{"session": ' + cJSonLChar + '~}'.
oJsonObject = CAST(oParser:Parse(cJSonLChar), JsonObject).

/* Fill the array with the names of the objects in the JSON */
myArray = oJsonObject:GetNames().

/* Process each JSON object in the file */
DO i1 = 1 TO EXTENT(myArray):
    ASSIGN
        cText   = oJsonObject:getJsonText(myArray[i1])
        cParent = myArray[i1]. 
    
    /* Cast the JSON to an object of type JsonObject */           
    oJsonObject2 = CAST(oParser:Parse(cText), JsonObject) NO-ERROR.
    
    IF ERROR-STATUS:NUM-MESSAGES > 0 THEN 
    ARRAYBLOKK:
    DO:
        /* If the CAST fails then the JSON is an Array, so CAST to a JSON Array */
/*        oJsonArray = CAST(oParser:Parse(cText), JsonArray) NO-ERROR.*/
/*        RUN ProcessArray (INPUT oJsonArray). /* Process the JSON Array */*/
    END. /* ARRAYBLOKK */
    ELSE 
    OBJECTBLOKK:
    DO:
      RUN ProcessObject (INPUT oJsonObject2). /* Process the JSON Object */
    END. /* ARRAYBLOKK */
END.

DELETE OBJECT oParser    NO-ERROR.

PROCEDURE ProcessObject:
    DEFINE INPUT PARAMETER oJsonObject AS JsonObject        NO-UNDO.
    
    DEFINE VARIABLE myArray AS CHARACTER EXTENT NO-UNDO.
    DEFINE VARIABLE i       AS INTEGER   NO-UNDO.  
    DEFINE VARIABLE cText   AS CHARACTER NO-UNDO.  
     
    myArray = oJsonObject:GetNames().

    DO i = 1 TO EXTENT(myArray):
        cText = oJsonObject:getJsonText(myArray[i]).

        MESSAGE 
            "Parent Object(Gurre) : " cParent SKIP(1)
            "Property : " myArray[i] " : " cText SKIP(1)
            "Number of Properties in the object : " EXTENT(myArray) SKIP
            "Current Property : " i SKIP                
            VIEW-AS ALERT-BOX TITLE "ProcessObject" .
    END.
END.

PROCEDURE ProcessArray:
    DEFINE INPUT PARAMETER oJsonArray AS JsonArray        NO-UNDO.
    
    DEFINE VARIABLE i           AS INTEGER    NO-UNDO.  
    DEFINE VARIABLE cText       AS CHARACTER  NO-UNDO.  
    DEFINE VARIABLE oJsonObject AS JsonObject NO-UNDO.

    /* Process each object in the JSON array */
    DO i = 1 TO oJsonArray:length:
        cText = oJsonArray:getJsonText(i).
    
        /* Cast the JSON to an object of type JsonObject - Assumes there are no more JSON arrays */           
        oJsonObject = CAST(oParser:Parse(cText), JsonObject) NO-ERROR.
        cTabell = 'Object'.        
        RUN ProcessObject (INPUT oJsonObject). /* Process the JSON Object */
    END.  
END.
