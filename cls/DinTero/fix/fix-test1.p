USING Progress.Json.ObjectModel.*.

/* ttCust definition */
DEFINE TEMP-TABLE ttRecord
    FIELD grant_type AS CHAR 
    FIELD audience AS CHAR 
    .
    
DEFINE VARIABLE httRecord AS HANDLE NO-UNDO.
DEFINE VARIABLE oJson AS JsonObject NO-UNDO. 
DEFINE VARIABLE lReturnValue AS LOGICAL NO-UNDO.
DEFINE VARIABLE lcJson AS LONGCHAR NO-UNDO.

httRecord = TEMP-TABLE ttRecord:HANDLE.

CREATE ttRecord.
ASSIGN 
    grant_type = "client_credentials"
    audience   = "https://api.dintero.com/v1/accounts/T11112542"
.

/* Create new JsonObject */
oJson = NEW JsonObject().

/* Fill with properties representing each field in each record of temp-table */ 
lReturnValue = oJson:Read( httRecord ).

/* Serialize the JsonObject to a LONGCHAR variable and pass it to an internal
    procedure that zeroes out the Balance field for each customer. In an actual
    application, work like that done in the internal procedure might be
    done by a remote service. */
oJson:Write(lcJson).
    
    
MESSAGE STRING(lcJSon)
    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
