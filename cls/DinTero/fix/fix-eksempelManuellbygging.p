USING Progress.Json.ObjectModel.JsonObject.


DEFINE VARIABLE jSon_string AS LONGCHAR                             NO-UNDO. 
DEFINE VARIABLE oJSon       AS Progress.Json.ObjectModel.JsonObject.

DEFINE TEMP-TABLE ttoJSon NO-UNDO 
    FIELD grant_type AS CHARACTER
    FIELD audience   AS CHARACTER.

CREATE ttoJSon. 
ASSIGN 
    ttoJson.grant_type = "client_credentials"
    ttoJSon.audience   = "https://api.dintero.com/v1/accounts/T11112542".

/* Create Json Object */
oJSon = NEW JsonObject(). 
FOR EACH ttoJson: 
    oJSon:add("grant_type",ttoJSon.grant_type). 
    oJSon:add("audience",ttoJSon.audience). 
END.

/* Write out the JsonObject as a json file */
oJSon:WriteFile("c:\tmp\Out.json", TRUE). 
oJSon:Write(jSon_string,TRUE). 
MESSAGE STRING(jSon_string) VIEW-AS ALERT-BOX.
