USING Progress.Json.ObjectModel.JsonObject.
USING Progress.Json.ObjectModel.JsonArray.

DEFINE TEMP-TABLE ttoJSon NO-UNDO 
  FIELD grant_type AS CHARACTER
  FIELD audience   AS CHARACTER.

DEFINE TEMP-TABLE ttUrl
  FIELD return_url AS CHARACTER 
  .
    
DEFINE TEMP-TABLE ttOrder
  FIELD amount             AS DECIMAL 
  FIELD currency           AS CHARACTER 
  FIELD merchant_reference AS CHARACTER 
  .
    
DEFINE TEMP-TABLE ttItems SERIALIZE-NAME ''
  FIELD id          AS CHARACTER 
  FIELD line_id     AS CHARACTER  
  FIELD Group_Id    AS CHARACTER SERIALIZE-HIDDEN
  FIELD description AS CHARACTER 
  FIELD quantity    AS INTEGER 
  FIELD amount      AS DECIMAL  
  FIELD vat_amount  AS DECIMAL 
  FIELD vat         AS DECIMAL
  INDEX idxItems AS UNIQUE PRIMARY id line_id 
  INDEX idxGroups Group_Id 
  .

DEFINE TEMP-TABLE ttGroups SERIALIZE-NAME 'groups'
  FIELD Group_Id   AS CHARACTER SERIALIZE-NAME 'id'
  FIELD GroupName AS CHARACTER SERIALIZE-NAME 'name' 
  INDEX idxGroups AS UNIQUE PRIMARY Group_Id 
  .
        
DEFINE TEMP-TABLE ttCustomer
  FIELD customer_id  AS CHARACTER 
  FIELD email        AS CHARACTER 
  FIELD phone_number AS CHARACTER 
  .   

DEFINE TEMP-TABLE ttRefund
  FIELD amount AS DECIMAL 
  FIELD reason AS CHARACTER 
  .

DEFINE DATASET dsItems SERIALIZE-HIDDEN 
  FOR ttItems, ttGroups
  DATA-RELATION drItemGroupe FOR ttItems, ttGroups RELATION-FIELDS (Group_Id, Group_Id) NESTED.
  .

DEFINE VAR poJSon AS CLASS JsonObject NO-UNDO.

  DEFINE VARIABLE piLoop AS INTEGER NO-UNDO.
  DEFINE VARIABLE piAnt  AS INTEGER NO-UNDO.
    DEF VAR cJSonString AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE poRefund      AS JsonObject NO-UNDO. 
    DEFINE VARIABLE poItemsArray  AS JsonArray NO-UNDO.
    DEFINE VARIABLE poItems       AS JsonObject NO-UNDO. 
    DEFINE VARIABLE poGroupArray  AS JsonArray NO-UNDO.
    DEFINE VARIABLE poGroup       AS JsonObject NO-UNDO. 
    DEFINE VARIABLE pcdsItem      AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE pcObject      AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE poTest        AS JsonObject NO-UNDO.
    DEFINE VARIABLE pcTest        AS LONGCHAR NO-UNDO. 
    
    CREATE ttRefund.
    ASSIGN 
        ttRefund.amount             = 0 
        ttRefund.reason             = '14 Feil farge' 
        .
    
    DO piant = 1 TO 1:
      CREATE ttItems.    
      ASSIGN 
          ttItems.id          = "chair-" + STRING(piant)
          ttItems.line_id     = "1"
          ttItems.Group_Id    = "B234"
          ttItems.description = "StablestolTNC"
          ttItems.quantity    = 1
          ttItems.amount      = 290
          ttItems.vat_amount  = 60
          ttItems.vat         = 25
          ttRefund.amount     = ttRefund.amount + ttItems.amount
          .
    END.

    CREATE ttGroups.
    ASSIGN 
      ttGroups.Group_Id  = "B234"
      ttGroups.GroupName = "Stol"
      .
    
    /* Create new JsonObjects */
    poJson       = NEW JsonObject().
    poItems      = NEW JsonObject().
    poItemsArray = NEW JsonArray( ).
    poGroupArray = NEW JsonArray(1).
    poGroup      = NEW JsonObject().
    
    piLoop = 0.
    ITEMBLOKK:
    FOR EACH ttItems, 
      FIRST ttGroups OF ttItems:
      
      poGroup:Read( TEMP-TABLE ttGroups:DEFAULT-BUFFER-HANDLE, TRUE).
      poGroupArray:ADD(1,poGroup). /* Skal bare ha en ekstent i denne arrayen. */
      poGroupArray:remove(1).
             
      piLoop = piLoop + 1.
      poItems:Read( TEMP-TABLE ttItems:DEFAULT-BUFFER-HANDLE, TRUE).
      poItems:Add("groups", poGroupArray).
      
      /*poItemsArray:ADD(piLoop,poItems).*/
      poItemsArray:ADD(poItems).
    END. /* ITEMBLOKK */

    /* Det er alltid en tom ekstent først... den skal bort.           */
    /* Men det kommer av en eller annen grunn også t tomme bakerst??? */
    /*
    DO piLoop = 1 TO poItemsArray:LENGTH:
        pcTest = poItemsArray:GetJsonText(piLoop).
      IF poItemsArray:IsNull(piLoop) THEN 
        poItemsArray:Remove(piLoop).
    END.
    */

    /* Bygger opp JSon retur objektetet.                                            */
    poJSon:ADD("amount", ttRefund.amount).
    poJSon:ADD("reason", ttRefund.reason).
    poJSon:Add("items", poItemsArray).

    
    /* For sjekk av det genererte dataobjektet. */
    poJson:Write(pcObject, TRUE).
    OUTPUT TO value('konv\test' + STRING(ETIME) + '.json').
      PUT UNFORMATTED 
        STRING(pcObject)
        SKIP.
    OUTPUT CLOSE.
