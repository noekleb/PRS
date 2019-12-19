/*------------------------------------------------------------------------
    File        : slettes_testjson.p
    Purpose     : 
 
 Use GetJsonObject() and then GetCharacter("description").
  ----------------------------------------------------------------------*/
 
/* ***************************  Definitions  ************************** */
 
ROUTINE-LEVEL ON ERROR UNDO, THROW.
 
/* ********************  Preprocessor Definitions  ******************** */
 
 
/* ***************************  Main Block  *************************** */
Def var myJsonContent as longchar no-undo.
Def var oParser as Progress.Json.ObjectModel.ObjectModelParser.
Def var oObject as Progress.Json.ObjectModel.JsonObject.
Def var oArray  as Progress.Json.ObjectModel.JsonArray.
Def var oInvoices  as Progress.Json.ObjectModel.JsonArray.
 
Def var oRows  as Progress.Json.ObjectModel.JsonArray.
def var ii as int no-undo.
def var iii as int no-undo.
 
function checkContent returns logical (input ipObject as class Progress.Json.ObjectModel.JsonObject) forward.
 
fix-codepage(myJsonContent) = 'UTF-8'.
Copy-lob from file 'konv\tmpNyArt153731.json' to object myJsonContent.
oParser = new Progress.Json.ObjectModel.ObjectModelParser().
oObject = cast(oParser:Parse(myJsonContent),Progress.Json.ObjectModel.JsonObject).
 
oInvoices = oObject:GetJsonArray('invoices').
 
do ii = 1 to oInvoices:Length:
  checkContent(oInvoices:GetJsonObject(ii)).
end.
 
function checkContent returns logical (input ipObject as class Progress.Json.ObjectModel.JsonObject):
  def var ContentList as longchar extent no-undo.
 
  ContentList = oInvoices:GetJsonObject(ii):GetNames().
  do iii = 1 to extent(ContentList):
    if ContentList[iii] = 'rows' then do:
      MESSAGE string(ipObject:GetJsonArray('rows'):GetJsonText())
      VIEW-AS ALERT-BOX.
    end.
  end.
  
end function. 
