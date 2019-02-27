DEF INPUT PARAM ioXml AS JBoxXmlDoc NO-UNDO.

/* Helper proc for Sax parser. */
PROCEDURE Characters :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Brynjar 20.mars.15: Lagt inn test på lengde av charData før cNodeValue oppdateres
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER charData AS LONGCHAR NO-UNDO.
DEFINE INPUT PARAMETER numChars AS INTEGER NO-UNDO.

ioXml:createNode(charData).

END PROCEDURE.

PROCEDURE StartElement :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER namespaceURI AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER localName AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER qName AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER attributes AS HANDLE NO-UNDO.

ASSIGN ioXml:cPrevActiveNode = ioXml:cActiveNode
       ioXml:cActiveNode     = qName
       ioXml:iLevel          = ioXml:iLevel + 1
       ioXml:bEndNode        = NO
       .
IF ioXml:iLevel NE ioXml:iPrevLevel THEN        
  ioXml:iParentNodeIdx  = ioXml:getParentNodeForLevel(ioXml:iLevel).
       
ioXml:iPrevLevel = ioXml:iLevel.

END PROCEDURE.

PROCEDURE EndElement :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER namespaceURI AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER localName AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER qName AS CHARACTER NO-UNDO.

ioXml:iLevel = ioXml:iLevel - 1.

IF qName = ioXml:cActiveNode AND ioXml:iLevel = ioXml:iPrevLevel - 1 THEN
  ioXml:bEndNode = YES.

END PROCEDURE.
