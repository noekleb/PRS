/* Save document to disk catalog
      
   Created 07.09.11 by brynjar@chemistry.no
*********************************************************************************/       
DEF INPUT  PARAM icDiskCat       AS CHAR   NO-UNDO.
DEF INPUT  PARAM ihBuffer        AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId     AS CHAR   NO-UNDO.
DEF OUTPUT PARAM ocReturn        AS CHAR   NO-UNDO.
DEF OUTPUT PARAM obOk            AS LOG    NO-UNDO.

FILE-INFO:FILE-NAME = icDiskCat.
IF FILE-INFO:FULL-PATHNAME = ? THEN OS-CREATE-DIR VALUE(icDiskCat).

COPY-LOB FROM ihBuffer::blDocument TO FILE icDiskCat + "/" + ihBuffer::cFileName.

CREATE JBoxDocument.
BUFFER JBoxDocument:BUFFER-COPY(ihBuffer,"iDocSize,blDocument").
JBoxDocument.cFileName = icDiskCat + "/" + ihBuffer::cFileName.

CREATE JBoxDocRel.
BUFFER JBoxDocRel:BUFFER-COPY(ihBuffer).
JBoxDocRel.iJBoxDocumentId = JBoxDocument.iJBoxDocumentId.

FINALLY:
  obOk = ocReturn = "".
END.  
