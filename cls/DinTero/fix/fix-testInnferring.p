/*  read-json-infer-pds2.p */
DEFINE VARIABLE hDataset AS HANDLE  NO-UNDO.
DEFINE VARIABLE lRetOK   AS LOGICAL NO-UNDO.
DEFINE VARIABLE hQuery   AS HANDLE  NO-UNDO.
DEFINE VARIABLE hBuffer  AS HANDLE  NO-UNDO.
DEFINE VARIABLE hField   AS HANDLE  NO-UNDO.
DEFINE VARIABLE idx1     AS INTEGER NO-UNDO.
DEFINE VARIABLE idx2     AS INTEGER NO-UNDO.
DEFINE VARIABLE idx3     AS INTEGER NO-UNDO.

/* Skaper et tomt datasett uten skjema. */
CREATE DATASET hDataset.

lRetOK = hDataset:READ-JSON("file", "cls\DinTero\getGetSessionDetailsResponse5427499.json", "empty").

OUTPUT TO "cls\DinTero\InferPDS2.out" APPEND.
RUN displayResults.
OUTPUT CLOSE.

DELETE OBJECT hDataset NO-ERROR.

PROCEDURE displayResults:
  MESSAGE "READ-JSON return value: " lRetOK SKIP.
  MESSAGE SKIP "** hDataset schema info **" SKIP.
  MESSAGE "ProDataSet name: " hDataset:NAME 
          "Num-buffers " hDataset:NUM-BUFFERS.
          
  /* Leser alle bufferne i datsettet og viser navnene på dem. */
  DO idx1 = 1 TO hDataset:NUM-BUFFERS:
    hBuffer = hDataset:GET-BUFFER-HANDLE(idx1).
    
    MESSAGE SKIP "Buffer " idx1 "Buffer name: " hBuffer:NAME.
    MESSAGE "Buffer Field info".
    
    DO idx2 = 1 TO hBuffer:NUM-FIELDS:
      hField = hBuffer:BUFFER-FIELD(idx2).
      
      MESSAGE "Field name: " hField:NAME "Data type: " hField:DATA-TYPE 
              " Extent: " hField:EXTENT.
    END. /* idx2 loop */
  END. /* idx1 loop */
  
  MESSAGE SKIP "** hDataset data **".

  DO idx1 = 1 TO hDataset:NUM-BUFFERS:
    hBuffer = hDataset:GET-BUFFER-HANDLE(idx1).
    
    MESSAGE "*** Buffer " hBuffer:NAME " Data: ***".
    
    CREATE QUERY hQuery.
    hQuery:SET-BUFFERS(hBuffer).
    hQuery:QUERY-PREPARE("for each " + hBuffer:NAME).
    hQuery:QUERY-OPEN.
    hQuery:GET-NEXT() NO-ERROR.
    
    DO WHILE NOT hQuery:QUERY-OFF-END: 
      MESSAGE SKIP.
      
        DO idx2 = 1 TO hBuffer:NUM-FIELDS:
          hField = hBuffer:BUFFER-FIELD(idx2).
          IF hField:EXTENT = 0 THEN
            MESSAGE hField:NAME ": " hField:BUFFER-VALUE.
          ELSE
            MESSAGE hField:NAME.
            DO idx3 = 1 TO hField:EXTENT:
              MESSAGE hField:NAME ": " hField:BUFFER-VALUE(idx3).
            END. /* idx3 loop */
          END. /* idx2 loop */
          hQuery:GET-NEXT() NO-ERROR.
    END. /* hQuery loop */
    
    MESSAGE SKIP.
    DELETE OBJECT hQuery NO-ERROR.
    
  END. /* idx1 loop */
END PROCEDURE.
