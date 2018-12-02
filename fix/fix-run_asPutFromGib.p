DEFINE VARIABLE lOK AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cReturn AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lcBlobData AS LONGCHAR     NO-UNDO. 

COPY-LOB FROM FILE "kom\in\customer.json" TO lcBlobData.

RUN asPutFromGib.p /* ON hServer */ ("CUSTOMER",lcBlobData,OUTPUT lOk,OUTPUT cReturn) NO-ERROR.

COPY-LOB FROM FILE "kom\in\order.json" TO lcBlobData.

RUN asPutFromGib.p /* ON hServer */ ("ORDER",lcBlobData,OUTPUT lOk,OUTPUT cReturn) NO-ERROR.
