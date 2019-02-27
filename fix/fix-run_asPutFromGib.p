DEFINE VARIABLE lOK AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cReturn AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lcBlobData AS LONGCHAR     NO-UNDO. 

/*
COPY-LOB FROM FILE "kom\in\customer.json" TO lcBlobData.

RUN asPutFromGib.p /* ON hServer */ ("CUSTOMER",lcBlobData,OUTPUT lOk,OUTPUT cReturn) NO-ERROR.

COPY-LOB FROM FILE "kom\in\order.json" TO lcBlobData.

RUN asPutFromGib.p /* ON hServer */ ("ORDER",lcBlobData,OUTPUT lOk,OUTPUT cReturn) NO-ERROR.
*/
COPY-LOB FROM FILE "kom\in\gant\phx\order207872.json" TO lcBlobData.
RUN asPutFromGib.p /* ON hServer */ ("CUSTOMER",lcBlobData,OUTPUT lOk,OUTPUT cReturn) NO-ERROR.


COPY-LOB FROM FILE "kom\in\gant\phx\order207871.json" TO lcBlobData.
RUN asPutFromGib.p /* ON hServer */ ("ORDER",lcBlobData,OUTPUT lOk,OUTPUT cReturn) NO-ERROR.


COPY-LOB FROM FILE "kom\in\gant\phx\order207874.json" TO lcBlobData.
RUN asPutFromGib.p /* ON hServer */ ("CUSTOMER",lcBlobData,OUTPUT lOk,OUTPUT cReturn) NO-ERROR.

COPY-LOB FROM FILE "kom\in\gant\phx\order207873.json" TO lcBlobData.
RUN asPutFromGib.p /* ON hServer */ ("ORDER",lcBlobData,OUTPUT lOk,OUTPUT cReturn) NO-ERROR.

COPY-LOB FROM FILE "kom\in\gant\phx\order207876.json" TO lcBlobData.
RUN asPutFromGib.p /* ON hServer */ ("CUSTOMER",lcBlobData,OUTPUT lOk,OUTPUT cReturn) NO-ERROR.

COPY-LOB FROM FILE "kom\in\gant\phx\order207875.json" TO lcBlobData.
RUN asPutFromGib.p /* ON hServer */ ("ORDER",lcBlobData,OUTPUT lOk,OUTPUT cReturn) NO-ERROR.

COPY-LOB FROM FILE "kom\in\gant\phx\order207878.json" TO lcBlobData.
RUN asPutFromGib.p /* ON hServer */ ("CUSTOMER",lcBlobData,OUTPUT lOk,OUTPUT cReturn) NO-ERROR.

COPY-LOB FROM FILE "kom\in\gant\phx\order207877.json" TO lcBlobData.
RUN asPutFromGib.p /* ON hServer */ ("ORDER",lcBlobData,OUTPUT lOk,OUTPUT cReturn) NO-ERROR.

COPY-LOB FROM FILE "kom\in\gant\phx\order207880.json" TO lcBlobData.
RUN asPutFromGib.p /* ON hServer */ ("CUSTOMER",lcBlobData,OUTPUT lOk,OUTPUT cReturn) NO-ERROR.

COPY-LOB FROM FILE "kom\in\gant\phx\order207879.json" TO lcBlobData.
RUN asPutFromGib.p /* ON hServer */ ("ORDER",lcBlobData,OUTPUT lOk,OUTPUT cReturn) NO-ERROR.

COPY-LOB FROM FILE "kom\in\gant\phx\order207882.json" TO lcBlobData.
RUN asPutFromGib.p /* ON hServer */ ("CUSTOMER",lcBlobData,OUTPUT lOk,OUTPUT cReturn) NO-ERROR.

COPY-LOB FROM FILE "kom\in\gant\phx\order207881.json" TO lcBlobData.
RUN asPutFromGib.p /* ON hServer */ ("ORDER",lcBlobData,OUTPUT lOk,OUTPUT cReturn) NO-ERROR.

COPY-LOB FROM FILE "kom\in\gant\phx\order207884.json" TO lcBlobData.
RUN asPutFromGib.p /* ON hServer */ ("CUSTOMER",lcBlobData,OUTPUT lOk,OUTPUT cReturn) NO-ERROR.

COPY-LOB FROM FILE "kom\in\gant\phx\order207883.json" TO lcBlobData.
RUN asPutFromGib.p /* ON hServer */ ("ORDER",lcBlobData,OUTPUT lOk,OUTPUT cReturn) NO-ERROR.

COPY-LOB FROM FILE "kom\in\gant\phx\order207886.json" TO lcBlobData.
RUN asPutFromGib.p /* ON hServer */ ("CUSTOMER",lcBlobData,OUTPUT lOk,OUTPUT cReturn) NO-ERROR.

COPY-LOB FROM FILE "kom\in\gant\phx\order207885.json" TO lcBlobData.
RUN asPutFromGib.p /* ON hServer */ ("ORDER",lcBlobData,OUTPUT lOk,OUTPUT cReturn) NO-ERROR.


