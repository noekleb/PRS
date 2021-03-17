DEFINE VARIABLE lOK AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cReturn AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lcBlobData AS LONGCHAR     NO-UNDO.  

/* Tømmer og gjør klart for ny import av ordre. */
RUN fix-sletsalg.p.

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


COPY-LOB FROM FILE "kom\in\gant\phx\order342678.json" TO lcBlobData.
RUN asPutFromGib.p /* ON hServer */ ("ORDER",lcBlobData,OUTPUT lOk,OUTPUT cReturn) NO-ERROR.

COPY-LOB FROM FILE "kom\in\gant\phx\order342679.json" TO lcBlobData.
RUN asPutFromGib.p /* ON hServer */ ("CUSTOMER",lcBlobData,OUTPUT lOk,OUTPUT cReturn) NO-ERROR.

COPY-LOB FROM FILE "kom\in\gant\phx\order342680.json" TO lcBlobData.
RUN asPutFromGib.p /* ON hServer */ ("ORDER",lcBlobData,OUTPUT lOk,OUTPUT cReturn) NO-ERROR.

COPY-LOB FROM FILE "kom\in\gant\phx\order342681.json" TO lcBlobData.
RUN asPutFromGib.p /* ON hServer */ ("CUSTOMER",lcBlobData,OUTPUT lOk,OUTPUT cReturn) NO-ERROR.

COPY-LOB FROM FILE "kom\in\gant\phx\order342682.json" TO lcBlobData.
RUN asPutFromGib.p /* ON hServer */ ("ORDER",lcBlobData,OUTPUT lOk,OUTPUT cReturn) NO-ERROR.

COPY-LOB FROM FILE "kom\in\gant\phx\order342683.json" TO lcBlobData.
RUN asPutFromGib.p /* ON hServer */ ("CUSTOMER",lcBlobData,OUTPUT lOk,OUTPUT cReturn) NO-ERROR.



COPY-LOB FROM FILE "kom\in\gant\phx\order387538.json" TO lcBlobData.
RUN asPutFromGib.p /* ON hServer */ ("ORDER",lcBlobData,OUTPUT lOk,OUTPUT cReturn) NO-ERROR.

COPY-LOB FROM FILE "kom\in\gant\phx\order387540.json" TO lcBlobData.
RUN asPutFromGib.p /* ON hServer */ ("ORDER",lcBlobData,OUTPUT lOk,OUTPUT cReturn) NO-ERROR.

COPY-LOB FROM FILE "kom\in\gant\phx\order387542.json" TO lcBlobData.
RUN asPutFromGib.p /* ON hServer */ ("ORDER",lcBlobData,OUTPUT lOk,OUTPUT cReturn) NO-ERROR.

COPY-LOB FROM FILE "kom\in\gant\phx\order387543.json" TO lcBlobData.
RUN asPutFromGib.p /* ON hServer */ ("CUSTOMER",lcBlobData,OUTPUT lOk,OUTPUT cReturn) NO-ERROR.
