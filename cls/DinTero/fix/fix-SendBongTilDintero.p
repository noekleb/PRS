/* Test av opprettelse av dsReceipts fra Bong. */
DEF VAR cReturn AS CHAR NO-UNDO.
DEF VAR lB_Id LIKE BongHode.B_Id NO-UNDO.
               
{cls\dintero\ttBong.i}
{cls\dintero\dsBong.i}

{cls\dintero\ttCustomerObj.i}
{cls\dintero\dsCustomerObj.i}
{cls\dintero\ttReceipts.i}
{cls\dintero\dsReceipts.i}

DEFINE VARIABLE rCustomerDintero    AS cls.Dintero.CustomerDintero NO-UNDO.
rCustomerDintero = NEW cls.Dintero.CustomerDintero( ) NO-ERROR.

ASSIGN
    lB_Id = 2004290000100100269017
    lB_Id = 2004290000110100106166
    lB_Id = 2004290000110100106167
    
    .

/* Renser loggpost. Kjøres bare når man ikke vil ha med eventuell responss */
/* fra dintero hentet tidligere.                                           */
/*
FOR EACH BongCRMLogg WHERE  
    BongCRMLogg.B_i = lB_Id:
    DELETE BongCRMLogg.
END.
*/

DO:
    /* Gjør bongen tilgjengelig. */
    FIND BongHode NO-LOCK WHERE
        BongHode.B_Id = lB_Id NO-ERROR.

    /* Fyller datasettet med aktuell bongdata. */
    DATASET dsBong:EMPTY-DATASET ().
    RUN cls\dintero\fyllDatasettBong.p (BongHode.B_Id, FALSE, INPUT-OUTPUT DATASET dsBong BY-REFERENCE).
END.

/* Skriver datasettet til en fil. */
FIND FIRST ttBongHode.
DATASET dsBong:WRITE-JSON('file','konv\dsBong_1_' + STRING(ttBongHode.B_Id) + '.json',FALSE).

/* Sender bongen til dintero. Når denne rutinen har kjørt, ligger det en */
/* komplett JSon melding i BongCRMLogg.cPayload.                         */
rCustomerDintero:CreateNewReceipts( INPUT DATASET dsBong, OUTPUT cReturn ).

/* Viser responskode fra Dintero. */
MESSAGE cReturn
    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
