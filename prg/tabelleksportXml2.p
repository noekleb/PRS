/* 
      ANROPANDE PROGRAM !!!!!

  Dette programmet gjør det samme som tabelleksportXml.p, men tar ogsåmed eventuelle koblede tabeller med et fremmed nøkkelfelt.
      
DEFINE VARIABLE hBuf    AS HANDLE     NO-UNDO.
DEFINE VARIABLE iantrec AS INTEGER    NO-UNDO.
DEFINE VARIABLE cOmmit  AS CHARACTER  NO-UNDO.
iantrec = 10.
cOmmit = "BrukerID,EDato,ETid,RegistrertAv,RegistrertDato,RegistrertTid".
hBuf = BUFFER farg:HANDLE.
RUN tabelleksportXml.p (hBuf,"Colours",iantrec,"farg",cOmmit,"color.xml")

/* parameter1: HANDLE till tabell                                          */
/* parameter2: docnod                                                      */
/* parameter3: antal records som dumpas                                    */
/* parameter4: attribut - kommaseparerad lista över nyckelfields, 1 räcker */
/* parameter5: lägg inte ut dessa fält                                     */
/* parameter6: Liste over underliggende tabeller. KOrdreLinje=KOrdre_Id (Tabelnavn og fremmednøkkel) */
/* parameter7: dokumentnamn                                                */
 
 */

DEFINE INPUT  PARAMETER hBuf     AS HANDLE     NO-UNDO.
DEFINE INPUT  PARAMETER cNodNavn AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER iNumrecords AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER ckeyfields AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER cIkkeFelter AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER cKobledeTabeller AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER cFilnavn AS CHARACTER  NO-UNDO.

DEFINE VARIABLE cValue    AS CHAR   NO-UNDO.
DEFINE VARIABLE qh        AS HANDLE NO-UNDO.
DEFINE VARIABLE ii        AS INT    NO-UNDO.
DEFINE VARIABLE cTekst    AS CHAR   NO-UNDO.
DEFINE VARIABLE piLoop    AS INTEGER  NO-UNDO.
/* DEFINE VARIABLE hBuf AS HANDLE. */

/* e-outcust.p - Export the Customer table to an xml file*/
DEFINE VARIABLE hDoc AS HANDLE.
DEFINE VARIABLE hRoot AS HANDLE.
DEFINE VARIABLE hRow AS HANDLE.
DEFINE VARIABLE hRow2 AS HANDLE.
DEFINE VARIABLE hField AS HANDLE.
DEFINE VARIABLE hField2 AS HANDLE.
DEFINE VARIABLE hText AS HANDLE.
DEFINE VARIABLE hText2 AS HANDLE.
DEFINE VARIABLE hDBFld AS HANDLE.
DEFINE VARIABLE hDBFld2 AS HANDLE.
DEFINE VARIABLE i AS INTEGER.
DEFINE VARIABLE iLoop AS INT NO-UNDO.

/* Koblet tabell */
DEFINE VARIABLE qhChild     AS HANDLE NO-UNDO.
DEFINE VARIABLE hbufChild   AS HANDLE NO-UNDO.
DEFINE VARIABLE cWhereChild AS CHARACTER NO-UNDO.

CREATE X-DOCUMENT hDoc.
hdoc:ENCODING = 'iso-8859-1'.
CREATE X-NODEREF hRoot.
CREATE X-NODEREF hRow.
CREATE X-NODEREF hRow2.
CREATE X-NODEREF hField.
CREATE X-NODEREF hText.
CREATE X-NODEREF hField2.
CREATE X-NODEREF hText2.

/* hBuf = BUFFER customer:HANDLE. */

CREATE QUERY qh.
qh:SET-BUFFERS(hBuf).

qh:QUERY-PREPARE("for each " + hBuf:NAME + " NO-LOCK").
/* qh:QUERY-PREPARE("for each customer where custnum < 5 NO-LOCK"). */

qh:QUERY-OPEN().
qh:GET-FIRST().

/*set up a root node*/
hDoc:CREATE-NODE(hRoot, cNodNavn, "ELEMENT").
hDoc:APPEND-CHILD(hRoot).
hRoot:SET-ATTRIBUTE("Numrec",STRING(iNumrecords)).

REPEAT WHILE NOT qh:QUERY-OFF-END:
   /*create a customer row node*/
   hDoc:CREATE-NODE(hRow, hBuf:NAME, "ELEMENT").
   hRoot:APPEND-CHILD(hRow).  /*put the row in the tree*/
   DO ii = 1 TO NUM-ENTRIES(ckeyfields):
       hRow:SET-ATTRIBUTE(ENTRY(ii,ckeyfields),STRING(hBuf:BUFFER-FIELD(ENTRY(ii,ckeyfields)):BUFFER-VALUE)).
   END.
/*    hRow:SET-ATTRIBUTE("Cust-num",STRING(hBuf:BUFFER-FIELD("custnum"):BUFFER-VALUE)). */
/*    hRow:SET-ATTRIBUTE("Name",hBuf:BUFFER-FIELD("name"):BUFFER-VALUE).                */

   /*Add the other fields as tags in the xml*/
   REPEAT i = 1 TO hBuf:NUM-FIELDS:
      hDBFld = hBuf:BUFFER-FIELD(i).
      IF CAN-DO(ckeyfields,hDBFld:NAME) OR CAN-DO(cIkkeFelter,hDBFld:NAME) THEN
          NEXT.
/*       IF hDBFld:NAME = "Cust-num" OR  hDBFld:NAME = "NAME" THEN NEXT. */

      /* fjerner eventuelle ugyldige karrakterer i feltnavn. */
      cTekst = replace(hDBFld:NAME,'%','_Proc').

      /*create a tag with the field name*/
      hDoc:CREATE-NODE(hField, cTekst, "ELEMENT").
      hRow:APPEND-CHILD(hField).  /*put the new field as next child of row*/
      hDoc:CREATE-NODE(hText, "", "TEXT").  /*add a node to hold field value*/
      hField:APPEND-CHILD(hText).           /*attach the text to the field*/
      cValue = STRING(hDBFld:BUFFER-VALUE).
      cValue = REPLACE(cValue,"&","&#38;").
      cValue = REPLACE(cValue,"'","&#39;").
      cValue = REPLACE(cValue,'"',"&#34;").
      cValue = REPLACE(cValue,"<","&#60;").
      cValue = REPLACE(cValue,">","&#62;").
      hText:NODE-VALUE = cValue
      NO-ERROR.

      IF ERROR-STATUS:ERROR AND ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
          PUBLISH 'webbutikkeksporterr' (STRING(ERROR-STATUS:NUM-MESSAGES) + 
                                         " feil oppsto ved eksport av tabell " + 
                                         hBuf:NAME + "." +
                                         hDBFld:NAME + " til fil " + 
                                         cFilnavn,50).
          DO iLoop = 1 TO ERROR-STATUS:NUM-MESSAGES:
            PUBLISH 'webbutikkeksporterr' (STRING(ERROR-STATUS:GET-NUMBER(iLoop)) + 
                                           " " + ERROR-STATUS:GET-MESSAGE(iLoop),50).
          END.
      END.
/*       hText:NODE-VALUE = STRING(hDBFld:BUFFER-VALUE). */
   END.
   /* Legger ut underliggende tabeller */
   IF cKobledeTabeller <> '' THEN
   KOBLEDE_TABELLER:
   DO:
     /* Looper rundt liste over underliggende tabeller */
     /* Disse tabellene har nøkkelfelt fra hovedtabell som fremmednøkkel */
     TABELL_LISTE_LOOP:
     DO piLoop = 1 TO NUM-ENTRIES(cKobledeTabeller):
       /* Bygger where sats for Child */
       WHERE_SATS:
       DO ii = 1 TO NUM-ENTRIES(ckeyfields):       
         cWhereChild = (IF cWhereChild = '' THEN '' ELSE ' AND ') +
                       ENTRY(ii,ckeyfields) + ' EQ ' + QUOTER(STRING(hBuf:BUFFER-FIELD(ENTRY(ii,ckeyfields)):BUFFER-VALUE)).
       END. /* WHERE_SATS */
       CREATE BUFFER hBufChild FOR TABLE ENTRY(piLoop,cKobledeTabeller).
       /* Skaper query for child. */
       CREATE QUERY qhChild.
       qhChild:SET-BUFFERS(hBufChild).
       qhChild:QUERY-PREPARE("for each " + hBufChild:NAME + " NO-LOCK WHERE " + cWhereChild).
       /* qh:QUERY-PREPARE("for each KOrdreLinje where KOrdre_Id = 5 NO-LOCK"). */
       qhChild:QUERY-OPEN().
       qhChild:GET-FIRST().
       
       CHILD_RECORD_LOOP:
       REPEAT WHILE NOT qhChild:QUERY-OFF-END:
         /* Oppretter node post */
         hDoc:CREATE-NODE (hRow2, hBufChild:NAME, "ELEMENT").
         hRow:APPEND-CHILD(hRow2).

         /*Add the other fields as tags in the xml*/
         REPEAT i = 1 TO hBufChild:NUM-FIELDS:
           hDBFld2 = hBufChild:BUFFER-FIELD(i).
           /* fjerner eventuelle ugyldige karrakterer i feltnavn. */
           cTekst = replace(hDBFld2:NAME,'%','_Proc').

           /*create a tag with the field name*/
           hDoc:CREATE-NODE(hField2, cTekst, "ELEMENT").
           hRow2:APPEND-CHILD(hField2).  /*put the new field as next child of row*/
           hDoc:CREATE-NODE(hText2, "", "TEXT").  /*add a node to hold field value*/
           hField2:APPEND-CHILD(hText2).           /*attach the text to the field*/
           cValue = STRING(hDBFld2:BUFFER-VALUE).
           cValue = REPLACE(cValue,"&","&#38;").
           cValue = REPLACE(cValue,"'","&#39;").
           cValue = REPLACE(cValue,'"',"&#34;").
           cValue = REPLACE(cValue,"<","&#60;").
           cValue = REPLACE(cValue,">","&#62;").
           hText2:NODE-VALUE = cValue
           NO-ERROR.

           IF ERROR-STATUS:ERROR AND ERROR-STATUS:NUM-MESSAGES > 0 THEN 
           DO:
              PUBLISH 'webbutikkeksporterr' (STRING(ERROR-STATUS:NUM-MESSAGES) + 
                                           " feil oppsto ved eksport av tabell " + 
                                           hBuf:NAME + "." +
                                           hDBFld:NAME + " til fil " + 
                                           cFilnavn,50).
             DO iLoop = 1 TO ERROR-STATUS:NUM-MESSAGES:
               PUBLISH 'webbutikkeksporterr' (STRING(ERROR-STATUS:GET-NUMBER(iLoop)) + 
                                             " " + ERROR-STATUS:GET-MESSAGE(iLoop),50).
             END.
           END.
         END.

         qhChild:GET-NEXT().   
       END. /* CHILD_RECORD_LOOP */
       
       DELETE OBJECT qhChild.
       DELETE OBJECT hBufChild.
       ASSIGN
         cWhereChild = ''.       
     END. /* TABELL_LISTE_LOOP */ 

   END. /* KOBLEDE_TABELLER */
   
   qh:GET-NEXT().
END.

DELETE OBJECT qh.

/*write the XML node tree to an xml file*/
hDoc:SAVE("file", cFilnavn).
DELETE OBJECT hDoc.
DELETE OBJECT hRoot.
DELETE OBJECT hRow.
DELETE OBJECT hField.
DELETE OBJECT hText.

