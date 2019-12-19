/* artlag_sendtimport.p

    
    
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEFINE VARIABLE hQuery    AS HANDLE NO-UNDO.
DEFINE VARIABLE iType AS INTEGER NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEFINE VARIABLE bTest AS LOG NO-UNDO.
DEFINE VARIABLE cRecord AS CHARACTER NO-UNDO.
DEFINE VARIABLE lModified AS LOG NO-UNDO.
DEFINE VARIABLE bNullstill AS LOG NO-UNDO.

DEFINE BUFFER bufPkSdlHode FOR PkSdlHode.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.


{ cls\StdFunk\dsttImpFil.i }

ASSIGN
  bTest       = IF SEARCH('test.txt') = ? THEN FALSE ELSE TRUE
  bTest       = TRUE /* Ta bort linje etter prøveperiode. */
  iType       = INTEGER(ENTRY(1,icParam,'|'))
  cLogg       = ENTRY(2,icParam,'|')
  bNullstill  = CAN-DO('1,yes,true',ENTRY(3,icParam,'|'))
  .
IF cLogg = '' THEN
  cLogg = 'artlag_sendtimport' + REPLACE(STRING(TODAY),'/','').

rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.

IF bTest THEN
DO:
  rStandardFunksjoner:SkrivTilLogg(cLogg,
                      'Start.' 
                      ).
  rStandardFunksjoner:SkrivTilLogg(cLogg,
                      '  parametrene: ' + icParam
                      ).
  rStandardFunksjoner:SkrivTilLogg(cLogg,
                      '  iType: ' + STRING(iType)
                      ).
  rStandardFunksjoner:SkrivTilLogg(cLogg,
                      '  bNullstill: ' + STRING(bNullstill)
                      ).
END.

IF bNullstill THEN 
DO:
  IF bTest THEN
    rStandardFunksjoner:SkrivTilLogg(cLogg, 
      '  Nullstiller NOS på artikkler.' 
      ).
  FOR EACH ArtBas EXCLUSIVE-LOCK WHERE 
    ArtBas.Lagerkoder > '':

    IF bTest THEN
      rStandardFunksjoner:SkrivTilLogg(cLogg, 
        '    Artikkel: ' + ArtBas.LevKod + ' ' + ArtBas.LevFargKod + ' ' + ArtBas.Lagerkoder  
        ).
      
    IF ArtBas.Lagerkoder = ? THEN 
      ArtBas.Lagerkoder = ''.
      
    ASSIGN 
      ArtBas.Lagerkoder = REPLACE(ArtBAs.Lagerkoder,'NOS','')
      ArtBas.Lagerkoder = TRIM(ArtBas.Lagerkoder,',')
      ArtBas.Lagerkoder = REPLACE(ArtBas.Lagerkoder,',,',',')
      .
  END.
  IF bTest THEN
    rStandardFunksjoner:SkrivTilLogg(cLogg, 
      '  Ferdig nullstiller NOS.' 
      ).
END.

IF bTest THEN
DO:
  rStandardFunksjoner:SkrivTilLogg(cLogg, 
    '  ' 
    ).
  rStandardFunksjoner:SkrivTilLogg(cLogg, 
    '  Start NOS merking.' 
    ).
END.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " NO-LOCK").
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().
BUFFER_LOOP:
REPEAT WHILE NOT hQuery:QUERY-OFF-END:

  ASSIGN 
    cRecord = ihBuffer:BUFFER-FIELD("Record"):BUFFER-VALUE
    lModified = FALSE 
    .

  IF iType = 1 THEN
  TYPE1:
  DO: 
    IF (ENTRY(3,cRecord,';') <> '' AND ENTRY(5,cRecord,';') <> '' AND ENTRY(6,cRecord,';') <> '') THEN
    DO: 
      FOR EACH ArtBas EXCLUSIVE-LOCK WHERE 
        ArtBas.LevKod     = ENTRY(5,cRecord,';') AND 
        ArtBas.LevFargKod = ENTRY(6,cRecord,';'):
           
        IF NOT ArtBas.Lagerkoder MATCHES '*' + ENTRY(3,cRecord,';') + '*' THEN 
        DO:
          ArtBas.Lagerkoder = ArtBas.Lagerkoder + 
                            (IF ArtBas.Lagerkoder > '' THEN ',' ELSE '') + 
                            ENTRY(3,cRecord,';').   
          IF bTest THEN
          DO:
            IF bTest THEN
              rStandardFunksjoner:SkrivTilLogg(cLogg, 
                '    Artikkel: ' + ArtBas.LevKod + ' ' + ArtBas.LevFargKod + ' ' + ArtBas.Lagerkoder  
                ).
          END.        
        END.  
      END.
    END.
  END. /* TYPE1 */
  
  hQuery:GET-NEXT().
END. /* BUFFER_LOOP */

IF bTest THEN
  rStandardFunksjoner:SkrivTilLogg(cLogg, 
    '  Ferdig NOS merking.' 
    ).

DELETE OBJECT hQuery.

IF bTest THEN
DO:
  rStandardFunksjoner:SkrivTilLogg(cLogg, 'Slutt.').
END.


ASSIGN
  obOK = YES
  obOk = ocReturn = ""
  .

/* **********************  Internal Procedures  *********************** */


