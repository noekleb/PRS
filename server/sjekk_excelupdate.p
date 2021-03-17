&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE INPUT-OUTPUT PARAMETER TABLE-HANDLE iothVareboklinjeRow.
DEFINE INPUT-OUTPUT PARAMETER TABLE-HANDLE iothVareboklinjeCol.
DEFINE OUTPUT PARAMETER TABLE-HANDLE othVareboklinje.
DEFINE OUTPUT PARAMETER TABLE-HANDLE othError.

DEF VAR thError             AS HANDLE NO-UNDO.
DEF VAR thVareboklinje      AS HANDLE NO-UNDO.

DEF VAR bhVareboklinjeCol   AS HANDLE NO-UNDO.
DEF VAR bhVareboklinje      AS HANDLE NO-UNDO.
DEF VAR bhttVareboklinje    AS HANDLE NO-UNDO.

DEF VAR qh                  AS HANDLE NO-UNDO.
DEF VAR qhRow               AS HANDLE NO-UNDO.
DEF VAR qhCol               AS HANDLE NO-UNDO.

DEF VAR cFieldList          AS CHAR   NO-UNDO.
DEF VAR cVareboklinjeFields AS CHAR   NO-UNDO.

DEF VAR rRowid              AS ROWID  NO-UNDO.

DEF VAR ix                  AS INT    NO-UNDO.

DEF TEMP-TABLE ttError NO-UNDO
  FIELD iRowNum    AS INT
  FIELD iColumnNum AS INT
  FIELD cFieldName AS CHAR
  FIELD cErrorType AS CHAR /*Error, Warning, Info*/
  FIELD cErrorText AS CHAR 
  INDEX iRowNum IS PRIMARY iRowNum
  .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-addMessage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD addMessage Procedure 
FUNCTION addMessage RETURNS LOGICAL (INPUT iiRowNum AS INT,
                   INPUT iiColumnNum AS INT, 
                   INPUT icErrorType AS CHAR, 
                   INPUT icMessage   AS CHAR) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-buildttVareboklinje) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD buildttVareboklinje Procedure 
FUNCTION buildttVareboklinje RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doValidate) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD doValidate Procedure 
FUNCTION doValidate RETURNS CHARACTER
  (INPUT icRowid  AS CHAR,
   INPUT icField  AS CHAR,
   INPUT icValue  AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getAllFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getAllFields Procedure 
FUNCTION getAllFields RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getVareboklinjeFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getVareboklinjeFields Procedure 
FUNCTION getVareboklinjeFields RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-update_levuke) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD update_levuke Procedure 
FUNCTION update_levuke RETURNS CHARACTER
  ( INPUT irRowid AS ROWID ,
    INPUT icValue AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-update_RavdBeskrivelse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD update_RavdBeskrivelse Procedure 
FUNCTION update_RavdBeskrivelse RETURNS CHARACTER
  ( INPUT irRowid AS ROWID,
    INPUT icValue AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-update_ravdnr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD update_ravdnr Procedure 
FUNCTION update_ravdnr RETURNS CHARACTER
  ( INPUT irRowid AS ROWID,
    INPUT icValue AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

RUN initializeObject.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-InitializeObject) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeObject Procedure 
PROCEDURE InitializeObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR cValueList          AS CHAR   NO-UNDO.
  DEF VAR ocReturn            AS CHAR   NO-UNDO.
  DEF VAR bhLookup            AS HANDLE NO-UNDO.  
  DEF VAR cFieldName          AS CHAR   NO-UNDO.  
  
  ASSIGN 
    othError          = TEMP-TABLE ttError:HANDLE
    bhVareboklinjeCol = iothVareboklinjeCol:DEFAULT-BUFFER-HANDLE
    bhVareboklinje    = BUFFER VareBokLinje:HANDLE
  .
  
  ASSIGN 
    cFieldList          = getAllFields()
    cVareboklinjeFields = getVareboklinjeFields()
  .

  buildttVareboklinje().
  
  /*Now use qh for getting all rows and travers the cFieldList to get all values within xxxCol to get the values*/
  CREATE QUERY qhRow.
  qhRow:SET-BUFFERS(iothVareboklinjeRow:DEFAULT-BUFFER-HANDLE).
  
  CREATE QUERY qhCol.
  qhCol:SET-BUFFERS(iothVareboklinjeCol:DEFAULT-BUFFER-HANDLE).

  qhRow:QUERY-PREPARE('FOR EACH ' + iothVareboklinjeRow:DEFAULT-BUFFER-HANDLE:NAME + ' WHERE rRowid NE ?').
  
  qhRow:QUERY-OPEN().
  qhRow:GET-FIRST().
  
  blokkA:
  DO TRANSACTION WHILE NOT qhRow:QUERY-OFF-END:
    IF qhCol:IS-OPEN THEN qhCol:QUERY-CLOSE.
    qhCol:QUERY-PREPARE('FOR EACH ' + iothVareboklinjeCol:DEFAULT-BUFFER-HANDLE:NAME + ' WHERE rRowid = TO-ROWID(' + QUOTER(STRING(qhRow:GET-BUFFER-HANDLE(1):BUFFER-FIELD('rRowid'):BUFFER-VALUE)) + ')').

    qhCol:QUERY-OPEN().
    qhCol:GET-FIRST().
    
    IF qhCol:GET-BUFFER-HANDLE(1):AVAIL THEN 
    DO:
      bhttVareboklinje:BUFFER-CREATE().
      cValueList = ''.
    END.
    
    blokkB:
    DO WHILE NOT qhCol:QUERY-OFF-END ON ERROR UNDO blokkB, NEXT blokkA:

      ASSIGN 
        bhttVareboklinje:BUFFER-FIELD('rRowid'):BUFFER-VALUE  = bhVareboklinjeCol:BUFFER-FIELD('rRowid'):BUFFER-VALUE
        bhttVareboklinje:BUFFER-FIELD('iRowNum'):BUFFER-VALUE = qhRow:GET-BUFFER-HANDLE(1):BUFFER-FIELD('iRowNum'):BUFFER-VALUE
        cFieldName                                            = qhCol:GET-BUFFER-HANDLE(1):BUFFER-FIELD('cFieldName'):BUFFER-VALUE
        ocReturn                                              = ''
      .
      CASE bhttVareboklinje:BUFFER-FIELD(cFieldName):DATA-TYPE:
        WHEN 'CHARACTER' THEN  bhttVareboklinje:BUFFER-FIELD(cFieldName):BUFFER-VALUE = bhVareboklinjeCol:BUFFER-FIELD('cValue'):BUFFER-VALUE NO-ERROR.
        WHEN 'DATE'      THEN  bhttVareboklinje:BUFFER-FIELD(cFieldName):BUFFER-VALUE = DATE(bhVareboklinjeCol:BUFFER-FIELD('cValue'):BUFFER-VALUE) NO-ERROR.
        WHEN 'DECIMAL'   THEN  bhttVareboklinje:BUFFER-FIELD(cFieldName):BUFFER-VALUE = DECIMAL(bhVareboklinjeCol:BUFFER-FIELD('cValue'):BUFFER-VALUE) NO-ERROR.
        WHEN 'INTEGER'   THEN  bhttVareboklinje:BUFFER-FIELD(cFieldName):BUFFER-VALUE = INTEGER(bhVareboklinjeCol:BUFFER-FIELD('cValue'):BUFFER-VALUE) NO-ERROR.
        WHEN 'LOGICAL'   THEN  bhttVareboklinje:BUFFER-FIELD(cFieldName):BUFFER-VALUE = LOGICAL(bhVareboklinjeCol:BUFFER-FIELD('cValue'):BUFFER-VALUE) NO-ERROR.
      END CASE.
      /****************** Check if input value is not of correct data-type*/
      IF ERROR-STATUS:ERROR THEN
      DO:
        ASSIGN 
          bhttVareboklinje:BUFFER-FIELD('bHasError'):BUFFER-VALUE           = TRUE
          qhRow:GET-BUFFER-HANDLE(1):BUFFER-FIELD('bHasError'):BUFFER-VALUE = TRUE
        .
        addMessage(qhRow:GET-BUFFER-HANDLE(1):BUFFER-FIELD('iRowNum'):BUFFER-VALUE,bhVareboklinjeCol:BUFFER-FIELD('iFieldNum'):BUFFER-VALUE,'Error',bhVareboklinjeCol:BUFFER-FIELD('cFieldName'):BUFFER-VALUE + ' har feil datatype. Feltet må ha datatype ' + bhttVareboklinje:BUFFER-FIELD(cFieldName):DATA-TYPE).
        qhRow:GET-NEXT(NO-LOCK).
        UNDO blokkB, NEXT blokkA.
      END.
      /***************** Check if input value is UNKNOWN and not of type CHARACTER*/
      ELSE IF bhttVareboklinje:BUFFER-FIELD(cFieldName):BUFFER-VALUE = ? AND bhttVareboklinje:BUFFER-FIELD(cFieldName):DATA-TYPE NE 'CHARACTER' THEN
      DO:
        ASSIGN
          bhttVareboklinje:BUFFER-FIELD('bHasError'):BUFFER-VALUE      = TRUE
          qhRow:GET-BUFFER-HANDLE(1):BUFFER-FIELD('bHasError'):BUFFER-VALUE = TRUE
        .
        addMessage(qhRow:GET-BUFFER-HANDLE(1):BUFFER-FIELD('iRowNum'):BUFFER-VALUE,bhVareboklinjeCol:BUFFER-FIELD('iFieldNum'):BUFFER-VALUE,'Error', bhVareboklinjeCol:BUFFER-FIELD('cFieldName'):BUFFER-VALUE + ' har feil verdi, ukjent er en ugyldig verdi.').
        qhRow:GET-NEXT(NO-LOCK).
        UNDO blokkB, NEXT blokkA.
      END.
      /***************** Check if not a Vareboklinje field*/
      ELSE IF NOT CAN-DO(cVareboklinjeFields,cFieldName) THEN
      DO:
        /*Find Vareboklinje by varebok and artikkel, use the rowid*/
        bhVareboklinje:FIND-UNIQUE('WHERE vareboknr = DEC(' + QUOTER(STRING(qhRow:GET-BUFFER-HANDLE(1):BUFFER-FIELD('Vareboknr'):BUFFER-VALUE)) + ')' 
                                 + ' AND artikkelnr = DEC(' + QUOTER(STRING(qhRow:GET-BUFFER-HANDLE(1):BUFFER-FIELD('Artikkelnr'):BUFFER-VALUE)) + ')',NO-LOCK) NO-ERROR.
        IF bhVareboklinje:AVAIL THEN rRowid = bhVareboklinje:ROWID.
        ELSE
        DO:
          ASSIGN 
            bhttVareboklinje:BUFFER-FIELD('bHasError'):BUFFER-VALUE           = TRUE
            qhRow:GET-BUFFER-HANDLE(1):BUFFER-FIELD('bHasError'):BUFFER-VALUE = TRUE
          .
          addMessage(qhRow:GET-BUFFER-HANDLE(1):BUFFER-FIELD('iRowNum'):BUFFER-VALUE,bhVareboklinjeCol:BUFFER-FIELD('iFieldNum'):BUFFER-VALUE,'Error',  'Rad: ' + STRING(qhRow:GET-BUFFER-HANDLE(1):BUFFER-FIELD('iRowNum'):BUFFER-VALUE) + ' - Kunne ikke finne Vareboklinje for vareboknr/artikkelnr ' + STRING(qhRow:GET-BUFFER-HANDLE(1):BUFFER-FIELD('Vareboknr'):BUFFER-VALUE) + '/' + STRING(qhRow:GET-BUFFER-HANDLE(1):BUFFER-FIELD('Artikkelnr'):BUFFER-VALUE) ).
          qhRow:GET-NEXT(NO-LOCK).
          UNDO blokkB, NEXT blokkA.
        END.
        /*************** Check if it is a LOOKUP field*/
        IF NUM-ENTRIES(bhVareboklinjeCol:BUFFER-FIELD('cUpdateField'):BUFFER-VALUE,'|') = 2 THEN
        DO:
          CREATE BUFFER bhLookup FOR TABLE ENTRY(1,bhVareboklinjeCol:BUFFER-FIELD('cUpdateField'):BUFFER-VALUE,'|').
          bhLookup:FIND-FIRST('WHERE ' + cFieldName + ' = ' + QUOTER(bhVareboklinjeCol:BUFFER-FIELD('cValue'):BUFFER-VALUE),NO-LOCK) NO-ERROR.

          /************** Check if update field (parameter 2,'|') is a vareboklinje field*/
          IF CAN-DO(cVareboklinjeFields,ENTRY(2,bhVareboklinjeCol:BUFFER-FIELD('cUpdateField'):BUFFER-VALUE,'|')) THEN
          DO:
            IF bhLookup:AVAIL AND NOT COMPARE(bhVareboklinje:BUFFER-FIELD(ENTRY(2,bhVareboklinjeCol:BUFFER-FIELD('cUpdateField'):BUFFER-VALUE,'|')):BUFFER-VALUE,'EQ',bhLookup:BUFFER-FIELD(ENTRY(2,bhVareboklinjeCol:BUFFER-FIELD('cUpdateField'):BUFFER-VALUE,'|')):BUFFER-VALUE,'RAW') THEN 
            DO:
              RUN update_vareboklinje.p (INPUT STRING(rRowid), INPUT ENTRY(2,bhVareboklinjeCol:BUFFER-FIELD('cUpdateField'):BUFFER-VALUE,'|'), INPUT  STRING(bhLookup:BUFFER-FIELD(ENTRY(2,bhVareboklinjeCol:BUFFER-FIELD('cUpdateField'):BUFFER-VALUE,'|')):BUFFER-VALUE),?, OUTPUT ocReturn).
              IF ocReturn NE '' THEN
              DO:
                ASSIGN 
                  bhttVareboklinje:BUFFER-FIELD('bHasError'):BUFFER-VALUE           = TRUE
                  qhRow:GET-BUFFER-HANDLE(1):BUFFER-FIELD('bHasError'):BUFFER-VALUE = TRUE
                .
                addMessage(qhRow:GET-BUFFER-HANDLE(1):BUFFER-FIELD('iRowNum'):BUFFER-VALUE,bhVareboklinjeCol:BUFFER-FIELD('iFieldNum'):BUFFER-VALUE,'Error',  ocReturn).
                
                bhLookup:BUFFER-RELEASE.
                DELETE OBJECT bhLookup.

                qhRow:GET-NEXT(NO-LOCK).
                UNDO blokkB, NEXT blokkA.
              END.
            END.
/*             ELSE /*What!*/ */
          END.
          /************** Parameter 2,'|' has to be added as a function like update_<field>*/          
          ELSE
          DO:
            IF NOT CAN-DO(THIS-PROCEDURE:INTERNAL-ENTRIES,'update_' + cFieldName) THEN
              MESSAGE 'Fant ikke valideringsfunksjonen: update_' cFieldName VIEW-AS ALERT-BOX INFO BUTTONS OK.

            DYNAMIC-FUNCTION('update_' + cFieldName,rRowid,INPUT  STRING(bhVareboklinjeCol:BUFFER-FIELD('cValue'):BUFFER-VALUE)) NO-ERROR.
            IF ocReturn NE '' THEN
            DO:
              ASSIGN 
                bhttVareboklinje:BUFFER-FIELD('bHasError'):BUFFER-VALUE           = TRUE
                qhRow:GET-BUFFER-HANDLE(1):BUFFER-FIELD('bHasError'):BUFFER-VALUE = TRUE
              .
              addMessage(qhRow:GET-BUFFER-HANDLE(1):BUFFER-FIELD('iRowNum'):BUFFER-VALUE,bhVareboklinjeCol:BUFFER-FIELD('iFieldNum'):BUFFER-VALUE,'Error',  ocReturn).
              
              bhLookup:BUFFER-RELEASE.
              DELETE OBJECT bhLookup.
              
              qhRow:GET-NEXT(NO-LOCK).
              UNDO blokkB, NEXT blokkA.
            END.  
          END.
          bhLookup:BUFFER-RELEASE.
          DELETE OBJECT bhLookup.
        END.
        /*************** Normal field that is not a lookup*/
        ELSE
        DO:
          bhVareboklinje:FIND-UNIQUE('WHERE vareboknr  = DEC(' + QUOTER(STRING(qhRow:GET-BUFFER-HANDLE(1):BUFFER-FIELD('Vareboknr'):BUFFER-VALUE)) + ')' 
                                    + ' AND artikkelnr = DEC(' + QUOTER(STRING(qhRow:GET-BUFFER-HANDLE(1):BUFFER-FIELD('Artikkelnr'):BUFFER-VALUE)) + ')',NO-LOCK) NO-ERROR.
          IF bhVareboklinje:AVAIL THEN rRowid = bhVareboklinje:ROWID.
          ELSE
          DO:
            ASSIGN 
              bhttVareboklinje:BUFFER-FIELD('bHasError'):BUFFER-VALUE           = TRUE
              qhRow:GET-BUFFER-HANDLE(1):BUFFER-FIELD('bHasError'):BUFFER-VALUE = TRUE
            .
            addMessage(qhRow:GET-BUFFER-HANDLE(1):BUFFER-FIELD('iRowNum'):BUFFER-VALUE,bhVareboklinjeCol:BUFFER-FIELD('iFieldNum'):BUFFER-VALUE,'Error',  'Rad: ' + STRING(qhRow:GET-BUFFER-HANDLE(1):BUFFER-FIELD('iRowNum'):BUFFER-VALUE) + ' - Kunne ikke finne Vareboklinje for vareboknr/artikkelnr ' + STRING(qhRow:GET-BUFFER-HANDLE(1):BUFFER-FIELD('Vareboknr'):BUFFER-VALUE) + '/' + STRING(qhRow:GET-BUFFER-HANDLE(1):BUFFER-FIELD('Artikkelnr'):BUFFER-VALUE) ).
            qhRow:GET-NEXT(NO-LOCK).
            UNDO blokkB, NEXT blokkA.
          END.
          bhVareboklinje:FIND-BY-ROWID(rRowid,NO-LOCK) NO-ERROR.
          /*Gidder ikke feilmelde for slappe programmerere funksjon må finnes...*/
          ocReturn = DYNAMIC-FUNCTION('update_' + cFieldName,rRowid,bhVareboklinjeCol:BUFFER-FIELD('cValue'):BUFFER-VALUE).
          IF ocReturn NE '' THEN
          DO:
            ASSIGN 
              bhttVareboklinje:BUFFER-FIELD('bHasError'):BUFFER-VALUE           = TRUE
              qhRow:GET-BUFFER-HANDLE(1):BUFFER-FIELD('bHasError'):BUFFER-VALUE = TRUE
            .
            addMessage(qhRow:GET-BUFFER-HANDLE(1):BUFFER-FIELD('iRowNum'):BUFFER-VALUE,bhVareboklinjeCol:BUFFER-FIELD('iFieldNum'):BUFFER-VALUE,'Error',  ocReturn).
            qhRow:GET-NEXT(NO-LOCK).
            UNDO blokkB, NEXT blokkA.
          END.           
        END.
      END.
      /***************** It is a field within vareboklinje, validate and update*/
      ELSE
      DO:
        bhVareboklinje:FIND-UNIQUE('WHERE vareboknr  = DEC(' + QUOTER(STRING(qhRow:GET-BUFFER-HANDLE(1):BUFFER-FIELD('Vareboknr'):BUFFER-VALUE)) + ')' 
                                  + ' AND artikkelnr = DEC(' + QUOTER(STRING(qhRow:GET-BUFFER-HANDLE(1):BUFFER-FIELD('Artikkelnr'):BUFFER-VALUE)) + ')',NO-LOCK) NO-ERROR.
        IF bhVareboklinje:AVAIL THEN rRowid = bhVareboklinje:ROWID.
        ELSE
        DO:
          ASSIGN 
            bhttVareboklinje:BUFFER-FIELD('bHasError'):BUFFER-VALUE           = TRUE
            qhRow:GET-BUFFER-HANDLE(1):BUFFER-FIELD('bHasError'):BUFFER-VALUE = TRUE
          .
          addMessage(qhRow:GET-BUFFER-HANDLE(1):BUFFER-FIELD('iRowNum'):BUFFER-VALUE,bhVareboklinjeCol:BUFFER-FIELD('iFieldNum'):BUFFER-VALUE,'Error',  'Rad: ' + STRING(qhRow:GET-BUFFER-HANDLE(1):BUFFER-FIELD('iRowNum'):BUFFER-VALUE) + ' - Kunne ikke finne Vareboklinje for vareboknr/artikkelnr ' + STRING(qhRow:GET-BUFFER-HANDLE(1):BUFFER-FIELD('Vareboknr'):BUFFER-VALUE) + '/' + STRING(qhRow:GET-BUFFER-HANDLE(1):BUFFER-FIELD('Artikkelnr'):BUFFER-VALUE) ).
          qhRow:GET-NEXT(NO-LOCK).
          UNDO blokkB, NEXT blokkA.
        END.
        bhVareboklinje:FIND-BY-ROWID(rRowid,NO-LOCK) NO-ERROR.

        /*Hvis de ikke er like, og (data-type ne 'character' og verdi = ?) tom string verdi er ? fra excel*/
        IF NOT COMPARE(bhVareboklinje:BUFFER-FIELD(cFieldName):BUFFER-VALUE,'EQ',bhttVareboklinje:BUFFER-FIELD(cFieldName):BUFFER-VALUE,'RAW') 
          AND NOT (bhttVareboklinje:BUFFER-FIELD(cFieldName):DATA-TYPE = 'CHARACTER' AND bhttVareboklinje:BUFFER-FIELD(cFieldName):BUFFER-VALUE = ?) THEN
        DO: /*Det har skjedd en endring i verdi, og den må lagres*/
          ocReturn = doValidate(INPUT STRING(rRowid), INPUT cFieldName, INPUT  bhVareboklinjeCol:BUFFER-FIELD('cValue'):BUFFER-VALUE).
          IF ocReturn NE '' THEN
          DO:
            ASSIGN 
              bhttVareboklinje:BUFFER-FIELD('bHasError'):BUFFER-VALUE           = TRUE
              qhRow:GET-BUFFER-HANDLE(1):BUFFER-FIELD('bHasError'):BUFFER-VALUE = TRUE
            .
            addMessage(qhRow:GET-BUFFER-HANDLE(1):BUFFER-FIELD('iRowNum'):BUFFER-VALUE,bhVareboklinjeCol:BUFFER-FIELD('iFieldNum'):BUFFER-VALUE,'Error',  ocReturn).
            qhRow:GET-NEXT(NO-LOCK).
            UNDO blokkB, NEXT blokkA.
          END.           
        END.
      END.
      qhCol:GET-NEXT(NO-LOCK).
    END. /*blokkB*/
    qhRow:GET-NEXT(NO-LOCK).
  END. /*blokkA*/
  othVareboklinje = thVareboklinje.
  
  qhCol:GET-BUFFER-HANDLE(1):BUFFER-RELEASE.
  qhCol:QUERY-CLOSE().
  DELETE OBJECT qhCol.

  qhRow:GET-BUFFER-HANDLE(1):BUFFER-RELEASE.
  qhRow:QUERY-CLOSE().
  DELETE OBJECT qhRow.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-addMessage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION addMessage Procedure 
FUNCTION addMessage RETURNS LOGICAL (INPUT iiRowNum AS INT,
                   INPUT iiColumnNum AS INT, 
                   INPUT icErrorType AS CHAR, 
                   INPUT icMessage   AS CHAR):
  othError:DEFAULT-BUFFER-HANDLE:BUFFER-CREATE().
  ASSIGN 
    othError:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD('iRowNum'):BUFFER-VALUE    = iiRowNum
    othError:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD('iColumnNum'):BUFFER-VALUE = iiColumnNum
    othError:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD('cErrorType'):BUFFER-VALUE = icErrorType
    othError:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD('cErrorText'):BUFFER-VALUE = icMessage
  .
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-buildttVareboklinje) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION buildttVareboklinje Procedure 
FUNCTION buildttVareboklinje RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  CREATE TEMP-TABLE thVareboklinje.
  
  thVareboklinje:CREATE-LIKE(BUFFER Vareboklinje:HANDLE,'Pris').
  thVareboklinje:ADD-NEW-FIELD('rRowid','ROWID').
  thVareboklinje:ADD-NEW-FIELD('iRowNum','INTEGER').
  thVareboklinje:ADD-NEW-FIELD('bHasError','LOGICAL').
  
  DO ix = 1 TO NUM-ENTRIES(cFieldList):
    IF NOT CAN-DO(cVareboklinjeFields,ENTRY(ix,cFieldlist)) THEN
    DO:
      bhVareboklinjeCol:FIND-FIRST('WHERE cFieldName=' + QUOTER(ENTRY(ix,cFieldList))) NO-ERROR.
      IF bhVareboklinjeCol:AVAIL THEN
      DO:
        thVareboklinje:ADD-NEW-FIELD(ENTRY(ix,cFieldList),bhVareboklinjeCol:BUFFER-FIELD('cDataType'):STRING-VALUE).
      END.
    END.
  END.
  
  thVareboklinje:ADD-NEW-INDEX('rRowid',TRUE,FALSE).
  thVareboklinje:ADD-INDEX-FIELD('rRowid','rRowid').
  thVareboklinje:ADD-NEW-INDEX('iRowNum',FALSE,FALSE).
  thVareboklinje:ADD-INDEX-FIELD('iRowNum','iRowNum').
  
  thVareboklinje:TEMP-TABLE-PREPARE('ttVareboklinje').
  bhttVareboklinje = thVareboklinje:DEFAULT-BUFFER-HANDLE.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doValidate) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION doValidate Procedure 
FUNCTION doValidate RETURNS CHARACTER
  (INPUT icRowid  AS CHAR,
   INPUT icField  AS CHAR,
   INPUT icValue  AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR ocReturn AS CHAR NO-UNDO.
  DEF VAR cFix     AS CHAR NO-UNDO.

  /*Excel BUG håndtering*/
  IF CAN-DO('kampanjeuker',icField) THEN
  DO:
    IF INDEX(icValue,',0000000000') gt 0 THEN
      ASSIGN 
        icValue = TRIM(icValue,',0000000000')
        icValue = STRING(INT(icValue),'99').
    ELSE IF NUM-ENTRIES(icValue) = 2 THEN
      ASSIGN 
        cFix    = STRING(DECIMAL(icValue),'99.99')
        icValue = STRING(INT(ENTRY(1,cFix)),'99') + ',' + STRING(INT(ENTRY(2,cFix)),'99')
      .
  END.  

  CASE icField: /*Ekstra valideringssjekk utenfor det sted den EGENTLIG bør gjøres*/
/*     WHEN 'SasBeskr' THEN                                                                                             */
/*     DO:                                                                                                              */
/*       FIND FIRST sasong WHERE sasong.sasBeskr = icValue NO-LOCK NO-ERROR.                                            */
/*       IF NOT AVAIL sasong THEN                                                                                       */
/*         ocReturn = 'Feltet ' + icField + ' har feil verdi. Kan ikke finne verdien ' + icValue + ' i sesong tabell.'. */
/*     END.                                                                                                             */
/*     WHEN 'levuke' THEN                                                                                                        */
/*     DO:                                                                                                                       */
/*       FIND FIRST vareboklinje WHERE ROWID(vareboklinje) = TO-ROWID(icRowid) NO-LOCK NO-ERROR.                                 */
/*       IF AVAIL vareboklinje THEN FIND FIRST varebokhode OF vareboklinje NO-LOCK NO-ERROR.                                     */
/*       IF AVAIL varebokhode  THEN FIND FIRST messe WHERE messe.messenr = varebokhode.messenr NO-LOCK NO-ERROR.                 */
/*       IF AVAIL messe THEN                                                                                                     */
/*       DO:                                                                                                                     */
/*           IF NOT CAN-DO(messe.sortimentkoder,icValue,'¤') AND icValue NE '' THEN                                                  */
/*             ocReturn = 'Feltet ' + icField + ' har feil verdi. Må ha en av følgende verdier [' +  messe.sortimentkoder + ']'. */
/*                                                                                                                               */
/*       END.                                                                                                                    */
/*     END.                                                                                                                      */
    WHEN 'sortimentkoder' THEN
    DO:
      FIND FIRST vareboklinje WHERE ROWID(vareboklinje) = TO-ROWID(icRowid) NO-LOCK NO-ERROR.
      IF AVAIL vareboklinje THEN FIND FIRST varebokhode OF vareboklinje NO-LOCK NO-ERROR.
      IF AVAIL varebokhode  THEN FIND FIRST messe WHERE messe.messenr = varebokhode.messenr NO-LOCK NO-ERROR.
      IF AVAIL messe THEN
      DO:
          IF NOT LOOKUP(icValue,messe.sortimentkoder,'¤') GT 0 THEN
            ocReturn = 'Feltet ' + icField +  ' (' + icValue + ') har feil verdi. Må ha en av følgende verdier [' +  messe.sortimentkoder + ']'.

      END.
    END.
    WHEN 'kampanjeuker' THEN
    DO:
      FIND FIRST vareboklinje WHERE ROWID(vareboklinje) = TO-ROWID(icRowid) NO-LOCK NO-ERROR.
      IF AVAIL vareboklinje THEN FIND FIRST varebokhode OF vareboklinje NO-LOCK NO-ERROR.
      IF AVAIL varebokhode  THEN FIND FIRST messe WHERE messe.messenr = varebokhode.messenr NO-LOCK NO-ERROR.
      IF AVAIL messe THEN
      DO:
        IF NOT LOOKUP(icValue,trim(messe.kampanjeuker),'¤') GT 0 THEN
          ocReturn = 'Feltet ' + icField + ' (' + icValue + ') har feil verdi. Må ha en av følgende verdier [' +  messe.kampanjeuker + ']'.

      END.
    END.
    WHEN 'Lagerkoder' THEN
    DO:
      FIND FIRST vareboklinje WHERE ROWID(vareboklinje) = TO-ROWID(icRowid) NO-LOCK NO-ERROR.
      IF AVAIL vareboklinje THEN FIND FIRST varebokhode OF vareboklinje NO-LOCK NO-ERROR.
      IF AVAIL varebokhode  THEN FIND FIRST messe WHERE messe.messenr = varebokhode.messenr NO-LOCK NO-ERROR.
      IF AVAIL messe THEN
      DO:
        IF NOT LOOKUP(icValue,messe.lagerkoder,'¤') GT 0 THEN
          ocReturn = 'Feltet ' + icField +  ' (' + icValue + ') har feil verdi. Må ha en av følgende verdier [' +  messe.lagerkoder + ']'.

      END.
    END.
    WHEN 'Kampanjestotte' THEN
    DO:
      FIND FIRST vareboklinje WHERE ROWID(vareboklinje) = TO-ROWID(icRowid) NO-LOCK NO-ERROR.
      IF AVAIL vareboklinje THEN FIND FIRST varebokhode OF vareboklinje NO-LOCK NO-ERROR.
      IF AVAIL varebokhode  THEN FIND FIRST messe WHERE messe.messenr = varebokhode.messenr NO-LOCK NO-ERROR.
      IF AVAIL messe THEN
      DO:
        IF NOT LOOKUP(icValue,messe.kampanjestotte,'¤') GT 0 THEN
          ocReturn = 'Feltet ' + icField +  ' (' + icValue + ') har feil verdi. Må ha en av følgende verdier [' +  messe.kampanjestotte + ']'.

      END.
    END.
    WHEN 'Vg' THEN
    DO:
      FIND FIRST VarGr WHERE VarGr.Vg = INT(icValue) NO-LOCK NO-ERROR.
      IF NOT AVAIL VarGr THEN
        ocReturn = 'Feltet ' + icField + ' har feil verdi. Kan ikke finne verdien ' + STRING(INT(icValue)) + ' i VarGr tabell.'.
    END.
  END CASE.
  IF (ocReturn = '' OR ocReturn = ?) /*AND icField NE 'SasBeskr'*/ THEN
    RUN update_vareboklinje.p (INPUT icRowid, INPUT icField, INPUT  icValue,?, OUTPUT ocReturn).
  
  RETURN ocReturn.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getAllFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getAllFields Procedure 
FUNCTION getAllFields RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR cReturnValue AS CHAR NO-UNDO.

  CREATE QUERY qh.
  
  /*Find all fields within VareboklinjeCol. Put them into a list and 
    use that list to retrieve the data*/
  iothVareboklinjeRow:DEFAULT-BUFFER-HANDLE:FIND-FIRST('WHERE TRUE').
  IF iothVareboklinjeRow:DEFAULT-BUFFER-HANDLE:AVAIL THEN
  DO:
    qh:SET-BUFFERS(iothVareboklinjeCol:DEFAULT-BUFFER-HANDLE).
    qh:QUERY-PREPARE('FOR EACH ' + iothVareboklinjeCol:DEFAULT-BUFFER-HANDLE:NAME 
                     + ' WHERE STRING(rRowid) = ' 
                     + QUOTER(STRING(iothVareboklinjeRow:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD('rRowid'):BUFFER-VALUE))).
    qh:QUERY-OPEN().
    qh:GET-FIRST(NO-LOCK).
  
    DO WHILE iothVareboklinjeCol:DEFAULT-BUFFER-HANDLE:AVAIL:
      cReturnValue = cReturnValue + iothVareboklinjeCol:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD('cFieldName'):BUFFER-VALUE + ','.
      qh:GET-NEXT(NO-LOCK).
    END.
    cReturnValue = TRIM(cReturnValue,',').
  
    qh:GET-BUFFER-HANDLE(1):BUFFER-RELEASE.
    qh:QUERY-CLOSE().
  END.
  DELETE OBJECT qh.
  RETURN cReturnValue.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getVareboklinjeFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getVareboklinjeFields Procedure 
FUNCTION getVareboklinjeFields RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR cReturnValue AS CHAR NO-UNDO.
  DEF VAR i            AS INT  NO-UNDO.

  DO i = 1 TO bhVareboklinje:NUM-FIELDS:
    cReturnValue = cReturnValue + bhVareboklinje:BUFFER-FIELD(i):NAME + ','.
  END.
  cReturnValue = TRIM(cReturnValue,',').
  RETURN cReturnValue.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-update_levuke) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION update_levuke Procedure 
FUNCTION update_levuke RETURNS CHARACTER
  ( INPUT irRowid AS ROWID ,
    INPUT icValue AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR iYear     AS INT  NO-UNDO.
DEF VAR iWeek     AS INT  NO-UNDO.
DEF VAR oiWeek    AS INT  NO-UNDO.
DEF VAR ix        AS INT  NO-UNDO.
DEF VAR dFirst    AS DATE NO-UNDO.
DEF VAR cRetValue AS CHAR NO-UNDO.

  IF INT(icValue) = 0 OR icValue = '' OR icValue = '0' THEN 
    cRetValue = ''.
  ELSE
  DO:
    ASSIGN 
      iYear = INT(SUBSTR(icValue,1,4)) 
      iWeek = INT(SUBSTR(icValue,5))
      NO-ERROR.

    IF iYear < YEAR(TODAY) OR iYear > YEAR(TODAY) + 10 OR iWeek < 1 OR iWeek > 53 THEN     
      cRetValue = 'Feltet levuke har feil verdi, må ha format YYYYWW hvor Y = år og W = uke'.
    ELSE
    DO:
      DO ix = 1 TO 5:  /*Testing for week 53*/    
        RUN weeknum.p (DATE(1,ix,INT(iYear)), OUTPUT oiWeek).
        IF INT(SUBST(STRING(oiWeek),5)) = 1 THEN LEAVE.
      END.
      dFirst = DATE(1,ix,iYear) + iWeek * 7 - 7.

      DO TRANS:
        FIND vareboklinje WHERE ROWID(vareboklinje) = irRowid EXCLUSIVE-LOCK NO-ERROR.
        IF AVAIL vareboklinje THEN vareboklinje.levDato1 = dFirst.
      END.
      FIND CURRENT vareboklinje NO-LOCK NO-ERROR.
    END.
  END.
  RETURN cRetValue.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-update_RavdBeskrivelse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION update_RavdBeskrivelse Procedure 
FUNCTION update_RavdBeskrivelse RETURNS CHARACTER
  ( INPUT irRowid AS ROWID,
    INPUT icValue AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND FIRST vareboklinje WHERE ROWID(vareboklinje) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL vareboklinje THEN 
  DO:
    FIND FIRST Regnskapsavdeling WHERE RegnskapsAvdeling.RavdBeskrivelse = icValue NO-LOCK NO-ERROR.
    IF AVAIL Regnskapsavdeling THEN 
    DO TRANS:
      FIND ArtBas EXCLUSIVE-LOCK WHERE ArtBas.ArtikkelNr = vareboklinje.artikkelnr NO-ERROR.
      IF AVAIL ArtBas AND regnskapsavdeling.ravdnr <> artbas.ravdnr THEN ArtBas.RAvdNr = regnskapsavdeling.ravdnr.
    END.
  END.
  RETURN ''.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-update_ravdnr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION update_ravdnr Procedure 
FUNCTION update_ravdnr RETURNS CHARACTER
  ( INPUT irRowid AS ROWID,
    INPUT icValue AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND FIRST vareboklinje WHERE ROWID(vareboklinje) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL vareboklinje THEN 
  DO TRANS:
    FIND ArtBas EXCLUSIVE-LOCK WHERE ArtBas.ArtikkelNr = vareboklinje.artikkelnr NO-ERROR.
    IF AVAIL ArtBas THEN ArtBas.RAvdNr = INT(icValue).
  END.
  RETURN ''.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

