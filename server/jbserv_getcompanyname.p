DEF INPUT  PARAM icSessionId       AS CHAR NO-UNDO.
DEF INPUT  PARAM iiJBoxCompanyId   AS INT  NO-UNDO.
DEF OUTPUT PARAM ocJBoxCompanyName AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocCodeMaster      AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocParentCompany   AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocCompanyLogo     AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocParam           AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOk              AS LOG  NO-UNDO.

DEF VAR ocReturn          AS CHAR   NO-UNDO.
DEF VAR hCompToCompBuffer AS HANDLE NO-UNDO.
DEF VAR hCompToCompQuery  AS HANDLE NO-UNDO.
DEF VAR hLogoField        AS HANDLE NO-UNDO.
DEF VAR hBuffGenCode      AS HANDLE NO-UNDO.
DEF VAR cCompRole         AS CHAR   NO-UNDO.

{incl/validatesession.i}

obOk = YES.

/*IF cCurrUserId NE "" AND icSessionId NE "validsession" THEN DO:                                                                     */
/*  FIND FIRST JBoxUser NO-LOCK                                                                                                       */
/*       WHERE JBoxUser.cJBoxUserId = cCurrUserId                                                                                     */
/*       NO-ERROR.                                                                                                                    */
/*                                                                                                                                    */
/*  IF NOT AVAIL JBoxUser THEN RETURN.                                                                                                */
/*                                                                                                                                    */
/*  IF NOT JBoxUser.bSuperUser THEN DO:                                                                                               */
/*    FIND FIRST JBoxCompanyUser                                                                                                      */
/*         WHERE JBoxCompanyUser.cJBoxUserId    = cCurrUserId                                                                         */
/*           AND JBoxCompanyUser.iJBoxCompanyId = iiJBoxCompanyId                                                                     */
/*         NO-ERROR.                                                                                                                  */
/*    IF NOT AVAIL JBoxCompanyUser THEN RETURN.                                                                                       */
/*  END.                                                                                                                              */
/*END.                                                                                                                                */
/*                                                                                                                                    */
/*FIND JBoxCompany WHERE JBoxCompany.iJBoxCompanyId = iiJBoxCompanyId NO-LOCK NO-ERROR.                                               */
/*IF NOT icSessionId = "validsession" AND AVAIL JBoxCompany THEN DO:                                                                  */
/*  ocJBoxCompanyName = JBoxCompany.cCompanyName.                                                                                     */
/*  FIND JBoxLoginSession WHERE JBoxLoginSession.cSessionId = icSessionId EXCLUSIVE-LOCK NO-ERROR.                                    */
/*  IF AVAIL JBoxLoginSession THEN                                                                                                    */
/*    ASSIGN JBoxLoginSession.iJBoxCompanyId = JBoxCompany.iJBoxCompanyId                                                             */
/*           obOk                            = YES                                                                                    */
/*           .                                                                                                                        */
/*END.                                                                                                                                */
/*ELSE IF icSessionId = "validsession" AND AVAIL JBoxCompany THEN                                                                     */
/*  ASSIGN ocJBoxCompanyName = JBoxCompany.cCompanyName                                                                               */
/*         obOk              = YES.                                                                                                   */
/*ELSE IF icSessionId = "validsession" AND NOT AVAIL JBoxCompany THEN                                                                 */
/*        obOk = YES.                                                                                                                 */
/*                                                                                                                                    */
/*IF obOk THEN DO:                                                                                                                    */
/*  CREATE BUFFER hCompToCompBuffer FOR TABLE "JBoxCompanyToCompany" NO-ERROR.                                                        */
/*  IF NOT ERROR-STATUS:ERROR THEN DO:                                                                                                */
/*    CREATE QUERY hCompToCompQuery.                                                                                                  */
/*    hCompToCompQuery:SET-BUFFERS(hCompToCompBuffer).                                                                                */
/*    hCompToCompQuery:QUERY-PREPARE("FOR EACH JBoxCompanyToCompany NO-LOCK WHERE JBoxCompanyToCompany.iJBoxCompanyId = "             */
/*                                                                              + STRING(JBoxCompany.iJBoxCompanyId)) NO-ERROR.       */
/*    IF NOT ERROR-STATUS:ERROR THEN DO:                                                                                              */
/*      hCompToCompQuery:QUERY-OPEN() NO-ERROR.                                                                                       */
/*      IF NOT ERROR-STATUS:ERROR THEN DO:                                                                                            */
/*        hCompToCompQuery:GET-FIRST().                                                                                               */
/*        REPEAT WHILE NOT hCompToCompQuery:QUERY-OFF-END:                                                                            */
/*          cCompRole = hCompToCompBuffer:BUFFER-FIELD("cCompanyRole"):BUFFER-VALUE.                                                  */
/*          CASE cCompRole:                                                                                                           */
/*            WHEN "parent" THEN                                                                                                      */
/*              ocParentCompany = STRING(hCompToCompBuffer:BUFFER-FIELD("iJBoxToCompanyId"):BUFFER-VALUE).                            */
/*            WHEN "codemaster" THEN                                                                                                  */
/*              ocCodeMaster = STRING(hCompToCompBuffer:BUFFER-FIELD("iJBoxToCompanyId"):BUFFER-VALUE).                               */
/*          END CASE.                                                                                                                 */
/*          hCompToCompQuery:GET-NEXT().                                                                                              */
/*        END.                                                                                                                        */
/*      END.                                                                                                                          */
/*    END.                                                                                                                            */
/*    DELETE OBJECT hCompToCompQuery NO-ERROR.                                                                                        */
/*    DELETE OBJECT hCompToCompBuffer NO-ERROR.                                                                                       */
/*  END.                                                                                                                              */
/*  IF AVAIL JBoxCompany THEN DO:                                                                                                     */
/*    hLogoField = BUFFER JBoxCompany:HANDLE:BUFFER-FIELD("cCompanyLogo") NO-ERROR.                                                   */
/*    IF VALID-HANDLE(hLogoField) AND hLogoField:BUFFER-VALUE NE "" THEN                                                              */
/*      ocCompanyLogo = hLogoField:BUFFER-VALUE.                                                                                      */
/*    ELSE IF VALID-HANDLE(hLogoField) THEN DO:                                                                                       */
/*      IF ocParentCompany NE "" THEN DO:                                                                                             */
/*        FIND FIRST JBoxCompany NO-LOCK                                                                                              */
/*             WHERE JBoxCompany.iJBoxCompanyId = INT(ocParentCompany)                                                                */
/*             NO-ERROR.                                                                                                              */
/*        IF AVAIL JBoxCompany THEN                                                                                                   */
/*          ocCompanyLogo = hLogoField:BUFFER-VALUE.                                                                                  */
/*      END.                                                                                                                          */
/*      IF ocCompanyLogo = "" AND ocCodeMaster NE "" THEN DO:                                                                         */
/*        FIND FIRST JBoxCompany NO-LOCK                                                                                              */
/*             WHERE JBoxCompany.iJBoxCompanyId = INT(ocCodeMaster)                                                                   */
/*             NO-ERROR.                                                                                                              */
/*        IF AVAIL JBoxCompany THEN                                                                                                   */
/*          ocCompanyLogo = hLogoField:BUFFER-VALUE.                                                                                  */
/*      END.                                                                                                                          */
/*    END.                                                                                                                            */
/*    IF ocCompanyLogo = "" THEN DO:                                                                                                  */
/*      CREATE BUFFER hBuffGenCode FOR TABLE "JBoxGenCode" NO-ERROR.                                                                  */
/*      IF NOT ERROR-STATUS:ERROR THEN DO:                                                                                            */
/*        hBuffGenCode:FIND-FIRST("WHERE cCodeType = 'companyLogo' AND iJBoxCompanyId = " + STRING(iiJBoxCompanyId),NO-LOCK) NO-ERROR.*/
/*        IF hBuffGenCode:AVAILABLE THEN                                                                                              */
/*          ocCompanyLogo = hBuffGenCode:BUFFER-FIELD("cCodeValue"):BUFFER-VALUE.                                                     */
/*        IF ocCompanyLogo = "" AND ocParentCompany NE "" THEN DO:                                                                    */
/*          hBuffGenCode:FIND-FIRST("WHERE cCodeType = 'companyLogo' AND iJBoxCompanyId = " + ocParentCompany,NO-LOCK) NO-ERROR.      */
/*          IF hBuffGenCode:AVAILABLE THEN                                                                                            */
/*            ocCompanyLogo = hBuffGenCode:BUFFER-FIELD("cCodeValue"):BUFFER-VALUE.                                                   */
/*        END.                                                                                                                        */
/*        IF ocCompanyLogo = "" AND ocCodeMaster NE "" THEN DO:                                                                       */
/*          hBuffGenCode:FIND-FIRST("WHERE cCodeType = 'companyLogo' AND iJBoxCompanyId = " + ocCodeMaster,NO-LOCK) NO-ERROR.         */
/*          IF hBuffGenCode:AVAILABLE THEN                                                                                            */
/*            ocCompanyLogo = hBuffGenCode:BUFFER-FIELD("cCodeValue"):BUFFER-VALUE.                                                   */
/*        END.                                                                                                                        */
/*        DELETE OBJECT hBuffGenCode NO-ERROR.                                                                                        */
/*      END.                                                                                                                          */
/*    END.                                                                                                                            */
/*  END.                                                                                                                              */
/*END.                                                                                                                                */
