/* Kopier artikkel 
   Parametere: Aksjon (new|edit|copy),artikkelnr,prisprofil
               temp-tabell med overstyrte verdier for ny artikkel
   
   Opprettet: 17.11.04 av BHa                  
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR cBrukerId AS CHAR NO-UNDO.

ASSIGN
    cBrukerId = ENTRY(1,icParam).

FOR EACH JBoxUserGroupMembers WHERE
    JBoxUserGroupMembers.cJBoxUserId = cBrukerId:
    DELETE JBoxUserGroupMembers.
END.
FOR EACH JBoxLoginSession WHERE
    JBoxLoginSession.cJBoxUserId = cBrukerId:
    DELETE JBoxLoginSession.
END.
FOR EACH _User WHERE
    _User._UserId = cBrukerId:
    DELETE _User.
END.
FOR EACH JBoxCompanyUser WHERE
    JBoxCompanyUser.cJBoxUserId = cBrukerId:
    DELETE JBoxCompanyUser.
END.

FOR EACH JBoxUser WHERE
    JBoxUser.cJBoxUserId = cBrukerId:
    DELETE JBoxUser.
END.

FOR EACH Bruker WHERE
    Bruker.BrukerId = cBrukerId:
    DELETE Bruker.
END.
FOR EACH BrukerLev WHERE
    BrukerLev.BrukerId = cBrukerId:
    DELETE BrukerLev.
END.

ASSIGN 
    obOk = TRUE
    ocReturn = ''
    .

