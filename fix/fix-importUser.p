CURRENT-WINDOW:WIDTH-CHARS = 150.

DEF TEMP-TABLE ttBruker
    FIELD cButNr AS CHARACTER 
    FIELD cEpost AS CHARACTER 
    FIELD iButnr AS INTEGER 
    .

DEF VAR cBrukerId   AS CHAR NO-UNDO.
DEF VAR cLng        AS CHAR NO-UNDO INIT "SE".
DEF VAR cLngList    AS CHAR NO-UNDO.
DEF VAR iButGrp     AS INT  NO-UNDO INIT 0.
DEF VAR cButGrpList AS CHAR NO-UNDO.
DEF VAR iBrukerType AS INT  NO-UNDO INIT 2.
DEF VAR iBrGrp      AS INT  NO-UNDO.
DEF VAR cBrGrpList  AS CHAR NO-UNDO.
DEF VAR cPwd        AS CHAR NO-UNDO INIT "1234".
DEF VAR cInputFile  AS CHAR NO-UNDO INIT "brukerliste.txt".

FOR EACH sprak NO-LOCK:
  cLngList = cLngList + (IF cLngList = "" THEN "Språkkode: " ELSE "/") 
           + sprak.Beskrivelse + " (" + sprak.Lng + ")"
           .
END.

FOR EACH BrukerGrp NO-LOCK:
  cButGrpList = cButGrpList + (IF cButGrpList = "" THEN "Butikkgrp: " ELSE "/")
              + BrukerGrp.Beskrivelse + " (" + STRING(BrukerGrp.BrGrpNr) + ")"
              .
END.

FOR EACH JBoxUserGroup NO-LOCK:
  cBrGrpList = cBrGrpList + (IF cBrGrpList = "" THEN "Brukergrp: " ELSE "/")
             + JBoxUserGroup.cUserGroupName + " (" + STRING(JBoxUserGroup.iJboxUserGroupId) + ")"
             .
END.

iBrGrp = 6.

DISP "Navn, importfil" FORMAT "X(30)" NO-LABEL cInputFile FORMAT "X(110)" NO-LABEL
                 VALIDATE(SEARCH(cInputFile) NE ?,"Ugyldig filnavn") SKIP
     cLngList    FORMAT "X(130)" NO-LABEL cLng  NO-LABEL 
                 VALIDATE(CAN-FIND(FIRST sprak WHERE sprak.Lng = cLng),"Ugyldig språk") SKIP
     "Brukertype: Kjede/System(1)/Butikk(2)/Leverandør (3)" FORMAT "X(130)" iBrukerType NO-LABEL
     cButGrpList FORMAT "X(130)" NO-LABEL iButGrp NO-LABEL
                 HELP "Angi 0 for å opprette ny grupper for hver butikk"
                 VALIDATE(CAN-FIND(FIRST BrukerGrp WHERE BrukerGrp.BrGrpNr = iButGrp) OR iButGrp = 0,"Ugyldig butikkgruppe") SKIP
     cBrGrpList  FORMAT "X(130)" NO-LABEL iBrGrp NO-LABEL 
                 VALIDATE(CAN-FIND(FIRST JBoxUserGroup WHERE JBoxUserGroup.iJboxUserGroupId = iBrGrp) OR iBrGrp = 0,"Ugyldig brukergruppe") SKIP
     "Standard passord" FORMAT "X(130)" NO-LABEL cPwd NO-LABEL
     WITH WIDTH 150 
       .
UPDATE cInputFile
       cLng
       iBrukerType
       /*iButGrp*/
       /*iBrGrp*/
       cPwd
       .

INPUT FROM VALUE(cInputFile).

REPEAT:
  CREATE ttBruker.
  IMPORT DELIMITER ";" ttBruker.
  ASSIGN
    ttBruker.iButNr = int(SUBSTRING(ttBruker.cButNr,1,3)).
  FIND FIRST Butiker NO-LOCK
       WHERE Butiker.Butik = ttBruker.iButnr
       NO-ERROR.
  IF NOT AVAIL Butiker THEN DO:
    DISP ttBruker.iButnr "finnes ikke i butikkregister"
         .
    DELETE ttBruker.
  END.
END.

INPUT CLOSE.

FOR EACH ttBruker WHERE ttBruker.iButnr > 0:
  cBrukerId = STRING(ttBruker.cButnr).

  FIND FIRST JBoxUser EXCLUSIVE-LOCK 
       WHERE JBoxUser.cJBoxUserId = cBrukerId
       NO-ERROR.
  IF AVAIL JBoxUser THEN
    DELETE JBoxUser.
       
  CREATE JBoxUser.
  ASSIGN JBoxUser.cJBoxUserId = cBrukerId
         JBoxUser.cUserName   = STRING(ttBruker.iButnr)
         JBoxUser.cEmail      = ttBruker.cEpost
         JBoxUser.dCreated    = TODAY
         JBoxUser.cCreatedBy  = "tomn"
         .

  FIND FIRST Bruker EXCLUSIVE-LOCK 
       WHERE Bruker.BrukerID = cBrukerId
       NO-ERROR.
  IF AVAIL Bruker THEN
    DELETE Bruker.

  FIND FIRST JBoxCompanyUser NO-LOCK
       WHERE JBoxCompanyUser.cJBoxUserId = cBrukerId
         AND JBoxCompanyUser.iJBoxCompanyId = 1
       NO-ERROR.
  IF NOT AVAIL JBoxCompanyUser THEN DO:
    CREATE JBoxCompanyUser.
    ASSIGN JBoxCompanyUser.iJBoxCompanyId = 1
           JBoxCompanyUser.cJBoxUserId    = cBrukerId
           .
  END.

  FOR EACH JBoxUserGroupMembers EXCLUSIVE-LOCK 
      WHERE JBoxUserGroupMembers.cJBoxUserId = cBrukerId
      :
    DELETE JBoxUserGroupMembers.
  END.
  IF iBrGrp > 0 THEN DO:
    CREATE JBoxUserGroupMembers.
    ASSIGN JBoxUserGroupMembers.iJboxUserGroupId = iBrGrp
           JBoxUserGroupMembers.cJBoxUserId = cBrukerId
           .
  END.

  IF NOT CAN-FIND(FIRST BrukerGrp 
                  WHERE BrukerGrp.BrGrpNr = ttBruker.iButnr) THEN DO:
    CREATE BrukerGrp.
    ASSIGN BrukerGrp.RegistrertDato = TODAY
           BrukerGrp.RegistrertAv   = "tomn"
           BrukerGrp.Beskrivelse    = STRING(ttBruker.iButnr)
           BrukerGrp.BrGrpNr        = ttBruker.iButnr
           .
  END.
  ELSE DO:
    FIND FIRST BrukerGrp NO-LOCK
         WHERE BrukerGrp.BrGrpNr = ttBruker.iButNr
         NO-ERROR.
    IF NOT AVAIL BrukerGrp THEN
      FIND FIRST BrukerGrp NO-LOCK.
  END.

  IF NOT CAN-FIND(ButikkTilgang WHERE
                  ButikkTilgang.BrGrpNr = ttBruker.iButNr AND
                  ButikkTilgang.Butik   = ttBruker.iButNr) THEN
  DO:
      CREATE ButikkTilgang.
      ASSIGN
          ButikkTilgang.BrGrpNr = ttBruker.iButNr
          ButikkTilgang.Butik   = ttBruker.iButNr.
  END.

  CREATE Bruker.
  ASSIGN Bruker.BrukerID       = cBrukerId
         Bruker.RegistrertDato = TODAY
         Bruker.RegistrertTid  = TIME 
         Bruker.Navn           = JBoxUser.cUserName
         Bruker.Lng            = cLng
         Bruker.BrGrpNr        = BrukerGrp.BrGrpNr
         Bruker.ButikkNr       = ttBruker.iButnr
         Bruker.BrukerType     = iBrukerType
         .

  FIND FIRST skotex._User EXCLUSIVE-LOCK 
       WHERE skotex._User._Userid = cBrukerId
       NO-ERROR.
  IF AVAIL skotex._User THEN 
    DELETE skotex._User.
       
  CREATE skotex._User.
  ASSIGN skotex._User._Userid      = cBrukerId
         skotex._User._User-Name   = JBoxUser.cUserName
         skotex._User._Create_date = NOW
         skotex._User._Password    = ENCODE(TRIM(ttBruker.cEpost))
         .


  IF NOT CAN-FIND(ButikkTeam WHERE 
                  ButikkTeam.BrGrpNr    = Bruker.BrGrpNr AND
                  Butikkteam.TeamTypeId = 2 AND 
                  ButikkTeam.TeamNr     = ttBruker.iButnr) THEN
  DO:
      CREATE ButikkTeam.
      ASSIGN
          ButikkTeam.BrGrpNr     = Bruker.BrGrpNr 
          Butikkteam.TeamTypeId  = 2  
          ButikkTeam.TeamNr      = ttBruker.iButnr
          ButikkTeam.Beskrivelse = 'Time ' + STRING(ttBruker.iButnr)
          .
      CREATE ButikkKobling.
      ASSIGN
          ButikkKobling.TeamTypeId  = 2  
          ButikkKobling.TeamNr      = ttBruker.iButnr
          ButikkKobling.Butik       = ttBruker.iButnr 
          ButikkKobling.BrGrpNr     = Bruker.BrGrpNr 
          .
  END.


END.
