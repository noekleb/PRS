/*
    File        : start.w
    Purpose     : Oppstart av SkoTex

    Syntax      : start.w

    Description : Rutinen utf›rer f›lgende:
                    1. Oppstart valg av database og innloggingskontroll.
                    2. Oppstart av prosedurebiblotek.
                    3. Oppstart av ModulMeny - Hovedmeny som benyttes av bruker.
                    4. Nedkj›ring av systemet. Rydder opp og sletter alle
                       aktive (Peristente programmer) f›r sesjonen avsluttes.
                       

    Author(s)   : Tom N›kleby
    Created     : 11/6-98
    Notes       :

        TN  8/3-99      Splittet rutinen i to for å ungå at databasene må være oppkoblet.

	Last change:  TN   22 Sep 99   10:09 am
*/

def var wAppDBpf       as char no-undo.
def var wRappDBpf      as char no-undo.
def var wDataDBpf      as char no-undo.
DEF VAR wVpiDBpf       AS CHAR NO-UNDO.
DEF VAR wOk            AS CHAR INITIAL 'OK' NO-UNDO.
DEF VAR wProsjektListe AS CHAR NO-UNDO.
DEF VAR iCount         AS INT  NO-UNDO.

&Scoped-define APP-DB  SkoTex
&Scoped-define RAPP-DB WR
&Scoped-define DATA-DB Data
&Scoped-define VPI-DB  VPI

DEF VAR cButikkListe AS CHAR NO-UNDO.
DEF VAR cBrukerId    AS CHAR NO-UNDO.
def var wWindows     as handle no-undo.

{adecomm/appserv.i}

/* Definerer variabler som benyttes av gamle SkoTex programmer. */
/*{syscom.i " " new global}.*/

/* Henter oppkoblingsinfo fra oppstartsicon. */
IF SESSION:PARAMETER <> "" THEN DO iCount = 1 TO NUM-ENTRIES(SESSION:PARAMETER):
    IF ENTRY(iCount,SESSION:PARAMETER) BEGINS "BRUKER" AND 
       NUM-ENTRIES(ENTRY(iCount,SESSION:PARAMETER),"=") = 2 THEN DO:
       ASSIGN cBrukerId = ENTRY(2,ENTRY(iCount,SESSION:PARAMETER),"=").
    END.
    IF ENTRY(iCount,SESSION:PARAMETER) BEGINS "AUTODB" AND 
       NUM-ENTRIES(ENTRY(iCount,SESSION:PARAMETER),"=") = 2 THEN DO:
       ASSIGN wProsjektListe = ENTRY(2,ENTRY(iCount,SESSION:PARAMETER),"=").
    END.
END.

/* RUN logindb.p ('c:\db\\skotex.pf;c:\db\\wr.pf;c:\db\\data.pf;c:\db\\vpi.pf'). /* Logger inn i applikasjonen. */ */
ASSIGN
    wProsjektListe = IF wProsjektListe = ""
                       THEN 'c:\db\skotex.pf;c:\db\wr.pf;c:\db\data.pf;c:\db\vpi.pf'
                       ELSE wProsjektListe
    wAppDBpf       = entry(1,wProsjektListe,";")
    wRappDBpf      = entry(2,wProsjektListe,";")
    wDataDBpf      = entry(3,wProsjektListe,";")
    wVpiDBpf       = entry(4,wProsjektListe,";")
    .  

/* MESSAGE                                 */
/* program-name(1)                         */
/* SKIP cBrukerId SKIP wProsjektListe SKIP */
/*     wAppDBpf       SKIP                 */
/*     wRappDBpf          SKIP             */
/*     wDataDBpf              SKIP         */
/*     wVpiDBpf                            */
/* VIEW-AS ALERT-BOX INFO BUTTONS OK.      */
/*                                         */

RUN Oppkobling.

/* MESSAGE                                                      */
/*     CONNECTED("{&APP-DB}") "{&APP-DB}" userid('dictdb') SKIP */
/*     CONNECTED("{&VPI-DB}") "{&VPI-DB}" SKIP                  */
/*     CONNECTED("{&DATA-DB}") "{&DATA-DB}" SKIP                */
/*     CONNECTED("{&RAPP-DB}") "{&RAPP-DB}" SKIP                */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK.                       */

RUN keyfunk.p. /* Setter opp tangentbordet. */

/* Initiering av JukeBox */
RUN initjukebox.p. 

/* Starter bibliotek for API manuelt. */
RUN VALUE("windows.p") PERSISTENT SET wWindows.
IF VALID-HANDLE(wWindows) then 
  DELETE PROCEDURE wWindows.

IF cBrukerId = "" THEN
    cBrukerId = os-getenv('username').

IF cBrukerID = "" THEN
DO:
    MESSAGE 
    "Brukerid er blankt." 
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
    QUIT.
END.

IF CONNECTED ('SkoTex') THEN
DO:
    IF NOT SETUSERID ( cBrukerId, '', 'SkoTex') THEN 
    do: 
        MESSAGE 
        "Klarte ikke å logge inn bruker " cBrukerId "." SKIP
        "Bruker må finnes med blankt passord."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
        QUIT.
    END.
END.
ELSE DO:
    MESSAGE 
    "Databaser ikke oppkoblet"      
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
    QUIT.
END.

DYNAMIC-FUNCTIO("setASuserId",cBrukerId,cBrukerId).
RUN setbutikkliste.p (INPUT cBrukerId, OUTPUT cButikkListe).

/* Lagring av parameterverdi i ttObject. Med handle = session:handle. */
/* Da kan alle programmer finne parameteren og dens verdi når de vet  */
/* hva den heter.                                                     */
DYNAMIC-FUNCTION('newobject',SESSION,SESSION,'sessionparam').
DYNAMIC-FUNCTION('setattribute',SESSION,'ButikkListe',cButikkListe).

RUN w-velgvarebok.w.
CASE RETURN-VALUE:
    WHEN "MESSE" THEN
        RUN START2.p ('varebehhode.w'). /* Starter oppkjøring av messeregistrering. */
    WHEN "SUPPLERING" THEN
        RUN START2.p ('nyvarebehforh.w'). /* Starter oppkjøring av suppleringsordre. */
    OTHERWISE
        QUIT.
END CASE.

PROCEDURE WinUserName :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  define output parameter wName as char.
                                                                     
  def var nr as integer no-undo initial 100.
  def var ReturnValue as integer no-undo.
  wName = fill(" ", nr).
  run GetUserNameA in wWindows (input-output wName,
                                input-output nr,
                                output ReturnValue).
END PROCEDURE.

PROCEDURE Oppkobling :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* Kobler opp applikasjonsdatabasen databasen */
IF NOT CONNECTED("{&APP-DB}") then
  do:
    connect -pf value(wAppDBpf) no-error.
    /* Sjekker at databasen er koblet opp. */
    IF NOT CONNECTED("{&APP-DB}") then
      DO:
        MESSAGE "** Klarte ikke å koble opp (Applik) - {&APP-DB} databasen! **" skip
                "Applikasjonen avsluttes." VIEW-AS ALERT-BOX.
        QUIT.
      END.
end.

/* Kobler opp rapportdatabasen */
IF NOT CONNECTED("{&RAPP-DB}") then
  do:
    connect -pf value(wRappDBpf) no-error.
    /* Sjekker at databasen er koblet opp. */
    IF NOT CONNECTED("{&RAPP-DB}") then
      DO:
        MESSAGE "** Klarte ikke å koble opp (Rapport) - {&RAPP-DB} databasen! **" skip
                "Applikasjonen avsluttes." VIEW-AS ALERT-BOX.
        QUIT.
      END.
end.

/* Kobler opp Data databasen */
IF wDataDBpf <> "" THEN
DO:
  IF NOT CONNECTED("{&DATA-DB}") then
  do:
    connect -pf value(wDataDBpf) no-error.
    /* Sjekker at databasen er koblet opp. */
    IF NOT CONNECTED("{&DATA-DB}") then
      DO:
        MESSAGE "** Klarte ikke å koble opp (Data) - {&DATA-DB} databasen! **" skip
                "Applikasjonen avsluttes." VIEW-AS ALERT-BOX.
        QUIT.
      END.
  END.
end.

/* Kobler opp VPI databasen */
IF wVPIDBpf <> "" THEN
DO:
  IF NOT CONNECTED("{&VPI-DB}") then
  do:
    connect -pf value(wVPIDBpf) no-error.

    /* Sjekker at databasen er koblet opp. */
    IF NOT CONNECTED("{&VPI-DB}") then
      DO:
        MESSAGE "** Klarte ikke å koble opp (VPI) - {&VPI-DB} databasen! **" skip
                "Pf fil: " wVPIDBpf
                "Applikasjonen avsluttes." VIEW-AS ALERT-BOX.
        QUIT.
      END.
  END.
end.


END PROCEDURE.
