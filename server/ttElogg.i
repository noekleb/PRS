
/*------------------------------------------------------------------------
    File        : ttElogg.i
    Purpose     : Felles definisjon i programmer som bruker temp-tabellen.  

    Syntax      :

    Description : Definisjon av ttElogg tabellen.

    Author(s)   : Tom Nøkleby
    Created     : Thu Nov 05 13:59:37 CET 2020 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttElogg NO-UNDO SERIALIZE-NAME 'elogg'
  FIELD EksterntSystem AS CHARACTER   FORMAT "X(20)" LABEL "Eksternt system"
  FIELD TabellNavn     AS CHARACTER   FORMAT "X(20)" LABEL "Tabellnavn"
  FIELD Verdier        AS CHARACTER   FORMAT "X(30)" LABEL "Verdier"
  FIELD EndringsType   AS INTEGER     FORMAT "9" INITIAL 1 LABEL "EndringsType"
  FIELD Behandlet      AS LOGICAL     LABEL "Behandlet"
  FIELD Opprettet      AS DECIMAL     DECIMALS 2 FORMAT ">>>>>>>>>>>>>>>9"
  FIELD RegistrertAv   AS CHARACTER   FORMAT "X(10)" LABEL "Registrert av" COLUMN-LABEL "Reg.Av"
  FIELD RegistrertDato AS DATE        FORMAT "99/99/9999" LABEL "Registrert dato" COLUMN-LABEL "RDato"
  FIELD RegistrertTid  AS INTEGER     LABEL "Registreringstidspunkt"
  FIELD BrukerID       AS CHARACTER   FORMAT "X(10)" LABEL "Bruker"
  FIELD EDato          AS DATE        FORMAT "99/99/9999" LABEL "Endret"
  FIELD ETid           AS INTEGER     LABEL "Endret tid" COLUMN-LABEL "ETid"
  .


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
