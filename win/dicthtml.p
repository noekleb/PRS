/************************************************************
    Program:  dicthtml.p
    Created:  TN   31 Oct 98
Description:

Last change:  TN    6 Dec 1998    8:46 pm
************************************************************/

define variable d_name  as character no-undo.
define variable d_title as character no-undo initial "SkoTex database".
define variable d_dir   as character no-undo initial "utskrift".
define variable d_web   as character no-undo initial "host.name".
define variable i as integer no-undo.

DEF VAR wFilNavn as CHAR initial "dict.html" NO-UNDO.

DEF VAR wKatalog as CHAR NO-UNDO.

{sww.i}

RUN HentKatalog.

output to value(wKatalog + wFilNavn ).

put unformatted "<html>" skip.
put unformatted "<head>" skip.
put unformatted "<title>" + d_title + "</title><P>" skip.
put unformatted "<head/>" skip.
put unformatted "<body>" skip.
put unformatted "<hr>" skip.
put unformatted "<h1>" + d_title + "</h1>" skip.
put unformatted "<hr>" skip.
put unformatted "<p>" skip.
put unformatted "<table>" skip.

for each _file where _file._file-name < "_" no-lock:
  i = i + 1.

  if i modulo 5 = 1
    then put unformatted "<tr>" skip.

  /** replace any # (hashes) with special encoding for URL **/
  d_name = _file._dump-name.
  do while index( d_name, "#" ) > 0:
    substring( d_name, index( d_name, "#" ), 1 ) = "%23".
  end.

  put unformatted '<td><a href='d_name +
                  '.html>' + _file._file-name +
                  '    </td>' skip.

  if i modulo 5 = 0
    then put unformatted "</tr>" skip.
end.

put unformatted "</table>" skip.
put unformatted "</p>" skip.
put unformatted "<hr>" skip.
put unformatted "<i>Last updated " + string( today, "99/99/9999" ) + " " + string( time, "hh:mm:ss am" ) + "</i>" skip.
put unformatted "</body>" skip.
put unformatted "</html>" skip.

output close.

for each _file where _file._file-name < "_" no-lock:
  output to value(wKatalog + _file._dump-name + ".html" ).
  put unformatted "<html>" skip.
  put unformatted "<head>" skip.
  put unformatted "<title>" + d_title + " -- " + _file._file-name + " </title><P>" skip.
  put unformatted "<head/>" skip.
  put unformatted "<body>" skip.
  put unformatted "<hr>" skip.
  put unformatted "<h2>" + d_title + "</h2>" skip.
  put unformatted "<hr>" skip.
  put unformatted "<h2>" + _file._file-name + "</h2>" skip.
  put unformatted "<p>" + _file._desc + "</p>" skip.
  put unformatted "<hr>" skip.
  put unformatted "<table>" skip.
  put unformatted "<p>" skip.
  put unformatted "<tr>" skip.
  put unformatted "<th align=left>Field Name</th>" skip.
  put unformatted "<th align=left>Datatype</th>" skip.
  put unformatted "<th align=right>Format</th>" skip.
  put unformatted "<th align=left>Extent</th>" skip.
  put unformatted "<th align=left>Default</th>" skip.
  put unformatted "<th align=left>Description</th>" skip.
  put unformatted "</tr>" skip.
  for each _field  where _field._file-recid = recid( _file ) no-lock by _order :
    put unformatted "<tr>" skip.
    put unformatted "<td>" + _field._field-name + "    </td>" skip.
    put unformatted "<td>" + _field._data-type  + "    </td>" skip.
    put unformatted "<td align=right>" + _field._format     + "    </td>" skip.
    if _field._extent > 0 then put unformatted "<td align=right>" _field._extent "</td>" skip. 
    else
    do:
      put unformatted "<td align=right>1</td>" skip.
    end.

    put unformatted "<td>" + _field._initial + "</td>" skip.
    put unformatted "<td>" + _field._desc + "</td>" skip.
    put unformatted "</tr>" skip.
  end.
  put unformatted "</table>" skip.
  put unformatted "</p>" skip.
  put unformatted "<hr>" skip.
  put unformatted "<p>" skip.

  for each _index where _index._file-recid = recid( _file ) no-lock:
    put unformatted "<b>" + _index._index-name + " </b>" skip.
    if _file._prime-index = recid( _index ) then put unformatted "<i>Primary </i>" skip.
    if _index._unique then put unformatted "<i>Unique </i>" skip.
    for each _index-field where _index-field._index-recid = recid( _index ) no-lock:
      find _field where recid( _field ) = _index-field._field-recid no-lock.
      put unformatted _field._field-name + " " skip.
    end.
    put unformatted "<br>" skip.
  end.
  put unformatted "</p>" skip.
  put unformatted "<hr>" skip.
  put unformatted "<i>Last updated " + string( today, "99/99/9999" ) + " " + string( time, "hh:mm:ss am" ) + "</i>" skip.
  put unformatted "<p>" skip.
  put unformatted '<td><a href="' + wFilNavn + '"> Return to Listing Of Database Tables </td>' skip.
  put unformatted "</body>" skip.
  put unformatted "</html>" skip.
  output close.
end.

{swn.i}

MESSAGE "Startprogram:  start.exe " wKatalog + wFilNavn VIEW-AS ALERT-BOX.

if SEARCH(wKatalog + wFilNavn) <> ?
  THEN OS-COMMAND NO-WAIT VALUE("start.exe") VALUE(wKatalog + wFilNavn).

RETURN.

PROCEDURE HentKatalog:
  DEF VAR wTekst as CHAR NO-UNDO.

  {syspara.i 1 1 9 wTekst}
  ASSIGN wKatalog = wTekst.
  {syspara.i 1 1 7 wTekst}
  ASSIGN wKatalog = wKatalog + wTekst.
  if substring(wKatalog, LENGTH(wKatalog),1) <> "\"
    THEN wKatalog = wKatalog + "\".
  {syspara.i 1 1 8 wTekst}
  ASSIGN wKatalog = wKatalog + wTekst.
  if substring(wKatalog, LENGTH(wKatalog),1) <> "\"
    THEN wKatalog = wKatalog + "\".
END PROCEDURE.
