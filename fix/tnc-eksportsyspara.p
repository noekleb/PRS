OUTPUT TO VALUE("syspara.d").
FOR EACH SysPAra WHERE SysPAra.syshid = 6 AND
SysPAra.SysGr = 250:

  EXPORT Syspara.

END.
OUTPUT CLOSE.

OUTPUT TO VALUE("syspara.d") APPEND.
FOR EACH SysPAra WHERE SysPAra.syshid = 9 AND
SysPAra.SysGr = 250:

  EXPORT Syspara.

END.

OUTPUT CLOSE.
