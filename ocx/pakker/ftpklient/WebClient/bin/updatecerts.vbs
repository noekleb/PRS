
' FILE: updatecerts.vbs
'  This is a VB script utility run from the mkhashfile.bat batch 
'  command.  It's purpose is to execute the RSA sslc command to 
'  generate a hash value from a Certificate PEM file. The hash
'  value is used to create a copy of the PEM file with that
'  hashvalue as the file name.  This is necessary for support
'  of certificate issuer validation within the Progress 4GL client.
'  
'  Argument: 
'    first:  Name of PEM-format certificate file (i.e. test.pem)
'    second: temporary file name to direct the sslc cmd output


Dim fso, ts, f1, s, objArgs, sCertFile, sCertTarget, sHashTarget, hashFile
Dim WshShell, WshSysEnv, dlc, Return, sslcCmd
Dim length, position, BackSlash, baseName, found
Const forReading = 1

   BackSlash = "\"

' get cert filename and temp filename from command line
Set objArgs = Wscript.Arguments
sCertFile = objArgs(0)  
hashFile = objArgs(1)

' Strip any path info from the filename 
' Find the last occurance of a backslash
position = InStrRev(sCertFile, BackSlash, -1, vbTextCompare)
length = Len(sCertFile)
baseName = Mid(sCertFile, position + 1, length - position)

' WScript.Echo "baseName " & baseName 
' WScript.Echo "certfile " & sCertFile 
' WScript.Echo "hashFile " & hashFile

' get DLC env var and get a full path to target PEM file
Set WshShell = WScript.CreateObject("WScript.Shell")
dlc = WshShell.ExpandEnvironmentStrings("%DLC%")
sCertTarget = dlc & "\certs\" & baseName
comSpec = WshShell.ExpandEnvironmentStrings("%comspec%")

' WScript.Echo "DLC is " & dlc
' WScript.Echo "full path to cert file " & sCertTarget
' WScript.Echo "Command Processor we use is " & comSpec


' Run the "sslc" command, a RSA certificate utility. We're using the X509
' command option to generate the hash value
sslcCmd = dlc & "\bin\sslc" 
' WScript.Echo "full path to sslc file " & sslcCmd
WScript.Echo "Running SSLC command ..."
Return = WshShell.Run(comSpec & " /C """ & sslcCmd & """ x509 -in " & sCertFile & " -hash -noout > " & hashfile, 0, TRUE)
If Return <> 0 Then
  WScript.Echo "Error running sslc command, unable to create hash file"
Else
  ' Read the contents of the temp filename. Running the sslc command
  ' will create this file which contains the hash value. Use it to 
  ' create a duplicate of the PEM file
  Set fso = CreateObject("Scripting.FileSystemObject")
  Set ts = fso.OpenTextFile(hashFile, forReading)
  s = ts.ReadLine
  ts.Close
  s = s & ".0"
  sHashTarget = dlc & "\certs\" & s 
  ' WScript.Echo "Target Hash FileName = " &  sHashTarget
  
  ' Copy user specified PEM file and it's hashed copy to the DLC\certs directory
  WScript.Echo "Copying " & sCertFile & " and " & s & " to " & dlc & "\certs"
  Set f1 = fso.GetFile(sCertFile)
  f1.Copy(sCertTarget)
  f1.Copy(sHashTarget)
End If


