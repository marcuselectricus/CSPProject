There is a task.json file in the .vscode folder.  This contains instructions for VSCODE to build
a DLL and a task to test the DLL.

The DLL assumes there are 3 Environment Variables
  1) CSP_DIR - This point to a Directory.  The contents of the directory should match the CSP directory
               in this project.  (e.g. TZINFO directory and the leap-seconds.list file)
               I normally create a directory at a top level (e.g. C:\CSP or D:\CSP)
               In that case the CSP_DIR environment variable would be set to "C:\CSP" or "D:\CSP"
               
  2) JPL_DIR - This points to a directory that contains the JPLEPH file. This is normally set to
               "C:\JPL" or "D:\JPL" and the JPLEPH file resides there.
               
  3) TZ_DIR  - This points to a directory where the TZinfo files are contained.  I normally make this
               a subdirectory under the CSP_DIR, TZINFO.  (e.g. C:\CSP\TZINFO  or D:\CSP\TZINFO
               There are 9 files in that directory:
               africa.txt, antarctica.txt, asia.txt, australasia.txt, europe.txt, northamerica.txt,
               southamerica.txt, default.txt.  These are used to format local time.
               
One the DLL builds there is a TestJPLEphemeris task that you can run to make sure that the SUN_POSITION 
function which uses the JPL Ephemeris DE431.