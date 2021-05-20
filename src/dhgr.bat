cd ..\build
ca65 -I ..\src -t apple2 ..\src\dhgr.asm -l dhgr.dis
cl65 -I ..\src -t apple2 -u __EXEHDR__ ..\src\dhgr.asm apple2.lib  -o dhgr.apple2 -C ..\src\start0C00.cfg

copy ..\disk\template_prodos.dsk dhgr_prodos.dsk
java -jar C:\jar\AppleCommander.jar -p  dhgr_prodos.dsk dhgr.system sys < C:\cc65\target\apple2\util\loader.system
java -jar C:\jar\AppleCommander.jar -as dhgr_prodos.dsk dhgr bin < dhgr.apple2 
copy dhgr_prodos.dsk ..\disk

C:\AppleWin\Applewin.exe -no-printscreen-dlg -d1 dhgr_prodos.dsk

