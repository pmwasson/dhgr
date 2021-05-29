::---------------------------------------------------------------------------
:: Compile code
::---------------------------------------------------------------------------
cd ..\build
:: First to create disassembly file
ca65 -I ..\src -t apple2 ..\src\dhgr.asm -l dhgr.dis
:: Second time to produce object
cl65 -I ..\src -t apple2 -u __EXEHDR__ ..\src\dhgr.asm apple2.lib  -o dhgr.apple2 -C ..\src\start4000.cfg

::---------------------------------------------------------------------------
:: Compile example
::---------------------------------------------------------------------------
cl65 -I ..\src -t apple2 -u __EXEHDR__ ..\src\tileset0.asm apple2.lib  -o tileset0.apple2 -C ..\src\start4000.cfg
cl65 -I ..\src -t apple2 -u __EXEHDR__ ..\src\tileset1.asm apple2.lib  -o tileset1.apple2 -C ..\src\start4000.cfg

::---------------------------------------------------------------------------
:: Build disk 
::---------------------------------------------------------------------------

:: Start with a blank prodos disk
copy ..\disk\template_prodos.dsk dhgr_prodos.dsk

:: Add the loader, which will load the program and execute it
java -jar C:\jar\AppleCommander.jar -p  dhgr_prodos.dsk dhgr.system sys < C:\cc65\target\apple2\util\loader.system

:: Then include the program
java -jar C:\jar\AppleCommander.jar -as dhgr_prodos.dsk dhgr bin < dhgr.apple2 

:: Add samples
java -jar C:\jar\AppleCommander.jar -as dhgr_prodos.dsk tileset0 bin < tileset0.apple2 
java -jar C:\jar\AppleCommander.jar -as dhgr_prodos.dsk tileset1 bin < tileset1.apple2 

:: Throw on basic and other games for fun
java -jar C:\jar\AppleCommander.jar -p dhgr_prodos.dsk basic.system sys < ..\disk\BASIC.SYSTEM 
java -jar C:\jar\AppleCommander.jar -as dhgr_prodos.dsk askey bin < ..\disk\askey.apple2 
java -jar C:\jar\AppleCommander.jar -as dhgr_prodos.dsk aa bin < ..\disk\aa.apple2 

:: Copy results out of the build directory
copy dhgr_prodos.dsk ..\disk

::---------------------------------------------------------------------------
:: Test on emulator
::---------------------------------------------------------------------------
C:\AppleWin\Applewin.exe -no-printscreen-dlg -d1 dhgr_prodos.dsk

