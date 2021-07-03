::---------------------------------------------------------------------------
:: Compile code
::   Assemble twice: 1 to generate listing, 2 to generate object
::---------------------------------------------------------------------------
cd ..\build

:: Toolbox
ca65 -I ..\src -t apple2 ..\src\dhgr.asm -l dhgr.dis
cl65 -I ..\src -t apple2 -u __EXEHDR__ ..\src\dhgr.asm apple2.lib  -o dhgr.apple2 -C ..\src\start4000.cfg

:: Compile Play
ca65 -I ..\src -t apple2 ..\src\play.asm -l play.dis
cl65 -I ..\src -t apple2 -u __EXEHDR__ ..\src\play.asm apple2.lib  -o play.apple2 -C ..\src\start6000.cfg

:: Compile Engine
ca65 -I ..\src -t apple2 ..\src\engine.asm -l engine.dis
cl65 -I ..\src -t apple2 -u __EXEHDR__ ..\src\engine.asm apple2.lib  -o engine.apple2 -C ..\src\startC00.cfg

:: Compile Game
ca65 -I ..\src -t apple2 ..\src\game.asm -l game.dis
cl65 -I ..\src -t apple2 -u __EXEHDR__ ..\src\game.asm apple2.lib  -o game.apple2 -C ..\src\start6000.cfg

::---------------------------------------------------------------------------
:: Compile example
::---------------------------------------------------------------------------
cl65 -I ..\src -t apple2 -u __EXEHDR__ ..\src\tileset0.asm apple2.lib  -o tileset0.apple2 -C ..\src\start6000.cfg
cl65 -I ..\src -t apple2 -u __EXEHDR__ ..\src\tileset1.asm apple2.lib  -o tileset1.apple2 -C ..\src\start6000.cfg
cl65 -I ..\src -t apple2 -u __EXEHDR__ ..\src\map0.asm     apple2.lib  -o map0.apple2     -C ..\src\start6000.cfg

::---------------------------------------------------------------------------
:: Build disk 
::---------------------------------------------------------------------------

:: Start with a blank prodos disk
copy ..\disk\template_prodos.dsk dhgr_prodos.dsk

:: Put boot program first

:: Play
java -jar C:\jar\AppleCommander.jar -p  dhgr_prodos.dsk game.system sys < C:\cc65\target\apple2\util\loader.system
java -jar C:\jar\AppleCommander.jar -as dhgr_prodos.dsk game bin < game.apple2 

:: Toolbox
java -jar C:\jar\AppleCommander.jar -p  dhgr_prodos.dsk dhgr.system sys < C:\cc65\target\apple2\util\loader.system
java -jar C:\jar\AppleCommander.jar -as dhgr_prodos.dsk dhgr bin < dhgr.apple2 

:: Engine
java -jar C:\jar\AppleCommander.jar -p  dhgr_prodos.dsk engine.system sys < C:\cc65\target\apple2\util\loader.system
java -jar C:\jar\AppleCommander.jar -as dhgr_prodos.dsk engine bin < engine.apple2 

:: Play
java -jar C:\jar\AppleCommander.jar -p  dhgr_prodos.dsk play.system sys < C:\cc65\target\apple2\util\loader.system
java -jar C:\jar\AppleCommander.jar -as dhgr_prodos.dsk play bin < play.apple2 

:: Throw on basic
java -jar C:\jar\AppleCommander.jar -p dhgr_prodos.dsk basic.system sys < ..\disk\BASIC.SYSTEM 

:: Add samples
java -jar C:\jar\AppleCommander.jar -as dhgr_prodos.dsk tileset0 bin < tileset0.apple2 
java -jar C:\jar\AppleCommander.jar -as dhgr_prodos.dsk tileset1 bin < tileset1.apple2 
java -jar C:\jar\AppleCommander.jar -as dhgr_prodos.dsk map0 bin < map0.apple2 

:: Copy results out of the build directory
copy dhgr_prodos.dsk ..\disk

::---------------------------------------------------------------------------
:: Test on emulator
::---------------------------------------------------------------------------
C:\AppleWin\Applewin.exe -no-printscreen-dlg -d1 dhgr_prodos.dsk

