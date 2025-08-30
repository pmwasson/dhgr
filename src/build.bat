::---------------------------------------------------------------------------
:: Compile code
::   Assemble twice: 1 to generate listing, 2 to generate object
::---------------------------------------------------------------------------
cd ..\build

:: Toolbox
ca65 -I ..\src -t apple2 ..\src\dhgr.asm -l dhgr.dis
cl65 -I ..\src -t apple2 -u __EXEHDR__ ..\src\dhgr.asm apple2.lib  -o dhgr.apple2 -C ..\src\start4000.cfg

:: Game
ca65 -I ..\src -t apple2 ..\src\game.asm -l game.dis
cl65 -I ..\src -t apple2 -u __EXEHDR__ ..\src\game.asm apple2.lib  -o game.apple2 -C ..\src\start6000.cfg

:: Engine
ca65 -I ..\src -t apple2 ..\src\engine.asm -l engine.dis
cl65 -I ..\src -t apple2 -u __EXEHDR__ ..\src\engine.asm apple2.lib  -o engine.apple2 -C ..\src\startC00.cfg

:: Loader
ca65 -I ..\src -t apple2 ..\src\loader.asm -l loader.dis
cl65 -I ..\src -t apple2 -u __EXEHDR__ ..\src\loader.asm apple2.lib  -o loader.apple2 -C ..\src\start2000.cfg

::---------------------------------------------------------------------------
:: Compile example
::---------------------------------------------------------------------------
cl65 -I ..\src -t apple2 -u __EXEHDR__ ..\src\tileset14x16_0.asm apple2.lib  -o tileset14x16_0.apple2 -C ..\src\start6000.cfg
cl65 -I ..\src -t apple2 -u __EXEHDR__ ..\src\tileset14x16_1.asm apple2.lib  -o tileset14x16_1.apple2 -C ..\src\start6000.cfg
cl65 -I ..\src -t apple2 -u __EXEHDR__ ..\src\tileset14x16_2.asm apple2.lib  -o tileset14x16_2.apple2 -C ..\src\start6000.cfg
cl65 -I ..\src -t apple2 -u __EXEHDR__ ..\src\tileset14x16_3.asm apple2.lib  -o tileset14x16_3.apple2 -C ..\src\start6000.cfg

cl65 -I ..\src -t apple2 -u __EXEHDR__ ..\src\tileset7x8_0.asm apple2.lib  -o tileset7x8_0.apple2 -C ..\src\start6000.cfg
cl65 -I ..\src -t apple2 -u __EXEHDR__ ..\src\tileset7x8_1.asm apple2.lib  -o tileset7x8_1.apple2 -C ..\src\start6000.cfg
cl65 -I ..\src -t apple2 -u __EXEHDR__ ..\src\tileset7x8_2.asm apple2.lib  -o tileset7x8_2.apple2 -C ..\src\start6000.cfg
cl65 -I ..\src -t apple2 -u __EXEHDR__ ..\src\tileset7x8_3.asm apple2.lib  -o tileset7x8_3.apple2 -C ..\src\start6000.cfg

cl65 -I ..\src -t apple2 -u __EXEHDR__ ..\src\map_0.asm     apple2.lib  -o map_0.apple2     -C ..\src\start6000.cfg

::---------------------------------------------------------------------------
:: Build disk 
::---------------------------------------------------------------------------

:: Start with a blank prodos disk
copy ..\disk\template_prodos.dsk dhgr_prodos.dsk

:: Put boot program first

:: Loader
java -jar C:\jar\AppleCommander.jar -p  dhgr_prodos.dsk loader.system sys < C:\cc65\target\apple2\util\loader.system
java -jar C:\jar\AppleCommander.jar -as dhgr_prodos.dsk loader bin < loader.apple2 

:: Toolbox
java -jar C:\jar\AppleCommander.jar -p  dhgr_prodos.dsk dhgr.system sys < C:\cc65\target\apple2\util\loader.system
java -jar C:\jar\AppleCommander.jar -as dhgr_prodos.dsk dhgr bin < dhgr.apple2 

:: Engine
:: java -jar C:\jar\AppleCommander.jar -p  dhgr_prodos.dsk engine.system sys < C:\cc65\target\apple2\util\loader.system
java -jar C:\jar\AppleCommander.jar -as dhgr_prodos.dsk data/engine bin < engine.apple2 

:: Game
:: java -jar C:\jar\AppleCommander.jar -p  dhgr_prodos.dsk game.system sys < C:\cc65\target\apple2\util\loader.system
java -jar C:\jar\AppleCommander.jar -as dhgr_prodos.dsk data/game bin < game.apple2 

:: Throw on basic
java -jar C:\jar\AppleCommander.jar -p dhgr_prodos.dsk basic.system sys < ..\disk\BASIC.SYSTEM 

:: Add samples
java -jar C:\jar\AppleCommander.jar -as dhgr_prodos.dsk data/tileset14x16.0 bin < tileset14x16_0.apple2 
java -jar C:\jar\AppleCommander.jar -as dhgr_prodos.dsk data/tileset14x16.1 bin < tileset14x16_1.apple2 
java -jar C:\jar\AppleCommander.jar -as dhgr_prodos.dsk data/tileset14x16.2 bin < tileset14x16_2.apple2 
java -jar C:\jar\AppleCommander.jar -as dhgr_prodos.dsk data/tileset14x16.3 bin < tileset14x16_3.apple2
java -jar C:\jar\AppleCommander.jar -as dhgr_prodos.dsk data/tileset7x8.0 bin < tileset7x8_0.apple2 
java -jar C:\jar\AppleCommander.jar -as dhgr_prodos.dsk data/tileset7x8.1 bin < tileset7x8_1.apple2 
java -jar C:\jar\AppleCommander.jar -as dhgr_prodos.dsk data/tileset7x8.2 bin < tileset7x8_2.apple2 
java -jar C:\jar\AppleCommander.jar -as dhgr_prodos.dsk data/tileset7x8.3 bin < tileset7x8_3.apple2
java -jar C:\jar\AppleCommander.jar -as dhgr_prodos.dsk data/map.0 bin < map_0.apple2 

:: Copy results out of the build directory
copy dhgr_prodos.dsk ..\disk

::---------------------------------------------------------------------------
:: Test on emulator
::---------------------------------------------------------------------------
C:\AppleWin\Applewin.exe -no-printscreen-dlg -d1 dhgr_prodos.dsk

