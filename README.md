# Riviera Client #

Riviera is a software package that interfaces Emacs to DBGp protocol
with which you can debug running scripts interactively.

 * PHP with Xdebug 2.0.0 +

Currently Riviera implements the following features.

 * continuation commands: run/stop/step-in/step-over/step-out
 * set/unset/listing breakpoints
 * expression evaluation
 * STDOUT/STDERR redirection
 * backtrace listing
 * variable inspection

***

### REQUIREMENTS ###


#### Server Side ####
 * DBGp protocol enabled script engine, like:
   * PHP with Xdebug
   * Python/Ruby with Komodo Debugger Extensions


#### Client Side ####
 * Emacs22.1 or later
 * GNU make or similar
 



### INSTALLATION ###

1. Unpack Riviera source code package and change directory to the
   unpacked directory.
2. ``make``
3. ``sudo make install`` or  ``SITELISP=$HOME/path/to/install make install``
4. Insert autoload hooks into your Emacs Configuration file (usually ~/.emacs).
   ``(autoload 'riviera "riviera" "PHP Remote Debugger for Emacs" t)``

***

### DEBUGGING ###

#### Major Mode Key Mapping ####

     space   step into/step over
     i       step into
     o       step over
     r       step out
     g       run
     c       run to cursor
     b       set a breakpoint at a line
     B       set a breakpoint interactively
     u       unset a breakpoint at a line
     U       clear all breakpoints
     \C-c b  display breakpoint list
     >       set redirection mode
     \C-u t  change redirection mode
     d       display backtrace
     t       display backtrace
     v       display context variables
     \C-c f  visit script file
     w       where
     q       stop




#### PHP Single User ####
1. Run Emacs.

2. To start riviera, type: ``M-x riviera``

3. Access to the target PHP script page with any browser.
   You may need to add a query parameter `XDEBUG_SESSION_START` if you
   configured Xdebug to require manual trigger to start a remote
   debugging session.
   e.g.) http://www.example.com/test.php?XDEBUG_SESSION_START=1

4. Soon the server and Riviera establish a debugging session
   connection. Then Emacs loads the script source code of the entry
   page in a buffer.

5. Run some of the commands listed above

6. If you felt you'd debugged enough, it's time to quit Riviera.
   To quit Riviera, type: ``M-x riviera-end``

## Ruby and Python Modes not yet implimented - Please use Beta if they are needed ##




