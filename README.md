# Instructions
#I am trying to use VI, look! This is the change I have made in order to show that I have made a 
#change that I will then commit and push back up stream! Yay!!!!!
This repository is for CSC348 Winter 2017.

You should check out your personal repository from BitBucket.  Make your changes to the files in
that repository, commit the changes, and then push back to BitBucket.  The instructor will then be
able to review your work.

For all work, you must commit source code that compiles without error.  Submissions where the
source code does not compile will receive 0 points.  If source code compiles correctly, partial
credit may be given for assignments where some of the tests fail.  For Scala assignments, you can
confirm that your source code compiles correctly by running the following command in your repository
directory:

    sbt compile

# Software Requirements

You will need to install:

- Java SE 8 http://www.oracle.com/technetwork/java/javase/downloads/index.html

- Scala Build Tool (SBT) http://www.scala-sbt.org/download.html

# Scala Build Tool (SBT)

The Scala Build Tool (SBT http://www.scala-sbt.org/) is used to compile and test the Scala
assignments.  Files must **NOT** be renamed or refactored, and the test code and build instruction
files must **NOT** be altered.  Specifically, you should only edit files under `src/main/`.

SBT will take care of downloading the correct version of Scala for you.  You will need to be
connected to the Internet when you do this.

## Launch SBT

From a shell window (Linux or OS X) or CMD/Powershell prompt (Windows), launch SBT using the command
you set up during SBT installation.  This is usually just `sbt`.  For example:

    $ sbt
    [info] Loading project definition from /csc348/main/project
    [info] Set current project to CSC348 Assignments (in build file:/csc348/main/)
    >
    
NOTE: you **must** be in the directory that contains the file `build.sbt` and has the directory `src` when you run the `sbt` command.  Use `cd` to change to that directory if necessary.  The `build.sbt` file contains instructions for building and testing the project.  It will compile code into a new `target` subdirectory. 

## Launch Scala Console from SBT

To launch the Scala console from the SBT console, run:

    console

To exit the Scala console and return to the SBT console, enter:

    :quit

You will have access to your source code files in the Scala console.

Note: if you paste into the Scala console, the parser looks for the smallest syntactically correct
expression, and this may not be what you want.  If this becomes problematic, you should either put
curly braces around the body of functions when pasting (this has been done for you already in the
homework assignments), or to investigate the `:paste` command in the Scala interpreter, i.e., type
`:paste`, then paste code in.

## Compiling with SBT

To compile the Scala source code with SBT, run:

    compile
    
To continuously compile, run:

    ~compile
    
For the assignments, you may prefer to simply run the tests as you go.  However, for tricky
exercises, you may wish to edit source code, compile it from the SBT console, and try it out in the
Scala console.

## Testing with SBT

From the SBT console, you can run various commands to compile and run the tests under `src/test`.

When you ask SBT to run any tests, it will automatically compile your source code and the test code
first (if it does not appear to do so, ensure that you have saved your source code file in your text
editor!).

To run all tests from the SBT console:

    test
    
## Offline Work with SBT

You need to have Internet connectivity when you initially ask SBT to compile and test in order to download to libraries.

To work offline in SBT once you have the dependencies, run the following command in the SBT console:

    set offline := true
    
# Text Editor

You should edit source code files with a text editor or IDE.  If you are using Notepad,
Write/Wordpad, or Word to edit code, stop now!

You might consider: 

- http://www.sublimetext.com/ (OS X, Windows, Linux)
- http://notepad-plus-plus.org/ (Windows)

If you want to go old school:

- Emacs
- VIM

Local Variables:
mode: markdown
fill-column: 100
End:
