# ECM2418 Coursework 2021

This is the template project for the ECM2418 coursework for 2021.
It consists of three folders:
- *src* contains templates for the three main modules: Compiler.hs, Interpreter.hs, and StackMachine.hs.
- *test* contains a simple testsuite which allows you to check that your code satisfies minimum criteria.
- *app* contains the main module which is invoked when the executable is executed.

In addition the project contains three configuration files:
- *package.yaml* to describe project dependencies
- *stack.yaml* to describe execution options

For your assignment you do **only** need to modify files in *src* and *app*. Do **not** change any other files.

The project is setup to be used with the Haskell Tool [Stack](https://docs.haskellstack.org/en/stable/README/) which is a cross-platform program for developing Haskell projects.

## How to install

Stack can be installed on most Unix-like (Un*x) operating systems, including macOS, and on Windows.

For most Un*x operating systems, the easiest way to install is to run:

    curl -sSL https://get.haskellstack.org/ | sh

or

    wget -qO- https://get.haskellstack.org/ | sh

On Windows, you can download and install the Windows 64-bit Installer.
Note that systems with antivirus software may need stack added to the list of "trusted" applications.

For other operating systems and direct downloads, check out the [install and upgrade guide](https://docs.haskellstack.org/en/stable/install_and_upgrade/).

If you already have stack installed, upgrade it to the latest version by running:
    
    stack upgrade

## Executing ghci

To execute the interpreter you can just execute

    stack ghci

from within the *coursework* directory.
This will download the correct version of *ghci* and execute it.

## Executing testcases

To execute the testcases you can execute

    stack test

This will execute all the tests from the *test* directory.

## Executing the project

You can also build the project by executing

    stack build

This will compile the project and produce an executable which executes the *main* method from the Main module (app/Main.hs).
The executable can then be executed with
    
    stack exec coursework-exe