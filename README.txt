                         The SARIT repository
                         ====================

Author: patrick mc allister
Date: 2011-04-04 20:52:57 CEST


Welcome, you are reading the description of the SARIT archive of Indic
e-texts. SARIT is short for "Search and Retrieval of Indic Texts."
(cf. [http://sarit.indology.info/])

This collection of files is special in that any changes you make to
your files can, if you choose so, be shared with other people. So if
you correct, annotate, or otherwise improve any file in this directory
or one of the directories below it, you might consider sharing your
work with others (and hope that they will share their work with you).

We hope that this will lead to incremental but continuous improvement
of these files, and of files you wish to add.


Table of Contents
=================
1 What to do next? 
    1.1 Installing git 
        1.1.1 Linux 
        1.1.2 MacOS X 
        1.1.3 Recent Windows 
    1.2 Getting a Graphical User Interface (GUI) 
    1.3 Get a copy of the collection you want 
        1.3.1 run `git clone git://indology.info/sarit' if you are using standard git, or 
        1.3.2 or consult the documentation of your git interface on how to point it at  git://indology.info/sarit 
    1.4 Learn about TEI P5 
    1.5 Learn about  git 
2 Where can I get help? 
    2.1 Editing files 
    2.2 Sharing/organizing files 
3 How does sharing work? 
    3.1 You changed different parts of a file 
    3.2 You changed the same part of a file 
4 What are these XML files? 


1 What to do next? 
~~~~~~~~~~~~~~~~~~~

1.1 Installing git 
===================

In order to make full use of the SARIT library, you need to install
[git]. Installation procedures are described in some detail in [Pro Git, chap. 4].

In a nutshell:


[git]: http://git-scm.org/
[Pro Git, chap. 4]: http://progit.org/book/ch1-4.html

1.1.1 Linux 
------------

Use your software manager to install git. On debian, e.g., this would
mean running `aptitude install git-core'.

1.1.2 MacOS X 
--------------

Download and run [Git OSX Installer].


[Git OSX Installer]: http://code.google.com/p/git-osx-installer

1.1.3 Recent Windows 
---------------------

Download and run [Git for Windows]. 


[Git for Windows]: http://code.google.com/p/msysgit/

1.2 Getting a Graphical User Interface (GUI) 
=============================================

The above step provides you with the full git program. If you do not
like working on a command line, you can use a GUI for git. The
standard GUI that is installed automatically by the steps above is
gitk on linux, and git gui on mac and windows.

These are stable but rather basic programs, and there are quite a few
other options you might want to explore if you're not happy with gitk
or git gui. In the following, we will give examples for [Smart Git], a
Java program that is free of charge for non-commercial use, and that
will run any platform that Java runs on (i.e., Linux, Windows, Mac).



[Smart Git]: http://www.syntevo.com/smartgit/index.html

1.3 Get a copy of the collection you want 
==========================================

Getting the first copy is called ``cloning." Depending on your
platform, you can either

1.3.1 run `git clone git://indology.info/sarit' if you are using standard git, or 
----------------------------------------------------------------------------------

1.3.2 or consult the documentation of your git interface on how to point it at  git://indology.info/sarit 
----------------------------------------------------------------------------------------------------------

For [Smart Git], do this:

1) Project --> Clone
2) Under "Remote Git or SVN repository", enter the name of the
   collection, e.g., ``git://sarit.indology.info/sarit", click Next.
3) Choose a directory where to put your files. This should be
   somewhere where you usually store your files, i.e., not in
   ``C:\Programs" or so. Clck Next.
4) Accept or choose the project name (sarit should be the default).

1.4 Learn about TEI P5 
=======================

The full guidelines can be found here:
[http://www.tei-c.org/Guidelines/P5/], and a simplified version, TEI
Lite: [http://www.tei-c.org/Guidelines/Customization/Lite/].

1.5 Learn about  git 
=====================

We have found the following introductions very helpful:

1) Comparing git to a computer game (and a good read):
   [http://www-cs-students.stanford.edu/~blynn/gitmagic/] (notice also
   the various translations)
2) Basic git commands, short and to the point:
   [http://www.kernel.org/pub/software/scm/git/docs/everyday.html]
3) More links can be found here: [http://git-scm.com/documentation]



2 Where can I get help? 
~~~~~~~~~~~~~~~~~~~~~~~~

That depends on the area you need help with:

2.1 Editing files 
==================

You probably do need an [XML aware editor]. There are many, and with
many different features, just as there are many tastes. Some popular
choices include:

1) [Jedit], with appropriate plugins (all platforms with Java installed)
2)  [http://www.oxygenxml.com/] (all platforms, not free)
3) [Emacs] + [nxml mode] (all platforms)

For a list to choose from,
cf. [http://en.wikipedia.org/wiki/List\_of\_XML\_editors] or
[http://www.xml.com/pub/rg/XML\_Editors].


[XML aware editor]: http://en.wikipedia.org/wiki/XML_editor
[Jedit]: http://www.jedit.org/index.php?page%3Ddownload
[Emacs]: http://www.gnu.org/software/emacs/
[nxml mode]: http://www.thaiopensource.com/nxml-mode/
[http://en.wikipedia.org/wiki/List\_of\_XML\_editors]: http://en.wikipedia.org/wiki/List_of_XML_editors
[http://www.xml.com/pub/rg/XML\_Editors]: http://www.xml.com/pub/rg/XML_Editors

2.2 Sharing/organizing files 
=============================

This is done via the program git: see [Learn about git] above for links
to documentation, and see [How does sharing work?] below for a general
description.



[Learn about git]: sec-1_5
[How does sharing work?]: sec-3

3 How does sharing work? 
~~~~~~~~~~~~~~~~~~~~~~~~~

Three steps are involved in sharing these files:

1) Getting what other people changed.
2) Letting other people get what you changed.
3) Merging the changes together.

To do this in an organised fashion, we are using a program called
[git]. It keeps track of changes to the files in this directory, and can
`pull' ([point 1 above]) and `push' ([point 2 above]) from or to another
instance of these files likewise controlled by git. What it pushes are
the changes you have made to these files, and what it pulls are the
changes another person (or a group of other persons) has made to these
files.

When it does this, two things can happen:


[git]: http://git-scm.com/
[point 1 above]: what%20other%20people%20changed
[point 2 above]: get%20what%20you%20changed

3.1 You changed different parts of a file 
==========================================

When, say, Jane corrects paragraph 1, and Jack corrects paragraph 2 of
the same file, git will be able to `merge' ([point 3 above]) . So if
Jack `pulls' Jane's changes, paragraph 1 of his file will
automatically be changed to paragraph 1 of Jane's file. Likewise, if
Jane `pulls' Jack's changes, her file will automatically be changed in
paragraph 2 according to Jack's changes. So they each edited only one
paragraph, but both have the same version of the file now, with both
paragraphs corrected.


[point 3 above]: Merging%20the%20changes

3.2 You changed the same part of a file 
========================================

In case both Jane and Jack change the same part of a file, git will
refuse to `merge' the files (since it doesn't know which change is the
correct one). In this situation, either Jack or Jane will have to
review the other person's changes, and decide which version to keep
(or make a third version that contains the changes of both). After
making these changes, git will understand that either Jack or Jane
have resolved the conflict, and they can continue to work in the
normal fashion.




4 What are these XML files? 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The files in this directory try to adhere to the Text Encoding
Initiatives standards in version P5 ([TEI P5]). These standards define a
vocabulary for describing things about a text: who is its author,
which other texts is it referring to, which page of a printed edition
is this paragraph on, who is "asya" referring to, etc.




[TEI P5]: http://www.tei-c.org/Guidelines/P5/

