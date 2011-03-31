                         The SARIT repository
                         ====================

Author: patrick mc allister
Date: 2011-03-31 13:26:38 CEST


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
2 Where can I get help? 
    2.1 Editing files 
    2.2 Sharing/organizing files 
3 How does sharing work? 
    3.1 You changed different parts of a file 
    3.2 You changed the same part of a file 
4 What are these XML files? 


1 What to do next? 
~~~~~~~~~~~~~~~~~~~

2 Where can I get help? 
~~~~~~~~~~~~~~~~~~~~~~~~

That depends on the area you need help with:

2.1 Editing files 
==================

2.2 Sharing/organizing files 
=============================


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

