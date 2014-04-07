# Instructions for installing custom highlighting in Notepad++

Download language xml file, store in appropriate place.

Open notepad++.

In the menubar, choose "Language".

From the Language drop down menu, choose "Define you own language...".

In the new window that appears:

1. Press the button "Import...".
2. locate the stored xml languagefile.
3. Press it and then press open.

Restart notepad++.

Now a new language should have appeared in the menubars Language menu, it is probably stored near the end of the list.

The dark themes are more updated than the light themes, my theme of choice is "Zenburn".

## Changelog

### prolog_byGurr3_v1.06_darkbg.xml
* 'is' moved from operator to keyword

### prolog_byGurr3_v1.07_darkbg.xml
* 'moved 'div, mod, rem' from operators to keywords'

##Known Problems

* When using a '%' during a function call or printing functions it considers the text behind % to be a comment. I don't know how to fix this without disallowing comments on the same line but behind functions.