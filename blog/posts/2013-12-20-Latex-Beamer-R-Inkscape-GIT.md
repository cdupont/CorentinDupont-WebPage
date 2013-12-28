---
title: My new tool-chain: LaTeX, Beamer, Inkscape, R and Git instead of Word, Excel and PowerPoint
description: I just revolutioned my tool-chain to create documents and presentations...
tags: Tools
---

Did you ever dreamed of getting rid of MS Office? I had a great revolution in the way I work and produce documents. 
In fact, I discovered... LaTeX! It works completely differently than "What You See Is What You Get" word processors, like MS Word.
In LaTeX, you write your document in a source file (like when programming), that you compile to obtain a PDF.

# [LaTeX](http://latex-project.org) for documents

LaTeX has several advantages over Word:

* LaTeX allows to separate distinctly the content from the style. In Word content and style are somewhat mixed: if you want to change the style of all the tables in a MS Word document (for example to put a double border), you will have to change the style of all tables manually. In LaTeX, this would be done in one place only (the class file or the preamble).
* the LaTeX file is plain text: anybody can edit it with a simple text editor. While with Word the .doc file is encrypted: you are at the mercy of a bug that can make you lose your work!
* you can use a version control system like GIT, and work in team comfortably. With Word, working in team is always awkward: if two persons work concurrently, the merging must be done manually, and work can be lost (this happens often with concurrent edits).
* pictures and graphics are linked in the LaTeX file, not copy/pasted: the original picture or graphic file will be used when compiling. This way, if you change something in the picture, it will automatically be updated in the final document. In Word, if the picture has been copy/pasted, you will probably not be able to modify it.

If you are writing a paper in LaTeX, you will find class files for the publication you aim at (for example ACM or IEEE have their own class files).
This is very practical: you just have to write the raw content of the paper, LaTeX with the appropriate class file will render your paper just the right way.
For equations, there is a built-in language: it looks very natural, you can learn it in an hour.
LaTeX embeds also a great tool to generate a bibliography/reference section (this is very painful to do in Word).

# [Beamer](http://en.wikibooks.org/wiki/LaTeX/Presentations) for presentations

When you will want to make a presentation for your paper, there is Beamer.
To produce your presentation, just create a new LaTeX file in the same folder than your paper, and include the beamer class file instead of the paper class file.
Your presentation can include the same pictures than the paper. If you change a picture, both the paper and the presentation will be updated! No copy/paste is necessary. Never repeat yourself!  
The beamer presentations looks very good. These are a lot of neat things that are generated automatically, such as a table of contents with an highlight on the current section on each page header.


# [Inkscape](http://inkscape.org) for pictures

Inkscape is a free vector drawing program.
It is very important to include only vector graphics in your documents, and no bitmap graphic: this way, the printing quality will be maximum (you can zoom indefinitely on a vector graphic).
Furthermore, a vector graphic can be opened and modified easily, since it's storing the geometrical primitives of your graphic. 
Inkscape produces SVG files, which is an XML based standard for vector graphics. So it's all text based!
For a small modifications like changing a word or two in a legend, you can open the file and modify it manually. 

# [R](http://www.r-project.org/) for graphs

R is a tool that allows you to generate complex graphs. It can replace advantageously Excel. It's also text based.


# [Git](http://git-scm.com/) to rule them all
 
When programming, Git is a must when it comes to version control and working in team. Why not for documents?
The idea is to put all the LaTeX, Inkscape and R source files on a [GitHub](https://github.com/) repository.
Since all the files are text baed, you will be able to see the differences for each commits.
This gives you a great control over the evolution of your document, especially when working in team.

# Conclusion

The LaTeX/Inkscape/R/Git combo (or a part of those tools) has tremendous advantages over MS Office.
It allows you much greater control over your work, and much less probability of screwing things up.
And the rendering is so nice!
