Home page Corentin Dupont
=========================

This is the source code for my website, www.corentindupont.info.
The web site is built with Hakyll, a tool to generate static web sites backed-up on Git (and no database).

It also demonstrates some nice technologies:

- [R-pandoc](http://www.corentindupont.info/blog/posts/Programming/2015-09-14-diagrams.html) for R graphics in blog posts
- [diagrams-pandoc](http://www.corentindupont.info/blog/posts/Programming/2015-09-14-diagrams.html) for diagrams in blog posts
- [pandoc-citeproc](http://www.corentindupont.info/blog/posts/Programming/2014-12-10-This-Blog.html) for bibliography
- [Less CSS](http://lesscss.org/)

Install
-------

Install stack, R and cairo:

```
$ curl -sSL https://get.haskellstack.org/ | sh
$ sudo apt-get install r-base
$ sudo apt-get install libghc-gtk-dev
```

Install and view this website:
```
$ git clone --recursive git@github.com:cdupont/CorentinDupont-WebPage.git
$ cd CorentinDupont-Webpage
$ stack install
$ site build
$ site watch
```

Deploy
------

```
$ docker build -t cdupont2/corentindupont-website .
$ docker push cdupont2/corentindupont-website
$ ecs-cli compose up
```
