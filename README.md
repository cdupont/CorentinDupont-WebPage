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

The website is running on an AWS VM.
The following command will upload the website:

```
rsync -avh  _site/* ec2-user@18.206.112.110:~/website/
```

On the VM, it is served by nginx. 
Config file is under `/etc/nginx/conf.d/default.config`.
The certificates are renewed by a crontab in `/etc/cron.d/certbot`.
If the VM is restarted from scratch, is should reconfigure using the VM `user data` startup script.


