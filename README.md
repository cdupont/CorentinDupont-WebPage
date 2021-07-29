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

The following commands can be used to deploy the website:
```
$ docker build -t cdupont2/corentindupont-website .
$ docker push cdupont2/corentindupont-website
$ ecs-cli compose up
```

It is necessary to install and configure ecs-cli:
```
curl -o /usr/local/bin/ecs-cli https://amazon-ecs-cli.s3.amazonaws.com/ecs-cli-linux-amd64-latest
chmod a+x /usr/local/bin/ecs-cli
ecs-cli configure profile --profile-name cdupont --access-key <key> --secret-key <secret>
ecs-cli configure --cluster Nomyx --default-launch-type EC2 --region us-east-1 --config-name nomyx
