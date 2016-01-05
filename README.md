Nāga: Erlang Web Framework (beta)
=================================

#Overview

Naga framework is a web development framework written in erlang which implements the server-side MVC pattern. [Nag](http://github.com/naga-framework/naga) is build on top of [N2O](http://synrc.com/) framework, many of  its components and concepts are from [ChicagoBoss](http://chicagoboss.org).

Naga provides the best of both framework - [ChicagoBoss](http://chicagoboss.org) and  [N2O](http://synrc.com/), for implementing realtime features in modern web HTML5 application.

# Requirements

- [Erlang/OTP](http://www.erlang.org) version >= 18,  require maps

# Get Started

  to create and build a naga application you need to install [mad](https://github.com/naga-framework/mad.git).

## Downloading

You can download mad directly from github:

    $ curl -O https://raw.githubusercontent.com/naga-framework/mad/naga_v2/mad

Then ensure it is executable

    $ chmod a+x mad

and drop it in your $PATH


## Build mad from source

```bash
    $ git clone https://github.com/naga-framework/mad.git
    $ cd mad
    $ make    
```

and drop it in your $PATH


## Create a Naga app:

  mad integrate a small template engine.

    $ mad create name=<app_name> tpl=<tpl_name> port=<9001>

```bash
mad create name=hello tpl=basic port=9000
Name = "hello"
TplName = "basic"
Vars = [{port,"9000"},{tpl,"basic"},{name,"hello"}]
Writing hello/apps/hello/rebar.config
Writing hello/apps/rebar.config
Writing hello/rebar.config
Writing hello/vm.args
Writing hello/sys.config
Writing hello/apps/hello/src/hello.app.src
Writing hello/apps/hello/src/hello.erl
Writing hello/apps/hello/src/controller/index.erl
Writing hello/apps/hello/src/controller/error.erl
Writing hello/apps/hello/src/hello_routes.erl
Writing hello/apps/hello/priv/hello.routes
Writing hello/apps/hello/src/view/layout/n2o.html
Writing hello/apps/hello/src/view/layout/page.html
Writing hello/apps/hello/src/view/index/basic.html
Failed to write output file "hello/apps/hello/src/view/index/basic.html": badarg
Writing hello/apps/hello/src/view/error/404.html
Writing hello/apps/hello/priv/templates/index.html
Writing hello/apps/hello/priv/static/synrc.css
Writing hello/apps/hello/priv/static/spa/index.htm
OK
```  
## start to develop

```shell
cd hello
mad clean deps comp plan repl -name hello@127.0.0.1
```
